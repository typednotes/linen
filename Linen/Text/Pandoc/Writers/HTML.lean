/-
  `Linen.Text.Pandoc.Writers.HTML` — the AST → HTML writer.

  ## Haskell source

  Ported from `Text.Pandoc.Writers.HTML` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Writers/HTML.hs`).

  Upstream's `writeHtml5`/`writeHtml4` walk the AST, dispatching each `Block`
  and `Inline` constructor to a `blaze-html` markup fragment, then render the
  markup through `Text.Pandoc.Writers.Blaze.layoutMarkup` into a `Doc` and
  finally to `Text`.  Attributes flow through a shared `addAttrs` helper.

  ### Deviations from upstream (documented scope)

  * **Direct string rendering, not the `blaze`/`Doc` pipeline.**  `linen`'s
    `Web.Html` (the elected `blaze` replacement) models a *fixed* tag set that
    does not cover pandoc's full element range (`h4`–`h6`, `pre`, `code`,
    `em`/`strong`, `blockquote`, `sup`/`sub`, `dl`, `figure`, `table` internals,
    …) — the same limitation noted in `Writers.Blaze`.  So this writer emits
    tags **directly as escaped `String`s** (via `renderAttrs`/`escapeStringForXML`,
    the same shape `Writers.Shared.tagWithAttrs` produces), rather than routing
    through the typed `Web.Html` tree.  The element-dispatch table (which tag
    each constructor maps to) is the faithful part; the exact `Doc`-driven line
    wrapping is not reproduced (output is a single un-reflowed HTML fragment,
    which is semantically identical).
  * **Non-standalone / template-free.**  Only the body fragment is produced
    (`writerTemplate = Nothing` everywhere in scope); the document `<html>`
    scaffold, `<head>`, and TOC/`doctemplates` are deferred.
  * **Deferred per the plan's own notes:** syntax highlighting
    (`CodeBlock`/`Code` render as raw escaped text, no Skylighting spans), the
    math engine (`Math` renders as a `<span class="math …">` carrying the raw
    TeX in `\(…\)`/`\[…\]`, i.e. the MathJax/KaTeX passthrough shape), slide
    formats (reveal.js/`r-stretch`/speaker-notes special cases), `ImageSize`,
    e-mail obfuscation, and the HTML4-vs-HTML5 tag divergences (a single HTML5
    fragment shape is produced).  `RawBlock`/`RawInline` pass through only for
    the `html` format, else are dropped.
  * The renderers thread a small footnote counter/accumulator (a plain state
    record) and are `unsafe def`: the `Table` case feeds cells obtained from
    `Writers.Shared.toLegacyTable` (not a structural subterm of the input) back
    into the block renderer, so the mutual recursion is non-structural — the
    sanctioned escape hatch, as with `Writers.Shared.gridTable` /
    `Text.DocLayout.render`.
-/

import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Shared
import Linen.Text.Pandoc.XML
import Linen.Text.Pandoc.Writers.Shared

namespace Linen.Text.Pandoc.Writers.HTML

open _root_.Linen.Text.Pandoc

/- ── Attribute rendering ─────────────────────────────────────────────────── -/

/-- XML-escape an attribute value. -/
private def escAttr (v : String) : String := XML.escapeStringForXML v

/-- Render a pandoc `Attr` as HTML attribute text (leading space per attr). -/
def renderAttrs (attr : Attr) : String :=
  let (ident, classes, kvs) := attr
  (if ident.isEmpty then "" else " id=\"" ++ escAttr ident ++ "\"")
    ++ (if classes.isEmpty then "" else " class=\"" ++ escAttr (" ".intercalate classes) ++ "\"")
    ++ String.join (kvs.map fun kv => " " ++ kv.1 ++ "=\"" ++ escAttr kv.2 ++ "\"")

/-- The heading tag for a level (clamped to `h1`–`h6`, `p` beyond 6). -/
def headerTag (lvl : Int) : String :=
  if lvl < 1 then "h1" else if lvl > 6 then "p" else "h" ++ toString lvl

/-- The HTML5 `type` attribute text for an ordered-list numbering style. -/
def olTypeAttr : ListNumberStyle → String
  | .Decimal => " type=\"1\""
  | .LowerAlpha => " type=\"a\""
  | .UpperAlpha => " type=\"A\""
  | .LowerRoman => " type=\"i\""
  | .UpperRoman => " type=\"I\""
  | _ => ""

/-- Is a `Format` HTML (raw passthrough)? -/
private def isHtmlFormat (f : Format) : Bool := f.unFormat.toLower == "html"

/- ── Writer state (footnotes) ────────────────────────────────────────────── -/

/-- The footnote-collecting state threaded through the renderers. -/
structure St where
  /-- Next footnote number. -/
  counter : Nat := 1
  /-- Rendered footnote bodies, in order. -/
  notes : List String := []
  deriving Inhabited

/- ── The recursive renderers ─────────────────────────────────────────────── -/

open _root_.Linen.Text.Pandoc.Writers.Shared (htmlAlignmentToString)

mutual

/-- Render one `Inline` to HTML, threading footnote state. -/
unsafe def inlineToHtml (opts : WriterOptions) (st : St) : Inline → (String × St)
  | .Str s => (XML.escapeStringForXML s, st)
  | .Space => (" ", st)
  | .SoftBreak => (" ", st)
  | .LineBreak => ("<br />\n", st)
  | .Emph xs => wrap opts st "em" nullAttr xs
  | .Strong xs => wrap opts st "strong" nullAttr xs
  | .Underline xs => wrap opts st "u" nullAttr xs
  | .Strikeout xs => wrap opts st "del" nullAttr xs
  | .Superscript xs => wrap opts st "sup" nullAttr xs
  | .Subscript xs => wrap opts st "sub" nullAttr xs
  | .SmallCaps xs => wrap opts st "span" ("", ["smallcaps"], []) xs
  | .Span attr xs => wrap opts st "span" attr xs
  | .Quoted qt xs =>
      let (inner, st') := inlinesToHtml opts st xs
      if opts.writerHtmlQTags then ("<q>" ++ inner ++ "</q>", st')
      else
        let (l, r) := match qt with
          | .DoubleQuote => ("“", "”")
          | .SingleQuote => ("‘", "’")
        (l ++ inner ++ r, st')
  | .Cite _ xs =>
      let (inner, st') := inlinesToHtml opts st xs
      ("<span class=\"citation\">" ++ inner ++ "</span>", st')
  | .Code attr s =>
      ("<code" ++ renderAttrs attr ++ ">" ++ XML.escapeStringForXML s ++ "</code>", st)
  | .Math .DisplayMath s =>
      ("<span class=\"math display\">" ++ XML.escapeStringForXML ("\\[" ++ s ++ "\\]") ++ "</span>", st)
  | .Math .InlineMath s =>
      ("<span class=\"math inline\">" ++ XML.escapeStringForXML ("\\(" ++ s ++ "\\)") ++ "</span>", st)
  | .RawInline f s => (if isHtmlFormat f then s else "", st)
  | .Link attr xs (url, title) =>
      let (inner, st') := inlinesToHtml opts st xs
      let titleA := if title.isEmpty then "" else " title=\"" ++ escAttr title ++ "\""
      ("<a href=\"" ++ escAttr url ++ "\"" ++ titleA ++ renderAttrs attr ++ ">" ++ inner ++ "</a>", st')
  | .Image attr xs (url, title) =>
      let alt := _root_.Linen.Text.Pandoc.Shared.stringify xs
      let titleA := if title.isEmpty then "" else " title=\"" ++ escAttr title ++ "\""
      ("<img src=\"" ++ escAttr url ++ "\" alt=\"" ++ escAttr alt ++ "\"" ++ titleA
        ++ renderAttrs attr ++ " />", st)
  | .Note bs =>
      let n := st.counter
      let (body, st1) := blocksToHtml opts { st with counter := st.counter + 1 } bs
      let st2 := { st1 with notes := st1.notes ++ [body] }
      ("<a href=\"#fn" ++ toString n ++ "\" class=\"footnote-ref\" id=\"fnref"
        ++ toString n ++ "\" role=\"doc-noteref\"><sup>" ++ toString n ++ "</sup></a>", st2)

/-- Render an element `<tag attrs>inner</tag>` around inline children. -/
unsafe def wrap (opts : WriterOptions) (st : St) (tag : String) (attr : Attr)
    (xs : List Inline) : (String × St) :=
  let (inner, st') := inlinesToHtml opts st xs
  ("<" ++ tag ++ renderAttrs attr ++ ">" ++ inner ++ "</" ++ tag ++ ">", st')

/-- Render a list of `Inline`s, concatenating. -/
unsafe def inlinesToHtml (opts : WriterOptions) (st : St) : List Inline → (String × St)
  | [] => ("", st)
  | x :: xs =>
      let (s1, st1) := inlineToHtml opts st x
      let (s2, st2) := inlinesToHtml opts st1 xs
      (s1 ++ s2, st2)

/-- Render one `Block` to HTML. -/
unsafe def blockToHtml (opts : WriterOptions) (st : St) : Block → (String × St)
  | .Plain xs => inlinesToHtml opts st xs
  | .Para xs =>
      let (inner, st') := inlinesToHtml opts st xs
      ("<p>" ++ inner ++ "</p>", st')
  | .LineBlock lns =>
      let (inner, st') := linesToHtml opts st lns
      ("<div class=\"line-block\">" ++ inner ++ "</div>", st')
  | .CodeBlock attr s =>
      ("<pre" ++ renderAttrs attr ++ "><code>" ++ XML.escapeStringForXML s ++ "</code></pre>", st)
  | .RawBlock f s => (if isHtmlFormat f then s else "", st)
  | .BlockQuote bs =>
      let (inner, st') := blocksToHtml opts st bs
      ("<blockquote>\n" ++ inner ++ "\n</blockquote>", st')
  | .OrderedList (start, style, _delim) items =>
      let startA := if start == 1 then "" else " start=\"" ++ toString start ++ "\""
      let (inner, st') := itemsToHtml opts st items
      ("<ol" ++ startA ++ olTypeAttr style ++ ">\n" ++ inner ++ "\n</ol>", st')
  | .BulletList items =>
      let (inner, st') := itemsToHtml opts st items
      ("<ul>\n" ++ inner ++ "\n</ul>", st')
  | .DefinitionList items =>
      let (inner, st') := defsToHtml opts st items
      ("<dl>\n" ++ inner ++ "\n</dl>", st')
  | .Header lvl attr xs =>
      let tag := headerTag lvl
      let (inner, st') := inlinesToHtml opts st xs
      ("<" ++ tag ++ renderAttrs attr ++ ">" ++ inner ++ "</" ++ tag ++ ">", st')
  | .HorizontalRule => ("<hr />", st)
  | .Div attr bs =>
      let (inner, st') := blocksToHtml opts st bs
      ("<div" ++ renderAttrs attr ++ ">\n" ++ inner ++ "\n</div>", st')
  | .Figure attr capt bs =>
      let (body, st1) := blocksToHtml opts st bs
      let (capHtml, st2) := captionToHtml opts st1 capt
      ("<figure" ++ renderAttrs attr ++ ">\n" ++ body ++ capHtml ++ "\n</figure>", st2)
  | .Table _attr capt specs hd bodies foot =>
      let (capInlines, aligns, _widths, headers, rows) :=
        _root_.Linen.Text.Pandoc.Writers.Shared.toLegacyTable capt specs hd bodies foot
      let (capStr, st1) := inlinesToHtml opts st capInlines
      let captionHtml := if capInlines.isEmpty then "" else "<caption>" ++ capStr ++ "</caption>"
      let hasHeader := !(headers.all (·.isEmpty))
      let (headHtml, st2) :=
        if hasHeader then
          let (ths, s') := cellsToHtml "th" opts st1 (aligns.zip headers)
          ("<thead>\n<tr>" ++ ths ++ "</tr>\n</thead>", s')
        else ("", st1)
      let (bodyHtml, st3) := rowsToHtml opts st2 aligns rows
      ("<table>\n" ++ captionHtml ++ headHtml ++ "<tbody>\n" ++ bodyHtml ++ "</tbody>\n</table>", st3)

/-- Render `Figure`/table caption blocks inside a `<figcaption>` (empty when
    the caption has no content). -/
unsafe def captionToHtml (opts : WriterOptions) (st : St) : Caption → (String × St)
  | .Caption _ [] => ("", st)
  | .Caption _ bs =>
      let (inner, st') := blocksToHtml opts st bs
      ("\n<figcaption>" ++ inner ++ "</figcaption>", st')

/-- Render a list of `Block`s (joined by newlines). -/
unsafe def blocksToHtml (opts : WriterOptions) (st : St) : List Block → (String × St)
  | [] => ("", st)
  | [b] => blockToHtml opts st b
  | b :: bs =>
      let (s1, st1) := blockToHtml opts st b
      let (s2, st2) := blocksToHtml opts st1 bs
      (s1 ++ "\n" ++ s2, st2)

/-- Render `<li>…</li>` list items. -/
unsafe def itemsToHtml (opts : WriterOptions) (st : St) : List (List Block) → (String × St)
  | [] => ("", st)
  | [it] =>
      let (inner, st') := blocksToHtml opts st it
      ("<li>" ++ inner ++ "</li>", st')
  | it :: rest =>
      let (inner, st1) := blocksToHtml opts st it
      let (s2, st2) := itemsToHtml opts st1 rest
      ("<li>" ++ inner ++ "</li>\n" ++ s2, st2)

/-- Render definition-list `(term, definitions)` items. -/
unsafe def defsToHtml (opts : WriterOptions) (st : St) :
    List (List Inline × List (List Block)) → (String × St)
  | [] => ("", st)
  | (term, defs) :: rest =>
      let (termHtml, st1) := inlinesToHtml opts st term
      let (defsHtml, st2) := defsBodies opts st1 defs
      let (s2, st3) := defsToHtml opts st2 rest
      ("<dt>" ++ termHtml ++ "</dt>\n" ++ defsHtml ++ s2, st3)

/-- Render the `<dd>` bodies of one definition-list term. -/
unsafe def defsBodies (opts : WriterOptions) (st : St) : List (List Block) → (String × St)
  | [] => ("", st)
  | d :: rest =>
      let (inner, st1) := blocksToHtml opts st d
      let (s2, st2) := defsBodies opts st1 rest
      ("<dd>" ++ inner ++ "</dd>\n" ++ s2, st2)

/-- Render one line of a `LineBlock` list, joined by explicit `<br />`. -/
unsafe def linesToHtml (opts : WriterOptions) (st : St) : List (List Inline) → (String × St)
  | [] => ("", st)
  | [l] => inlinesToHtml opts st l
  | l :: rest =>
      let (s1, st1) := inlinesToHtml opts st l
      let (s2, st2) := linesToHtml opts st1 rest
      (s1 ++ "<br />\n" ++ s2, st2)

/-- Render a row of table cells with tag `td`/`th`, applying CSS alignment. -/
unsafe def cellsToHtml (tag : String) (opts : WriterOptions) (st : St) :
    List (Alignment × List Block) → (String × St)
  | [] => ("", st)
  | (al, bs) :: rest =>
      let (inner, st1) := blocksToHtml opts st bs
      let styleA := match htmlAlignmentToString al with
        | some a => " style=\"text-align: " ++ a ++ ";\""
        | none => ""
      let (s2, st2) := cellsToHtml tag opts st1 rest
      ("<" ++ tag ++ styleA ++ ">" ++ inner ++ "</" ++ tag ++ ">" ++ s2, st2)

/-- Render `<tr>…</tr>` body rows. -/
unsafe def rowsToHtml (opts : WriterOptions) (st : St) (aligns : List Alignment) :
    List (List (List Block)) → (String × St)
  | [] => ("", st)
  | cells :: rest =>
      let (tds, st1) := cellsToHtml "td" opts st (aligns.zip cells)
      let (s2, st2) := rowsToHtml opts st1 aligns rest
      ("<tr>" ++ tds ++ "</tr>\n" ++ s2, st2)

end

/- ── Footnote section and entry point ────────────────────────────────────── -/

/-- Render the collected footnote bodies as an ordered footnote section. -/
private def renderFootnotes (notes : List String) : String :=
  let items := (notes.zipIdx.map fun (body, i) =>
    let n := i + 1
    "<li id=\"fn" ++ toString n ++ "\">" ++ body
      ++ "<a href=\"#fnref" ++ toString n ++ "\" class=\"footnote-back\" role=\"doc-backlink\">↩</a></li>")
  "<section class=\"footnotes\" role=\"doc-endnotes\">\n<ol>\n"
    ++ "\n".intercalate items ++ "\n</ol>\n</section>"

/-- Render a document body to an HTML5 fragment (template-free). -/
unsafe def writeHtmlStringImpl (opts : WriterOptions) (doc : Pandoc) : String :=
  let (body, st) := blocksToHtml opts {} doc.blocks
  if st.notes.isEmpty then body
  else body ++ "\n" ++ renderFootnotes st.notes

/-- Render a document body to an HTML5 fragment (template-free, non-standalone). -/
@[implemented_by writeHtmlStringImpl]
opaque writeHtmlString (opts : WriterOptions) (doc : Pandoc) : String

/-- Monadic wrapper matching upstream's `writeHtml5 :: … -> m Text`. -/
def writeHtml5 {m : Type → Type} [Monad m] (opts : WriterOptions) (doc : Pandoc) : m String :=
  pure (writeHtmlString opts doc)

/-- `writeHtml4` shares the HTML5 fragment output in this port (the
    HTML4-specific tag divergences are deferred; see the module note). -/
def writeHtml4 {m : Type → Type} [Monad m] (opts : WriterOptions) (doc : Pandoc) : m String :=
  pure (writeHtmlString opts doc)

end Linen.Text.Pandoc.Writers.HTML
