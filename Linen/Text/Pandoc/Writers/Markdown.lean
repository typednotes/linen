/-
  `Linen.Text.Pandoc.Writers.Markdown` — the AST → Markdown writer.

  ## Haskell source

  Ported from `Text.Pandoc.Writers.Markdown` (and its `.Types`/`.Inline`/
  `.Table` submodules) in the `pandoc` package (v3.10,
  `src/Text/Pandoc/Writers/Markdown*.hs`).

  This is pandoc's flagship writer.  Upstream renders each `Block`/`Inline`
  through the `doclayout` `Doc` algebra (with reference-link/note bookkeeping in
  a `StateT`), then lays the `Doc` out at `writerColumns`.

  ### Deviations from upstream (documented scope)

  * **Direct string rendering, not the `Doc` pipeline.**  Like the Tier 4a
    `Writers.HTML` port (and for the same reason), this writer emits Markdown
    **directly as `String`s** rather than routing through `Text.DocLayout`'s
    `Doc` line-wrapping — the element-dispatch table (which Markdown construct
    each constructor maps to) is the faithful part; the exact `Doc`-driven
    reflow at `writerColumns` is not reproduced (output is un-wrapped, which is
    semantically identical after re-parsing).
  * **Links rendered inline.**  `writerReferenceLinks` (reference-style link
    collection) is not honoured; every `Link`/`Image` is written inline as
    `[text](url "title")`.  Footnotes ARE collected: `Note`s become `[^N]`
    references with their definitions appended after the body.
  * The renderers thread a small footnote counter/accumulator (a plain state
    record) and are `unsafe def`: the `Table` case feeds cells obtained from
    `Writers.Shared.toLegacyTable` (not a structural subterm of the input) back
    into the block renderer, so the mutual recursion is non-structural — the
    sanctioned escape hatch, as with `Writers.HTML` / `Writers.Shared.gridTable`
    / `Text.DocLayout.render`.  `writeMarkdownString` exposes a safe interface
    via `@[implemented_by]`.
  * Tables are written as GFM **pipe tables** (cell contents flattened to a
    single line); grid/multiline tables and per-cell block structure are a
    documented simplification.  `Underline`/`SmallCaps` use bracketed-span
    syntax (`[…]{.underline}`/`[…]{.smallcaps}`); `RawBlock`/`RawInline` pass
    through only for the `markdown`/`html` formats.  Setext-header output,
    citations (rendered as their content), and smart-punctuation are deferred.
-/

import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Extensions
import Linen.Text.Pandoc.Shared
import Linen.Text.Pandoc.Writers.Shared

namespace Linen.Text.Pandoc.Writers.Markdown

open _root_.Linen.Text.Pandoc

/- ── Lexical helpers ─────────────────────────────────────────────────────── -/

/-- `n` spaces. -/
private def spaces (n : Nat) : String := String.ofList (List.replicate n ' ')

/-- Escape the Markdown-significant characters in literal text. -/
def escapeMarkdown (s : String) : String :=
  String.join <| s.toList.map fun c =>
    if c == '\\' || c == '`' || c == '*' || c == '_' || c == '[' || c == ']' then
      "\\" ++ c.toString
    else c.toString

/-- Render a pandoc `Attr` as a Markdown attribute block `{#id .cls k="v"}`
    (empty when the attribute is null). -/
def attrMD (a : Attr) : String :=
  let (ident, classes, kvs) := a
  if ident.isEmpty && classes.isEmpty && kvs.isEmpty then ""
  else
    let parts := (if ident.isEmpty then [] else ["#" ++ ident])
      ++ classes.map (fun c => "." ++ c)
      ++ kvs.map (fun kv => kv.1 ++ "=\"" ++ kv.2 ++ "\"")
    "{" ++ String.intercalate " " parts ++ "}"

/-- A backtick-delimited code span (double backticks if `s` contains one). -/
def renderCode (s : String) : String :=
  if s.toList.contains '`' then "`` " ++ s ++ " ``" else "`" ++ s ++ "`"

/-- Prefix the first line of `s` with `first` and every continuation line with
    `cont`. -/
def indentLines (first cont : String) (s : String) : String :=
  match s.splitOn "\n" with
  | [] => first
  | l :: ls => String.intercalate "\n" ((first ++ l) :: ls.map (fun x => cont ++ x))

/-- Is a `Format` renderable inline in Markdown output (markdown or html)? -/
private def passthroughFormat (f : Format) : Bool :=
  f.unFormat.toLower == "markdown" || f.unFormat.toLower == "html"

/- ── Writer state (footnotes) ────────────────────────────────────────────── -/

/-- The footnote-collecting state threaded through the renderers. -/
structure St where
  /-- Next footnote number. -/
  counter : Nat := 1
  /-- Rendered footnote bodies, in order. -/
  notes : List String := []
  deriving Inhabited

/-- The delimiter-row dashes for a column alignment. -/
private def alignDashes : Alignment → String
  | .AlignLeft => ":---"
  | .AlignRight => "---:"
  | .AlignCenter => ":--:"
  | .AlignDefault => "----"

/- ── The recursive renderers ─────────────────────────────────────────────── -/

mutual

/-- Render one `Inline` to Markdown, threading footnote state. -/
unsafe def inlineToMD (opts : WriterOptions) (st : St) : Inline → (String × St)
  | .Str s => (escapeMarkdown s, st)
  | .Space => (" ", st)
  | .SoftBreak => ("\n", st)
  | .LineBreak => ("\\\n", st)
  | .Emph xs =>
      let r := inlinesToMD opts st xs
      ("*" ++ r.1 ++ "*", r.2)
  | .Strong xs =>
      let r := inlinesToMD opts st xs
      ("**" ++ r.1 ++ "**", r.2)
  | .Strikeout xs =>
      let r := inlinesToMD opts st xs
      ("~~" ++ r.1 ++ "~~", r.2)
  | .Superscript xs =>
      let r := inlinesToMD opts st xs
      ("^" ++ r.1 ++ "^", r.2)
  | .Subscript xs =>
      let r := inlinesToMD opts st xs
      ("~" ++ r.1 ++ "~", r.2)
  | .Underline xs =>
      let r := inlinesToMD opts st xs
      ("[" ++ r.1 ++ "]{.underline}", r.2)
  | .SmallCaps xs =>
      let r := inlinesToMD opts st xs
      ("[" ++ r.1 ++ "]{.smallcaps}", r.2)
  | .Quoted qt xs =>
      let r := inlinesToMD opts st xs
      let (l, rr) := match qt with | .DoubleQuote => ("\"", "\"") | .SingleQuote => ("'", "'")
      (l ++ r.1 ++ rr, r.2)
  | .Cite _ xs => inlinesToMD opts st xs
  | .Code _ s => (renderCode s, st)
  | .Math .InlineMath s => ("$" ++ s ++ "$", st)
  | .Math .DisplayMath s => ("$$" ++ s ++ "$$", st)
  | .RawInline f s => (if passthroughFormat f then s else "", st)
  | .Link attr xs (url, title) =>
      let r := inlinesToMD opts st xs
      let titleP := if title.isEmpty then "" else " \"" ++ title ++ "\""
      ("[" ++ r.1 ++ "](" ++ url ++ titleP ++ ")" ++ attrMD attr, r.2)
  | .Image attr xs (url, title) =>
      let r := inlinesToMD opts st xs
      let titleP := if title.isEmpty then "" else " \"" ++ title ++ "\""
      ("![" ++ r.1 ++ "](" ++ url ++ titleP ++ ")" ++ attrMD attr, r.2)
  | .Span attr xs =>
      let r := inlinesToMD opts st xs
      if attrMD attr == "" then (r.1, r.2) else ("[" ++ r.1 ++ "]" ++ attrMD attr, r.2)
  | .Note bs =>
      let n := st.counter
      let r := blocksToMD opts { st with counter := st.counter + 1 } bs
      ("[^" ++ toString n ++ "]", { r.2 with notes := r.2.notes ++ [r.1] })

/-- Render a list of `Inline`s, concatenating. -/
unsafe def inlinesToMD (opts : WriterOptions) (st : St) : List Inline → (String × St)
  | [] => ("", st)
  | x :: xs =>
      let r1 := inlineToMD opts st x
      let r2 := inlinesToMD opts r1.2 xs
      (r1.1 ++ r2.1, r2.2)

/-- Render one `Block` to Markdown. -/
unsafe def blockToMD (opts : WriterOptions) (st : St) : Block → (String × St)
  | .Plain xs => inlinesToMD opts st xs
  | .Para xs => inlinesToMD opts st xs
  | .LineBlock lns =>
      let r := linesToMD opts st lns
      (r.1, r.2)
  | .CodeBlock (_, classes, _) s =>
      let lang := classes.headD ""
      ("```" ++ lang ++ "\n" ++ s ++ "\n```", st)
  | .RawBlock f s => (if passthroughFormat f then s else "", st)
  | .BlockQuote bs =>
      let r := blocksToMD opts st bs
      (indentLines "> " "> " r.1, r.2)
  | .BulletList items => itemsMD opts st "-   " items
  | .OrderedList attrs items =>
      let markers := _root_.Linen.Text.Pandoc.Shared.orderedListMarkersN items.length attrs
      orderedMD opts st markers items
  | .DefinitionList items => defListMD opts st items
  | .Header lvl attr xs =>
      let hashes := String.ofList (List.replicate (max 1 (min 6 lvl.toNat)) '#')
      let r := inlinesToMD opts st xs
      let attrS := if attrMD attr == "" then "" else " " ++ attrMD attr
      (hashes ++ " " ++ r.1 ++ attrS, r.2)
  | .HorizontalRule => ("---", st)
  | .Div attr bs =>
      let r := blocksToMD opts st bs
      if attrMD attr == "" then (r.1, r.2)
      else ("::: " ++ attrMD attr ++ "\n" ++ r.1 ++ "\n:::", r.2)
  | .Figure _ (.Caption _ capt) bs =>
      let r1 := blocksToMD opts st bs
      if capt.isEmpty then (r1.1, r1.2)
      else
        let r2 := blocksToMD opts r1.2 capt
        (r1.1 ++ "\n\n" ++ r2.1, r2.2)
  | .Table _ capt specs hd bodies foot =>
      let leg := _root_.Linen.Text.Pandoc.Writers.Shared.toLegacyTable capt specs hd bodies foot
      let aligns := leg.2.1
      let headerCells := leg.2.2.2.1
      let bodyRows := leg.2.2.2.2
      let hr := rowToMD opts st headerCells
      let br := rowsToMD opts hr.2 bodyRows
      let delim := "| " ++ String.intercalate " | " (aligns.map alignDashes) ++ " |"
      let headLine := if headerCells.all (fun bs => bs.isEmpty) then "" else hr.1 ++ "\n"
      (headLine ++ delim ++ (if br.1.isEmpty then "" else "\n" ++ br.1), br.2)

/-- Render `LineBlock` lines, each prefixed with `| `. -/
unsafe def linesToMD (opts : WriterOptions) (st : St) : List (List Inline) → (String × St)
  | [] => ("", st)
  | [l] =>
      let r := inlinesToMD opts st l
      ("| " ++ r.1, r.2)
  | l :: rest =>
      let r1 := inlinesToMD opts st l
      let r2 := linesToMD opts r1.2 rest
      ("| " ++ r1.1 ++ "\n" ++ r2.1, r2.2)

/-- Render bullet-list items, each with the given `marker`. -/
unsafe def itemsMD (opts : WriterOptions) (st : St) (marker : String) :
    List (List Block) → (String × St)
  | [] => ("", st)
  | [it] =>
      let r := blocksToMD opts st it
      (indentLines marker (spaces marker.length) r.1, r.2)
  | it :: rest =>
      let r1 := blocksToMD opts st it
      let r2 := itemsMD opts r1.2 marker rest
      (indentLines marker (spaces marker.length) r1.1 ++ "\n" ++ r2.1, r2.2)

/-- Render ordered-list items in lockstep with their markers. -/
unsafe def orderedMD (opts : WriterOptions) (st : St) :
    List String → List (List Block) → (String × St)
  | _, [] => ("", st)
  | [], _ => ("", st)
  | [m], [it] =>
      let marker := m ++ " "
      let r := blocksToMD opts st it
      (indentLines marker (spaces marker.length) r.1, r.2)
  | m :: ms, it :: rest =>
      let marker := m ++ " "
      let r1 := blocksToMD opts st it
      let r2 := orderedMD opts r1.2 ms rest
      (indentLines marker (spaces marker.length) r1.1 ++ "\n" ++ r2.1, r2.2)

/-- Render definition-list `(term, definitions)` items. -/
unsafe def defListMD (opts : WriterOptions) (st : St) :
    List (List Inline × List (List Block)) → (String × St)
  | [] => ("", st)
  | (term, defs) :: rest =>
      let rt := inlinesToMD opts st term
      let rd := defsMD opts rt.2 defs
      let r2 := defListMD opts rd.2 rest
      (rt.1 ++ "\n" ++ rd.1 ++ (if r2.1.isEmpty then "" else "\n\n" ++ r2.1), r2.2)

/-- Render the definition bodies for one term. -/
unsafe def defsMD (opts : WriterOptions) (st : St) : List (List Block) → (String × St)
  | [] => ("", st)
  | d :: rest =>
      let r1 := blocksToMD opts st d
      let r2 := defsMD opts r1.2 rest
      (indentLines ":   " "    " r1.1 ++ (if r2.1.isEmpty then "" else "\n" ++ r2.1), r2.2)

/-- Render one table row's cells as a `| … | … |` line (cells flattened). -/
unsafe def rowToMD (opts : WriterOptions) (st : St) (cells : List (List Block)) : (String × St) :=
  let r := cellsToMD opts st cells
  ("| " ++ String.intercalate " | " r.1 ++ " |", r.2)

/-- Render each cell's blocks to a single line. -/
unsafe def cellsToMD (opts : WriterOptions) (st : St) : List (List Block) → (List String × St)
  | [] => ([], st)
  | c :: cs =>
      let r1 := blocksToMD opts st c
      let r2 := cellsToMD opts r1.2 cs
      ((r1.1.replace "\n" " ") :: r2.1, r2.2)

/-- Render the body rows of a table. -/
unsafe def rowsToMD (opts : WriterOptions) (st : St) : List (List (List Block)) → (String × St)
  | [] => ("", st)
  | [r] => rowToMD opts st r
  | r :: rest =>
      let r1 := rowToMD opts st r
      let r2 := rowsToMD opts r1.2 rest
      (r1.1 ++ "\n" ++ r2.1, r2.2)

/-- Render a list of `Block`s, separated by blank lines. -/
unsafe def blocksToMD (opts : WriterOptions) (st : St) : List Block → (String × St)
  | [] => ("", st)
  | [b] => blockToMD opts st b
  | b :: bs =>
      let r1 := blockToMD opts st b
      let r2 := blocksToMD opts r1.2 bs
      (r1.1 ++ "\n\n" ++ r2.1, r2.2)

end

/- ── Footnote section and entry point ────────────────────────────────────── -/

/-- Render the collected footnote bodies as `[^N]: body` definitions. -/
private def renderFootnotes (notes : List String) : String :=
  "\n".intercalate (notes.zipIdx.map fun (body, i) =>
    "[^" ++ toString (i + 1) ++ "]: " ++ body)

/-- Render a document to a Markdown string (implementation). -/
unsafe def writeMarkdownStringImpl (opts : WriterOptions) (doc : Pandoc) : String :=
  let r := blocksToMD opts {} doc.blocks
  if r.2.notes.isEmpty then r.1
  else r.1 ++ "\n\n" ++ renderFootnotes r.2.notes

/-- Render a document to a Markdown string (template-free). -/
@[implemented_by writeMarkdownStringImpl]
opaque writeMarkdownString (opts : WriterOptions) (doc : Pandoc) : String

/-- Monadic wrapper matching upstream's `writeMarkdown :: … -> m Text`. -/
def writeMarkdown {m : Type → Type} [Monad m] (opts : WriterOptions) (doc : Pandoc) : m String :=
  pure (writeMarkdownString opts doc)

end Linen.Text.Pandoc.Writers.Markdown
