/-
  `Linen.Text.Pandoc.Readers.HTML` — the HTML → AST reader.

  ## Haskell source

  Ported from `Text.Pandoc.Readers.HTML` (and its `.Parsing`/`.Table`/`.Types`/
  `.TagCategories` submodules) in the `pandoc` package (v3.10,
  `src/Text/Pandoc/Readers/HTML*.hs`).

  Upstream tokenizes the input with the `tagsoup` package
  (`Text.HTML.TagSoup.parseTagsOptions`) into a `[Tag Text]` stream, then runs a
  Parsec-style `block`/`inline` dispatch over the tokens, mapping HTML elements
  onto pandoc `Block`/`Inline` nodes.

  ### The folded-in tagsoup slice (scoping decision)

  `tagsoup` is **not** ported in `linen` and has no consumer besides this
  reader.  Following the same precedent this codebase already sets for a small,
  single-consumer external dependency — the way `Emoji.lean` folds in a bounded
  slice of the `emojis` package's table and `MIME.lean` folds in a MIME subset
  rather than adding a separate top-level dependency entry — a **bounded HTML
  tokenizer** is folded in directly here (the `TagTok` sum type and a permissive
  `tokenize`): attribute parsing (quoted/unquoted/bare), self-closing tags,
  comments, doctype/PI skipping, and void elements — *not* full HTML5-spec
  compliance (no error-recovery insertion modes, no character-encoding sniffing,
  no `tagsoup` position tracking).  There is intentionally **no**
  `docs/imports/tagsoup/` entry: this is a deliberate inline fold, not a
  package import.

  ### Other deviations (documented scope)

  * The token-tree builder is a permissive recursive descent over the token
    list.  Its recursion is guarded by token consumption rather than a
    structural argument (and the `Table` path rebuilds rows from non-subterm
    data), so the mutually-recursive builders are `unsafe def` — the sanctioned
    escape hatch used elsewhere in this port (`Text.DocLayout.render`,
    `Generic.topDown`).  `readHtml` exposes a safe interface via
    `@[implemented_by]`.
  * Mismatched/stray close tags are skipped rather than stack-balanced; the
    parse is best-effort over well-formed input.  Unknown tags: block-level
    unknown containers become a `Div`; unknown inline containers are unwrapped;
    truly unrecognised leading tokens are dropped (upstream's `Ext_raw_html`
    `RawBlock`/`RawInline` passthrough is not reproduced).
  * MathML (`<math>`), reveal.js/EPUB semantic branches, base-URL
    canonicalisation, e-mail-obfuscation decoding, CSS `style` inspection
    (`pickStyleAttrProps`), `<q>` nested quote-context switching, and the
    document-metadata / `role="main"` unwrapping are out of scope; `<table>`
    reads `tr`/`th`/`td` into a simple `Table` (colgroup/caption/colspan/rowspan
    dropped).
-/

import Std.Internal.Parsec.String
import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Builder
import Linen.Text.Pandoc.Shared
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Error
import Linen.Text.Pandoc.XML

namespace Linen.Text.Pandoc.Readers.HTML

open _root_.Linen.Text.Pandoc

/- ── The folded-in tag tokenizer ─────────────────────────────────────────── -/

/-- An HTML token (bounded `tagsoup`-style slice, folded in — see module note). -/
inductive TagTok where
  /-- An open tag `<name …>` (with `selfClose` set for `<name … />`). -/
  | TagOpen (name : String) (attrs : List (String × String)) (selfClose : Bool)
  /-- A close tag `</name>`. -/
  | TagClose (name : String)
  /-- A run of text (entity-decoded). -/
  | TagText (text : String)
  /-- A comment `<!-- … -->`. -/
  | TagComment (text : String)
  deriving Repr, Inhabited, BEq

open Std.Internal.Parsec
open Std.Internal.Parsec.String

/-- Skip a run of whitespace. -/
private def wsT : Parser Unit := do let _ ← manyChars (satisfy (·.isWhitespace)); pure ()

/-- Parse a (lower-cased) tag name. -/
private def tagNameP : Parser String := do
  let s ← many1Chars (satisfy (fun c => c.isAlphanum || c == '-' || c == ':'))
  pure s.toLower

/-- Read text up to (and consuming) the terminator string. -/
private def takeUntil (stop : String) : Parser String := do
  let s ← manyChars (attempt (do notFollowedBy (skipString stop); any))
  skipString stop
  pure s

/-- Parse one `name="value"` / `name='value'` / `name=value` / bare `name`
    attribute (value entity-decoded). -/
private def oneAttr : Parser (String × String) := do
  wsT
  let n ← many1Chars (satisfy (fun c =>
    c != '=' && c != '>' && c != '/' && !c.isWhitespace))
  let v ← (attempt (do
      wsT; skipChar '='; wsT
      (do skipChar '"'; let s ← manyChars (satisfy (· != '"')); skipChar '"'; pure s)
        <|> (do skipChar '\''; let s ← manyChars (satisfy (· != '\'')); skipChar '\''; pure s)
        <|> many1Chars (satisfy (fun c => c != '>' && c != '/' && !c.isWhitespace))))
    <|> pure ""
  pure (n.toLower, XML.fromEntities v)

/-- Parse a comment `<!-- … -->`. -/
private def commentP : Parser TagTok := do
  skipString "<!--"; let body ← takeUntil "-->"; pure (.TagComment body)

/-- Skip a markup declaration / processing instruction (`<!…>` / `<?…>`). -/
private def declP : Parser TagTok := do
  skipChar '<'; (skipChar '!' <|> skipChar '?'); let _ ← takeUntil ">"; pure (.TagComment "")

/-- Parse a close tag `</name>`. -/
private def closeP : Parser TagTok := do
  skipChar '<'; skipChar '/'; let n ← tagNameP; wsT; skipChar '>'; pure (.TagClose n)

/-- Parse an open (or self-closing) tag `<name …>` / `<name … />`. -/
private def openP : Parser TagTok := do
  skipChar '<'; let n ← tagNameP
  let attrs ← many (attempt oneAttr)
  wsT
  let sc ← (attempt (skipChar '/') *> pure true) <|> pure false
  skipChar '>'
  pure (.TagOpen n attrs.toList sc)

/-- Parse a text run (up to the next `<`), entity-decoded. -/
private def textP : Parser TagTok := do
  let s ← many1Chars (satisfy (· != '<'))
  pure (.TagText (XML.fromEntities s))

/-- Parse one token. -/
private def tokenP : Parser TagTok :=
  attempt commentP <|> attempt closeP <|> attempt openP <|> attempt declP <|> textP

/-- Tokenize an HTML string into a `TagTok` list (best-effort). -/
def tokenize (input : String) : List TagTok :=
  match Parser.run (many tokenP) input with
  | .ok a => a.toList
  | .error _ => []

/- ── Element categories ──────────────────────────────────────────────────── -/

/-- Block-level tag names handled by the reader. -/
def isBlockName (n : String) : Bool :=
  [ "p", "h1", "h2", "h3", "h4", "h5", "h6", "blockquote", "pre", "ul", "ol",
    "dl", "table", "div", "section", "article", "aside", "header", "footer",
    "main", "nav", "hr", "figure", "figcaption", "address", "fieldset",
    "form" ].contains n

/-- Inline-level tag names handled by the reader. -/
def isInlineName (n : String) : Bool :=
  [ "em", "i", "strong", "b", "u", "ins", "s", "strike", "del", "sup", "sub",
    "q", "cite", "code", "tt", "samp", "var", "kbd", "a", "span", "mark",
    "abbr", "dfn", "br", "wbr", "img" ].contains n

/-- Void (self-closing, childless) element names. -/
def isVoidName (n : String) : Bool :=
  [ "br", "wbr", "img", "hr", "input", "meta", "link", "area", "base", "col",
    "embed", "source", "track" ].contains n

/- ── Attribute + text helpers ────────────────────────────────────────────── -/

/-- Is a text run entirely whitespace? -/
def isBlank (s : String) : Bool := s.all Char.isWhitespace

/-- Look up a raw attribute value (default `""`). -/
def getAttr (k : String) (attrs : List (String × String)) : String :=
  ((attrs.find? (·.1 == k)).map (·.2)).getD ""

/-- Convert a raw attribute list to a pandoc `Attr` (id, classes, other kvs). -/
def toAttr (attrs : List (String × String)) : Attr :=
  let ident := getAttr "id" attrs
  let classes := match (attrs.find? (·.1 == "class")).map (·.2) with
    | some c => (c.splitOn " ").filter (· != "")
    | none => []
  let kvs := attrs.filter (fun p => p.1 != "id" && p.1 != "class")
  (ident, classes, kvs)

/-- Group a character list into maximal whitespace / non-whitespace runs. -/
private def groupWS : List Char → List (List Char)
  | [] => []
  | c :: cs =>
    match groupWS cs with
    | (g@(g0 :: _)) :: gs => if c.isWhitespace == g0.isWhitespace then (c :: g) :: gs else [c] :: g :: gs
    | _ => [[c]]

/-- Turn a text run into inlines (whitespace runs → `Space`, else `Str`). -/
def textToInlines (s : String) : List Inline :=
  (groupWS s.toList).filterMap fun g =>
    match g with
    | [] => none
    | c :: _ => if c.isWhitespace then some .Space else some (.Str (String.ofList g))

/-- Build an inline from a recognised inline element and its parsed content. -/
def mkInline (name : String) (attr : Attr) (attrs : List (String × String))
    (content : List Inline) : List Inline :=
  match name with
  | "em" | "i" => [.Emph content]
  | "strong" | "b" => [.Strong content]
  | "u" | "ins" => [.Underline content]
  | "s" | "strike" | "del" => [.Strikeout content]
  | "sup" => [.Superscript content]
  | "sub" => [.Subscript content]
  | "q" => [.Quoted .DoubleQuote content]
  | "cite" => [.Cite [] content]
  | "code" | "tt" | "samp" | "var" | "kbd" =>
      [.Code attr (_root_.Linen.Text.Pandoc.Shared.stringify content)]
  | "a" =>
      let href := getAttr "href" attrs
      let title := getAttr "title" attrs
      let attr' := (attr.1, attr.2.1, attr.2.2.filter (fun p => p.1 != "href" && p.1 != "title"))
      if href.isEmpty then [.Span attr' content] else [.Link attr' content (href, title)]
  | "span" =>
      if attr.2.1.contains "smallcaps" then [.SmallCaps content] else [.Span attr content]
  | "mark" | "abbr" | "dfn" => [.Span attr content]
  | _ => content

/-- Build an inline from a recognised void inline element. -/
def mkVoidInline (name : String) (attr : Attr) (attrs : List (String × String)) : List Inline :=
  match name with
  | "br" | "wbr" => [.LineBreak]
  | "img" =>
      let src := getAttr "src" attrs
      let title := getAttr "title" attrs
      let alt := getAttr "alt" attrs
      let attr' := (attr.1, attr.2.1,
        attr.2.2.filter (fun p => p.1 != "src" && p.1 != "title" && p.1 != "alt"))
      [.Image attr' (if alt.isEmpty then [] else [.Str alt]) (src, title)]
  | _ => []

/-- The heading level for an `hN` tag name. -/
def headerLevel (n : String) : Int :=
  match n with
  | "h1" => 1 | "h2" => 2 | "h3" => 3 | "h4" => 4 | "h5" => 5 | "h6" => 6 | _ => 1

/-- Div-like container tag names (rendered as `Div`). -/
def isDivLike (n : String) : Bool :=
  [ "div", "section", "article", "aside", "header", "footer", "main", "nav",
    "figcaption", "address", "fieldset", "form" ].contains n

/-- Ordered-list attributes from an `<ol>`'s raw attributes. -/
def olAttributes (attrs : List (String × String)) : ListAttributes :=
  let start := match (getAttr "start" attrs).toInt? with | some n => n | none => 1
  let style := match getAttr "type" attrs with
    | "1" => ListNumberStyle.Decimal
    | "a" => .LowerAlpha
    | "A" => .UpperAlpha
    | "i" => .LowerRoman
    | "I" => .UpperRoman
    | _ => .DefaultStyle
  (start, style, .DefaultDelim)

/- ── The token-tree builder ──────────────────────────────────────────────── -/

mutual

/-- Gather a maximal inline run, stopping (without consuming) at end, a
    block-level open tag, or any close tag. -/
unsafe def inlines (toks : List TagTok) : List Inline × List TagTok :=
  match toks with
  | [] => ([], [])
  | t :: ts =>
    match t with
    | .TagComment _ => inlines ts
    | .TagText s =>
        let _t1 := inlines ts
        let rest := _t1.1
        let r := _t1.2
        (textToInlines s ++ rest, r)
    | .TagClose _ => ([], toks)
    | .TagOpen name attrs sc =>
        if isInlineName name then
          let _t2 := inlineElement name attrs sc ts
          let el := _t2.1
          let r1 := _t2.2
          let _t3 := inlines r1
          let rest := _t3.1
          let r2 := _t3.2
          (el ++ rest, r2)
        else ([], toks)

/-- Parse one inline element's content and matching close. -/
unsafe def inlineElement (name : String) (attrs : List (String × String)) (sc : Bool)
    (ts : List TagTok) : (List Inline × List TagTok) :=
  let attr := toAttr attrs
  if sc || isVoidName name then
    (mkVoidInline name attr attrs, ts)
  else
    let _t4 := inlines ts
    let content := _t4.1
    let r := _t4.2
    (mkInline name attr attrs content, dropClose name r)

/-- Consume a matching close tag if it is at the head (skipping comments). -/
unsafe def dropClose (name : String) (toks : List TagTok) : List TagTok :=
  match toks with
  | [] => []
  | t :: ts =>
    match t with
    | .TagComment _ => dropClose name ts
    | .TagClose n => if n == name then ts else toks
    | _ => toks

/-- Parse a block sequence until an optional matching close tag. -/
unsafe def blocksUntil (stop : Option String) (toks : List TagTok) : List Block × List TagTok :=
  match toks with
  | [] => ([], [])
  | t :: ts =>
    match t with
    | .TagComment _ => blocksUntil stop ts
    | .TagText s =>
        if isBlank s then blocksUntil stop ts
        else
          let ir := inlines toks
          let br := blocksUntil stop ir.2
          (.Plain ir.1 :: br.1, br.2)
    | .TagClose n =>
        if stop == some n then (([] : List Block), ts) else blocksUntil stop ts
    | .TagOpen name attrs sc =>
        if isBlockName name then
          let _t5 := blockElement name attrs sc ts
          let b := _t5.1
          let r := _t5.2
          let _t6 := blocksUntil stop r
          let bs := _t6.1
          let r2 := _t6.2
          (b ++ bs, r2)
        else if isInlineName name then
          let _t7 := inlines toks
          let ils := _t7.1
          let r := _t7.2
          let _t8 := blocksUntil stop r
          let bs := _t8.1
          let r2 := _t8.2
          (.Plain ils :: bs, r2)
        else
          blocksUntil stop ts

/-- Parse one recognised block element (`ts` is the token stream after its
    open tag). -/
unsafe def blockElement (name : String) (attrs : List (String × String)) (_sc : Bool)
    (ts : List TagTok) : (List Block × List TagTok) :=
  let attr := toAttr attrs
  if name == "hr" then ([.HorizontalRule], ts)
  else if name == "p" then
    let _t9 := inlines ts
    let ils := _t9.1
    let r := _t9.2
    ([.Para ils], dropClose "p" r)
  else if name == "h1" || name == "h2" || name == "h3"
          || name == "h4" || name == "h5" || name == "h6" then
    let _t10 := inlines ts
    let ils := _t10.1
    let r := _t10.2
    ([.Header (headerLevel name) attr ils], dropClose name r)
  else if name == "blockquote" then
    let _t11 := blocksUntil (some "blockquote") ts
    let bs := _t11.1
    let r := _t11.2
    ([.BlockQuote bs], r)
  else if name == "pre" then
    let _t12 := preContent ts
    let txt := _t12.1
    let r := _t12.2
    ([.CodeBlock attr (_root_.Linen.Text.Pandoc.Shared.stripTrailingNewlines txt)], r)
  else if name == "ul" then
    let _t13 := listItems "ul" ts
    let items := _t13.1
    let r := _t13.2
    ([.BulletList items], r)
  else if name == "ol" then
    let _t14 := listItems "ol" ts
    let items := _t14.1
    let r := _t14.2
    ([.OrderedList (olAttributes attrs) items], r)
  else if name == "dl" then
    let _t15 := defItems ts
    let items := _t15.1
    let r := _t15.2
    ([.DefinitionList items], r)
  else if name == "table" then
    tableBlock ts
  else if name == "figure" then
    let _t16 := blocksUntil (some "figure") ts
    let bs := _t16.1
    let r := _t16.2
    ([.Figure attr (.Caption none []) bs], r)
  else -- div-like / generic container
    let _t17 := blocksUntil (some name) ts
    let bs := _t17.1
    let r := _t17.2
    ([.Div attr bs], r)

/-- Collect the raw text of a `<pre>` (inner tags dropped), up to `</pre>`. -/
unsafe def preContent (toks : List TagTok) : String × List TagTok :=
  match toks with
  | [] => ("", [])
  | t :: ts =>
    match t with
    | .TagClose "pre" => ("", ts)
    | .TagText s =>
        let pr := preContent ts
        (s ++ pr.1, pr.2)
    | _ => preContent ts

/-- Parse `<li>` items until the container's close tag. -/
unsafe def listItems (container : String) (toks : List TagTok) : List (List Block) × List TagTok :=
  match toks with
  | [] => ([], [])
  | t :: ts =>
    match t with
    | .TagClose n => if n == container then (([] : List (List Block)), ts) else listItems container ts
    | .TagOpen "li" _ _ =>
        let _t18 := blocksUntil (some "li") ts
        let item := _t18.1
        let r := _t18.2
        let _t19 := listItems container r
        let rest := _t19.1
        let r2 := _t19.2
        (item :: rest, r2)
    | _ => listItems container ts

/-- Parse `<dt>`/`<dd>` groups until `</dl>`. -/
unsafe def defItems (toks : List TagTok) : List (List Inline × List (List Block)) × List TagTok :=
  match toks with
  | [] => ([], [])
  | t :: ts =>
    match t with
    | .TagClose "dl" => (([] : List (List Inline × List (List Block))), ts)
    | .TagOpen "dt" _ _ =>
        let _t20 := inlines ts
        let term := _t20.1
        let r1 := _t20.2
        let r1' := dropClose "dt" r1
        let _t21 := ddItems r1'
        let defs := _t21.1
        let r2 := _t21.2
        let _t22 := defItems r2
        let rest := _t22.1
        let r3 := _t22.2
        ((term, defs) :: rest, r3)
    | _ => defItems ts

/-- Parse the `<dd>` definitions following a `<dt>` term. -/
unsafe def ddItems (toks : List TagTok) : List (List Block) × List TagTok :=
  match toks with
  | [] => ([], [])
  | t :: ts =>
    match t with
    | .TagComment _ => ddItems ts
    | .TagText s => if isBlank s then ddItems ts else (([] : List (List Block)), toks)
    | .TagOpen "dd" _ _ =>
        let _t23 := blocksUntil (some "dd") ts
        let bs := _t23.1
        let r := _t23.2
        let _t24 := ddItems r
        let rest := _t24.1
        let r2 := _t24.2
        (bs :: rest, r2)
    | _ => (([] : List (List Block)), toks)

/-- Parse a table into `(isHeaderRow, cells)` rows, up to `</table>`. -/
unsafe def tableRows (toks : List TagTok) : List (Bool × List (List Block)) × List TagTok :=
  match toks with
  | [] => ([], [])
  | t :: ts =>
    match t with
    | .TagClose "table" => (([] : List (Bool × List (List Block))), ts)
    | .TagOpen "tr" _ _ =>
        let _t25 := rowCells ts
        let cells := _t25.1
        let isHdr := _t25.2.1
        let r := _t25.2.2
        let _t26 := tableRows r
        let rest := _t26.1
        let r2 := _t26.2
        ((isHdr, cells) :: rest, r2)
    | _ => tableRows ts

/-- Parse the `<th>`/`<td>` cells of one row, up to `</tr>`; the `Bool` flags a
    header row (any `<th>`). -/
unsafe def rowCells (toks : List TagTok) : List (List Block) × Bool × List TagTok :=
  match toks with
  | [] => ([], false, [])
  | t :: ts =>
    match t with
    | .TagClose "tr" => (([] : List (List Block)), false, ts)
    | .TagOpen "th" _ _ =>
        let _t27 := blocksUntil (some "th") ts
        let bs := _t27.1
        let r := _t27.2
        let _t28 := rowCells r
        let rest := _t28.1
        let r2 := _t28.2.2
        (bs :: rest, true, r2)
    | .TagOpen "td" _ _ =>
        let _t29 := blocksUntil (some "td") ts
        let bs := _t29.1
        let r := _t29.2
        let _t30 := rowCells r
        let rest := _t30.1
        let hdr := _t30.2.1
        let r2 := _t30.2.2
        (bs :: rest, hdr, r2)
    | _ => rowCells ts

/-- Build a simple `Table` from the parsed rows. -/
unsafe def tableBlock (ts : List TagTok) : (List Block × List TagTok) :=
  let _t31 := tableRows ts
  let rows := _t31.1
  let r := _t31.2
  let mkRow (cells : List (List Block)) : Row :=
    .Row nullAttr (cells.map fun bs => .Cell nullAttr .AlignDefault 1 1 bs)
  let headerRows := (rows.filter (·.1)).map (fun x => mkRow x.2)
  let bodyRows := (rows.filter (fun x => !x.1)).map (fun x => mkRow x.2)
  let numcols := (rows.map (fun x => x.2.length)).foldl Nat.max 0
  let specs := List.replicate numcols (Alignment.AlignDefault, ColWidth.ColWidthDefault)
  ([.Table nullAttr (.Caption none []) specs
      (.TableHead nullAttr headerRows)
      [.TableBody nullAttr 0 [] bodyRows]
      (.TableFoot nullAttr [])], r)

end

/- ── Entry point ─────────────────────────────────────────────────────────── -/

/-- Read an HTML document into the pandoc AST (implementation). -/
unsafe def readHtmlImpl (_opts : ReaderOptions) (input : String) : Except PandocError Pandoc :=
  let toks := tokenize input
  let _t32 := blocksUntil none toks
  let bs := _t32.1
  .ok ⟨nullMeta, bs⟩

/-- Read an HTML document into the pandoc AST. -/
@[implemented_by readHtmlImpl]
opaque readHtml (opts : ReaderOptions) (input : String) : Except PandocError Pandoc

end Linen.Text.Pandoc.Readers.HTML
