/-
  `Linen.Text.Pandoc.Readers.Markdown` — the Markdown → AST reader.

  ## Haskell source

  Ported from `Text.Pandoc.Readers.Markdown` (and its YAML-metadata submodule
  `Text.Pandoc.Readers.Metadata`) in the `pandoc` package (v3.10,
  `src/Text/Pandoc/Readers/Markdown.hs`, `src/Text/Pandoc/Readers/Metadata.hs`).

  This is pandoc's flagship reader.  Upstream is a very large, dense,
  stateful Parsec parser threading a `ParserState` (reference-key table,
  note table, quote context, …).  This port keeps the structural block/inline
  dispatch — the part that carries the format's meaning — and scopes down the
  long tail of edge cases, documented below.

  ### The folded-in YAML-subset parser (scoping decision)

  Upstream optionally parses a `---\n…\n---` YAML metadata block at the top of a
  document via the full `HsYAML`/`libyaml`-backed `Text.Pandoc.Readers.Metadata`
  machinery.  Full YAML is **not** ported in `linen` and no other module needs
  it.  Following the same precedent this codebase already sets for a small,
  single-consumer dependency — the way Tier 4a's `Readers/HTML.lean` folds in a
  bounded `tagsoup` tokenizer rather than adding a package entry, and the way
  `Emoji.lean`/`MIME.lean` fold in bounded subsets of the `emojis`/`mime-types`
  data before that — a **bounded YAML-subset parser is folded in directly here**
  (`parseYamlMap`/`parseYamlSeq`/`scalarValue`): flat and nested block mappings
  (`key: value`), nested mappings via indentation, simple `- item` block
  sequences, and quoted/unquoted scalar strings plus `true`/`false` booleans.
  It is **not** the general YAML 1.2 spec: no anchors/tags, no flow collections
  (`{…}`/`[…]`), no multi-document streams, no folded/literal block scalars.
  There is intentionally **no** `docs/imports/yaml/` entry: this is a deliberate
  inline fold, not a package import.  Scalar values are kept as `MetaString`
  (upstream parses them as inline Markdown); `true`/`false` become `MetaBool`.

  ### Other deviations (documented scope)

  * The block parser is line-based and the inline parser is a
    `Std.Internal.Parsec` char parser; both recurse in ways that are guarded by
    input consumption rather than a structural argument (block containers and
    emphasis nest recursively).  So the recursive parsers are `unsafe def` — the
    sanctioned escape hatch used throughout this port (`Text.DocLayout.render`,
    `Generic.topDown`, Tier 4a's `Readers/HTML.lean`).  `readMarkdown` exposes a
    safe interface via `@[implemented_by]`.
  * **Blocks supported:** ATX (`#`) and setext (`===`/`---`) headers, fenced
    (```` ``` ````/`~~~`) and indented code blocks, blockquotes, bullet and
    ordered lists (with task-list ASCII checkboxes), horizontal rules, GFM pipe
    tables, embedded raw-HTML blocks (parsed via `Readers.HTML`), reference-link
    and footnote definitions, and paragraphs.  Deferred/simplified: grid and
    multiline tables, definition lists, line blocks, fenced divs, header
    attributes and auto-identifiers (headers get `nullAttr`), lazy blockquote
    continuation, and loose/tight subtleties (lists render tight unless an item
    carries multiple blocks — `Shared.compactify`).
  * **Inlines supported:** emphasis (`*`/`_`) and strong (`**`/`__`), strikeout
    (`~~`, gated by `Ext_strikeout`), inline code spans, links (inline,
    reference, and shortcut) and images, autolinks (`<uri>`/`<email>`), inline
    raw HTML (gated by `Ext_raw_html`), `$…$` inline math (gated by
    `Ext_tex_math_dollars`), footnote references (`[^id]`), backslash escapes,
    HTML/numeric entities, hard/soft line breaks, and text.  Deferred:
    smart-punctuation, citations, bracketed spans, wikilinks, and the
    `Ext_intraword_underscores` nuance.
  * `Readers.LaTeX`'s raw-TeX passthrough slice (`rawLaTeXInline`/`Block`) is
    out of scope; only raw-HTML passthrough is reproduced.
-/

import Std.Internal.Parsec.String
import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Builder
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Extensions
import Linen.Text.Pandoc.Shared
import Linen.Text.Pandoc.Parsing
import Linen.Text.Pandoc.Error
import Linen.Text.Pandoc.XML
import Linen.Text.Pandoc.Readers.HTML
import Linen.Data.Map

namespace Linen.Text.Pandoc.Readers.Markdown

open Std.Internal.Parsec
open Std.Internal.Parsec.String
open _root_.Linen.Text.Pandoc

/- ── Reader context ──────────────────────────────────────────────────────── -/

/-- The parsing context threaded through the reader: options, the resolved
    reference-link table (keys normalised via `Parsing.toKey`), and the
    footnote table (id → raw text). -/
structure Ctx where
  /-- The reader options. -/
  opts : ReaderOptions
  /-- Reference-link definitions: normalised key → `(url, title)`. -/
  refs : List (String × (String × String))
  /-- Footnote definitions: id → raw text. -/
  notes : List (String × String)

/-- Is extension `e` enabled in the context's reader options? -/
private def extOn (ctx : Ctx) (e : Extension) : Bool :=
  extensionEnabled e ctx.opts.readerExtensions

/-- Look up a reference-link definition by (un-normalised) key. -/
private def lookupRef (ctx : Ctx) (key : String) : Option (String × String) :=
  let nk := (_root_.Linen.Text.Pandoc.Parsing.toKey key).unKey
  (ctx.refs.find? (fun kv => kv.1 == nk)).map (fun kv => kv.2)

/-- Merge adjacent `Str` inlines produced by single-character fallbacks. -/
private def mergeStr (xs : List Inline) : List Inline :=
  xs.foldr (fun x acc =>
    match x, acc with
    | .Str a, .Str b :: rest => .Str (a ++ b) :: rest
    | _, _ => x :: acc) []

/- ── Inline lexical helpers (non-recursive) ──────────────────────────────── -/

/-- Characters that begin an inline construct and so terminate a plain run. -/
private def inlineStop (c : Char) : Bool :=
  c == '*' || c == '_' || c == '`' || c == '[' || c == ']' || c == '<' ||
  c == '\\' || c == '~' || c == '$' || c == '!' || c == '&' ||
  c == ' ' || c == '\t' || c == '\n' || c == '\r'

/-- Punctuation that may follow a backslash escape. -/
private def isPunctChar (c : Char) : Bool :=
  "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".toList.contains c

/-- A backslash-escaped punctuation character. -/
private def pEscaped : Parser Inline := attempt do
  let _ ← pchar '\\'
  let c ← any
  if isPunctChar c then pure (.Str (String.singleton c)) else fail "not an escape"

/-- A hard line break from a trailing backslash. -/
private def pBreakBackslash : Parser Inline := attempt do
  let _ ← pchar '\\'
  let _ ← pchar '\n'
  pure .LineBreak

/-- A hard line break from two-or-more trailing spaces. -/
private def pBreakSpaces : Parser Inline := attempt do
  let sp ← many1Chars (satisfy (fun c => c == ' '))
  let _ ← pchar '\n'
  if sp.length ≥ 2 then pure .LineBreak else fail "not a hard break"

/-- A soft break (a single newline, absorbing surrounding spaces). -/
private def pSoftBreak : Parser Inline := attempt do
  let _ ← manyChars (satisfy (fun c => c == ' ' || c == '\t'))
  let _ ← pchar '\n'
  pure .SoftBreak

/-- A run of literal whitespace → `Space`. -/
private def pSpace : Parser Inline := do
  let _ ← many1Chars (satisfy (fun c => c == ' ' || c == '\t'))
  pure .Space

/-- A backtick code span (`` `code` ``/``` ``code`` ```). -/
private def pCode : Parser Inline := attempt do
  let ticks ← many1Chars (pchar '`')
  let content ← manyChars (satisfy (fun c => c != '`'))
  let closing ← many1Chars (pchar '`')
  if closing.length == ticks.length then
    pure (.Code nullAttr (_root_.Linen.Text.Pandoc.Shared.trim content))
  else fail "unbalanced code span"

/-- Inline `$…$` math, gated by `Ext_tex_math_dollars`. -/
private def pMathInline (ctx : Ctx) : Parser Inline := attempt do
  if !extOn ctx .Ext_tex_math_dollars then fail "no tex_math_dollars"
  let _ ← pchar '$'
  let _ ← notFollowedBy (satisfy (fun c => c == ' ' || c == '\n'))
  let content ← many1Chars (satisfy (fun c => c != '$' && c != '\n'))
  let _ ← pchar '$'
  pure (.Math .InlineMath (_root_.Linen.Text.Pandoc.Shared.trim content))

/-- An HTML/numeric entity → its decoded text. -/
private def pEntity : Parser Inline := attempt do
  let _ ← pchar '&'
  let body ← many1Chars (satisfy (fun c => c != ';' && c != '&' && !c.isWhitespace))
  let _ ← pchar ';'
  pure (.Str (XML.fromEntities ("&" ++ body ++ ";")))

/-- An autolink `<uri>` or `<email>`. -/
private def pAutolink : Parser Inline := attempt do
  let _ ← pchar '<'
  let body ← many1Chars (satisfy (fun c => c != '>' && c != '<' && c != ' ' && c != '\n'))
  let _ ← pchar '>'
  if body.any (fun c => c == ':') && !(body.any (fun c => c == '@' && !body.any (fun d => d == ':'))) then
    pure (.Link nullAttr [.Str body] (body, ""))
  else if body.any (fun c => c == '@') then
    pure (.Link nullAttr [.Str body] ("mailto:" ++ body, ""))
  else fail "not an autolink"

/-- A link URL, either `<…>`-delimited or bare. -/
private def pLinkUrl : Parser String :=
  (attempt (do
    let _ ← pchar '<'
    let u ← manyChars (satisfy (fun c => c != '>' && c != '\n'))
    let _ ← pchar '>'
    pure u))
  <|> manyChars (satisfy (fun c => c != ' ' && c != ')' && c != '\n'))

/-- An optional link title in `"…"`/`'…'`, after intervening spaces. -/
private def pLinkTitle : Parser String :=
  (attempt (do
    let _ ← many1Chars (satisfy (fun c => c == ' '))
    (do let _ ← pchar '"'; let t ← manyChars (satisfy (fun c => c != '"')); let _ ← pchar '"'; pure t)
      <|> (do let _ ← pchar '\''; let t ← manyChars (satisfy (fun c => c != '\'')); let _ ← pchar '\''; pure t)))
  <|> pure ""

/- ── The recursive inline parsers ────────────────────────────────────────── -/

mutual

/-- Parse a single inline element.  See the module note for the alternative
    order and supported constructs. -/
unsafe def pInline (ctx : Ctx) : Parser Inline :=
  attempt pBreakBackslash
  <|> attempt pBreakSpaces
  <|> attempt pSoftBreak
  <|> attempt pCode
  <|> attempt (pMathInline ctx)
  <|> attempt (pImage ctx)
  <|> attempt (pStrong ctx)
  <|> attempt (pEmph ctx)
  <|> attempt (pStrikeout ctx)
  <|> attempt pAutolink
  <|> attempt (pFootnote ctx)
  <|> attempt (pLink ctx)
  <|> attempt (pRawInlineHtml ctx)
  <|> attempt pEntity
  <|> attempt pEscaped
  <|> attempt pSpace
  <|> attempt pStr
  <|> ((fun c => Inline.Str (String.singleton c)) <$> any)

/-- Parse a run of ordinary text characters. -/
unsafe def pStr : Parser Inline := do
  let s ← many1Chars (satisfy (fun c => !inlineStop c))
  pure (.Str s)

/-- Parse inlines until `closer` succeeds (which is consumed). -/
unsafe def pInlinesTill (ctx : Ctx) (closer : Parser Unit) : Parser (List Inline) := do
  let xs ← many (attempt (do let _ ← notFollowedBy closer; pInline ctx))
  let _ ← closer
  pure (mergeStr xs.toList)

/-- Strong emphasis `**…**` / `__…__`. -/
unsafe def pStrong (ctx : Ctx) : Parser Inline := attempt do
  let opener ← (attempt (pstring "**")) <|> pstring "__"
  let _ ← notFollowedBy (satisfy (fun c => c == ' ' || c == '\n'))
  let xs ← pInlinesTill ctx (skipString opener)
  pure (.Strong xs)

/-- Emphasis `*…*` / `_…_`. -/
unsafe def pEmph (ctx : Ctx) : Parser Inline := attempt do
  let c ← (pchar '*') <|> pchar '_'
  let _ ← notFollowedBy (satisfy (fun d => d == ' ' || d == '\n'))
  let xs ← pInlinesTill ctx (skipChar c)
  pure (.Emph xs)

/-- Strikeout `~~…~~`, gated by `Ext_strikeout`. -/
unsafe def pStrikeout (ctx : Ctx) : Parser Inline := attempt do
  if !extOn ctx .Ext_strikeout then fail "no strikeout"
  let _ ← pstring "~~"
  let xs ← pInlinesTill ctx (skipString "~~")
  pure (.Strikeout xs)

/-- A link: inline `[t](url "title")`, reference `[t][id]`, or shortcut `[t]`. -/
unsafe def pLink (ctx : Ctx) : Parser Inline := attempt do
  let _ ← pchar '['
  let label ← pInlinesTill ctx (skipChar ']')
  (attempt (do
      let _ ← pchar '('
      let _ ← manyChars (satisfy (fun c => c == ' '))
      let url ← pLinkUrl
      let title ← pLinkTitle
      let _ ← manyChars (satisfy (fun c => c == ' '))
      let _ ← pchar ')'
      pure (.Link nullAttr label (url, title))))
    <|> (attempt (do
      let _ ← pchar '['
      let refRaw ← manyChars (satisfy (fun c => c != ']'))
      let _ ← pchar ']'
      let key := if (_root_.Linen.Text.Pandoc.Shared.trim refRaw).isEmpty
                 then _root_.Linen.Text.Pandoc.Shared.stringify label else refRaw
      match lookupRef ctx key with
      | some t => pure (.Link nullAttr label t)
      | none => fail "reference not found"))
    <|> (do
      match lookupRef ctx (_root_.Linen.Text.Pandoc.Shared.stringify label) with
      | some t => pure (.Link nullAttr label t)
      | none => fail "shortcut reference not found")

/-- An image `![alt](url "title")` or `![alt][id]`. -/
unsafe def pImage (ctx : Ctx) : Parser Inline := attempt do
  let _ ← pchar '!'
  let _ ← pchar '['
  let alt ← pInlinesTill ctx (skipChar ']')
  (attempt (do
      let _ ← pchar '('
      let _ ← manyChars (satisfy (fun c => c == ' '))
      let url ← pLinkUrl
      let title ← pLinkTitle
      let _ ← manyChars (satisfy (fun c => c == ' '))
      let _ ← pchar ')'
      pure (.Image nullAttr alt (url, title))))
    <|> (do
      let _ ← pchar '['
      let refRaw ← manyChars (satisfy (fun c => c != ']'))
      let _ ← pchar ']'
      let key := if (_root_.Linen.Text.Pandoc.Shared.trim refRaw).isEmpty
                 then _root_.Linen.Text.Pandoc.Shared.stringify alt else refRaw
      match lookupRef ctx key with
      | some t => pure (.Image nullAttr alt t)
      | none => fail "image reference not found")

/-- A footnote reference `[^id]`, resolved against the note table. -/
unsafe def pFootnote (ctx : Ctx) : Parser Inline := attempt do
  let _ ← pchar '['
  let _ ← pchar '^'
  let id ← many1Chars (satisfy (fun c => c != ']'))
  let _ ← pchar ']'
  match (ctx.notes.find? (fun kv => kv.1 == id)).map (fun kv => kv.2) with
  | some txt => pure (.Note [.Para (mergeStr (runInlinesM ctx txt))])
  | none => fail "footnote not found"

/-- Inline raw HTML `<tag …>`/`</tag>`, gated by `Ext_raw_html`. -/
unsafe def pRawInlineHtml (ctx : Ctx) : Parser Inline := attempt do
  if !extOn ctx .Ext_raw_html then fail "no raw_html"
  let _ ← pchar '<'
  let inner ← many1Chars (satisfy (fun c => c != '>' && c != '<'))
  let _ ← pchar '>'
  let c0 := inner.toList.headD ' '
  if c0.isAlpha || c0 == '/' || c0 == '!' then
    pure (.RawInline ⟨"html"⟩ ("<" ++ inner ++ ">"))
  else fail "not a tag"

/-- Run the inline parser over a whole string (best effort). -/
unsafe def runInlinesM (ctx : Ctx) (s : String) : List Inline :=
  match Parser.run (many (pInline ctx)) s with
  | .ok arr => mergeStr arr.toList
  | .error _ => [.Str s]

end

/- ── Block classification helpers (non-recursive) ────────────────────────── -/

/-- Number of leading spaces. -/
private def leadSpaces (l : String) : Nat := (l.toList.takeWhile (fun c => c == ' ')).length

/-- Is a line blank (whitespace only)? -/
private def isBlank (l : String) : Bool := l.toList.all (fun c => c.isWhitespace)

/-- Parse an ATX header line into `(level, text)`. -/
private def atxHeader (l : String) : Option (Int × String) :=
  if leadSpaces l ≤ 3 then
    let t := l.toList.dropWhile (fun c => c == ' ')
    let hashes := t.takeWhile (fun c => c == '#')
    let n := hashes.length
    if n ≥ 1 && n ≤ 6 then
      let after := t.drop n
      if after.isEmpty || after.headD ' ' == ' ' then
        let body := _root_.Linen.Text.Pandoc.Shared.trim (String.ofList after)
        let body := (body.toList.reverse.dropWhile (fun c => c == '#')).reverse
        some (Int.ofNat n, _root_.Linen.Text.Pandoc.Shared.trim (String.ofList body))
      else none
    else none
  else none

/-- Parse a fence opener into `(char, length, info)`. -/
private def fenceStart (l : String) : Option (Char × Nat × String) :=
  let t := l.toList.dropWhile (fun c => c == ' ')
  match t with
  | c :: _ =>
    if c == '`' || c == '~' then
      let run := t.takeWhile (fun d => d == c)
      if run.length ≥ 3 then
        some (c, run.length, _root_.Linen.Text.Pandoc.Shared.trim (String.ofList (t.drop run.length)))
      else none
    else none
  | [] => none

/-- Does a line close a fence of char `c`, length `≥ n`? -/
private def isFenceClose (c : Char) (n : Nat) (l : String) : Bool :=
  let t := l.toList.dropWhile (fun d => d == ' ')
  let run := t.takeWhile (fun d => d == c)
  run.length ≥ n && (t.drop run.length).all (fun d => d == ' ')

/-- The `Attr` from a fence info string (first word → a class). -/
private def fenceAttr (info : String) : Attr :=
  if info.isEmpty then nullAttr
  else ("", [(info.splitOn " ").headD info], [])

/-- Is a line a horizontal rule (`---`/`***`/`___`)? -/
private def isHr (l : String) : Bool :=
  if leadSpaces l ≤ 3 then
    let t := (l.toList.dropWhile (fun c => c == ' ')).filter (fun c => c != ' ')
    match t with
    | c :: _ => (c == '-' || c == '*' || c == '_') && t.length ≥ 3 && t.all (fun d => d == c)
    | [] => false
  else false

/-- Is a line a blockquote line? -/
private def isBQ (l : String) : Bool := (l.toList.dropWhile (fun c => c == ' ')).headD ' ' == '>'

/-- Strip one level of `>` blockquote marker. -/
private def stripBQ (l : String) : String :=
  match l.toList.dropWhile (fun c => c == ' ') with
  | '>' :: rest => String.ofList (if rest.headD ' ' == ' ' then rest.drop 1 else rest)
  | _ => l

/-- Is a line an indented (4-space) code line? -/
private def isIndentedCode (l : String) : Bool := l.startsWith "    "

/-- Drop up to 4 leading spaces. -/
private def dedent4 (l : String) : String := String.ofList (l.toList.drop (min 4 (leadSpaces l)))

/-- Parse a bullet marker into `(markerWidth, restOfLine)`. -/
private def bulletMarker (l : String) : Option (Nat × String) :=
  let lead := leadSpaces l
  if lead ≤ 3 then
    match l.toList.drop lead with
    | m :: rest =>
      if m == '-' || m == '*' || m == '+' then
        match rest with
        | ' ' :: r => some (lead + 2, String.ofList r)
        | [] => some (lead + 2, "")
        | _ => none
      else none
    | [] => none
  else none

/-- Parse an ordered marker into `(markerWidth, start, delim, restOfLine)`. -/
private def orderedInfo (l : String) : Option (Nat × Int × ListNumberDelim × String) :=
  let lead := leadSpaces l
  if lead ≤ 3 then
    let t := l.toList.drop lead
    let digits := t.takeWhile (fun c => c.isDigit)
    if digits.length ≥ 1 && digits.length ≤ 9 then
      match t.drop digits.length with
      | d :: ' ' :: r =>
        if d == '.' || d == ')' then
          some (lead + digits.length + 2, ((String.ofList digits).toNat!),
                (if d == ')' then .OneParen else .Period), String.ofList r)
        else none
      | [d] =>
        if d == '.' || d == ')' then
          some (lead + digits.length + 1, ((String.ofList digits).toNat!),
                (if d == ')' then .OneParen else .Period), "")
        else none
      | _ => none
    else none
  else none

/-- Is a line a setext underline?  `=`→level 1, `-`→level 2. -/
private def isSetext (l : String) : Option Int :=
  if leadSpaces l ≤ 3 then
    let t := l.toList.dropWhile (fun c => c == ' ')
    match t with
    | '=' :: _ => if t.all (fun c => c == '=' || c == ' ') then some 1 else none
    | '-' :: _ => if t.all (fun c => c == '-' || c == ' ') then some 2 else none
    | _ => none
  else none

/-- Is a line the start of a raw-HTML block? -/
private def isHtmlBlockStart (l : String) : Bool :=
  match l.toList.dropWhile (fun c => c == ' ') with
  | '<' :: c :: _ => c.isAlpha || c == '/' || c == '!'
  | _ => false

/-- Is a line a block starter (used to end paragraphs)? -/
private def isBlockStarter (l : String) : Bool :=
  (atxHeader l).isSome || (fenceStart l).isSome || isHr l || isBQ l ||
  (bulletMarker l).isSome || (orderedInfo l).isSome || isHtmlBlockStart l

/-- Split a pipe-table row into trimmed cells (outer pipes dropped). -/
private def splitRow (l : String) : List String :=
  let t := _root_.Linen.Text.Pandoc.Shared.trim l
  let t := if t.startsWith "|" then String.ofList (t.toList.drop 1) else t
  let t := if t.endsWith "|" then String.ofList (t.toList.dropLast) else t
  (t.splitOn "|").map _root_.Linen.Text.Pandoc.Shared.trim

/-- Is a line a pipe-table delimiter row (`--- | :--:`)? -/
private def isDelimRow (l : String) : Bool :=
  let t := _root_.Linen.Text.Pandoc.Shared.trim l
  !t.isEmpty && t.toList.all (fun c => c == '|' || c == '-' || c == ':' || c == ' ')
    && t.toList.any (fun c => c == '-')

/-- The alignment implied by a delimiter cell. -/
private def alignOf (cell : String) : Alignment :=
  let t := _root_.Linen.Text.Pandoc.Shared.trim cell
  let l := t.startsWith ":"
  let r := t.endsWith ":"
  if l && r then .AlignCenter else if r then .AlignRight else if l then .AlignLeft else .AlignDefault

/-- Do the first two lines begin a pipe table? -/
private def isPipeTable (ls : List String) : Bool :=
  match ls with
  | h :: d :: _ => h.toList.any (fun c => c == '|') && isDelimRow d
  | _ => false

/-- Drop trailing blank lines. -/
private def dropTrailingBlanks (ls : List String) : List String :=
  (ls.reverse.dropWhile isBlank).reverse

/-- Collect fenced-code lines up to the closing fence. -/
private def collectFence (c : Char) (n : Nat) : List String → List String × List String
  | [] => ([], [])
  | l :: ls =>
    if isFenceClose c n l then ([], ls)
    else let r := collectFence c n ls; (l :: r.1, r.2)

/-- Gather a paragraph's lines, detecting a setext underline. -/
private def gatherPara (acc : List String) : List String → List String × Option Int × List String
  | [] => (acc.reverse, none, [])
  | l :: ls =>
    if !acc.isEmpty && (isSetext l).isSome then (acc.reverse, isSetext l, ls)
    else if isBlank l then (acc.reverse, none, l :: ls)
    else if !acc.isEmpty && isBlockStarter l then (acc.reverse, none, l :: ls)
    else gatherPara (l :: acc) ls

/- ── List collection (guarded by consumption; see module note) ───────────── -/

/-- Gather a list item's continuation lines (dedented by `mw`). -/
private unsafe def takeCont (mw : Nat) : List String → List String × List String
  | [] => ([], [])
  | l :: rest =>
    if isBlank l then ([], l :: rest)
    else if leadSpaces l ≥ mw then
      let r := takeCont mw rest
      (String.ofList (l.toList.drop mw) :: r.1, r.2)
    else if (bulletMarker l).isSome || (orderedInfo l).isSome || isBlockStarter l then
      ([], l :: rest)
    else
      let r := takeCont mw rest
      (l :: r.1, r.2)

/-- Collect the line-groups of a list, one per item. -/
private unsafe def collectList (isM : String → Option (Nat × String)) :
    List String → List (List String) × List String
  | [] => ([], [])
  | l :: ls =>
    match isM l with
    | none => ([], l :: ls)
    | some mwRest =>
      let cont := takeCont mwRest.1 ls
      let group := mwRest.2 :: cont.1
      match cont.2.dropWhile isBlank with
      | l2 :: rest2 =>
        if (isM l2).isSome then
          let more := collectList isM (l2 :: rest2)
          (group :: more.1, more.2)
        else (group :: [], cont.2)
      | [] => ([group], [])

/- ── YAML-subset front-matter parser (folded-in; see module note) ────────── -/

/-- Parse a scalar value into a `MetaValue` (`true`/`false` → `MetaBool`; a
    quoted or bare string → `MetaString`). -/
private def scalarValue (s : String) : MetaValue :=
  let t := _root_.Linen.Text.Pandoc.Shared.trim s
  if t == "true" then .MetaBool true
  else if t == "false" then .MetaBool false
  else
    let quoted :=
      (t.startsWith "\"" && t.endsWith "\"" && t.length ≥ 2) ||
      (t.startsWith "'" && t.endsWith "'" && t.length ≥ 2)
    .MetaString (if quoted then _root_.Linen.Text.Pandoc.Shared.stripFirstAndLast t else t)

/-- Split a `key: value` mapping line (trimmed). -/
private def parseKV (s : String) : Option (String × String) :=
  match s.splitOn ":" with
  | k :: rest =>
    if rest.isEmpty then none
    else some (_root_.Linen.Text.Pandoc.Shared.trim k,
               _root_.Linen.Text.Pandoc.Shared.trim (String.intercalate ":" rest))
  | [] => none

/-- The content of a `- item` sequence line, or `none`. -/
private def seqItem (s : String) : Option String :=
  if s == "-" then some ""
  else if s.startsWith "- " then some (_root_.Linen.Text.Pandoc.Shared.trim (String.ofList (s.toList.drop 2)))
  else none

/-- The indentation of the first non-blank line, if any. -/
private def firstNonBlankIndent (ls : List String) : Option Nat :=
  (ls.dropWhile isBlank).head?.map leadSpaces

/-- Does the first non-blank line begin a sequence? -/
private def seqStartsHere (ls : List String) : Bool :=
  match (ls.dropWhile isBlank).head? with
  | some l => (seqItem (_root_.Linen.Text.Pandoc.Shared.trim l)).isSome
  | none => false

mutual

/-- Parse a YAML block mapping at the given indentation. -/
private unsafe def parseYamlMap (indent : Nat) : List String → List (String × MetaValue) × List String
  | [] => ([], [])
  | l :: ls =>
    if isBlank l then
      let r := parseYamlMap indent ls
      (r.1, r.2)
    else if leadSpaces l != indent then ([], l :: ls)
    else match parseKV (_root_.Linen.Text.Pandoc.Shared.trim l) with
      | none => ([], l :: ls)
      | some kv =>
        let val := _root_.Linen.Text.Pandoc.Shared.trim kv.2
        if !val.isEmpty then
          let rest := parseYamlMap indent ls
          ((kv.1, scalarValue val) :: rest.1, rest.2)
        else
          match firstNonBlankIndent ls with
          | some nind =>
            if nind > indent then
              if seqStartsHere ls then
                let sq := parseYamlSeq nind ls
                let rest := parseYamlMap indent sq.2
                ((kv.1, .MetaList sq.1) :: rest.1, rest.2)
              else
                let mp := parseYamlMap nind ls
                let rest := parseYamlMap indent mp.2
                ((kv.1, .MetaMap mp.1) :: rest.1, rest.2)
            else
              let rest := parseYamlMap indent ls
              ((kv.1, .MetaString "") :: rest.1, rest.2)
          | none => ((kv.1, .MetaString "") :: [], [])

/-- Parse a YAML block sequence at the given indentation. -/
private unsafe def parseYamlSeq (indent : Nat) : List String → List MetaValue × List String
  | [] => ([], [])
  | l :: ls =>
    if isBlank l then
      let r := parseYamlSeq indent ls
      (r.1, r.2)
    else if leadSpaces l != indent then ([], l :: ls)
    else match seqItem (_root_.Linen.Text.Pandoc.Shared.trim l) with
      | none => ([], l :: ls)
      | some item =>
        let r := parseYamlSeq indent ls
        (scalarValue item :: r.1, r.2)

end

/-- Extract a leading `---\n…\n---`/`…` YAML block, returning its fields and the
    remaining lines. -/
private unsafe def extractYaml (opts : ReaderOptions) (lines : List String) :
    List (String × MetaValue) × List String :=
  if extensionEnabled .Ext_yaml_metadata_block opts.readerExtensions && lines.headD "" == "---" then
    let rest := lines.drop 1
    let body := rest.takeWhile (fun l => l != "---" && l != "...")
    if body.length < rest.length then
      ((parseYamlMap 0 body).1, rest.drop (body.length + 1))
    else ([], lines)
  else ([], lines)

/- ── Reference / footnote definition collection ──────────────────────────── -/

/-- Parse a reference-link definition `[label]: url "title"`. -/
private def refDef (l : String) : Option (String × (String × String)) :=
  match l.toList.dropWhile (fun c => c == ' ') with
  | '[' :: rest =>
    if rest.headD ' ' == '^' then none
    else
      let label := rest.takeWhile (fun c => c != ']')
      match rest.drop label.length with
      | ']' :: ':' :: r =>
        let r2 := r.dropWhile (fun c => c == ' ')
        let urlChars := r2.takeWhile (fun c => c != ' ')
        let url0 := String.ofList urlChars
        let url := if url0.startsWith "<" && url0.endsWith ">"
                   then _root_.Linen.Text.Pandoc.Shared.stripFirstAndLast url0 else url0
        let titleP := _root_.Linen.Text.Pandoc.Shared.trim (String.ofList (r2.drop urlChars.length))
        let title :=
          if (titleP.startsWith "\"" && titleP.endsWith "\"") ||
             (titleP.startsWith "'" && titleP.endsWith "'") ||
             (titleP.startsWith "(" && titleP.endsWith ")")
          then _root_.Linen.Text.Pandoc.Shared.stripFirstAndLast titleP else ""
        if url.isEmpty then none else some (String.ofList label, (url, title))
      | _ => none
  | _ => none

/-- Parse a footnote definition `[^id]: text`. -/
private def noteDef (l : String) : Option (String × String) :=
  match l.toList.dropWhile (fun c => c == ' ') with
  | '[' :: '^' :: rest =>
    let id := rest.takeWhile (fun c => c != ']')
    match rest.drop id.length with
    | ']' :: ':' :: r => some (String.ofList id, _root_.Linen.Text.Pandoc.Shared.trim (String.ofList r))
    | _ => none
  | _ => none

/-- Strip reference-link and footnote definitions from the line list, returning
    them alongside the remaining lines. -/
private def collectDefs :
    List String → List (String × (String × String)) × List (String × String) × List String
  | [] => ([], [], [])
  | l :: ls =>
    let r := collectDefs ls
    match noteDef l with
    | some nd => (r.1, nd :: r.2.1, r.2.2)
    | none =>
      match refDef l with
      | some rd => (rd :: r.1, r.2.1, r.2.2)
      | none => (r.1, r.2.1, l :: r.2.2)

/- ── The block parser ────────────────────────────────────────────────────── -/

/-- Parse a block from a pipe-table header/delimiter/body, returning the table
    and the leftover lines. -/
private unsafe def pipeTableBlock (ctx : Ctx) (h d : String) (rest : List String) :
    Block × List String :=
  let headers := splitRow h
  let aligns := (splitRow d).map alignOf
  let bodyLines := rest.takeWhile (fun x => !isBlank x && x.toList.any (fun c => c == '|'))
  let leftover := rest.drop bodyLines.length
  let ncol := headers.length
  let alignAt (i : Nat) : Alignment := (aligns[i]?).getD .AlignDefault
  let mkCell (i : Nat) (cells : List String) : Cell :=
    .Cell nullAttr (alignAt i) 1 1 [.Plain (mergeStr (runInlinesM ctx ((cells[i]?).getD "")))]
  let mkRow (cells : List String) : Row :=
    .Row nullAttr ((List.range ncol).map (fun i => mkCell i cells))
  let specs := (List.range ncol).map (fun i => (alignAt i, ColWidth.ColWidthDefault))
  let headRow := mkRow headers
  let bodyRows := bodyLines.map (fun bl => mkRow (splitRow bl))
  (.Table nullAttr (.Caption none []) specs
     (.TableHead nullAttr [headRow])
     [.TableBody nullAttr 0 [] bodyRows]
     (.TableFoot nullAttr []), leftover)

/-- Parse Markdown block structure from a list of lines. -/
private unsafe def parseBlocks (ctx : Ctx) : List String → List Block
  | [] => []
  | allLines@(l :: ls) =>
    if isBlank l then parseBlocks ctx ls
    else match atxHeader l with
    | some hd =>
      .Header hd.1 nullAttr (mergeStr (runInlinesM ctx hd.2)) :: parseBlocks ctx ls
    | none =>
    match fenceStart l with
    | some f =>
      let cf := collectFence f.1 f.2.1 ls
      .CodeBlock (fenceAttr f.2.2) (String.intercalate "\n" cf.1) :: parseBlocks ctx cf.2
    | none =>
    if isHr l then .HorizontalRule :: parseBlocks ctx ls
    else if isBQ l then
      let s := allLines.span isBQ
      .BlockQuote (parseBlocks ctx (s.1.map stripBQ)) :: parseBlocks ctx s.2
    else if isIndentedCode l then
      let s := allLines.span (fun x => isIndentedCode x || isBlank x)
      let codeLines := dropTrailingBlanks s.1
      .CodeBlock nullAttr (String.intercalate "\n" (codeLines.map dedent4)) :: parseBlocks ctx s.2
    else match bulletMarker l with
    | some _ =>
      let r := collectList bulletMarker allLines
      let items := r.1.map (fun grp => parseBlocks ctx grp)
      let tight := (_root_.Linen.Text.Pandoc.Shared.compactify (items.map Many.fromList)).map Many.toList
      let tasked := tight.map (_root_.Linen.Text.Pandoc.Shared.taskListItemFromAscii ctx.opts.readerExtensions)
      .BulletList tasked :: parseBlocks ctx r.2
    | none =>
    match orderedInfo l with
    | some info =>
      let r := collectList (fun x => (orderedInfo x).map (fun oi => (oi.1, oi.2.2.2))) allLines
      let items := r.1.map (fun grp => parseBlocks ctx grp)
      let tight := (_root_.Linen.Text.Pandoc.Shared.compactify (items.map Many.fromList)).map Many.toList
      .OrderedList (info.2.1, .Decimal, info.2.2.1) tight :: parseBlocks ctx r.2
    | none =>
    if extOn ctx .Ext_pipe_tables && isPipeTable allLines then
      match allLines with
      | h :: d :: rest =>
        let tbl := pipeTableBlock ctx h d rest
        tbl.1 :: parseBlocks ctx tbl.2
      | _ => parseBlocks ctx ls
    else if isHtmlBlockStart l then
      let s := allLines.span (fun x => !isBlank x)
      let htmlText := String.intercalate "\n" s.1
      let hb := match _root_.Linen.Text.Pandoc.Readers.HTML.readHtml ctx.opts htmlText with
        | .ok d => d.blocks
        | .error _ => [.RawBlock ⟨"html"⟩ htmlText]
      hb ++ parseBlocks ctx s.2
    else
      let gp := gatherPara [] allLines
      let paraText := _root_.Linen.Text.Pandoc.Shared.trim (String.intercalate "\n" gp.1)
      match gp.2.1 with
      | some lvl => .Header lvl nullAttr (mergeStr (runInlinesM ctx paraText)) :: parseBlocks ctx gp.2.2
      | none =>
        if paraText.isEmpty then parseBlocks ctx gp.2.2
        else .Para (mergeStr (runInlinesM ctx paraText)) :: parseBlocks ctx gp.2.2

/- ── Entry point ─────────────────────────────────────────────────────────── -/

/-- Read a Markdown document into the pandoc AST (implementation). -/
unsafe def readMarkdownImpl (opts : ReaderOptions) (input : String) : Except PandocError Pandoc :=
  let norm := ((input.replace "\r\n" "\n").replace "\r" "\n")
  let expanded := _root_.Linen.Text.Pandoc.Shared.tabFilter opts.readerTabStop norm
  let allLines := expanded.splitOn "\n"
  let ey := extractYaml opts allLines
  let defs := collectDefs ey.2
  let refs := defs.1.map (fun kv => ((_root_.Linen.Text.Pandoc.Parsing.toKey kv.1).unKey, kv.2))
  let ctx : Ctx := ⟨opts, refs, defs.2.1⟩
  let blocks := parseBlocks ctx defs.2.2
  .ok ⟨⟨Data.Map.fromList ey.1⟩, blocks⟩

/-- Read a Markdown document into the pandoc AST. -/
@[implemented_by readMarkdownImpl]
opaque readMarkdown (opts : ReaderOptions) (input : String) : Except PandocError Pandoc

end Linen.Text.Pandoc.Readers.Markdown
