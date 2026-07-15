/-
  `Linen.Text.Pandoc.Readers.Native` — the native (AST-literal) reader.

  ## Haskell source

  Ported from `Text.Pandoc.Readers.Native` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Readers/Native.hs`).

  Upstream's native reader parses the AST's own Haskell-literal serialization:
  it runs `safeRead` (a total wrapper over Haskell's derived `Read`) at each
  granularity, cascading `Pandoc` → `[Block]` → `Block` → `[Inline]` →
  `Inline`, so that even a bare `Str "hi"` becomes a minimal one-paragraph
  document.

  ### Deviations from upstream

  * Lean has no derived `Read`, so instead of `safeRead` this module provides a
    hand-written recursive-descent parser (over `Std.Internal.Parsec`) for the
    **value syntax** the counterpart `Writers.Native` emits — the same
    `Show`-shaped constructor/list/tuple form pandoc's native writer produces
    (`Str "hi"`, `Para [Str "hi",Space]`, `("id",["c"],[("k","v")])`, …).  The
    reader and writer are exact inverses on this grammar.
  * The mutually-recursive value parsers are `unsafe def`: their recursion is
    guarded by input consumption rather than a structural argument, the
    sanctioned escape hatch used elsewhere in this port (e.g. Tier-0
    `Generic.topDown`, `Text.DocLayout.render`).  This is confined to the
    parser; `readNative`'s cascade is a plain `def`.
  * The `RowSpan`/`ColSpan`/`RowHeadColumns` newtypes (bare `Int` in this
    port's `Definition`) are read as bare integers, not `RowSpan 1`; the
    string escapes handled are `\n \t \r \" \\` (matching the writer).
-/

import Std.Internal.Parsec.String
import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Builder
import Linen.Text.Pandoc.Shared
import Linen.Text.Pandoc.Parsing
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Error
import Linen.Data.Map

namespace Linen.Text.Pandoc.Readers.Native

open Std.Internal.Parsec
open Std.Internal.Parsec.String
open _root_.Linen.Text.Pandoc

/- ── Lexical primitives ─────────────────────────────────────────────────── -/

/-- Skip any run of layout whitespace. -/
private def nlws : Parser Unit := do
  let _ ← manyChars (satisfy (fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r'))
  pure ()

/-- Match a literal after skipping whitespace. -/
private def sym (s : String) : Parser Unit := do nlws; skipString s

/-- Parse a Haskell-style constructor / enum identifier. -/
private def identP : Parser String := do
  nlws; many1Chars (satisfy (fun c => c.isAlpha))

/-- Decode one character of a string literal, resolving backslash escapes. -/
private def strChar : Parser Char := do
  let c ← satisfy (fun c => c != '"')
  if c == '\\' then
    let d ← any
    pure (match d with
      | 'n' => '\n' | 't' => '\t' | 'r' => '\r'
      | '"' => '"' | '\\' => '\\' | x => x)
  else pure c

/-- Parse a `"…"` string literal. -/
private def strLit : Parser String := do
  nlws; skipChar '"'
  let s ← manyChars strChar
  skipChar '"'
  pure s

/-- Parse a signed integer literal. -/
private def intLit : Parser Int := do
  nlws
  let neg ← (attempt (skipChar '-') *> pure true) <|> pure false
  let ds ← many1Chars (satisfy Char.isDigit)
  let n := Int.ofNat ds.toNat!
  pure (if neg then -n else n)

/-- Parse a (possibly signed, possibly fractional) floating-point literal. -/
private def floatLit : Parser Float := do
  nlws
  let neg ← (attempt (skipChar '-') *> pure true) <|> pure false
  let whole ← many1Chars (satisfy Char.isDigit)
  let frac ← (attempt (do skipChar '.'; many1Chars (satisfy Char.isDigit))) <|> pure ""
  let wf := whole.toNat!.toFloat
  let ff := if frac.isEmpty then 0.0
            else frac.toNat!.toFloat / ((10 ^ frac.length).toFloat)
  let f := wf + ff
  pure (if neg then -f else f)

/-- Parse a bracketed, comma-separated list `[a,b,…]`. -/
private def listOf (p : Parser α) : Parser (List α) := do
  nlws; skipChar '['
  let r ← (do
      let x ← p
      let xs ← many (attempt (do nlws; skipChar ','; p))
      pure (x :: xs.toList))
    <|> pure []
  nlws; skipChar ']'
  pure r

/-- Parse a 2-tuple `(a,b)`. -/
private def tuple2 (pa : Parser α) (pb : Parser β) : Parser (α × β) := do
  nlws; skipChar '('
  let a ← pa
  nlws; skipChar ','
  let b ← pb
  nlws; skipChar ')'
  pure (a, b)

/-- Parse a 3-tuple `(a,b,c)`. -/
private def tuple3 (pa : Parser α) (pb : Parser β) (pc : Parser γ) : Parser (α × β × γ) := do
  nlws; skipChar '('
  let a ← pa
  nlws; skipChar ','
  let b ← pb
  nlws; skipChar ','
  let c ← pc
  nlws; skipChar ')'
  pure (a, b, c)

/-- Parse an `Attr` triple `(id, classes, kvs)`. -/
private def attrP : Parser Attr :=
  tuple3 strLit (listOf strLit) (listOf (tuple2 strLit strLit))

/-- Parse a `Target` pair `(url, title)`. -/
private def targetP : Parser Target := tuple2 strLit strLit

/-- Parse a `Format "…"` newtype. -/
private def formatP : Parser Format := do sym "Format"; (fun s => ⟨s⟩) <$> strLit

/-- Parse a parenthesised `(Format "…")`, as emitted for raw-block/inline
    format arguments. -/
private def parenFormatP : Parser Format := do
  nlws; skipChar '('; let f ← formatP; nlws; skipChar ')'; pure f

/- ── Enumerations ───────────────────────────────────────────────────────── -/

private def quoteTypeP : Parser QuoteType := do
  match ← identP with
  | "SingleQuote" => pure .SingleQuote
  | "DoubleQuote" => pure .DoubleQuote
  | s => fail s!"expected QuoteType, got {s}"

private def mathTypeP : Parser MathType := do
  match ← identP with
  | "DisplayMath" => pure .DisplayMath
  | "InlineMath" => pure .InlineMath
  | s => fail s!"expected MathType, got {s}"

private def alignmentP : Parser Alignment := do
  match ← identP with
  | "AlignLeft" => pure .AlignLeft
  | "AlignRight" => pure .AlignRight
  | "AlignCenter" => pure .AlignCenter
  | "AlignDefault" => pure .AlignDefault
  | s => fail s!"expected Alignment, got {s}"

private def listNumberStyleP : Parser ListNumberStyle := do
  match ← identP with
  | "DefaultStyle" => pure .DefaultStyle
  | "Example" => pure .Example
  | "Decimal" => pure .Decimal
  | "LowerRoman" => pure .LowerRoman
  | "UpperRoman" => pure .UpperRoman
  | "LowerAlpha" => pure .LowerAlpha
  | "UpperAlpha" => pure .UpperAlpha
  | s => fail s!"expected ListNumberStyle, got {s}"

private def listNumberDelimP : Parser ListNumberDelim := do
  match ← identP with
  | "DefaultDelim" => pure .DefaultDelim
  | "Period" => pure .Period
  | "OneParen" => pure .OneParen
  | "TwoParens" => pure .TwoParens
  | s => fail s!"expected ListNumberDelim, got {s}"

private def citationModeP : Parser CitationMode := do
  match ← identP with
  | "AuthorInText" => pure .AuthorInText
  | "SuppressAuthor" => pure .SuppressAuthor
  | "NormalCitation" => pure .NormalCitation
  | s => fail s!"expected CitationMode, got {s}"

private def boolP : Parser Bool := do
  match ← identP with
  | "True" => pure true
  | "False" => pure false
  | s => fail s!"expected Bool, got {s}"

private def colWidthP : Parser ColWidth := do
  match ← identP with
  | "ColWidth" => (fun w => .ColWidth w) <$> floatLit
  | "ColWidthDefault" => pure .ColWidthDefault
  | s => fail s!"expected ColWidth, got {s}"

private def listAttributesP : Parser ListAttributes :=
  tuple3 intLit listNumberStyleP listNumberDelimP

private def colSpecP : Parser ColSpec := tuple2 alignmentP colWidthP

/- ── The recursive value grammar ────────────────────────────────────────── -/

mutual

/-- Parse a single `Inline`. -/
unsafe def inlineP : Parser Inline := do
  match ← identP with
  | "Str" => .Str <$> strLit
  | "Emph" => .Emph <$> inlineListP
  | "Underline" => .Underline <$> inlineListP
  | "Strong" => .Strong <$> inlineListP
  | "Strikeout" => .Strikeout <$> inlineListP
  | "Superscript" => .Superscript <$> inlineListP
  | "Subscript" => .Subscript <$> inlineListP
  | "SmallCaps" => .SmallCaps <$> inlineListP
  | "Quoted" => do let q ← quoteTypeP; let xs ← inlineListP; pure (.Quoted q xs)
  | "Cite" => do let cs ← citationListP; let xs ← inlineListP; pure (.Cite cs xs)
  | "Code" => do let a ← attrP; let s ← strLit; pure (.Code a s)
  | "Space" => pure .Space
  | "SoftBreak" => pure .SoftBreak
  | "LineBreak" => pure .LineBreak
  | "Math" => do let mt ← mathTypeP; let s ← strLit; pure (.Math mt s)
  | "RawInline" => do let f ← parenFormatP; let s ← strLit; pure (.RawInline f s)
  | "Link" => do let a ← attrP; let xs ← inlineListP; let t ← targetP; pure (.Link a xs t)
  | "Image" => do let a ← attrP; let xs ← inlineListP; let t ← targetP; pure (.Image a xs t)
  | "Note" => .Note <$> blockListP
  | "Span" => do let a ← attrP; let xs ← inlineListP; pure (.Span a xs)
  | s => fail s!"unknown Inline: {s}"

/-- Parse a list of `Inline`s. -/
unsafe def inlineListP : Parser (List Inline) := listOf inlineP

/-- Parse a list of inline-lines (for `LineBlock`). -/
unsafe def inlineListListP : Parser (List (List Inline)) := listOf inlineListP

/-- Parse a single `Block`. -/
unsafe def blockP : Parser Block := do
  match ← identP with
  | "Plain" => .Plain <$> inlineListP
  | "Para" => .Para <$> inlineListP
  | "LineBlock" => .LineBlock <$> inlineListListP
  | "CodeBlock" => do let a ← attrP; let s ← strLit; pure (.CodeBlock a s)
  | "RawBlock" => do let f ← parenFormatP; let s ← strLit; pure (.RawBlock f s)
  | "BlockQuote" => .BlockQuote <$> blockListP
  | "OrderedList" => do let la ← listAttributesP; let items ← blockListListP; pure (.OrderedList la items)
  | "BulletList" => .BulletList <$> blockListListP
  | "DefinitionList" => .DefinitionList <$> defListP
  | "Header" => do let n ← intLit; let a ← attrP; let xs ← inlineListP; pure (.Header n a xs)
  | "HorizontalRule" => pure .HorizontalRule
  | "Table" => do
      let a ← attrP; let c ← captionP; let specs ← listOf colSpecP
      let hd ← tableHeadP; let bs ← listOf tableBodyP; let ft ← tableFootP
      pure (.Table a c specs hd bs ft)
  | "Figure" => do let a ← attrP; let c ← captionP; let bs ← blockListP; pure (.Figure a c bs)
  | "Div" => do let a ← attrP; let bs ← blockListP; pure (.Div a bs)
  | s => fail s!"unknown Block: {s}"

/-- Parse a list of `Block`s. -/
unsafe def blockListP : Parser (List Block) := listOf blockP

/-- Parse a list of block-lists (list items). -/
unsafe def blockListListP : Parser (List (List Block)) := listOf blockListP

/-- Parse a definition list's `(term, definitions)` items. -/
unsafe def defListP : Parser (List (List Inline × List (List Block))) :=
  listOf (tuple2 inlineListP blockListListP)

/-- Parse a single `Citation`. -/
unsafe def citationP : Parser Citation := do
  sym "Citation"; nlws; skipChar '{'
  sym "citationId"; sym "="; let cid ← strLit
  sym ","; sym "citationPrefix"; sym "="; let pref ← inlineListP
  sym ","; sym "citationSuffix"; sym "="; let suff ← inlineListP
  sym ","; sym "citationMode"; sym "="; let mode ← citationModeP
  sym ","; sym "citationNoteNum"; sym "="; let nn ← intLit
  sym ","; sym "citationHash"; sym "="; let hash ← intLit
  nlws; skipChar '}'
  pure (.mk cid pref suff mode nn hash)

/-- Parse a list of citations. -/
unsafe def citationListP : Parser (List Citation) := listOf citationP

/-- Parse a `Cell`. -/
unsafe def cellP : Parser Cell := do
  sym "Cell"; let a ← attrP; let al ← alignmentP; let rs ← intLit; let cs ← intLit
  let bs ← blockListP
  pure (.Cell a al rs cs bs)

/-- Parse a `Row`. -/
unsafe def rowP : Parser Row := do
  sym "Row"; let a ← attrP; let cells ← listOf cellP; pure (.Row a cells)

/-- Parse a `TableHead`. -/
unsafe def tableHeadP : Parser TableHead := do
  sym "TableHead"; let a ← attrP; let rows ← listOf rowP; pure (.TableHead a rows)

/-- Parse a `TableBody`. -/
unsafe def tableBodyP : Parser TableBody := do
  sym "TableBody"; let a ← attrP; let rhc ← intLit
  let ih ← listOf rowP; let bd ← listOf rowP
  pure (.TableBody a rhc ih bd)

/-- Parse a `TableFoot`. -/
unsafe def tableFootP : Parser TableFoot := do
  sym "TableFoot"; let a ← attrP; let rows ← listOf rowP; pure (.TableFoot a rows)

/-- Parse a `Caption`. -/
unsafe def captionP : Parser Caption := do
  sym "Caption"
  nlws; skipChar '('
  let short ← (do sym "Nothing"; pure (none : Option (List Inline)))
    <|> (do sym "Just"; some <$> inlineListP)
  nlws; skipChar ')'
  let bs ← blockListP
  pure (.Caption short bs)

/-- Parse a `MetaValue`. -/
unsafe def metaValueP : Parser MetaValue := do
  match ← identP with
  | "MetaMap" => do
      nlws; skipChar '('; sym "fromList"
      let kvs ← listOf (tuple2 strLit metaValueP)
      nlws; skipChar ')'
      pure (.MetaMap kvs)
  | "MetaList" => .MetaList <$> listOf metaValueP
  | "MetaBool" => .MetaBool <$> boolP
  | "MetaString" => .MetaString <$> strLit
  | "MetaInlines" => .MetaInlines <$> inlineListP
  | "MetaBlocks" => .MetaBlocks <$> blockListP
  | s => fail s!"unknown MetaValue: {s}"

end

/-- Parse a `Meta` record `Meta {unMeta = fromList [(k, v)]}`. -/
private unsafe def metaP : Parser Meta := do
  sym "Meta"; nlws; skipChar '{'; sym "unMeta"; sym "="; sym "fromList"
  let kvs ← listOf (tuple2 strLit metaValueP)
  nlws; skipChar '}'
  pure ⟨Data.Map.fromList kvs⟩

/-- Parse a whole `Pandoc` document `Pandoc <meta> <blocks>`. -/
private unsafe def pandocP : Parser Pandoc := do
  sym "Pandoc"; let m ← metaP; let bs ← blockListP; pure ⟨m, bs⟩

/- ── The cascading reader ────────────────────────────────────────────────── -/

/-- Run a parser over the full input (leading/trailing whitespace allowed),
    returning `some` on complete success. -/
private unsafe def runFull (p : Parser α) (input : String) : Option α :=
  match (do nlws; let x ← p; nlws; eof; pure x).run input with
  | .ok a => some a
  | .error _ => none

/-- Read a native (AST-literal) document.  Cascades `Pandoc` → `[Block]` →
    `Block` → `[Inline]` → `Inline`, matching upstream's `safeRead` fallback
    chain. -/
unsafe def readNativeImpl (_opts : ReaderOptions) (input : String) : Except PandocError Pandoc :=
  match runFull pandocP input with
  | some d => .ok d
  | none =>
    match runFull blockListP input with
    | some bs => .ok ⟨nullMeta, bs⟩
    | none =>
      match runFull blockP input with
      | some b => .ok ⟨nullMeta, [b]⟩
      | none =>
        match runFull inlineListP input with
        | some ils => .ok ⟨nullMeta, [.Plain ils]⟩
        | none =>
          match runFull inlineP input with
          | some il => .ok ⟨nullMeta, [.Plain [il]]⟩
          | none => .error (.PandocParseError s!"Could not read: {input}")

/-- Read a native (AST-literal) document from a string. -/
@[implemented_by readNativeImpl]
opaque readNative (opts : ReaderOptions) (input : String) : Except PandocError Pandoc

end Linen.Text.Pandoc.Readers.Native
