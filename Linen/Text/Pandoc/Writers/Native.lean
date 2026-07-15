/-
  `Linen.Text.Pandoc.Writers.Native` — the native (AST-literal) writer.

  ## Haskell source

  Ported from `Text.Pandoc.Writers.Native` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Writers/Native.hs`).

  Upstream's `writeNative` renders the AST via `Text.Show.Pretty.ppDoc` (the
  `pretty-show` library): a generic, derived-`Show`-based pretty-printer.  When
  `writerTemplate` is set it pretty-prints the whole `Pandoc` (metadata
  included); otherwise it prints only the `blocks` list.

  ### Deviations from upstream

  * `pretty-show` (which drives `ppDoc`'s exact indentation) is deferred (see
    `docs/imports/pandoc/dependencies.md`), and Lean's derived `Repr` qualifies
    constructor names (`Linen.Text.Pandoc.Inline.Str …`).  So this module hosts
    a small hand-written pretty-printer that emits the **same `Show`-shaped
    value syntax** pandoc's native format uses — unqualified constructors,
    `[…]` lists, `(…)` tuples, `Format "…"` — which the counterpart
    `Readers.Native` reads back exactly.  The two are exact inverses.
  * The output is compact (single expression, no `pretty-show` line-wrapping),
    since the reader is whitespace-insensitive; the shape (not the exact
    indentation) is what round-trips.
  * `RowSpan`/`ColSpan`/`RowHeadColumns` are emitted as bare integers (they are
    bare `Int` in this port's `Definition`), and string escapes cover
    `\n \t \r \" \\`.
-/

import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Writers.Shared

namespace Linen.Text.Pandoc.Writers.Native

open _root_.Linen.Text.Pandoc

/- ── Leaf renderers ─────────────────────────────────────────────────────── -/

/-- Render a string as a Haskell-style double-quoted literal. -/
def showStr (s : String) : String :=
  "\"" ++ String.join (s.toList.map fun c =>
    match c with
    | '"' => "\\\""
    | '\\' => "\\\\"
    | '\n' => "\\n"
    | '\t' => "\\t"
    | '\r' => "\\r"
    | _ => c.toString) ++ "\""

/-- Render a Haskell-style list `[a,b,…]` given an element renderer. -/
private def showList (f : α → String) (xs : List α) : String :=
  "[" ++ ",".intercalate (xs.map f) ++ "]"

private def showAttr (a : Attr) : String :=
  let (ident, classes, kvs) := a
  "(" ++ showStr ident ++ ","
    ++ showList showStr classes ++ ","
    ++ showList (fun (kv : String × String) => "(" ++ showStr kv.1 ++ "," ++ showStr kv.2 ++ ")") kvs
    ++ ")"

private def showTarget (t : Target) : String :=
  "(" ++ showStr t.1 ++ "," ++ showStr t.2 ++ ")"

private def showFormat (f : Format) : String := "Format " ++ showStr f.unFormat

private def showQuoteType : QuoteType → String
  | .SingleQuote => "SingleQuote"
  | .DoubleQuote => "DoubleQuote"

private def showMathType : MathType → String
  | .DisplayMath => "DisplayMath"
  | .InlineMath => "InlineMath"

private def showAlignment : Alignment → String
  | .AlignLeft => "AlignLeft"
  | .AlignRight => "AlignRight"
  | .AlignCenter => "AlignCenter"
  | .AlignDefault => "AlignDefault"

private def showListNumberStyle : ListNumberStyle → String
  | .DefaultStyle => "DefaultStyle"
  | .Example => "Example"
  | .Decimal => "Decimal"
  | .LowerRoman => "LowerRoman"
  | .UpperRoman => "UpperRoman"
  | .LowerAlpha => "LowerAlpha"
  | .UpperAlpha => "UpperAlpha"

private def showListNumberDelim : ListNumberDelim → String
  | .DefaultDelim => "DefaultDelim"
  | .Period => "Period"
  | .OneParen => "OneParen"
  | .TwoParens => "TwoParens"

private def showCitationMode : CitationMode → String
  | .AuthorInText => "AuthorInText"
  | .SuppressAuthor => "SuppressAuthor"
  | .NormalCitation => "NormalCitation"

private def showColWidth : ColWidth → String
  | .ColWidth w => "ColWidth " ++ toString w
  | .ColWidthDefault => "ColWidthDefault"

private def showListAttributes (la : ListAttributes) : String :=
  "(" ++ toString la.1 ++ "," ++ showListNumberStyle la.2.1 ++ "," ++ showListNumberDelim la.2.2 ++ ")"

private def showColSpec (c : ColSpec) : String :=
  "(" ++ showAlignment c.1 ++ "," ++ showColWidth c.2 ++ ")"

/- ── The recursive AST renderers ────────────────────────────────────────── -/

mutual

/-- Render an `Inline` in native (`Show`-shaped) value syntax. -/
def showInline : Inline → String
  | .Str s => "Str " ++ showStr s
  | .Emph xs => "Emph " ++ showInlineList xs
  | .Underline xs => "Underline " ++ showInlineList xs
  | .Strong xs => "Strong " ++ showInlineList xs
  | .Strikeout xs => "Strikeout " ++ showInlineList xs
  | .Superscript xs => "Superscript " ++ showInlineList xs
  | .Subscript xs => "Subscript " ++ showInlineList xs
  | .SmallCaps xs => "SmallCaps " ++ showInlineList xs
  | .Quoted q xs => "Quoted " ++ showQuoteType q ++ " " ++ showInlineList xs
  | .Cite cs xs => "Cite " ++ showCitationList cs ++ " " ++ showInlineList xs
  | .Code a s => "Code " ++ showAttr a ++ " " ++ showStr s
  | .Space => "Space"
  | .SoftBreak => "SoftBreak"
  | .LineBreak => "LineBreak"
  | .Math mt s => "Math " ++ showMathType mt ++ " " ++ showStr s
  | .RawInline f s => "RawInline " ++ "(" ++ showFormat f ++ ") " ++ showStr s
  | .Link a xs t => "Link " ++ showAttr a ++ " " ++ showInlineList xs ++ " " ++ showTarget t
  | .Image a xs t => "Image " ++ showAttr a ++ " " ++ showInlineList xs ++ " " ++ showTarget t
  | .Note bs => "Note " ++ showBlockList bs
  | .Span a xs => "Span " ++ showAttr a ++ " " ++ showInlineList xs

/-- Render a list of `Inline`s. -/
def showInlineList : List Inline → String
  | [] => "[]"
  | x :: xs => "[" ++ showInline x ++ tailInlines xs ++ "]"

private def tailInlines : List Inline → String
  | [] => ""
  | x :: xs => "," ++ showInline x ++ tailInlines xs

/-- Render a list of inline-lines. -/
def showInlineListList : List (List Inline) → String
  | [] => "[]"
  | x :: xs => "[" ++ showInlineList x ++ tailInlineLists xs ++ "]"

private def tailInlineLists : List (List Inline) → String
  | [] => ""
  | x :: xs => "," ++ showInlineList x ++ tailInlineLists xs

/-- Render a `Block` in native value syntax. -/
def showBlock : Block → String
  | .Plain xs => "Plain " ++ showInlineList xs
  | .Para xs => "Para " ++ showInlineList xs
  | .LineBlock xss => "LineBlock " ++ showInlineListList xss
  | .CodeBlock a s => "CodeBlock " ++ showAttr a ++ " " ++ showStr s
  | .RawBlock f s => "RawBlock " ++ "(" ++ showFormat f ++ ") " ++ showStr s
  | .BlockQuote bs => "BlockQuote " ++ showBlockList bs
  | .OrderedList la items => "OrderedList " ++ showListAttributes la ++ " " ++ showBlockListList items
  | .BulletList items => "BulletList " ++ showBlockListList items
  | .DefinitionList items => "DefinitionList " ++ showDefList items
  | .Header n a xs => "Header " ++ toString n ++ " " ++ showAttr a ++ " " ++ showInlineList xs
  | .HorizontalRule => "HorizontalRule"
  | .Table a c specs hd bs ft =>
      "Table " ++ showAttr a ++ " " ++ showCaption c ++ " "
        ++ showColSpecList specs ++ " "
        ++ showTableHead hd ++ " "
        ++ showTableBodyList bs ++ " "
        ++ showTableFoot ft
  | .Figure a c bs => "Figure " ++ showAttr a ++ " " ++ showCaption c ++ " " ++ showBlockList bs
  | .Div a bs => "Div " ++ showAttr a ++ " " ++ showBlockList bs

/-- Render a list of `Block`s. -/
def showBlockList : List Block → String
  | [] => "[]"
  | x :: xs => "[" ++ showBlock x ++ tailBlocks xs ++ "]"

private def tailBlocks : List Block → String
  | [] => ""
  | x :: xs => "," ++ showBlock x ++ tailBlocks xs

/-- Render a list of block-lists. -/
def showBlockListList : List (List Block) → String
  | [] => "[]"
  | x :: xs => "[" ++ showBlockList x ++ tailBlockLists xs ++ "]"

private def tailBlockLists : List (List Block) → String
  | [] => ""
  | x :: xs => "," ++ showBlockList x ++ tailBlockLists xs

private def showDef (d : List Inline × List (List Block)) : String :=
  "(" ++ showInlineList d.1 ++ "," ++ showBlockListList d.2 ++ ")"

private def showDefList : List (List Inline × List (List Block)) → String
  | [] => "[]"
  | x :: xs => "[" ++ showDef x ++ tailDefs xs ++ "]"

private def tailDefs : List (List Inline × List (List Block)) → String
  | [] => ""
  | x :: xs => "," ++ showDef x ++ tailDefs xs

private def showCitation : Citation → String
  | .mk cid pref suff mode nn hash =>
      "Citation {citationId = " ++ showStr cid
        ++ ", citationPrefix = " ++ showInlineList pref
        ++ ", citationSuffix = " ++ showInlineList suff
        ++ ", citationMode = " ++ showCitationMode mode
        ++ ", citationNoteNum = " ++ toString nn
        ++ ", citationHash = " ++ toString hash ++ "}"

private def showCitationList : List Citation → String
  | [] => "[]"
  | x :: xs => "[" ++ showCitation x ++ tailCitations xs ++ "]"

private def tailCitations : List Citation → String
  | [] => ""
  | x :: xs => "," ++ showCitation x ++ tailCitations xs

private def showCell : Cell → String
  | .Cell a al rs cs bs =>
      "Cell " ++ showAttr a ++ " " ++ showAlignment al ++ " "
        ++ toString rs ++ " " ++ toString cs ++ " " ++ showBlockList bs

private def showCellList : List Cell → String
  | [] => "[]"
  | x :: xs => "[" ++ showCell x ++ tailCells xs ++ "]"

private def tailCells : List Cell → String
  | [] => ""
  | x :: xs => "," ++ showCell x ++ tailCells xs

private def showRow : Row → String
  | .Row a cells => "Row " ++ showAttr a ++ " " ++ showCellList cells

private def showRowList : List Row → String
  | [] => "[]"
  | x :: xs => "[" ++ showRow x ++ tailRows xs ++ "]"

private def tailRows : List Row → String
  | [] => ""
  | x :: xs => "," ++ showRow x ++ tailRows xs

private def showTableHead : TableHead → String
  | .TableHead a rows => "TableHead " ++ showAttr a ++ " " ++ showRowList rows

private def showTableBody : TableBody → String
  | .TableBody a rhc ih bd =>
      "TableBody " ++ showAttr a ++ " " ++ toString rhc ++ " "
        ++ showRowList ih ++ " " ++ showRowList bd

private def showTableFoot : TableFoot → String
  | .TableFoot a rows => "TableFoot " ++ showAttr a ++ " " ++ showRowList rows

private def showTableBodyList : List TableBody → String
  | [] => "[]"
  | x :: xs => "[" ++ showTableBody x ++ tailTableBodies xs ++ "]"

private def tailTableBodies : List TableBody → String
  | [] => ""
  | x :: xs => "," ++ showTableBody x ++ tailTableBodies xs

private def showColSpecList : List ColSpec → String :=
  fun specs => "[" ++ ",".intercalate (specs.map showColSpec) ++ "]"

private def showCaption : Caption → String
  | .Caption short bs =>
      let s := match short with
        | none => "Nothing"
        | some ils => "Just " ++ showInlineList ils
      "Caption " ++ "(" ++ s ++ ") " ++ showBlockList bs

private def showMetaValue : MetaValue → String
  | .MetaMap m => "MetaMap (fromList " ++ showMetaMap m ++ ")"
  | .MetaList xs => "MetaList " ++ showMetaList xs
  | .MetaBool b => "MetaBool " ++ (if b then "True" else "False")
  | .MetaString s => "MetaString " ++ showStr s
  | .MetaInlines xs => "MetaInlines " ++ showInlineList xs
  | .MetaBlocks bs => "MetaBlocks " ++ showBlockList bs

private def showMetaList : List MetaValue → String
  | [] => "[]"
  | x :: xs => "[" ++ showMetaValue x ++ tailMetaValues xs ++ "]"

private def tailMetaValues : List MetaValue → String
  | [] => ""
  | x :: xs => "," ++ showMetaValue x ++ tailMetaValues xs

private def showMetaField (kv : String × MetaValue) : String :=
  "(" ++ showStr kv.1 ++ "," ++ showMetaValue kv.2 ++ ")"

private def showMetaMap : List (String × MetaValue) → String
  | [] => "[]"
  | x :: xs => "[" ++ showMetaField x ++ tailMetaFields xs ++ "]"

private def tailMetaFields : List (String × MetaValue) → String
  | [] => ""
  | x :: xs => "," ++ showMetaField x ++ tailMetaFields xs

end

/-- Render `Meta` as `Meta {unMeta = fromList […]}`. -/
def showMeta (m : Meta) : String :=
  "Meta {unMeta = fromList " ++ showMetaMap m.unMeta.toList' ++ "}"

/- ── The writer entry point ─────────────────────────────────────────────── -/

/-- Render a document in native (AST-literal) syntax.  When a template is set
    the whole `Pandoc` (metadata included) is printed; otherwise only the
    block list, matching upstream. -/
def writeNativeString (opts : WriterOptions) (doc : Pandoc) : String :=
  match opts.writerTemplate with
  | some _ => "Pandoc " ++ showMeta doc.docMeta ++ " " ++ showBlockList doc.blocks
  | none => showBlockList doc.blocks

/-- Monadic wrapper matching upstream's `writeNative :: … -> m Text`. -/
def writeNative {m : Type → Type} [Monad m] (opts : WriterOptions) (doc : Pandoc) : m String :=
  pure (writeNativeString opts doc)

end Linen.Text.Pandoc.Writers.Native
