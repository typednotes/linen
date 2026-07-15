/-
  `Linen.Text.Pandoc.Definition` — the Pandoc document AST.

  ## Haskell source

  Ported from `Text.Pandoc.Definition` in the `pandoc-types` package
  (v1.23.1, `src/Text/Pandoc/Definition.hs`).

  Provides the format-neutral document model: `Pandoc`, `Meta`/`MetaValue`,
  `Block`, `Inline`, the table sub-structures (`Row`/`Cell`/`TableHead`/
  `TableBody`/`TableFoot`/`Caption`/`ColSpec`), `Citation`, and the small
  enum/attribute types (`Alignment`, `ColWidth`, `ListNumberStyle`,
  `ListNumberDelim`, `QuoteType`, `MathType`, `CitationMode`, `Format`,
  `Attr`, `Target`).  It also carries the hand-tuned `ToJSON`/`FromJSON`
  bridge (upstream defines these here via `deriveJSON`, with the pandoc
  tagged-object encoding `{"t": …, "c": …}`).

  ### Deviations from upstream

  * `MetaValue.MetaMap` holds an association `List (String × MetaValue)`
    rather than a `Data.Map`: Lean's kernel rejects a recursive inductive
    that occurs nested inside `Data.Map` (a `Std.TreeMap`, itself a
    well-formedness-bundled balanced tree), so the self-referential map field
    must be a plain list.  The
    non-recursive `Meta` newtype still wraps a genuine `Data.Map`.  Both
    serialise to a JSON object, matching aeson.
  * The `RowSpan`/`ColSpan`/`RowHeadColumns` newtypes over `Int` become bare
    `Int` abbreviations (Lean has no `GeneralizedNewtypeDeriving`; they encode
    as plain JSON numbers exactly as upstream).
  * `Format`'s equality is case-insensitive (upstream `Eq`/`Ord` fold case);
    this is realised as a case-folding `BEq` instance, so the derived `BEq`
    for `Inline`/`Block` compares raw-block/raw-inline formats case-insensitively.
  * `Double` becomes `Float`; `Text` becomes `String`.  The `Data`/`Generic`/
    `NFData`/`Read`/`Ord`/pattern-synonym (`SimpleFigure`) derivations are
    dropped (no Lean analogue / out of scope).
  * The `FromJSON` decoder expects the canonical field order `{"t":…,"c":…}`
    that pandoc (and our encoder) emits, matching against it positionally so
    the recursion is structural over the JSON tree (no `partial`/`unsafe`).
-/

import Linen.Data.Json
import Linen.Data.Map

namespace Linen.Text.Pandoc

open Data.Json

-- ── Attributes, formats and simple aliases ────────────────────────────

/-- Attributes: identifier, classes, key-value pairs. -/
abbrev Attr := String × List String × List (String × String)

/-- The empty attribute triple `("", [], [])`. -/
def nullAttr : Attr := ("", [], [])

/-- Link target: `(URL, title)`. -/
abbrev Target := String × String

/-- Case-fold a string (lowercasing), used for `Format`'s comparison. -/
private def caseFold (s : String) : String := String.ofList (s.toList.map Char.toLower)

/-- Formats for raw blocks/inlines.  Equality is case-insensitive. -/
structure Format where
  unFormat : String
  deriving Repr, Inhabited

instance : BEq Format where
  beq a b := caseFold a.unFormat == caseFold b.unFormat

instance : Coe String Format := ⟨fun s => ⟨caseFold s⟩⟩

-- ── Enumerations ──────────────────────────────────────────────────────

/-- Style of list numbers. -/
inductive ListNumberStyle
  | DefaultStyle | Example | Decimal
  | LowerRoman | UpperRoman | LowerAlpha | UpperAlpha
  deriving Repr, BEq, Inhabited, DecidableEq

/-- Delimiter of list numbers. -/
inductive ListNumberDelim
  | DefaultDelim | Period | OneParen | TwoParens
  deriving Repr, BEq, Inhabited, DecidableEq

/-- Alignment of a table column. -/
inductive Alignment
  | AlignLeft | AlignRight | AlignCenter | AlignDefault
  deriving Repr, BEq, Inhabited, DecidableEq

/-- The width of a table column, as a fraction of the text width. -/
inductive ColWidth
  | ColWidth (w : Float)
  | ColWidthDefault
  deriving Repr, BEq, Inhabited

/-- Type of quotation marks used in a `Quoted` inline. -/
inductive QuoteType
  | SingleQuote | DoubleQuote
  deriving Repr, BEq, Inhabited, DecidableEq

/-- Type of math element (display or inline). -/
inductive MathType
  | DisplayMath | InlineMath
  deriving Repr, BEq, Inhabited, DecidableEq

/-- The citation mode: author-in-text, suppressed-author, or normal. -/
inductive CitationMode
  | AuthorInText | SuppressAuthor | NormalCitation
  deriving Repr, BEq, Inhabited, DecidableEq

/-- List attributes: start number, numbering style, delimiter style. -/
abbrev ListAttributes := Int × ListNumberStyle × ListNumberDelim

/-- The specification for a single table column. -/
abbrev ColSpec := Alignment × ColWidth

/-- Number of rows/columns occupied by a cell, and the row-head width of a
    `TableBody` (bare `Int`; see the module deviation note). -/
abbrev RowSpan := Int
abbrev ColSpan := Int
abbrev RowHeadColumns := Int

-- ── The document AST (mutually recursive core) ────────────────────────

mutual

/-- Inline elements. -/
inductive Inline
  | Str (text : String)
  | Emph (contents : List Inline)
  | Underline (contents : List Inline)
  | Strong (contents : List Inline)
  | Strikeout (contents : List Inline)
  | Superscript (contents : List Inline)
  | Subscript (contents : List Inline)
  | SmallCaps (contents : List Inline)
  | Quoted (quoteType : QuoteType) (contents : List Inline)
  | Cite (citations : List Citation) (contents : List Inline)
  | Code (attr : Attr) (text : String)
  | Space
  | SoftBreak
  | LineBreak
  | Math (mathType : MathType) (text : String)
  | RawInline (format : Format) (text : String)
  | Link (attr : Attr) (contents : List Inline) (target : Target)
  | Image (attr : Attr) (contents : List Inline) (target : Target)
  | Note (contents : List Block)
  | Span (attr : Attr) (contents : List Inline)

/-- Block elements. -/
inductive Block
  | Plain (contents : List Inline)
  | Para (contents : List Inline)
  | LineBlock (lines : List (List Inline))
  | CodeBlock (attr : Attr) (text : String)
  | RawBlock (format : Format) (text : String)
  | BlockQuote (contents : List Block)
  | OrderedList (attrs : ListAttributes) (items : List (List Block))
  | BulletList (items : List (List Block))
  | DefinitionList (items : List (List Inline × List (List Block)))
  | Header (level : Int) (attr : Attr) (contents : List Inline)
  | HorizontalRule
  | Table (attr : Attr) (caption : Caption) (colSpecs : List ColSpec)
          (head : TableHead) (bodies : List TableBody) (foot : TableFoot)
  | Figure (attr : Attr) (caption : Caption) (contents : List Block)
  | Div (attr : Attr) (contents : List Block)

/-- A citation. -/
inductive Citation
  | mk (citationId : String) (citationPrefix : List Inline)
       (citationSuffix : List Inline) (citationMode : CitationMode)
       (citationNoteNum : Int) (citationHash : Int)

/-- A table row. -/
inductive Row
  | Row (attr : Attr) (cells : List Cell)

/-- The head of a table. -/
inductive TableHead
  | TableHead (attr : Attr) (rows : List Row)

/-- A body of a table, with an intermediate head and body. -/
inductive TableBody
  | TableBody (attr : Attr) (rowHeadColumns : RowHeadColumns)
              (intermediateHead : List Row) (body : List Row)

/-- The foot of a table. -/
inductive TableFoot
  | TableFoot (attr : Attr) (rows : List Row)

/-- A table cell. -/
inductive Cell
  | Cell (attr : Attr) (alignment : Alignment) (rowSpan : RowSpan)
         (colSpan : ColSpan) (contents : List Block)

/-- The caption of a table or figure, with an optional short caption. -/
inductive Caption
  | Caption (short : Option (List Inline)) (contents : List Block)

/-- A metadata value. -/
inductive MetaValue
  | MetaMap (map : List (String × MetaValue))
  | MetaList (values : List MetaValue)
  | MetaBool (b : Bool)
  | MetaString (s : String)
  | MetaInlines (contents : List Inline)
  | MetaBlocks (contents : List Block)

end

/-- A short caption, for use in e.g. lists of figures. -/
abbrev ShortCaption := List Inline

deriving instance Repr, BEq for
  Inline, Block, Citation, Row, TableHead, TableBody, TableFoot, Cell,
  Caption, MetaValue

instance : Inhabited Inline := ⟨.Space⟩
instance : Inhabited Block := ⟨.HorizontalRule⟩
instance : Inhabited MetaValue := ⟨.MetaBool false⟩

-- ── Citation field accessors ──────────────────────────────────────────

/-- The citation identifier. -/
def Citation.citationId : Citation → String | .mk i _ _ _ _ _ => i
/-- The citation prefix inlines. -/
def Citation.citationPrefix : Citation → List Inline | .mk _ p _ _ _ _ => p
/-- The citation suffix inlines. -/
def Citation.citationSuffix : Citation → List Inline | .mk _ _ s _ _ _ => s
/-- The citation mode. -/
def Citation.citationMode : Citation → CitationMode | .mk _ _ _ m _ _ => m
/-- The citation note number. -/
def Citation.citationNoteNum : Citation → Int | .mk _ _ _ _ n _ => n
/-- The citation hash. -/
def Citation.citationHash : Citation → Int | .mk _ _ _ _ _ h => h

-- ── Meta and Pandoc ───────────────────────────────────────────────────

/-- Document metadata: a map from keys to `MetaValue`s. -/
structure Meta where
  unMeta : Data.Map String MetaValue
  deriving Repr, BEq, Inhabited

/-- A Pandoc document: metadata plus a list of blocks. -/
structure Pandoc where
  docMeta : Meta
  blocks : List Block
  deriving Repr, BEq, Inhabited

/-- The empty metadata. -/
def nullMeta : Meta := ⟨Data.Map.empty⟩

/-- Is the metadata empty? -/
def isNullMeta (m : Meta) : Bool := m.unMeta.null

/-- Retrieve the metadata value for a given key. -/
def lookupMeta (key : String) (m : Meta) : Option MetaValue := m.unMeta.lookup key

/-- Extract the document title from metadata. -/
def docTitle (m : Meta) : List Inline :=
  match lookupMeta "title" m with
  | some (.MetaString s) => [.Str s]
  | some (.MetaInlines ils) => ils
  | some (.MetaBlocks [.Plain ils]) => ils
  | some (.MetaBlocks [.Para ils]) => ils
  | _ => []

/-- Extract document authors from metadata. -/
def docAuthors (m : Meta) : List (List Inline) :=
  match lookupMeta "author" m with
  | some (.MetaString s) => [[.Str s]]
  | some (.MetaInlines ils) => [ils]
  | some (.MetaList ms) =>
    ms.filterMap fun
      | .MetaInlines ils => some ils
      | .MetaBlocks [.Plain ils] => some ils
      | .MetaBlocks [.Para ils] => some ils
      | .MetaString x => some [.Str x]
      | _ => none
  | _ => []

/-- Extract the date from metadata. -/
def docDate (m : Meta) : List Inline :=
  match lookupMeta "date" m with
  | some (.MetaString s) => [.Str s]
  | some (.MetaInlines ils) => ils
  | some (.MetaBlocks [.Plain ils]) => ils
  | some (.MetaBlocks [.Para ils]) => ils
  | _ => []

-- ── Monoid structure ──────────────────────────────────────────────────

/-- `Meta` is a monoid: the union of the two maps, with the right argument's
    keys winning on conflicts (matching upstream's left-biased `M.union m2 m1`). -/
instance : Append Meta where
  append a b := ⟨Data.Map.union b.unMeta a.unMeta⟩

instance : Append Pandoc where
  append a b := ⟨a.docMeta ++ b.docMeta, a.blocks ++ b.blocks⟩

/-- The version branch reported in the `pandoc-api-version` JSON field. -/
def pandocTypesVersion : List Int := [1, 23, 1]

-- ── JSON: tagged-object helpers ───────────────────────────────────────

/-- A nullary tagged object `{"t": t}`. -/
private def tag (t : String) : Value := .object [("t", .string t)]

/-- A tagged object with contents `{"t": t, "c": c}`. -/
private def tagC (t : String) (c : Value) : Value := .object [("t", .string t), ("c", c)]

private def jInt (n : Int) : Value := .number (Float.ofInt n)

/-- Truncate a `Float` toward zero, via its string form (matches `Data.Json`). -/
private def floatToInt (f : Float) : Int :=
  match (toString f).splitOn "." with
  | w :: _ => w.toInt!
  | [] => 0

-- ── JSON encoders for the leaf types ──────────────────────────────────

private def encFormat (f : Format) : Value := .string f.unFormat
private def encTarget (t : Target) : Value := .array #[.string t.1, .string t.2]

private def encAttr (a : Attr) : Value :=
  .array #[ .string a.1,
            .array (a.2.1.map Value.string).toArray,
            .array (a.2.2.map (fun kv => .array #[.string kv.1, .string kv.2])).toArray ]

private def encAlignment : Alignment → Value
  | .AlignLeft => tag "AlignLeft"
  | .AlignRight => tag "AlignRight"
  | .AlignCenter => tag "AlignCenter"
  | .AlignDefault => tag "AlignDefault"

private def encColWidth : ColWidth → Value
  | .ColWidth w => tagC "ColWidth" (.number w)
  | .ColWidthDefault => tag "ColWidthDefault"

private def encColSpec (c : ColSpec) : Value := .array #[encAlignment c.1, encColWidth c.2]

private def encListNumberStyle : ListNumberStyle → Value
  | .DefaultStyle => tag "DefaultStyle"
  | .Example => tag "Example"
  | .Decimal => tag "Decimal"
  | .LowerRoman => tag "LowerRoman"
  | .UpperRoman => tag "UpperRoman"
  | .LowerAlpha => tag "LowerAlpha"
  | .UpperAlpha => tag "UpperAlpha"

private def encListNumberDelim : ListNumberDelim → Value
  | .DefaultDelim => tag "DefaultDelim"
  | .Period => tag "Period"
  | .OneParen => tag "OneParen"
  | .TwoParens => tag "TwoParens"

private def encQuoteType : QuoteType → Value
  | .SingleQuote => tag "SingleQuote"
  | .DoubleQuote => tag "DoubleQuote"

private def encMathType : MathType → Value
  | .DisplayMath => tag "DisplayMath"
  | .InlineMath => tag "InlineMath"

private def encCitationMode : CitationMode → Value
  | .AuthorInText => tag "AuthorInText"
  | .SuppressAuthor => tag "SuppressAuthor"
  | .NormalCitation => tag "NormalCitation"

private def encListAttributes (la : ListAttributes) : Value :=
  .array #[jInt la.1, encListNumberStyle la.2.1, encListNumberDelim la.2.2]

-- ── JSON encoders for the recursive AST ───────────────────────────────

mutual

private def encInline : Inline → Value
  | .Str s => tagC "Str" (.string s)
  | .Emph xs => tagC "Emph" (.array (encInlineList xs).toArray)
  | .Underline xs => tagC "Underline" (.array (encInlineList xs).toArray)
  | .Strong xs => tagC "Strong" (.array (encInlineList xs).toArray)
  | .Strikeout xs => tagC "Strikeout" (.array (encInlineList xs).toArray)
  | .Superscript xs => tagC "Superscript" (.array (encInlineList xs).toArray)
  | .Subscript xs => tagC "Subscript" (.array (encInlineList xs).toArray)
  | .SmallCaps xs => tagC "SmallCaps" (.array (encInlineList xs).toArray)
  | .Quoted qt xs => tagC "Quoted" (.array #[encQuoteType qt, .array (encInlineList xs).toArray])
  | .Cite cs xs =>
    tagC "Cite" (.array #[.array (encCitationList cs).toArray, .array (encInlineList xs).toArray])
  | .Code a s => tagC "Code" (.array #[encAttr a, .string s])
  | .Space => tag "Space"
  | .SoftBreak => tag "SoftBreak"
  | .LineBreak => tag "LineBreak"
  | .Math mt s => tagC "Math" (.array #[encMathType mt, .string s])
  | .RawInline f s => tagC "RawInline" (.array #[encFormat f, .string s])
  | .Link a xs t => tagC "Link" (.array #[encAttr a, .array (encInlineList xs).toArray, encTarget t])
  | .Image a xs t => tagC "Image" (.array #[encAttr a, .array (encInlineList xs).toArray, encTarget t])
  | .Note bs => tagC "Note" (.array (encBlockList bs).toArray)
  | .Span a xs => tagC "Span" (.array #[encAttr a, .array (encInlineList xs).toArray])

private def encInlineList : List Inline → List Value
  | [] => []
  | x :: xs => encInline x :: encInlineList xs

private def encInlineListList : List (List Inline) → List Value
  | [] => []
  | x :: xs => .array (encInlineList x).toArray :: encInlineListList xs

private def encBlock : Block → Value
  | .Plain xs => tagC "Plain" (.array (encInlineList xs).toArray)
  | .Para xs => tagC "Para" (.array (encInlineList xs).toArray)
  | .LineBlock xss => tagC "LineBlock" (.array (encInlineListList xss).toArray)
  | .CodeBlock a s => tagC "CodeBlock" (.array #[encAttr a, .string s])
  | .RawBlock f s => tagC "RawBlock" (.array #[encFormat f, .string s])
  | .BlockQuote bs => tagC "BlockQuote" (.array (encBlockList bs).toArray)
  | .OrderedList la items =>
    tagC "OrderedList" (.array #[encListAttributes la, .array (encBlockListList items).toArray])
  | .BulletList items => tagC "BulletList" (.array (encBlockListList items).toArray)
  | .DefinitionList items => tagC "DefinitionList" (.array (encDefList items).toArray)
  | .Header lvl a xs => tagC "Header" (.array #[jInt lvl, encAttr a, .array (encInlineList xs).toArray])
  | .HorizontalRule => tag "HorizontalRule"
  | .Table a capt specs hd bs ft =>
    tagC "Table" (.array #[ encAttr a, encCaption capt,
                            .array (specs.map encColSpec).toArray,
                            encTableHead hd,
                            .array (encTableBodyList bs).toArray,
                            encTableFoot ft ])
  | .Figure a capt bs => tagC "Figure" (.array #[encAttr a, encCaption capt, .array (encBlockList bs).toArray])
  | .Div a bs => tagC "Div" (.array #[encAttr a, .array (encBlockList bs).toArray])

private def encBlockList : List Block → List Value
  | [] => []
  | x :: xs => encBlock x :: encBlockList xs

private def encBlockListList : List (List Block) → List Value
  | [] => []
  | x :: xs => .array (encBlockList x).toArray :: encBlockListList xs

private def encDefList : List (List Inline × List (List Block)) → List Value
  | [] => []
  | (term, defs) :: rest =>
    .array #[.array (encInlineList term).toArray, .array (encBlockListList defs).toArray]
      :: encDefList rest

private def encCitation : Citation → Value
  | .mk cid pref suff mode nn hash =>
    .object [ ("citationId", .string cid),
              ("citationPrefix", .array (encInlineList pref).toArray),
              ("citationSuffix", .array (encInlineList suff).toArray),
              ("citationMode", encCitationMode mode),
              ("citationNoteNum", jInt nn),
              ("citationHash", jInt hash) ]

private def encCitationList : List Citation → List Value
  | [] => []
  | x :: xs => encCitation x :: encCitationList xs

private def encRow : Row → Value
  | .Row a cells => .array #[encAttr a, .array (encCellList cells).toArray]

private def encRowList : List Row → List Value
  | [] => []
  | x :: xs => encRow x :: encRowList xs

private def encCell : Cell → Value
  | .Cell a align rs cs bs =>
    .array #[encAttr a, encAlignment align, jInt rs, jInt cs, .array (encBlockList bs).toArray]

private def encCellList : List Cell → List Value
  | [] => []
  | x :: xs => encCell x :: encCellList xs

private def encTableHead : TableHead → Value
  | .TableHead a rows => .array #[encAttr a, .array (encRowList rows).toArray]

private def encTableBody : TableBody → Value
  | .TableBody a rhc hd bd =>
    .array #[encAttr a, jInt rhc, .array (encRowList hd).toArray, .array (encRowList bd).toArray]

private def encTableBodyList : List TableBody → List Value
  | [] => []
  | x :: xs => encTableBody x :: encTableBodyList xs

private def encTableFoot : TableFoot → Value
  | .TableFoot a rows => .array #[encAttr a, .array (encRowList rows).toArray]

private def encCaption : Caption → Value
  | .Caption short bs =>
    let shortV := match short with | none => .null | some ils => .array (encInlineList ils).toArray
    .array #[shortV, .array (encBlockList bs).toArray]

private def encMetaValue : MetaValue → Value
  | .MetaMap m => tagC "MetaMap" (.object (encMetaFields m))
  | .MetaList xs => tagC "MetaList" (.array (encMetaValueList xs).toArray)
  | .MetaBool b => tagC "MetaBool" (.bool b)
  | .MetaString s => tagC "MetaString" (.string s)
  | .MetaInlines xs => tagC "MetaInlines" (.array (encInlineList xs).toArray)
  | .MetaBlocks bs => tagC "MetaBlocks" (.array (encBlockList bs).toArray)

private def encMetaValueList : List MetaValue → List Value
  | [] => []
  | x :: xs => encMetaValue x :: encMetaValueList xs

private def encMetaFields : List (String × MetaValue) → List (String × Value)
  | [] => []
  | (k, v) :: rest => (k, encMetaValue v) :: encMetaFields rest

end

private def encMeta (m : Meta) : Value :=
  .object (encMetaFields m.unMeta.toList')

private def encPandoc (d : Pandoc) : Value :=
  .object [ ("pandoc-api-version", .array (pandocTypesVersion.map jInt).toArray),
            ("meta", encMeta d.docMeta),
            ("blocks", .array (encBlockList d.blocks).toArray) ]

-- ── JSON decoders for the leaf types ──────────────────────────────────

private def decFormat : Value → Except String Format
  | .string s => .ok ⟨s⟩
  | _ => .error "expected format string"

private def decTarget : Value → Except String Target
  | .array #[.string u, .string t] => .ok (u, t)
  | _ => .error "expected [url, title] target"

private def decStringList : List Value → Except String (List String)
  | [] => .ok []
  | .string s :: rest => (s :: ·) <$> decStringList rest
  | _ => .error "expected string"

private def decKVList : List Value → Except String (List (String × String))
  | [] => .ok []
  | .array #[.string k, .string v] :: rest => ((k, v) :: ·) <$> decKVList rest
  | _ => .error "expected [key, value] pair"

private def decAttr : Value → Except String Attr
  | .array #[.string i, .array classes, .array kvs] => do
    let cs ← decStringList classes.toList
    let kv ← decKVList kvs.toList
    .ok (i, cs, kv)
  | _ => .error "expected [id, classes, kvs] attr"

private def decAlignment : Value → Except String Alignment
  | .object [("t", .string "AlignLeft")] => .ok .AlignLeft
  | .object [("t", .string "AlignRight")] => .ok .AlignRight
  | .object [("t", .string "AlignCenter")] => .ok .AlignCenter
  | .object [("t", .string "AlignDefault")] => .ok .AlignDefault
  | _ => .error "expected Alignment"

private def decColWidth : Value → Except String ColWidth
  | .object [("t", .string "ColWidth"), ("c", .number w)] => .ok (.ColWidth w)
  | .object [("t", .string "ColWidthDefault")] => .ok .ColWidthDefault
  | _ => .error "expected ColWidth"

private def decColSpec : Value → Except String ColSpec
  | .array #[a, w] => do return (← decAlignment a, ← decColWidth w)
  | _ => .error "expected [alignment, colwidth]"

private def decColSpecList : List Value → Except String (List ColSpec)
  | [] => .ok []
  | x :: xs => do return (← decColSpec x) :: (← decColSpecList xs)

private def decListNumberStyle : Value → Except String ListNumberStyle
  | .object [("t", .string "DefaultStyle")] => .ok .DefaultStyle
  | .object [("t", .string "Example")] => .ok .Example
  | .object [("t", .string "Decimal")] => .ok .Decimal
  | .object [("t", .string "LowerRoman")] => .ok .LowerRoman
  | .object [("t", .string "UpperRoman")] => .ok .UpperRoman
  | .object [("t", .string "LowerAlpha")] => .ok .LowerAlpha
  | .object [("t", .string "UpperAlpha")] => .ok .UpperAlpha
  | _ => .error "expected ListNumberStyle"

private def decListNumberDelim : Value → Except String ListNumberDelim
  | .object [("t", .string "DefaultDelim")] => .ok .DefaultDelim
  | .object [("t", .string "Period")] => .ok .Period
  | .object [("t", .string "OneParen")] => .ok .OneParen
  | .object [("t", .string "TwoParens")] => .ok .TwoParens
  | _ => .error "expected ListNumberDelim"

private def decQuoteType : Value → Except String QuoteType
  | .object [("t", .string "SingleQuote")] => .ok .SingleQuote
  | .object [("t", .string "DoubleQuote")] => .ok .DoubleQuote
  | _ => .error "expected QuoteType"

private def decMathType : Value → Except String MathType
  | .object [("t", .string "DisplayMath")] => .ok .DisplayMath
  | .object [("t", .string "InlineMath")] => .ok .InlineMath
  | _ => .error "expected MathType"

private def decCitationMode : Value → Except String CitationMode
  | .object [("t", .string "AuthorInText")] => .ok .AuthorInText
  | .object [("t", .string "SuppressAuthor")] => .ok .SuppressAuthor
  | .object [("t", .string "NormalCitation")] => .ok .NormalCitation
  | _ => .error "expected CitationMode"

private def decListAttributes : Value → Except String ListAttributes
  | .array #[.number s, style, delim] => do
    return (floatToInt s, ← decListNumberStyle style, ← decListNumberDelim delim)
  | _ => .error "expected [start, style, delim]"

-- ── JSON decoders for the recursive AST ───────────────────────────────

mutual

private def decInline : Value → Except String Inline
  | .object [("t", .string "Str"), ("c", .string s)] => .ok (.Str s)
  | .object [("t", .string "Emph"), ("c", .array a)] => .Emph <$> decInlineList a.toList
  | .object [("t", .string "Underline"), ("c", .array a)] => .Underline <$> decInlineList a.toList
  | .object [("t", .string "Strong"), ("c", .array a)] => .Strong <$> decInlineList a.toList
  | .object [("t", .string "Strikeout"), ("c", .array a)] => .Strikeout <$> decInlineList a.toList
  | .object [("t", .string "Superscript"), ("c", .array a)] => .Superscript <$> decInlineList a.toList
  | .object [("t", .string "Subscript"), ("c", .array a)] => .Subscript <$> decInlineList a.toList
  | .object [("t", .string "SmallCaps"), ("c", .array a)] => .SmallCaps <$> decInlineList a.toList
  | .object [("t", .string "Quoted"), ("c", .array #[qt, .array a])] => do
    return .Quoted (← decQuoteType qt) (← decInlineList a.toList)
  | .object [("t", .string "Cite"), ("c", .array #[.array cs, .array a])] => do
    return .Cite (← decCitationList cs.toList) (← decInlineList a.toList)
  | .object [("t", .string "Code"), ("c", .array #[attr, .string s])] => do
    return .Code (← decAttr attr) s
  | .object [("t", .string "Space")] => .ok .Space
  | .object [("t", .string "SoftBreak")] => .ok .SoftBreak
  | .object [("t", .string "LineBreak")] => .ok .LineBreak
  | .object [("t", .string "Math"), ("c", .array #[mt, .string s])] => do
    return .Math (← decMathType mt) s
  | .object [("t", .string "RawInline"), ("c", .array #[f, .string s])] => do
    return .RawInline (← decFormat f) s
  | .object [("t", .string "Link"), ("c", .array #[attr, .array a, tgt])] => do
    return .Link (← decAttr attr) (← decInlineList a.toList) (← decTarget tgt)
  | .object [("t", .string "Image"), ("c", .array #[attr, .array a, tgt])] => do
    return .Image (← decAttr attr) (← decInlineList a.toList) (← decTarget tgt)
  | .object [("t", .string "Note"), ("c", .array a)] => .Note <$> decBlockList a.toList
  | .object [("t", .string "Span"), ("c", .array #[attr, .array a])] => do
    return .Span (← decAttr attr) (← decInlineList a.toList)
  | _ => .error "expected Inline"

private def decInlineList : List Value → Except String (List Inline)
  | [] => .ok []
  | x :: xs => do return (← decInline x) :: (← decInlineList xs)

private def decInlineListList : List Value → Except String (List (List Inline))
  | [] => .ok []
  | .array a :: xs => do return (← decInlineList a.toList) :: (← decInlineListList xs)
  | _ => .error "expected array of inlines"

private def decBlock : Value → Except String Block
  | .object [("t", .string "Plain"), ("c", .array a)] => .Plain <$> decInlineList a.toList
  | .object [("t", .string "Para"), ("c", .array a)] => .Para <$> decInlineList a.toList
  | .object [("t", .string "LineBlock"), ("c", .array a)] => .LineBlock <$> decInlineListList a.toList
  | .object [("t", .string "CodeBlock"), ("c", .array #[attr, .string s])] => do
    return .CodeBlock (← decAttr attr) s
  | .object [("t", .string "RawBlock"), ("c", .array #[f, .string s])] => do
    return .RawBlock (← decFormat f) s
  | .object [("t", .string "BlockQuote"), ("c", .array a)] => .BlockQuote <$> decBlockList a.toList
  | .object [("t", .string "OrderedList"), ("c", .array #[la, .array a])] => do
    return .OrderedList (← decListAttributes la) (← decBlockListList a.toList)
  | .object [("t", .string "BulletList"), ("c", .array a)] => .BulletList <$> decBlockListList a.toList
  | .object [("t", .string "DefinitionList"), ("c", .array a)] => .DefinitionList <$> decDefList a.toList
  | .object [("t", .string "Header"), ("c", .array #[.number lvl, attr, .array a])] => do
    return .Header (floatToInt lvl) (← decAttr attr) (← decInlineList a.toList)
  | .object [("t", .string "HorizontalRule")] => .ok .HorizontalRule
  | .object [("t", .string "Table"), ("c", .array #[attr, capt, .array specs, hd, .array bs, ft])] => do
    return .Table (← decAttr attr) (← decCaption capt) (← decColSpecList specs.toList)
                  (← decTableHead hd) (← decTableBodyList bs.toList) (← decTableFoot ft)
  | .object [("t", .string "Figure"), ("c", .array #[attr, capt, .array a])] => do
    return .Figure (← decAttr attr) (← decCaption capt) (← decBlockList a.toList)
  | .object [("t", .string "Div"), ("c", .array #[attr, .array a])] => do
    return .Div (← decAttr attr) (← decBlockList a.toList)
  | _ => .error "expected Block"

private def decBlockList : List Value → Except String (List Block)
  | [] => .ok []
  | x :: xs => do return (← decBlock x) :: (← decBlockList xs)

private def decBlockListList : List Value → Except String (List (List Block))
  | [] => .ok []
  | .array a :: xs => do return (← decBlockList a.toList) :: (← decBlockListList xs)
  | _ => .error "expected array of blocks"

private def decDefList : List Value → Except String (List (List Inline × List (List Block)))
  | [] => .ok []
  | .array #[.array term, .array defs] :: rest => do
    return (← decInlineList term.toList, ← decBlockListList defs.toList) :: (← decDefList rest)
  | _ => .error "expected [term, definitions] pair"

private def decCitation : Value → Except String Citation
  | .object [ ("citationId", .string cid),
              ("citationPrefix", .array pref),
              ("citationSuffix", .array suff),
              ("citationMode", mode),
              ("citationNoteNum", .number nn),
              ("citationHash", .number hash) ] => do
    return .mk cid (← decInlineList pref.toList) (← decInlineList suff.toList)
               (← decCitationMode mode) (floatToInt nn) (floatToInt hash)
  | _ => .error "expected Citation"

private def decCitationList : List Value → Except String (List Citation)
  | [] => .ok []
  | x :: xs => do return (← decCitation x) :: (← decCitationList xs)

private def decCell : Value → Except String Cell
  | .array #[attr, align, .number rs, .number cs, .array bs] => do
    return .Cell (← decAttr attr) (← decAlignment align) (floatToInt rs) (floatToInt cs)
                 (← decBlockList bs.toList)
  | _ => .error "expected Cell"

private def decCellList : List Value → Except String (List Cell)
  | [] => .ok []
  | x :: xs => do return (← decCell x) :: (← decCellList xs)

private def decRow : Value → Except String Row
  | .array #[attr, .array cells] => do return .Row (← decAttr attr) (← decCellList cells.toList)
  | _ => .error "expected Row"

private def decRowList : List Value → Except String (List Row)
  | [] => .ok []
  | x :: xs => do return (← decRow x) :: (← decRowList xs)

private def decTableHead : Value → Except String TableHead
  | .array #[attr, .array rows] => do return .TableHead (← decAttr attr) (← decRowList rows.toList)
  | _ => .error "expected TableHead"

private def decTableBody : Value → Except String TableBody
  | .array #[attr, .number rhc, .array hd, .array bd] => do
    return .TableBody (← decAttr attr) (floatToInt rhc) (← decRowList hd.toList) (← decRowList bd.toList)
  | _ => .error "expected TableBody"

private def decTableBodyList : List Value → Except String (List TableBody)
  | [] => .ok []
  | x :: xs => do return (← decTableBody x) :: (← decTableBodyList xs)

private def decTableFoot : Value → Except String TableFoot
  | .array #[attr, .array rows] => do return .TableFoot (← decAttr attr) (← decRowList rows.toList)
  | _ => .error "expected TableFoot"

private def decCaption : Value → Except String Caption
  | .array #[.null, .array bs] => do return .Caption none (← decBlockList bs.toList)
  | .array #[.array short, .array bs] => do
    return .Caption (some (← decInlineList short.toList)) (← decBlockList bs.toList)
  | _ => .error "expected Caption"

private def decMetaValue : Value → Except String MetaValue
  | .object [("t", .string "MetaBool"), ("c", .bool b)] => .ok (.MetaBool b)
  | .object [("t", .string "MetaString"), ("c", .string s)] => .ok (.MetaString s)
  | .object [("t", .string "MetaInlines"), ("c", .array a)] => .MetaInlines <$> decInlineList a.toList
  | .object [("t", .string "MetaBlocks"), ("c", .array a)] => .MetaBlocks <$> decBlockList a.toList
  | .object [("t", .string "MetaList"), ("c", .array a)] => .MetaList <$> decMetaValueList a.toList
  | .object [("t", .string "MetaMap"), ("c", .object fields)] => .MetaMap <$> decMetaFields fields
  | _ => .error "expected MetaValue"

private def decMetaValueList : List Value → Except String (List MetaValue)
  | [] => .ok []
  | x :: xs => do return (← decMetaValue x) :: (← decMetaValueList xs)

private def decMetaFields : List (String × Value) → Except String (List (String × MetaValue))
  | [] => .ok []
  | (k, v) :: rest => do return (k, ← decMetaValue v) :: (← decMetaFields rest)

end

private def decMeta : Value → Except String Meta
  | .object fields => (fun kvs => Meta.mk (Data.Map.fromList kvs)) <$> decMetaFields fields
  | _ => .error "expected Meta object"

private def decPandoc : Value → Except String Pandoc
  | .object fields =>
    match fields.lookup "meta", fields.lookup "blocks" with
    | some mv, some (.array bs) => do return ⟨← decMeta mv, ← decBlockList bs.toList⟩
    | _, _ => .error "JSON missing meta/blocks"
  | _ => .error "expected Pandoc object"

-- ── ToJSON / FromJSON instances ───────────────────────────────────────

instance : ToJSON Format := ⟨encFormat⟩
instance : ToJSON Inline := ⟨encInline⟩
instance : ToJSON Block := ⟨encBlock⟩
instance : ToJSON Citation := ⟨encCitation⟩
instance : ToJSON MetaValue := ⟨encMetaValue⟩
instance : ToJSON Meta := ⟨encMeta⟩
instance : ToJSON Pandoc := ⟨encPandoc⟩

instance : FromJSON Format := ⟨decFormat⟩
instance : FromJSON Inline := ⟨decInline⟩
instance : FromJSON Block := ⟨decBlock⟩
instance : FromJSON Citation := ⟨decCitation⟩
instance : FromJSON MetaValue := ⟨decMetaValue⟩
instance : FromJSON Meta := ⟨decMeta⟩
instance : FromJSON Pandoc := ⟨decPandoc⟩

end Linen.Text.Pandoc
