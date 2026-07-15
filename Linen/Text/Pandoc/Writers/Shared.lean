/-
  `Linen.Text.Pandoc.Writers.Shared` — shared writer helpers.

  ## Haskell source

  Ported from `Text.Pandoc.Writers.Shared` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Writers/Shared.hs`).

  This is the writers' counterpart to `Text.Pandoc.Shared`: template-context
  plumbing (`metaToContext`/`defField`/`resetField`/`setField`/`getField`),
  the metadata lookups (`lookupMetaBool`/`Blocks`/`Inlines`/`String`,
  `getLang`), the HTML attribute helpers (`tagWithAttrs`/`htmlAttrs`/
  `htmlAlignmentToString`/`htmlAddStyle`), the math helpers (`isDisplayMath`/
  `fixDisplayMath`), typographic helpers (`unsmartify`, `toSubscript`/
  `toSuperscript` and their inline variants), the `[Block]` predicate
  `endsWithPlain`, the table normalisation (`toLegacyTable`) and the ASCII
  grid-table renderer (`gridTable`).

  ### Deviations from upstream

  * **Context / templating.**  Upstream builds a `doctemplates`
    `Context`/`Val`, but the `doctemplates` engine is deferred and the
    in-scope writers run template-free (see `docs/imports/pandoc/
    dependencies.md` and `Options`, where `Context α := List (String × α)`).
    So `metaToContext` takes the template-free branch (upstream returns
    `mempty` when `writerTemplate = Nothing`, which is always the case here)
    and returns an empty context; `metaToContext'`/`addVariablesToContext`
    (the `Val`-valued metadata serialisation and the `meta-json` variable) are
    deferred with the engine.  `getField`/`defField`/`resetField`/`setField`
    operate over the simplified assoc-list `Context`; with no `Val` list value
    to append into, `setField` coincides with `resetField` (documented at its
    definition).
  * **`gridTable`.**  Upstream renders through `Text.Pandoc.Writers.
    AnnotatedTable` (a deferred module).  This port instead builds on the
    exported `toLegacyTable`, which cuts row/column spans into `1×1` cells,
    and lays the result out with `Text.DocLayout`'s block combinators — the
    classic pre-annotated-table algorithm.  It is `unsafe` because those
    combinators (`lblock`/`render`/…) are `unsafe` in the `doclayout` port.
  * **`splitSentences`/`toTableOfContents`/`ensureValidXmlIdentifiers`/
    `setupTranslations`/`isOrderedListMarker`** depend on `Doc`-token
    rewriting, `makeSections`/`Text.Pandoc.Chunks`, whole-document `Attr`
    walking, the translation tables, or the parser toolkit — each tied to a
    deferred subtree — and are left to the writer/reader tiers that need them.
  * `Text` → `String`; `Double` widths → `Float`; `cssAttributes` (from
    `Text.Pandoc.CSS`, not ported) is inlined as a small `; `/`:`-splitting
    parser for `htmlAddStyle`.
-/

import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Builder
import Linen.Text.Pandoc.Walk
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Shared
import Linen.Text.Pandoc.XML
import Linen.Text.Pandoc.Templates
import Linen.Text.DocLayout

namespace Linen.Text.Pandoc.Writers
namespace Shared

open _root_.Text.DocLayout (Doc)

/- ── Template context helpers ──────────────────────────────────────────── -/

/-- Look a field up in a `Context` (assoc-list). -/
def getField {α : Type} (key : String) (ctx : Context α) : Option α :=
  (ctx.find? (·.1 == key)).map (·.2)

/-- Set a field, overwriting any prior value (upstream `resetField`). -/
def resetField {α : Type} (key : String) (val : α) (ctx : Context α) : Context α :=
  (ctx.filter (·.1 != key)) ++ [(key, val)]

/-- Set a field only if it is not already present (upstream `defField`). -/
def defField {α : Type} (key : String) (val : α) (ctx : Context α) : Context α :=
  if ctx.any (·.1 == key) then ctx else ctx ++ [(key, val)]

/-- Set a field.  Upstream appends into a `Val` list on collision; with the
    simplified assoc-list `Context` (no `Val`), this coincides with
    `resetField`. -/
def setField {α : Type} (key : String) (val : α) (ctx : Context α) : Context α :=
  resetField key val ctx

/-- Build a template `Context` from document metadata.  Template-free scope:
    upstream returns `mempty` when `writerTemplate = Nothing`, and every
    in-scope writer runs template-free — so this always returns the empty
    context.  The template branch (`metaToContext'`, needing the deferred
    `doctemplates` `Val` model) is omitted; the block/inline writers are
    accepted for signature compatibility. -/
def metaToContext {α : Type} {m : Type → Type} [Monad m] (opts : WriterOptions)
    (_blockWriter : List Block → m (Doc α)) (_inlineWriter : List Inline → m (Doc α))
    (_meta : Meta) : m (Context α) :=
  match opts.writerTemplate with
  | none => pure []
  | some _ => pure []  -- template path deferred (see module note)

/- ── Metadata lookups ──────────────────────────────────────────────────── -/

/-- Look up a metadata field as a `Bool` (any blocks/inlines, a non-empty
    string, or `MetaBool True` count as true). -/
def lookupMetaBool (key : String) (mta : Meta) : Bool :=
  match lookupMeta key mta with
  | some (.MetaBlocks _) => true
  | some (.MetaInlines _) => true
  | some (.MetaString s) => !s.isEmpty
  | some (.MetaBool b) => b
  | _ => false

/-- Look up a metadata field as a list of blocks. -/
def lookupMetaBlocks (key : String) (mta : Meta) : List Block :=
  match lookupMeta key mta with
  | some (.MetaBlocks bs) => bs
  | some (.MetaInlines ils) => [.Plain ils]
  | some (.MetaString s) => [.Plain [.Str s]]
  | _ => []

/-- Look up a metadata field as a list of inlines. -/
def lookupMetaInlines (key : String) (mta : Meta) : List Inline :=
  match lookupMeta key mta with
  | some (.MetaString s) => [.Str s]
  | some (.MetaInlines ils) => ils
  | some (.MetaBlocks [.Plain ils]) => ils
  | some (.MetaBlocks [.Para ils]) => ils
  | _ => []

/-- Look up a metadata field as a plain string. -/
def lookupMetaString (key : String) (mta : Meta) : String :=
  match lookupMeta key mta with
  | some (.MetaString s) => s
  | some (.MetaInlines ils) => _root_.Linen.Text.Pandoc.Shared.stringify ils
  | some (.MetaBlocks bs) => _root_.Linen.Text.Pandoc.Shared.stringify bs
  | some (.MetaBool b) => toString b
  | _ => ""

/-- The document language: the `lang` writer variable if set, else the `lang`
    metadata field. -/
def getLang (opts : WriterOptions) (mta : Meta) : Option String :=
  match getField "lang" opts.writerVariables with
  | some l => some l
  | none =>
    match lookupMeta "lang" mta with
    | some (.MetaBlocks [.Para [.Str s]]) => some s
    | some (.MetaBlocks [.Plain [.Str s]]) => some s
    | some (.MetaInlines [.Str s]) => some s
    | some (.MetaString s) => some s
    | _ => none

/- ── HTML attribute helpers ────────────────────────────────────────────── -/

/-- The CSS text-align value for a table alignment (`AlignDefault` → none). -/
def htmlAlignmentToString : Alignment → Option String
  | .AlignLeft => some "left"
  | .AlignRight => some "right"
  | .AlignCenter => some "center"
  | .AlignDefault => none

/-- Prefix a raw key for an HTML attribute: recognised HTML5/RDFa attribute
    names (excluding `"label"`), names containing `:`, and names already
    starting with `data-`/`aria-` are kept verbatim; everything else is
    `data-`-prefixed. -/
def formatKey (k : String) : String :=
  if k == "label" then "data-label"
  else if XML.html5Attributes.contains k || XML.rdfaAttributes.contains k then k
  else if k.any (· == ':') then k
  else if k.startsWith "data-" || k.startsWith "aria-" then k
  else "data-" ++ k

/-- Render an `Attr` as HTML attribute text, with a leading space when
    non-empty. -/
def htmlAttrs (attr : Attr) : Doc String :=
  let (ident, classes, kvs) := attr
  let idPart := if ident.isEmpty then [] else [s!"id=\"{XML.escapeStringForXML ident}\""]
  let classPart :=
    if classes.isEmpty then []
    else [s!"class=\"{XML.escapeStringForXML (String.intercalate " " classes)}\""]
  let kvParts := kvs.map fun (k, v) => s!"{formatKey k}=\"{XML.escapeStringForXML v}\""
  let parts := idPart ++ classPart ++ kvParts
  if parts.isEmpty then _root_.Text.DocLayout.empty
  else _root_.Text.DocLayout.literal (" " ++ String.intercalate " " parts)

/-- The opening tag `<tag …>` with rendered attributes (upstream
    `tagWithAttrs`). -/
def tagWithAttrs (tag : String) (attr : Attr) : Doc String :=
  _root_.Text.DocLayout.literal ("<" ++ tag) ++ htmlAttrs attr
    ++ _root_.Text.DocLayout.literal ">"

/-- Parse an inline CSS `style` value into key/value pairs (inlined
    replacement for `Text.Pandoc.CSS.cssAttributes`). -/
def cssAttributes (s : String) : List (String × String) :=
  (s.splitOn ";").filterMap fun decl =>
    let decl := _root_.Linen.Text.Pandoc.Shared.trim decl
    if decl.isEmpty then none
    else match decl.splitOn ":" with
      | k :: rest => some (_root_.Linen.Text.Pandoc.Shared.trim k,
          _root_.Linen.Text.Pandoc.Shared.trim (String.intercalate ":" rest))
      | [] => none

/-- Add or replace a `(key, value)` CSS declaration inside an attribute list's
    `style` attribute (upstream `htmlAddStyle`). -/
def htmlAddStyle (kv : String × String) (attrs : List (String × String)) :
    List (String × String) :=
  let (k, v) := kv
  let render (pairs : List (String × String)) : String :=
    String.intercalate " " (pairs.map fun (a, b) => s!"{a}: {b};")
  match attrs.find? (·.1 == "style") with
  | some (_, existing) =>
    let parsed := cssAttributes existing
    let replaced :=
      if parsed.any (·.1 == k)
      then parsed.map (fun p => if p.1 == k then (k, v) else p)
      else (k, v) :: parsed
    attrs.map fun p => if p.1 == "style" then ("style", render replaced) else p
  | none => ("style", render [(k, v)]) :: attrs

/- ── Math helpers ──────────────────────────────────────────────────────── -/

/-- Is an inline display math (bare, or a `Span` wrapping exactly one display
    math)? -/
def isDisplayMath : Inline → Bool
  | .Math .DisplayMath _ => true
  | .Span _ [x] => isDisplayMath x
  | _ => false

/-- Drop leading and trailing `Space`/`SoftBreak` from an inline list. -/
def stripLeadingTrailingSpace (ils : List Inline) : List Inline :=
  let isSp : Inline → Bool := fun x => x matches .Space | .SoftBreak
  (((ils.dropWhile isSp).reverse.dropWhile isSp).reverse)

/-- Group a list into maximal runs of elements agreeing on `key`. -/
private def groupOn {α β : Type} [BEq β] (key : α → β) : List α → List (List α)
  | [] => []
  | x :: xs =>
      match groupOn key xs with
      | (g@(g0 :: _)) :: gs => if key x == key g0 then (x :: g) :: gs else [x] :: g :: gs
      | _ => [[x]]

/-- Wrap `Plain`/`Para` blocks that *mix* display math and non-display-math
    inlines in a `Div` of class `math`, one paragraph per contiguous run. -/
def fixDisplayMath : Block → Block
  | .Plain ils => fixWith .Plain ils
  | .Para ils => fixWith .Para ils
  | b => b
where
  fixWith (mk : List Inline → Block) (ils : List Inline) : Block :=
    if ils.all isDisplayMath || !ils.any isDisplayMath then mk ils
    else
      let runs := (groupOn isDisplayMath ils).map stripLeadingTrailingSpace
      let runs := runs.filter (· != [])
      .Div ("", ["math"], []) (runs.map mk)

/- ── Typographic helpers ───────────────────────────────────────────────── -/

/-- Replace "smart" Unicode punctuation with ASCII, dash length depending on
    the `old_dashes` extension. -/
def unsmartify (opts : WriterOptions) (s : String) : String :=
  let oldDashes := extensionEnabled .Ext_old_dashes opts.writerExtensions
  String.join <| s.toList.map fun c =>
    match c with
    | '‘' => "'"    -- ‘
    | '’' => "'"    -- ’
    | '“' => "\""   -- “
    | '”' => "\""   -- ”
    | '…' => "..."  -- …
    | '–' => if oldDashes then "-" else "--"   -- – en dash
    | '—' => if oldDashes then "--" else "---" -- — em dash
    | _ => c.toString

/-- The Unicode superscript form of a character, if one exists.  Superscript
    `1`/`2`/`3` are the irregular Latin-1 code points; the other digits follow
    the `U+2070` block (upstream `toSuperscript`). -/
def toSuperscript (c : Char) : Option Char :=
  if c.isWhitespace then some c
  else match c with
    | '1' => some '¹'
    | '2' => some '²'
    | '3' => some '³'
    | '+' => some '⁺'
    | '−' => some '⁻'  -- U+2212 minus sign
    | '-' => some '⁻'
    | '=' => some '⁼'
    | '(' => some '⁽'
    | ')' => some '⁾'
    | _ =>
      if '0' ≤ c && c ≤ '9' then some (Char.ofNat (0x2070 + (c.toNat - '0'.toNat)))
      else none

/-- The Unicode subscript form of a character, if one exists. -/
def toSubscript (c : Char) : Option Char :=
  if c.isWhitespace then some c
  else if '0' ≤ c && c ≤ '9' then
    some (Char.ofNat (0x2080 + (c.toNat - '0'.toNat)))
  else match c with
    | '+' => some '₊'
    | '-' => some '₋'
    | '=' => some '₌'
    | '(' => some '₍'
    | ')' => some '₎'
    | _ => none

/-- Apply a per-character super/subscript map across an inline, failing (with
    `none`) if any `Str` character has no mapped form. -/
private def mapScriptInline (f : Char → Option Char) : Inline → Option Inline
  | .Str t => (t.toList.mapM f).map (fun cs => .Str (String.ofList cs))
  | .Space => some .Space
  | .SoftBreak => some .SoftBreak
  | .LineBreak => some .LineBreak
  | _ => none

/-- Render inlines as Unicode superscripts, or `none` if not fully mappable. -/
def toSuperscriptInline (ils : List Inline) : Option (List Inline) :=
  ils.mapM (mapScriptInline toSuperscript)

/-- Render inlines as Unicode subscripts, or `none` if not fully mappable. -/
def toSubscriptInline (ils : List Inline) : Option (List Inline) :=
  ils.mapM (mapScriptInline toSubscript)

/- ── Block predicates ──────────────────────────────────────────────────── -/

-- `endsWithPlain` and helpers form a structurally-recursive mutual trio (each
-- call descends into a syntactic subterm), avoiding a termination proof.
mutual
  /-- Does a block list end with a `Plain`, recursing into the last item of a
      trailing bullet/ordered list? -/
  def endsWithPlain : List Block → Bool
    | [] => false
    | [b] => blockEndsPlain b
    | _ :: bs => endsWithPlain bs
  /-- Does a single block end with a `Plain`? -/
  def blockEndsPlain : Block → Bool
    | .Plain _ => true
    | .BulletList items => itemsEndPlain items
    | .OrderedList _ items => itemsEndPlain items
    | _ => false
  /-- Does the last item of a list of item-block-lists end with a `Plain`? -/
  def itemsEndPlain : List (List Block) → Bool
    | [] => false
    | [it] => endsWithPlain it
    | _ :: rest => itemsEndPlain rest
end

/- ── Remove links ──────────────────────────────────────────────────────── -/

/-- Replace `Link`s with their contents wrapped in a `Span` (upstream
    `removeLinks`). -/
def removeLinks (ils : List Inline) : List Inline :=
  walk (b := List Inline) (fun (i : Inline) => match i with
    | .Link attr xs _ => .Span attr xs
    | x => x) ils

/- ── Legacy table conversion ───────────────────────────────────────────── -/

/-- Cut one row into exactly `pending.length` `1×1` cells, tracking column
    spans (`fill`: remaining columns of the current cell's colspan, with its
    row-span) and returning the row's cells together with the per-column
    row-span counts still pending below.  Content is placed only in a span's
    upper-left cell; every other covered cell becomes empty. -/
private def cutOneRow : List Nat → List Cell → Option (Nat × Nat) →
    (List (List Block) × List Nat)
  | [], _, _ => ([], [])
  | p :: ps, cells, fill =>
    if p > 0 then
      let (rest, ps') := cutOneRow ps cells fill
      ([] :: rest, (p - 1) :: ps')
    else
      match fill with
      | some (Nat.succ k, rs) =>
          let (rest, ps') := cutOneRow ps cells (if k == 0 then none else some (k, rs))
          ([] :: rest, (rs - 1) :: ps')
      | _ =>
        match cells with
        | [] =>
            let (rest, ps') := cutOneRow ps [] none
            ([] :: rest, 0 :: ps')
        | .Cell _ _ rs cs content :: cells' =>
            let csN := Nat.max 1 cs.toNat
            let rsN := Nat.max 1 rs.toNat
            let nextFill := if csN == 1 then none else some (csN - 1, rsN)
            let (rest, ps') := cutOneRow ps cells' nextFill
            (content :: rest, (rsN - 1) :: ps')

/-- Cut a list of rows into a grid of `1×1` cells, threading the pending
    row-span counts between rows. -/
private def cutRows (pending : List Nat) : List Row → List (List (List Block))
  | [] => []
  | .Row _ cells :: rows =>
      let (out, pending') := cutOneRow pending cells none
      out :: cutRows pending' rows

/-- Convert a modern (`ColSpec`/`TableHead`/`TableBody`/`TableFoot`) table to
    the legacy `(caption, alignments, widths, header-cells, body-rows)` tuple.
    Row/column spans are cut into `1×1` cells (`cutRows`); the caption blocks
    are flattened to inlines.  Multi-row headers are flattened to the first
    header row (a documented simplification of upstream, which threads all
    head rows). -/
def toLegacyTable (capt : Caption) (specs : List ColSpec) (thead : TableHead)
    (tbodies : List TableBody) (tfoot : TableFoot) :
    List Inline × List Alignment × List Float × List (List Block) ×
      List (List (List Block)) :=
  let numcols := specs.length
  let cbody := match capt with | .Caption _ bs => bs
  let cbody' := _root_.Linen.Text.Pandoc.Shared.blocksToInlines cbody
  let aligns := specs.map (·.1)
  let widths := specs.map fun s => match s.2 with
    | .ColWidth w => if w > 0 then w else 0.0
    | .ColWidthDefault => 0.0
  let headRows := match normalizeTableHead numcols thead with | .TableHead _ rs => rs
  let bodyRows := (tbodies.map (normalizeTableBody numcols)).flatMap
    fun | .TableBody _ _ ih bd => ih ++ bd
  let footRows := match normalizeTableFoot numcols tfoot with | .TableFoot _ rs => rs
  let init := List.replicate numcols 0
  let th' := match cutRows init headRows with
    | r :: _ => r
    | [] => List.replicate numcols []
  let tb' := cutRows init (bodyRows ++ footRows)
  (cbody', aligns, widths, th', tb')

/- ── ASCII grid tables ─────────────────────────────────────────────────── -/

/-- A horizontal grid border `+ccc+ccc+` for the given per-column widths and
    fill character (`'-'` normal, `'='` header separator). -/
def gridBorder (fill : Char) (widths : List Int) : Doc String :=
  _root_.Text.DocLayout.literal
    ("+" ++ String.intercalate "+"
      (widths.map fun w => String.ofList (List.replicate (w + 2).toNat fill)) ++ "+")

/-- Lay out one cell as a fixed-width block, respecting its column alignment.
    `unsafe` because the underlying `doclayout` block combinators are. -/
private unsafe def alignCell (al : Alignment) (w : Int) (d : Doc String) : Doc String :=
  let inner := _root_.Text.DocLayout.literal " " ++ d
  match al with
  | .AlignRight => _root_.Text.DocLayout.rblock (w + 2) inner
  | .AlignCenter => _root_.Text.DocLayout.cblock (w + 2) inner
  | _ => _root_.Text.DocLayout.lblock (w + 2) inner

/-- Lay out a row of cells between `|` bars. -/
private unsafe def gridRow (widths : List Int) (aligns : List Alignment)
    (cells : List (Doc String)) : Doc String :=
  let bar := _root_.Text.DocLayout.lblock 1 (_root_.Text.DocLayout.vfill "|")
  let blocks := (aligns.zip widths).zip cells |>.map fun ((al, w), d) => alignCell al w d
  _root_.Text.DocLayout.hcat ((blocks.map fun b => bar ++ b) ++ [bar])

/-- Render a table as an ASCII grid table (upstream `gridTable`, over
    `toLegacyTable` and `doclayout`).  Column widths come from the `ColSpec`
    fractions of `writerColumns`, or an even split when none are given.
    `unsafe` because it uses the `unsafe` `doclayout` block/render combinators
    (see the module note). -/
unsafe def gridTable {m : Type → Type} [Monad m] (opts : WriterOptions)
    (blocksToDoc : WriterOptions → List Block → m (Doc String))
    (capt : Caption) (specs : List ColSpec) (thead : TableHead)
    (tbodies : List TableBody) (tfoot : TableFoot) : m (Doc String) := do
  let (_, aligns, widths, headers, rows) := toLegacyTable capt specs thead tbodies tfoot
  let numcols := aligns.length
  let total := opts.writerColumns
  let avail := max 1 (total - Int.ofNat numcols - 1)
  let widthsInChars : List Int :=
    if widths.all (· == 0.0) then
      List.replicate numcols (max 3 (if numcols == 0 then avail else avail / Int.ofNat numcols))
    else
      widths.map fun w => max 3 (Int.ofNat (w * avail.toNat.toFloat).toUInt64.toNat)
  let headerDocs ← headers.mapM (blocksToDoc opts)
  let rowDocs ← rows.mapM fun r => r.mapM (blocksToDoc opts)
  let hasHeader := !(headers.all fun bs => bs.isEmpty)
  let topBorder := gridBorder '-' widthsInChars
  let headerSep := gridBorder '=' widthsInChars
  let rowSep := gridBorder '-' widthsInChars
  let headerRow := gridRow widthsInChars aligns headerDocs
  let bodyRows := rowDocs.map (gridRow widthsInChars aligns)
  let bodyPart := (bodyRows.map fun r => [r, rowSep]).flatten
  let lines :=
    if hasHeader then topBorder :: headerRow :: headerSep :: bodyPart
    else topBorder :: bodyPart
  pure (_root_.Text.DocLayout.vcat lines)

end Shared
end Writers
end Linen.Text.Pandoc
