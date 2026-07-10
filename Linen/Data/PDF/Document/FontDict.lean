/-
  Data.PDF.Document.FontDict — font dictionaries

  Ports `Pdf.Document.FontDict` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox,
  `document/lib/Pdf/Document/FontDict.hs`, fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/FontDict.hs`),
  module 9 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  `fontDictSubtype` reads a font dictionary's `/Subtype` (PDF32000-1:2008
  §9.6). `fontDictLoadInfo` builds a `Data.PDF.Content.FontInfo.FontInfo`
  from the font dictionary, dispatching between simple and composite
  (Type 0) fonts and reusing `Data.PDF.Content`'s already-ported types
  throughout (`FontInfo`, `FISimple`, `FIComposite`, `CIDFontWidths`,
  `UnicodeCMap`, `FontDescriptor`). No untrusted-graph recursion here: every
  reference this module follows is exactly one level deep.

  ## Design

  - `requiredInDict`/`optionalInDict` are ported directly, with upstream's
    `String`-typed field-name argument replaced by the `Name` key itself
    (rendered via `reprStr` in error messages) — the field name and the
    dictionary key coincide upstream (`decodeUtf8With ignore` on the same
    `Name`), so there is no information lost, only a redundant
    `Text`-decoding step dropped.
  - `Int64`'s role for `/Flags` narrows to `UInt32` here (as it already does
    in `Data.PDF.Content.FontDescriptor`, which this module builds on).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Core.Types
import Linen.Data.PDF.Content
import Linen.Data.PDF.Document.Internal.Types
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Stream

namespace Data.PDF.Document.FontDict

open Data.PDF.Core.Object (Name Dict Object)
open Data.PDF.Core.Object.Util
  (nameValue stringValue arrayValue refValue dictValue intValue int64Value realValue streamValue)
open Data.PDF.Core.Exception (sure corrupted unexpected)
open Data.PDF.Core.Util (notice)
open Data.PDF.Core.Types (Rectangle rectangleFromArray)
open Data.PDF.Content
open Data.PDF.Document.Internal.Types (Pdf FontDict)

export Data.PDF.Document.Internal.Types (FontDict)

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ── Font subtypes ── -/

/-- A font dictionary's `/Subtype` (PDF32000-1:2008 §9.6). Mirrors
    upstream's `FontSubtype`. -/
inductive FontSubtype where
  /-- Type 0 (composite/CID). -/
  | type0
  /-- Type 1. -/
  | type1
  /-- Multiple master Type 1. -/
  | mmType1
  /-- Type 3. -/
  | type3
  /-- TrueType. -/
  | trueType
deriving BEq, Repr

/-- Get the font dictionary's subtype. Mirrors upstream's
    `fontDictSubtype`. -/
def fontDictSubtype (fd : FontDict) : IO FontSubtype := do
  let obj ← sure (notice (fd.dict.get? (mkName "Subtype")) "Subtype should exist")
  let obj' ← Data.PDF.Document.Pdf.deref fd.pdf obj
  let n ← sure (notice (nameValue obj') "Subtype should be a name")
  if n == mkName "Type0" then pure .type0
  else if n == mkName "Type1" then pure .type1
  else if n == mkName "MMType1" then pure .mmType1
  else if n == mkName "Type3" then pure .type3
  else if n == mkName "TrueType" then pure .trueType
  else throw (unexpected s!"Unexpected font subtype: {reprStr n}")

/-! ── Shared "required/optional dictionary field" helpers ── -/

/-- Read a required field of `dict`, following one indirect reference and
    checking its type with `typeFun`. Throws `corrupted` if the field is
    absent or has the wrong type. Mirrors upstream's `requiredInDict`. -/
private def requiredInDict (pdf : Pdf) (context : String) (key : Name)
    (typeFun : Object → Option α) (dict : Dict) : IO α := do
  match dict.get? key with
  | none => throw (corrupted s!"{context}: {reprStr key} should exist")
  | some oIn => do
    let o ← Data.PDF.Document.Pdf.deref pdf oIn
    sure (notice (typeFun o) s!"{context}: {reprStr key} type failure")

/-- Read an optional field of `dict`, following one indirect reference and
    checking its type with `typeFun` if present. Throws `corrupted` if the
    field is present but has the wrong type. Mirrors upstream's
    `optionalInDict`. -/
private def optionalInDict (pdf : Pdf) (context : String) (key : Name)
    (typeFun : Object → Option α) (dict : Dict) : IO (Option α) := do
  match dict.get? key with
  | none => pure none
  | some oIn => do
    let o ← Data.PDF.Document.Pdf.deref pdf oIn
    some <$> sure (notice (typeFun o) s!"{context}: {reprStr key} type failure")

/-- Try to view an object as a `Name`'s underlying bytes. Shorthand shared
    by `requiredInDict`/`optionalInDict` calls below reading a `Name`-typed
    field as raw bytes (upstream's inline `fmap Name.toByteString . nameValue`). -/
private def nameBytesValue (o : Object) : Option Data.ByteString :=
  nameValue o |>.map Data.PDF.Core.Name.Name.toByteString

/-! ── Font descriptors (PDF32000-1:2008 §9.8) ── -/

/-- Load a font's descriptor, if present. Mirrors upstream's
    `loadFontDescriptor`. -/
private def loadFontDescriptor (pdf : Pdf) (fontDict : Dict) : IO (Option FontDescriptor) := do
  match fontDict.get? (mkName "FontDescriptor") with
  | none => pure none
  | some o => do
    let ref ← sure (notice (refValue o) "FontDescriptor should be a reference")
    let obj ← Data.PDF.Document.Pdf.lookupObject pdf ref
    let fd ← sure (notice (dictValue obj) "FontDescriptor: not a dictionary")
    -- Note: `required`/`optional` are *not* factored into local `let`
    -- bindings here (unlike `requiredInDict`/`optionalInDict`'s own
    -- top-level polymorphism) — a `let`-bound partial application in a `do`
    -- block is monomorphic in Lean, so binding one and reusing it at several
    -- different `α`s (`ByteString`, `Int`, `Float`, `Rectangle Float`, ...)
    -- below would silently pin `α` to whichever type the first call site
    -- picked. Each field instead calls `requiredInDict`/`optionalInDict`
    -- directly, so each gets its own, independently inferred `α`.
    let fontName ← requiredInDict pdf "FontDescriptor" (mkName "FontName") nameBytesValue fd
    let fontFamily ← optionalInDict pdf "FontDescriptor" (mkName "FontFamily") stringValue fd
    let fontStretch ← optionalInDict pdf "FontDescriptor" (mkName "FontStretch") nameBytesValue fd
    let fontWeight ← optionalInDict pdf "FontDescriptor" (mkName "FontWeight") intValue fd
    let flags ← requiredInDict pdf "FontDescriptor" (mkName "Flags") int64Value fd
    let fontBBox ← optionalInDict pdf "FontDescriptor" (mkName "FontBBox")
      (fun o => (arrayValue o).bind (fun arr => (rectangleFromArray arr).toOption)) fd
    let italicAngle ← requiredInDict pdf "FontDescriptor" (mkName "ItalicAngle") realValue fd
    let ascent ← optionalInDict pdf "FontDescriptor" (mkName "Ascent") realValue fd
    let descent ← optionalInDict pdf "FontDescriptor" (mkName "Descent") realValue fd
    let leading ← optionalInDict pdf "FontDescriptor" (mkName "Leading") realValue fd
    let capHeight ← optionalInDict pdf "FontDescriptor" (mkName "CapHeight") realValue fd
    let xHeight ← optionalInDict pdf "FontDescriptor" (mkName "XHeight") realValue fd
    let stemV ← optionalInDict pdf "FontDescriptor" (mkName "StemV") realValue fd
    let stemH ← optionalInDict pdf "FontDescriptor" (mkName "StemH") realValue fd
    let avgWidth ← optionalInDict pdf "FontDescriptor" (mkName "AvgWidth") realValue fd
    let maxWidth ← optionalInDict pdf "FontDescriptor" (mkName "MaxWidth") realValue fd
    let missingWidth ← optionalInDict pdf "FontDescriptor" (mkName "MissingWidth") realValue fd
    let charSet ← optionalInDict pdf "FontDescriptor" (mkName "CharSet") stringValue fd
    pure (some {
      fontName := fontName, fontFamily := fontFamily, fontStretch := fontStretch,
      fontWeight := fontWeight, flags := flags.toNat.toUInt32, fontBBox := fontBBox,
      italicAngle := italicAngle, ascent := ascent, descent := descent, leading := leading,
      capHeight := capHeight, xHeight := xHeight, stemV := stemV, stemH := stemH,
      avgWidth := avgWidth, maxWidth := maxWidth, missingWidth := missingWidth,
      charSet := charSet })

/-! ── `ToUnicode` CMaps ── -/

/-- Load a font's `ToUnicode` CMap, if present. Mirrors upstream's
    `loadUnicodeCMap`. -/
private def loadUnicodeCMap (pdf : Pdf) (fontDict : Dict) : IO (Option UnicodeCMap) := do
  match fontDict.get? (mkName "ToUnicode") with
  | none => pure none
  | some o => do
    let ref ← sure (notice (refValue o) "ToUnicode should be a reference")
    let toUnicode ← Data.PDF.Document.Pdf.lookupObject pdf ref
    match streamValue toUnicode with
    | some s => do
      let is ← Data.PDF.Document.Pdf.streamContent pdf ref s
      let chunks ← Data.PDF.Stream.toList is
      let content := Data.ByteString.pack (chunks.foldl (· ++ ·) ByteArray.empty).toList
      match parseUnicodeCMap content with
      | .error e => throw (corrupted s!"can't parse cmap: {e}")
      | .ok cmap => pure (some cmap)
    | none => throw (corrupted "ToUnicode: not a stream")

/-! ── Simple-font encoding differences ── -/

/-- Walk a `/Differences` array (PDF32000-1:2008 §9.6.6.2), accumulating
    `(code, glyphName)` pairs and re-numbering after each integer entry.
    Structural recursion on the (already fully materialized) list of
    remaining array entries. Mirrors the body of upstream's
    `loadEncodingDifferences` (its local `go`). -/
private def encodingDifferencesLoop :
    List (UInt8 × Data.ByteString) → Int → List Object → IO (List (UInt8 × Data.ByteString))
  | res, _, [] => pure res
  | res, n, o :: rest =>
    match o with
    | .number _ => do
      let n' ← sure (notice (intValue o) "Differences: elements should be integers")
      encodingDifferencesLoop res n' rest
    | .name name =>
      encodingDifferencesLoop
        ((n.toNat.toUInt8, Data.PDF.Core.Name.Name.toByteString name) :: res) (n + 1) rest
    | _ => throw (corrupted s!"Differences array: unexpected object: {reprStr o}")

/-- Load a `/Differences` array, if present. Mirrors upstream's
    `loadEncodingDifferences`. -/
private def loadEncodingDifferences (pdf : Pdf) (dict : Dict) :
    IO (List (UInt8 × Data.ByteString)) := do
  match dict.get? (mkName "Differences") with
  | none => pure []
  | some v => do
    let v' ← Data.PDF.Document.Pdf.deref pdf v
    let arr ← sure (notice (arrayValue v') "Differences should be an array")
    match arr.toList with
    | [] => pure []
    | o :: rest => do
      let n0 ← sure (notice (intValue o) "Differences: the first element should be integer")
      encodingDifferencesLoop [] n0 rest

/-- Load a simple font's `/Encoding` entry when it isn't one of the two
    literal `WinAnsiEncoding`/`MacRomanEncoding` names — the general case
    where `/Encoding` is (or resolves to) a dictionary with its own
    `/BaseEncoding`/`/Differences`. Mirrors the `Just o -> ...` branch of
    upstream's inline `case` in `loadFontInfoSimple`. -/
private def genericEncoding (pdf : Pdf) (o : Object) : IO (Option SimpleFontEncoding) := do
  let o' ← Data.PDF.Document.Pdf.deref pdf o
  let encDict ← sure (notice (dictValue o') "Encoding should be a dictionary")
  match encDict.get? (mkName "BaseEncoding") with
  | some baseObj =>
    match nameValue baseObj with
    | some n =>
      if n == mkName "WinAnsiEncoding" then do
        let diffs ← loadEncodingDifferences pdf encDict
        pure (some { simpleFontBaseEncoding := .winAnsi, simpleFontDifferences := diffs })
      else if n == mkName "MacRomanEncoding" then do
        let diffs ← loadEncodingDifferences pdf encDict
        pure (some { simpleFontBaseEncoding := .macRoman, simpleFontDifferences := diffs })
      else
        pure none
    | none => pure none
  | none => do
    let diffs ← loadEncodingDifferences pdf encDict
    -- XXX: should be StandardEncoding? (upstream's own comment, carried forward)
    pure (some { simpleFontBaseEncoding := .winAnsi, simpleFontDifferences := diffs })

/-! ── Simple fonts ── -/

/-- Load font info for a simple (non-Type-0) font. Mirrors upstream's
    `loadFontInfoSimple`. -/
private def loadFontInfoSimple (pdf : Pdf) (fontDict : Dict) : IO FISimple := do
  let toUnicode ← loadUnicodeCMap pdf fontDict
  let encoding ← match fontDict.get? (mkName "Encoding") with
    | none => pure none
    | some o =>
      match nameValue o with
      | some n =>
        if n == mkName "WinAnsiEncoding" then
          pure (some { simpleFontBaseEncoding := .winAnsi, simpleFontDifferences := [] })
        else if n == mkName "MacRomanEncoding" then
          pure (some { simpleFontBaseEncoding := .macRoman, simpleFontDifferences := [] })
        else
          genericEncoding pdf o
      | none => genericEncoding pdf o
  let widths ← match fontDict.get? (mkName "Widths") with
    | none => pure none
    | some v => do
      let v' ← Data.PDF.Document.Pdf.deref pdf v
      let array ← sure (notice (arrayValue v') "Widths should be an array")
      let ws ← array.toList.mapM fun o =>
        sure (notice (realValue o) "Widths elements should be real")
      let firstChar ← sure
        (notice (fontDict.get? (mkName "FirstChar") >>= intValue) "FirstChar should be an integer")
      let lastChar ← sure
        (notice (fontDict.get? (mkName "LastChar") >>= intValue) "LastChar should be an integer")
      pure (some (firstChar, lastChar, ws))
  let fontDescriptor ← loadFontDescriptor pdf fontDict
  pure {
    fiSimpleUnicodeCMap := toUnicode,
    fiSimpleEncoding := encoding,
    fiSimpleWidths := widths,
    fiSimpleFontMatrix := Data.PDF.Content.Transform.scale 0.001 0.001,
    fiSimpleFontDescriptor := fontDescriptor }

/-! ── Composite (Type 0 / CID) fonts ── -/

/-- Load font info for a Type 0 (composite/CID) font. Mirrors upstream's
    `loadFontInfoComposite`. -/
private def loadFontInfoComposite (pdf : Pdf) (fontDict : Dict) : IO FIComposite := do
  let toUnicode ← loadUnicodeCMap pdf fontDict
  let descFont ← do
    let descFontObj ← sure
      (notice (fontDict.get? (mkName "DescendantFonts")) "DescendantFonts should exist")
    let descFontObj' ← Data.PDF.Document.Pdf.deref pdf descFontObj
    let descFontArr ← sure (notice (arrayValue descFontObj') "DescendantFonts should be an array")
    match descFontArr.toList with
    | [o] => do
      let o' ← Data.PDF.Document.Pdf.deref pdf o
      sure (notice (dictValue o') "DescendantFonts element should be a dictionary")
    | _ => throw (corrupted "Unexpected value of DescendantFonts key in font dictionary")
  let defaultWidth ← match descFont.get? (mkName "DW") with
    | none => pure 1000
    | some o => do
      let o' ← Data.PDF.Document.Pdf.deref pdf o
      sure (notice (realValue o') "DW should be real")
  let widths ← match descFont.get? (mkName "W") with
    | none => pure (∅ : CIDFontWidths)
    | some o => do
      let o' ← Data.PDF.Document.Pdf.deref pdf o
      let arr ← sure (notice (arrayValue o') "W should be an array")
      let arr' ← arr.mapM (Data.PDF.Document.Pdf.deref pdf)
      sure (makeCIDFontWidths arr')
  let fontDescriptor ← loadFontDescriptor pdf descFont
  pure {
    fiCompositeUnicodeCMap := toUnicode,
    fiCompositeWidths := widths,
    fiCompositeDefaultWidth := defaultWidth,
    fiCompositeFontDescriptor := fontDescriptor }

/-! ── The public entry point ── -/

/-- Load font info for the font, dispatching on `fontDictSubtype`. A Type 3
    font's `/FontMatrix` (PDF32000-1:2008 §9.6.5.3) overrides the default
    `0.001`-scale matrix `loadFontInfoSimple` otherwise assumes. Mirrors
    upstream's `fontDictLoadInfo`. -/
def fontDictLoadInfo (fd : FontDict) : IO FontInfo := do
  match ← fontDictSubtype fd with
  | .type0 => .composite <$> loadFontInfoComposite fd.pdf fd.dict
  | .type3 => do
    let fi ← loadFontInfoSimple fd.pdf fd.dict
    let obj ← sure (notice (fd.dict.get? (mkName "FontMatrix")) "FontMatrix should exist")
    let obj' ← Data.PDF.Document.Pdf.deref fd.pdf obj
    let arr ← sure (notice (arrayValue obj') "FontMatrix should be an array")
    match arr.toList.mapM realValue with
    | some [a, b, c, d, e, f] =>
      pure (.simple { fi with fiSimpleFontMatrix := { a := a, b := b, c := c, d := d, e := e, f := f } })
    | some _ => throw (corrupted "FontMatrix: wrong number of elements")
    | none => throw (corrupted "FontMatrics should contain numbers")
  | _ => .simple <$> loadFontInfoSimple fd.pdf fd.dict

end Data.PDF.Document.FontDict
