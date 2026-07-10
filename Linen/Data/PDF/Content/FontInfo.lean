/-
  Data.PDF.Content.FontInfo — font metadata needed to decode glyphs

  Ports `Pdf.Content.FontInfo` from Hackage's `pdf-toolbox-content`
  (https://github.com/Yuras/pdf-toolbox, `content/lib/Pdf/Content/FontInfo.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/content/lib/Pdf/Content/FontInfo.hs`),
  module 12 of the `pdf-toolbox-content` import documented in
  `docs/imports/PdfToolboxContent/dependencies.md`.

  Ties together every other `pdf-toolbox-content` module: a `FontInfo` (built
  elsewhere, from a font dictionary — that construction lives in
  `pdf-toolbox-document`, out of this package's scope) is either `simple`
  (single-byte codes, PDF32000-1:2008 §9.6.6) or `composite` (a Type 0/CID
  font, §9.7), and `fontInfoDecodeGlyphs` decodes a shown string into
  `Data.PDF.Content.Processor.Glyph`s using whichever combination of
  `ToUnicode` CMap, base/`Differences` encoding, or raw UTF-8 fallback the
  font provides.

  ## Design

  - Upstream's `FontBaseEncoding` constructors `FontBaseEncodingWinAnsi`/
    `FontBaseEncodingMacRoman` drop their redundant type-name prefix here
    (`.winAnsi`/`.macRoman`), matching this project's established convention
    for small closed enumerations (e.g. `Data.PDF.Core.Exception.Kind`).
    Every record field, by contrast, keeps its upstream `fiSimple`/
    `fiComposite`/`simpleFont`/`cidFontWidths` prefix — matching
    `Data.PDF.Content.Processor.GraphicsState`'s precedent (its `gs`-prefixed
    fields) — since several of these records have same-named-if-unprefixed
    fields (e.g. an "encoding" or "widths" field on more than one record)
    that would otherwise collide after `open`.

  - `CIDFontWidths`'s upstream `Monoid`/`Semigroup` instance (`mempty`,
    `mappend = (<>)`, both fields' `Map`/list combined pointwise) has no
    general `Semigroup`/`Monoid` typeclass counterpart in `linen`
    (`Data.PDF.Core.Name`'s module doc-comment already establishes the
    project's substitute); ported the same way, as `Append`/`EmptyCollection`
    instances.

  - `tryDecode2byte`'s composite-font fallback and `cmapDecodeString`
    (its `ToUnicode`-CMap-driven counterpart) both consume a `ByteString`
    two bytes (respectively, one glyph) at a time. `tryDecode2byte` recurses
    directly on its own already-destructured tail (`b1 :: b2 :: rest`, a
    genuine structural subterm — no fuel needed, same shape as
    `Data.PDF.Content.UnicodeCMap.hexPairs`); `cmapDecodeString` calls
    `Data.PDF.Content.UnicodeCMap.unicodeCMapNextGlyph`, whose returned
    remainder is only known to be *shorter*, not a syntactic subterm, of the
    original `Data.ByteString` — so it is given an explicit `fuel : Nat`
    seeded from `str.length`, per this project's established
    `parseObjectFuel`/`nextGlyphFuel` convention (a well-formed input can
    never need more steps than there are bytes left).

  - Upstream's `tryDecode2byte` computes a composite font's fallback 2-byte
    code as `fromIntegral b1 * 255 + fromIntegral b2` — `* 255`, not the
    more usual `* 256` a big-endian 16-bit code would need. This reads like
    an upstream typo, but per this project's faithful-port convention (see
    `Data.PDF.Content.Encoding.PdfDoc`'s documented byte-22/23 duplicate-entry
    note for the same policy) it is ported byte-for-byte as-is, not silently
    corrected — this fallback path is itself only reached when a composite
    font supplies *no* `ToUnicode` CMap to begin with (upstream's own `-- XXX:
    use encoding here` comment already flags this path as an approximation).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Content.Transform
import Linen.Data.PDF.Content.FontDescriptor
import Linen.Data.PDF.Content.GlyphList
import Linen.Data.PDF.Content.TexGlyphList
import Linen.Data.PDF.Content.Encoding.WinAnsi
import Linen.Data.PDF.Content.Encoding.MacRoman
import Linen.Data.PDF.Content.UnicodeCMap
import Linen.Data.PDF.Content.Processor
import Linen.Data.Text.Encoding
import Std.Data.HashMap

namespace Data.PDF.Content.FontInfo

open Data.PDF.Core.Object
open Data.PDF.Core.Object.Util (intValue realValue)
open Data.PDF.Core.Util (notice)
open Data.PDF.Content.Transform
open Data.PDF.Content.FontDescriptor (FontDescriptor)
open Data.PDF.Content.Processor (Glyph)
open Data.PDF.Content.UnicodeCMap (UnicodeCMap unicodeCMapDecodeGlyph unicodeCMapNextGlyph)

/-! ── Simple fonts ── -/

/-- Which standard base encoding a simple font's glyph codes are drawn from,
    before any per-font `Differences`. Mirrors upstream's
    `FontBaseEncoding` (see the module doc-comment for the dropped
    `FontBaseEncoding`-prefix on its constructors). -/
inductive FontBaseEncoding where
  /-- PDF32000-1:2008 Annex D.2's WinAnsiEncoding. -/
  | winAnsi
  /-- PDF32000-1:2008 Annex D.2's MacRomanEncoding. -/
  | macRoman
deriving BEq, Repr

/-- A simple font's encoding: a base encoding, plus any per-code overrides
    from the font dictionary's `Differences` array. Mirrors upstream's
    `SimpleFontEncoding`. -/
structure SimpleFontEncoding where
  /-- The base encoding `simpleFontDifferences` overrides individual codes
      from. -/
  simpleFontBaseEncoding : FontBaseEncoding
  /-- Per-code overrides: a glyph code together with the (PDF32000-1:2008
      Annex D) glyph name it should decode as instead of the base
      encoding's mapping for that code. -/
  simpleFontDifferences : List (UInt8 × Data.ByteString)
deriving Repr

/-- Font info for a simple (single-byte-code) font. Mirrors upstream's
    `FISimple`. -/
structure FISimple where
  /-- The font's `ToUnicode` CMap, if it has one — consulted before
      `fiSimpleEncoding`. -/
  fiSimpleUnicodeCMap : Option UnicodeCMap
  /-- The font's encoding, if not relying solely on `fiSimpleUnicodeCMap`. -/
  fiSimpleEncoding : Option SimpleFontEncoding
  /-- `(FirstChar, LastChar, widths)` from the font dictionary, if present. -/
  fiSimpleWidths : Option (Int × Int × List Float)
  /-- The font matrix (PDF32000-1:2008 §9.2.4), mapping glyph space to text
      space; used to scale a raw width into text-space units.
      -- FIXME: no `Option` as soon as this library provides metrics for
      the 14 standard fonts (upstream's own comment, carried forward). -/
  fiSimpleFontMatrix : Transform Float
  /-- The font's descriptor, if present. -/
  fiSimpleFontDescriptor : Option FontDescriptor
deriving Repr

/-! ── Composite (Type 0 / CID) fonts ── -/

/-- Glyph widths for a CID font (its `"W"` array, PDF32000-1:2008 §9.7.4.3).
    Mirrors upstream's `CIDFontWidths`. -/
structure CIDFontWidths where
  /-- Individual `code → width` overrides. -/
  cidFontWidthsChars : Std.HashMap Int Float
  /-- Contiguous `(firstCode, lastCode, width)` ranges. -/
  cidFontWidthsRanges : List (Int × Int × Float)
deriving Repr

-- Substitutes for upstream's `Semigroup`/`Monoid` instances (see the module
-- doc-comment).

/-- The empty `CIDFontWidths` (upstream's `Monoid` identity, `mempty`). -/
instance : EmptyCollection CIDFontWidths where
  emptyCollection := { cidFontWidthsChars := {}, cidFontWidthsRanges := [] }

/-- Combine two `CIDFontWidths` fieldwise (upstream's `Semigroup`/`Monoid`
    `(<>)`/`mappend`). -/
instance : Append CIDFontWidths where
  append w1 w2 :=
    { cidFontWidthsChars := Std.HashMap.ofList (w2.cidFontWidthsChars.toList ++ w1.cidFontWidthsChars.toList),
      cidFontWidthsRanges := w1.cidFontWidthsRanges ++ w2.cidFontWidthsRanges }

/-- Font info for a Type 0 (composite/CID) font. Mirrors upstream's
    `FIComposite`. -/
structure FIComposite where
  /-- The font's `ToUnicode` CMap, if it has one. -/
  fiCompositeUnicodeCMap : Option UnicodeCMap
  /-- The descendant CIDFont's glyph widths. -/
  fiCompositeWidths : CIDFontWidths
  /-- The descendant CIDFont's default width (`"DW"`, defaults to 1000 per
      spec, but that default is applied by the caller building this
      record, not here). -/
  fiCompositeDefaultWidth : Float
  /-- The descendant CIDFont's descriptor, if present.
      A `FontDescriptor` is present in CIDFonts, but per spec shall not be
      used with Type 0 fonts themselves — carried forward from upstream's
      own comment. -/
  fiCompositeFontDescriptor : Option FontDescriptor
deriving Repr

/-! ── `FontInfo` itself ── -/

/-- Font info, for either a simple or a composite font. Mirrors upstream's
    `FontInfo`. -/
inductive FontInfo where
  /-- A simple (single-byte-code) font. -/
  | simple (fi : FISimple)
  /-- A Type 0 (composite/CID) font. -/
  | composite (fi : FIComposite)
deriving Repr

/-! ── Glyph bounding-box Y coordinates ── -/

/-- The `(bottom, top)` Y coordinates for a glyph's bounding box, in
    text-space units scaled by `scaling` (always `1000` at the sole call
    site below, matching a glyph-space font matrix's usual `0.001` scale).
    Tries, in order: the font descriptor's `FontBBox`, then its
    `Descent`/`Ascent` pair, then just its `CapHeight` (paired with `0`).
    Mirrors upstream's `fdYCoordinates`. -/
def fdYCoordinates (scaling : Float) (fd : FontDescriptor) : Option (Float × Float) :=
  let viaBBox : Option (Float × Float) :=
    fd.fontBBox.map (fun r => (r.lly / scaling, r.ury / scaling))
  let viaDescAscent : Option (Float × Float) := do
    let d ← fd.descent
    let a ← fd.ascent
    pure (d / scaling, a / scaling)
  let viaCapHeight : Option (Float × Float) :=
    fd.capHeight.map (fun c => (0, c / scaling))
  viaBBox.orElse (fun _ => viaDescAscent) |>.orElse (fun _ => viaCapHeight)

/-- The `(bottom, top)` Y coordinates for a glyph's bounding box, defaulting
    to `(0, 1)` if `fInfo` has no font descriptor (or that descriptor
    specifies none of `FontBBox`/`Descent`+`Ascent`/`CapHeight`). Mirrors
    upstream's `getGlyphYCoordinates`, which ignores its glyph-code argument
    entirely (kept here only for signature fidelity — a vertical script
    could in principle need per-glyph heights, per upstream's own comment,
    but upstream itself doesn't implement that). -/
def getGlyphYCoordinates (fInfo : FontInfo) (_code : Int) : Float × Float :=
  let fd :=
    match fInfo with
    | .simple fi => fi.fiSimpleFontDescriptor
    | .composite fi => fi.fiCompositeFontDescriptor
  (fd.bind (fdYCoordinates 1000)).getD (0, 1)

/-! ── Simple-font encoding lookup ── -/

/-- Decode a simple font's glyph code to text via its encoding: an explicit
    per-code override (`Differences`) takes priority, resolved through the
    Adobe Glyph List and then the TeX glyph list; otherwise the base
    encoding's own table. Mirrors upstream's `simpleFontEncodingDecode`. -/
def simpleFontEncodingDecode (enc : SimpleFontEncoding) (code : UInt8) : Option Data.Text :=
  match (enc.simpleFontDifferences.find? (·.1 == code)).map Prod.snd with
  | none =>
    match enc.simpleFontBaseEncoding with
    | .winAnsi => Data.PDF.Content.Encoding.WinAnsi.winAnsiEncoding[code]?
    | .macRoman => Data.PDF.Content.Encoding.MacRoman.macRomanEncoding[code]?
  | some glyphName =>
    match Data.PDF.Content.GlyphList.adobeGlyphList[glyphName]? with
    | some c => some (Data.Text.singleton c)
    | none =>
      match Data.PDF.Content.TexGlyphList.texGlyphList[glyphName]? with
      | some c => some (Data.Text.singleton c)
      | none => none

/-! ── CID font widths ── -/

/-- Parse a CIDFont's `"W"` array (PDF32000-1:2008 §9.7.4.3): each entry is
    either `cFirst cLast width` (a range) or `cFirst [w0 w1 ...]` (explicit
    per-code widths starting at `cFirst`). Structural recursion on the
    already-destructured tail of the operand list. Mirrors upstream's
    (local, `Maybe`-monad) `go`. -/
private def widthsGo : CIDFontWidths → List Object → Option CIDFontWidths
  | res, [] => some res
  | res, (x1@(.number _)) :: (x2@(.number _)) :: (x3@(.number _)) :: xs => do
    let n1 ← intValue x1
    let n2 ← intValue x2
    let n3 ← realValue x3
    widthsGo { res with cidFontWidthsRanges := (n1, n2, n3) :: res.cidFontWidthsRanges } xs
  | res, x :: .array arr :: xs => do
    let n ← intValue x
    let ws ← arr.toList.mapM realValue
    let newEntries := (List.range ws.length).zip ws |>.map (fun (i, w) => (n + (i : Int), w))
    let merged := Std.HashMap.ofList (res.cidFontWidthsChars.toList ++ newEntries)
    widthsGo { res with cidFontWidthsChars := merged } xs
  | _, _ => none

/-- Make `CIDFontWidths` from the value of a `"W"` key in a descendant
    font. Mirrors upstream's `makeCIDFontWidths`. -/
def makeCIDFontWidths (vals : Array Object) : Except String CIDFontWidths :=
  notice (widthsGo {} vals.toList) s!"Can't parse CIDFont width {reprStr vals}"

/-- Get a CID font's glyph width by glyph code: an individual override, if
    any, else the first range containing `code`, else `none`. Mirrors
    upstream's `cidFontGetWidth`. -/
def cidFontGetWidth (w : CIDFontWidths) (code : Int) : Option Float :=
  match w.cidFontWidthsChars[code]? with
  | some width => some width
  | none =>
    match w.cidFontWidthsRanges.find? (fun (start, stop, _) => start ≤ code && code ≤ stop) with
    | some (_, _, width) => some width
    | none => none

/-! ── Decoding a shown string into glyphs ── -/

/-- Decode one byte of a simple font's shown string into a glyph, trying
    (in priority order) the `ToUnicode` CMap, then the encoding, then a raw
    UTF-8 fallback — falling further down the chain at each step only when
    the previous one didn't produce text. Mirrors the per-byte body of
    upstream's `fontInfoDecodeGlyphs (FontInfoSimple fi)`. -/
private def decodeSimpleByte (fi : FISimple) (fInfo : FontInfo) (c : UInt8) : Glyph × Float :=
  let code : Int := (c.toNat : Int)
  let asciiFallback : Option Data.Text :=
    match Data.Text.Encoding.decodeUtf8' (Data.ByteString.pack [c]) with
    | .ok t => some t
    | .error _ => none
  let viaEncoding : Option Data.Text :=
    fi.fiSimpleEncoding.bind (fun enc => simpleFontEncodingDecode enc c)
  let txt : Option Data.Text :=
    match fi.fiSimpleUnicodeCMap with
    | none => viaEncoding.orElse (fun _ => asciiFallback)
    | some toUnicode =>
      (unicodeCMapDecodeGlyph toUnicode code.toNat).orElse (fun _ => viaEncoding.orElse (fun _ => asciiFallback))
  let width : Float :=
    match fi.fiSimpleWidths with
    | none => 0
    | some (firstChar, lastChar, widths) =>
      if code ≥ firstChar ∧ code ≤ lastChar ∧ code - firstChar < (widths.length : Int) then
        (transform fi.fiSimpleFontMatrix ⟨widths.getD (code - firstChar).toNat 0, 0⟩).x
      else 0
  let (yBottom, yTop) := getGlyphYCoordinates fInfo code
  ({ glyphCode := code, glyphTopLeft := ⟨0, yBottom⟩, glyphBottomRight := ⟨width, yTop⟩, glyphText := txt },
   width)

/-- Composite-font fallback used when there is no `ToUnicode` CMap: try
    2-byte codes (the common case for composite fonts), one pair at a time.
    See the module doc-comment for the `* 255` (rather than `* 256`) code
    computation, kept verbatim from upstream. Structurally recursive on the
    already-destructured tail. Mirrors upstream's `tryDecode2byte`. -/
private def tryDecode2byte (fic : FIComposite) (fInfo : FontInfo) : List UInt8 → List (Glyph × Float)
  | b1 :: b2 :: rest =>
    let code : Int := (b1.toNat : Int) * 255 + (b2.toNat : Int)
    let width := ((cidFontGetWidth fic.fiCompositeWidths code).getD fic.fiCompositeDefaultWidth) / 1000
    let txt :=
      match Data.Text.Encoding.decodeUtf8' (Data.ByteString.pack [b1, b2]) with
      | .ok t => some t
      | .error _ => none
    let (yBottom, yTop) := getGlyphYCoordinates fInfo code
    let g : Glyph :=
      { glyphCode := code, glyphTopLeft := ⟨0, yBottom⟩, glyphBottomRight := ⟨width, yTop⟩, glyphText := txt }
    (g, width) :: tryDecode2byte fic fInfo rest
  | _ => []

/-- Composite-font decoding via a `ToUnicode` CMap: repeatedly peel off the
    next glyph code with `unicodeCMapNextGlyph`. `fuel`, seeded from the
    remaining byte count at the public call site below, bounds the number
    of glyphs decoded — see the module doc-comment's termination note. -/
private def cmapDecodeStringFuel (getWidth : Int → Float) (cmap : UnicodeCMap) (fInfo : FontInfo) :
    Nat → Data.ByteString → List (Glyph × Float)
  | 0, _ => []
  | fuel + 1, str =>
    match unicodeCMapNextGlyph cmap str with
    | none => []
    | some (g, rest) =>
      let code : Int := (g : Int)
      let width := getWidth code / 1000
      let (yBottom, yTop) := getGlyphYCoordinates fInfo code
      let glyph : Glyph :=
        { glyphCode := code, glyphTopLeft := ⟨0, yBottom⟩, glyphBottomRight := ⟨width, yTop⟩,
          glyphText := unicodeCMapDecodeGlyph cmap g }
      (glyph, width) :: cmapDecodeStringFuel getWidth cmap fInfo fuel rest

/-- `cmapDecodeStringFuel`, seeding `fuel` from `str`'s own length (see the
    module doc-comment). Mirrors upstream's `cmapDecodeString`. -/
private def cmapDecodeString (getWidth : Int → Float) (cmap : UnicodeCMap) (fInfo : FontInfo)
    (str : Data.ByteString) : List (Glyph × Float) :=
  cmapDecodeStringFuel getWidth cmap fInfo str.length str

/-- Decode a shown string into glyphs and their widths, dispatching on
    whether `fi` is a simple or composite font. Mirrors upstream's
    `fontInfoDecodeGlyphs`. -/
def fontInfoDecodeGlyphs (fi : FontInfo) (bs : Data.ByteString) : List (Glyph × Float) :=
  match fi with
  | .simple fis => bs.unpack.map (decodeSimpleByte fis fi)
  | .composite fic =>
    match fic.fiCompositeUnicodeCMap with
    | some toUnicode =>
      let getWidth (code : Int) : Float := (cidFontGetWidth fic.fiCompositeWidths code).getD fic.fiCompositeDefaultWidth
      cmapDecodeString getWidth toUnicode fi bs
    | none => tryDecode2byte fic fi bs.unpack

end Data.PDF.Content.FontInfo
