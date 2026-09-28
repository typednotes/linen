/-
  Tests for `Linen.Data.PDF.Content.FontInfo`.

  Every definition here is pure, so every test is a `#guard`.
-/
import Linen.Data.PDF.Content.FontInfo

open Data.PDF.Core.Object Data.PDF.Content.Transform Data.PDF.Content.FontDescriptor
open Data.PDF.Content.Processor (Glyph)
open Data.PDF.Content.FontInfo

private def bs (s : String) : Data.ByteString := Data.ByteString.pack s.toUTF8.toList

-- A minimal font descriptor, overridable field-by-field per test.
private def testDescriptor : FontDescriptor :=
  { fontName := bs "Test", fontFamily := none, fontStretch := none, fontWeight := none, flags := 0,
    fontBBox := none, italicAngle := 0, ascent := none, descent := none, leading := none, capHeight := none,
    xHeight := none, stemV := none, stemH := none, avgWidth := none, maxWidth := none, missingWidth := none,
    charSet := none }

-- ── `fdYCoordinates` ──

-- `FontBBox` takes priority when present.
#guard fdYCoordinates 1000 { testDescriptor with
    fontBBox := some { llx := 0, lly := -200, urx := 1000, ury := 800 } } == some (-0.2, 0.8)

-- Falls back to `Descent`/`Ascent` when there is no `FontBBox`.
#guard fdYCoordinates 1000 { testDescriptor with descent := some (-200), ascent := some 800 } ==
  some (-0.2, 0.8)

-- Falls back to `CapHeight` (paired with `0`) when neither is present.
#guard fdYCoordinates 1000 { testDescriptor with capHeight := some 700 } == some (0, 0.7)

-- `none` when the descriptor specifies none of the three.
#guard fdYCoordinates 1000 testDescriptor == none

-- ── `getGlyphYCoordinates` ──

private def noFISimple : FISimple :=
  { fiSimpleUnicodeCMap := none, fiSimpleEncoding := none, fiSimpleWidths := none,
    fiSimpleFontMatrix := identity, fiSimpleFontDescriptor := none }

#guard getGlyphYCoordinates (.simple noFISimple) 65 == (0, 1)

#guard getGlyphYCoordinates
    (.simple { noFISimple with fiSimpleFontDescriptor := some { testDescriptor with capHeight := some 700 } })
    65 == (0, 0.7)

-- ── `simpleFontEncodingDecode` ──

-- With no `Differences`, falls through to the base encoding's own table.
#guard simpleFontEncodingDecode { simpleFontBaseEncoding := .winAnsi, simpleFontDifferences := [] } 0x41 ==
  some "A"

-- A `Differences` override resolved via the Adobe Glyph List takes
-- priority over the base encoding.
#guard simpleFontEncodingDecode
    { simpleFontBaseEncoding := .winAnsi, simpleFontDifferences := [(0x41, bs "Euro")] } 0x41 == some "€"

-- An unknown code, and an unknown glyph name, both decode to `none`.
#guard simpleFontEncodingDecode { simpleFontBaseEncoding := .winAnsi, simpleFontDifferences := [] } 0x81 ==
  none
#guard simpleFontEncodingDecode
    { simpleFontBaseEncoding := .winAnsi, simpleFontDifferences := [(0x41, bs "notARealGlyphName")] } 0x41 ==
  none

-- ── `makeCIDFontWidths`/`cidFontGetWidth` ──

-- A range entry: `cFirst cLast width`.
#guard match makeCIDFontWidths #[.number 1, .number 3, .number 500] with
  | .ok w => cidFontGetWidth w 2 == some 500 ∧ cidFontGetWidth w 4 == none
  | .error _ => false

-- An explicit-widths entry: `cFirst [w0 w1 ...]`.
#guard match makeCIDFontWidths #[.number 10, .array #[.number 100, .number 200]] with
  | .ok w => cidFontGetWidth w 10 == some 100 ∧ cidFontGetWidth w 11 == some 200 ∧ cidFontGetWidth w 12 == none
  | .error _ => false

-- An individual-code override takes priority over a range containing the
-- same code.
#guard match makeCIDFontWidths #[.number 1, .number 3, .number 500, .number 2, .array #[.number 999]] with
  | .ok w => cidFontGetWidth w 2 == some 999
  | .error _ => false

-- A malformed `"W"` array is rejected.
#guard match makeCIDFontWidths #[.number 1, .name (Data.PDF.Core.Name.Name.empty)] with
  | .error _ => true
  | .ok _ => false

-- ── `CIDFontWidths`'s `Append`/`EmptyCollection` instances ──

#guard match (∅ : CIDFontWidths) with
  | w => w.cidFontWidthsChars.isEmpty ∧ w.cidFontWidthsRanges.isEmpty

#guard
  let w1 : CIDFontWidths := { cidFontWidthsChars := Std.HashMap.ofList [((1 : Int), (10 : Float))], cidFontWidthsRanges := [] }
  let w2 : CIDFontWidths := { cidFontWidthsChars := Std.HashMap.ofList [((2 : Int), (20 : Float))], cidFontWidthsRanges := [] }
  let w := w1 ++ w2
  cidFontGetWidth w 1 == some 10 ∧ cidFontGetWidth w 2 == some 20

-- ── `fontInfoDecodeGlyphs`, simple fonts ──

-- With no `ToUnicode` CMap and no encoding, falls back to decoding the
-- byte as raw ASCII/UTF-8.
#guard
  match fontInfoDecodeGlyphs (.simple noFISimple) (bs "A") with
  | [(g, w)] => g.glyphText == some "A" ∧ w == 0
  | _ => false

-- Widths are read from `fiSimpleWidths`, scaled by the font matrix.
#guard
  let fi : FISimple := { noFISimple with fiSimpleWidths := some (0x41, 0x5A, [500]), fiSimpleFontMatrix := scale 0.001 0.001 }
  match fontInfoDecodeGlyphs (.simple fi) (bs "A") with
  | [(_, w)] => w == 0.5
  | _ => false

-- An encoding is consulted (and takes priority over the raw fallback) when
-- there is no `ToUnicode` CMap.
#guard
  let enc : SimpleFontEncoding := { simpleFontBaseEncoding := .winAnsi, simpleFontDifferences := [] }
  let fi : FISimple := { noFISimple with fiSimpleEncoding := some enc }
  match fontInfoDecodeGlyphs (.simple fi) (Data.ByteString.pack [0x41]) with
  | [(g, _)] => g.glyphText == some "A"
  | _ => false

-- ── `fontInfoDecodeGlyphs`, composite fonts ──

-- With no `ToUnicode` CMap, falls back to 2-byte codes (note: this uses
-- upstream's own `* 255`, not `* 256`, code computation — see the module
-- doc-comment).
#guard
  let fi : FIComposite := { fiCompositeUnicodeCMap := none, fiCompositeWidths := ∅, fiCompositeDefaultWidth := 1000, fiCompositeFontDescriptor := none }
  match fontInfoDecodeGlyphs (.composite fi) (Data.ByteString.pack [0x00, 0x41]) with
  | [(g, w)] => g.glyphCode == 0x41 ∧ w == 1
  | _ => false

-- Two 2-byte codes are decoded in order.
#guard
  let fi : FIComposite := { fiCompositeUnicodeCMap := none, fiCompositeWidths := ∅, fiCompositeDefaultWidth := 1000, fiCompositeFontDescriptor := none }
  match fontInfoDecodeGlyphs (.composite fi) (Data.ByteString.pack [0x00, 0x41, 0x00, 0x42]) with
  | [(g1, _), (g2, _)] => g1.glyphCode == 0x41 ∧ g2.glyphCode == 0x42
  | _ => false
