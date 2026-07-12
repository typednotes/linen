/-
  Tests for `Linen.Codec.Picture.Png`.

  `encodePng`/`decodePng`/`encodePalettedPng`/`decodePngWithPaletteAndMetadata`
  are `IO`-returning (see that module's doc-comment for why), so round trips
  are checked with `#eval show IO Unit from do ... unless ... throw
  (IO.userError ...)`, matching `Tests/Linen/Crypto/Zlib/FFITest.lean`'s
  convention for `IO`/FFI-backed code, rather than plain `#guard`.

  Fixture names are prefixed `png` to avoid cross-file `Tests` namespace
  collisions (bare names like `img`/`bytes` have collided across sibling test
  files before).
-/
import Linen.Codec.Picture.Png

open Codec.Picture

namespace Tests.Codec.Picture.Png

-- ── A `Data.ByteString` → `ByteArray` helper (encode output → decode input) ──

private def pngToByteArray (bs : Data.ByteString) : ByteArray :=
  bs.data.extract bs.off (bs.off + bs.len)

-- ── Round trip: 8-bit true-colour RGB ──

private def pngRgbImg : Image PixelRGB8 :=
  generateImage (fun x y => (⟨(x * 40 + 10).toUInt8, (y * 50 + 5).toUInt8, ((x + y) * 20).toUInt8⟩ :
    PixelRGB8)) 4 3

#eval show IO Unit from do
  let encoded ← encodePng pngRgbImg
  match ← decodePng (pngToByteArray encoded) with
  | .error e => throw (IO.userError s!"rgb8 round trip failed to decode: {e}")
  | .ok (.rgb8 img) =>
    unless img.width == 4 ∧ img.height == 3 ∧
        img.getPixel 2 1 == pngRgbImg.getPixel 2 1 ∧ img.getPixel 3 2 == pngRgbImg.getPixel 3 2 do
      throw (IO.userError "rgb8 round trip: pixel mismatch")
  | .ok _ => throw (IO.userError "rgb8 round trip: wrong DynamicImage variant")

-- ── Round trip: 8-bit true-colour RGBA ──

private def pngRgbaImg : Image PixelRGBA8 :=
  generateImage (fun x y => (⟨(x * 30).toUInt8, (y * 60).toUInt8, 128, (x + y).toUInt8⟩ :
    PixelRGBA8)) 3 3

#eval show IO Unit from do
  let encoded ← encodePng pngRgbaImg
  match ← decodePng (pngToByteArray encoded) with
  | .error e => throw (IO.userError s!"rgba8 round trip failed to decode: {e}")
  | .ok (.rgba8 img) =>
    unless img.width == 3 ∧ img.height == 3 ∧
        img.getPixel 0 0 == pngRgbaImg.getPixel 0 0 ∧ img.getPixel 2 2 == pngRgbaImg.getPixel 2 2 do
      throw (IO.userError "rgba8 round trip: pixel mismatch")
  | .ok _ => throw (IO.userError "rgba8 round trip: wrong DynamicImage variant")

-- ── Round trip: 8-bit greyscale ──

private def pngGreyImg : Image Pixel8 :=
  generateImage (fun x y => (x * 10 + y * 3).toUInt8) 5 2

#eval show IO Unit from do
  let encoded ← encodePng pngGreyImg
  match ← decodePng (pngToByteArray encoded) with
  | .error e => throw (IO.userError s!"y8 round trip failed to decode: {e}")
  | .ok (.y8 img) =>
    unless img.width == 5 ∧ img.height == 2 ∧ img.data == pngGreyImg.data do
      throw (IO.userError "y8 round trip: pixel mismatch")
  | .ok _ => throw (IO.userError "y8 round trip: wrong DynamicImage variant")

-- ── Round trip: indexed colour (palette) ──

private def pngPalette : Image PixelRGB8 :=
  generateImage (fun x _ => (⟨(x * 50).toUInt8, (x * 30).toUInt8, (x * 10).toUInt8⟩ : PixelRGB8)) 4 1

private def pngIndices : Image Pixel8 :=
  generateImage (fun x y => ((x + y) % 4).toUInt8) 3 2

#eval show IO Unit from do
  match ← encodePalettedPng pngPalette pngIndices with
  | .error e => throw (IO.userError s!"paletted encode failed: {e}")
  | .ok encoded =>
    match decodePngWithPaletteAndMetadata (pngToByteArray encoded) with
    | .error e => throw (IO.userError s!"paletted decode failed to parse: {e}")
    | .ok (_metas, action) =>
      match ← action with
      | .error e => throw (IO.userError s!"paletted decode failed: {e}")
      | .ok (.inl _) => throw (IO.userError "paletted round trip: expected an indexed result")
      | .ok (.inr pal) =>
        unless pal.indexedImage.width == 3 ∧ pal.indexedImage.height == 2 ∧
            pal.indexedImage.data == pngIndices.data ∧
            pal.palette.getPixel 2 0 == pngPalette.getPixel 2 0 do
          throw (IO.userError "paletted round trip: mismatch")

-- ── Hand-filtered fixture: a non-interlaced 8-bit greyscale image whose four
--   scanlines use, in turn, the `None`/`Up`/`Sub`/`Paeth` filters — decoded
--   independently of this module's own encoder (which only ever emits
--   `None`), exercising `unfilterPass`'s other three branches directly. ──

/-- Original (unfiltered) pixel values, row-major, 3 pixels wide × 4 tall. -/
private def pngFilteredRows : List (List UInt8) :=
  [[10, 20, 30], [15, 25, 35], [12, 20, 29], [14, 22, 33]]

/-- The same rows, PNG-filtered by hand (filter tag byte prepended to each):
    row 0 `None` (byte-identical to the original), row 1 `Up` (each byte is
    `orig - row0`, all `= 5`), row 2 `Sub` (each byte is `orig - previous
    byte in the same row`), row 3 `Paeth` (worked out by hand from the Paeth
    predictor against row 2 and row 3's own already-unfiltered prefix). -/
private def pngFilteredBytes : ByteArray :=
  ByteArray.mk #[
    0, 10, 20, 30,
    2, 5, 5, 5,
    1, 12, 8, 9,
    4, 2, 2, 4]

#eval show IO Unit from do
  let compressed ← Crypto.Zlib.compress pngFilteredBytes
  let ihdr : PngIHdr :=
    { width := 3, height := 4, bitDepth := 8, colourType := .greyscale,
      compressionMethod := 0, filterMethod := 0, interlaceMethod := .noInterlace }
  let raw : PngRawImage :=
    { header := ihdr, chunks := [mkRawChunk iDATSignature compressed, mkRawChunk iENDSignature ByteArray.empty] }
  let bytes := (Data.ByteString.copy (putPngRawImage raw).toStrictByteString).data
  match ← decodePng bytes with
  | .error e => throw (IO.userError s!"hand-filtered fixture failed to decode: {e}")
  | .ok (.y8 img) =>
    let expected : Array UInt8 := (pngFilteredRows.flatten).toArray
    unless img.width == 3 ∧ img.height == 4 ∧ img.data == expected do
      throw (IO.userError s!"hand-filtered fixture: pixel mismatch, got {img.data.toList}")
  | .ok _ => throw (IO.userError "hand-filtered fixture: wrong DynamicImage variant")

end Tests.Codec.Picture.Png
