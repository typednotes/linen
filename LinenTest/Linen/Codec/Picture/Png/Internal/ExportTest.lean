/-
  Tests for `Linen.Codec.Picture.Png.Internal.Export` — raw (unfiltered,
  `PngFilter.none`-tagged) scanline byte streams (`rawScanlineBytes8`/
  `rawScanlineBytes16`), plain/paletted PNG chunk assembly
  (`encodePngUsing`/`encodePngWithMetadataUsing`/`encodePalettedPngUsing`),
  and `DynamicImage` dispatch (`encodeDynamicPngUsing`). Every "compress"
  step below is exercised with `deflate := id` (see the module doc-comment
  for why a real deflate isn't available), so the assembled `IDAT` payload
  is exactly the raw scanline bytes.
-/
import Linen.Codec.Picture.Png.Internal.Export

open Codec.Picture

/-- `l`'s `i`-th chunk, or an `IEND` placeholder if `i` is out of range
    (`PngRawChunk` derives no `Inhabited`, so plain `l[i]!` isn't available). -/
private def chunkAt (l : List PngRawChunk) (i : Nat) : PngRawChunk :=
  l.getD i (mkRawChunk iENDSignature ByteArray.empty)

-- ── `rawScanlineBytes8` / `rawScanlineBytes16` ──

/-- A 3-wide, 2-tall single-component (`Pixel8`) image. -/
def pngExportGrey : Image Pixel8 :=
  { width := 3, height := 2, data := #[1, 2, 3, 4, 5, 6] }

-- Each row is prefixed with filter byte `0` (`PngFilter.none`).
#guard (rawScanlineBytes8 pngExportGrey).toList == [0, 1, 2, 3, 0, 4, 5, 6]

/-- A 2-wide, 1-tall `PixelRGB8` image (`3` components per pixel). -/
def pngExportRgb : Image PixelRGB8 :=
  { width := 2, height := 1, data := #[10, 20, 30, 40, 50, 60] }

#guard (rawScanlineBytes8 pngExportRgb).toList == [0, 10, 20, 30, 40, 50, 60]

/-- A 2-wide, 1-tall single-component (`Pixel16`) image, exercising the
    big-endian high-byte-first split. -/
def pngExportGrey16 : Image Pixel16 :=
  { width := 2, height := 1, data := #[0x0102, 0x0304] }

#guard (rawScanlineBytes16 pngExportGrey16).toList == [0, 1, 2, 3, 4]

/-- A 1-wide, 2-tall `PixelRGB16` image, exercising both the per-row filter
    byte and the per-component 16-bit split across multiple rows. -/
def pngExportRgb16 : Image PixelRGB16 :=
  { width := 1, height := 2, data := #[0x00FF, 0x0100, 0x0201, 0xFFFF, 0x0000, 0x1234] }

#guard (rawScanlineBytes16 pngExportRgb16).toList ==
  [0, 0, 0xFF, 1, 0, 2, 1, 0, 0xFF, 0xFF, 0, 0, 0x12, 0x34]

-- ── `encodePngUsing` / `encodePngWithMetadataUsing` (plain images) ──

/-- A single-pixel `PixelRGB8` image, small enough to hand-check the
    assembled chunk stream. -/
def pngExportOnePixelRgb : Image PixelRGB8 :=
  { width := 1, height := 1, data := #[10, 20, 30] }

private def pngExportDummyHeader : PngIHdr :=
  { width := 0, height := 0, bitDepth := 0, colourType := .trueColour,
    compressionMethod := 0, filterMethod := 0, interlaceMethod := .noInterlace }

def pngExportOnePixelBytes : PngRawImage :=
  match parseRawPngImage (Data.ByteString.unpack (encodePngUsing id pngExportOnePixelRgb)) with
  | .ok img => img
  | .error _ => { header := pngExportDummyHeader, chunks := [] }

#guard pngExportOnePixelBytes.header.width == 1
#guard pngExportOnePixelBytes.header.height == 1
#guard pngExportOnePixelBytes.header.bitDepth == 8
#guard pngExportOnePixelBytes.header.colourType == .trueColour
#guard pngExportOnePixelBytes.header.interlaceMethod == .noInterlace
#guard pngExportOnePixelBytes.chunks.length == 2
#guard (chunkAt pngExportOnePixelBytes.chunks 0).chunkType == iDATSignature
#guard (chunkAt pngExportOnePixelBytes.chunks 0).chunkData.toList == (rawScanlineBytes8 pngExportOnePixelRgb).toList
#guard (chunkAt pngExportOnePixelBytes.chunks 1).chunkType == iENDSignature

/-- Attaching metadata (`Title`) adds one `tEXt` chunk before the `IDAT`
    chunk, matching `encodeMetadatas`' chunk order. -/
def pngExportWithMetas : PngRawImage :=
  match parseRawPngImage
      (Data.ByteString.unpack
        (encodePngWithMetadataUsing id (Metadatas.singleton .title "A pixel") pngExportOnePixelRgb)) with
  | .ok img => img
  | .error _ => { header := pngExportOnePixelBytes.header, chunks := [] }

#guard pngExportWithMetas.chunks.length == 3
#guard (chunkAt pngExportWithMetas.chunks 0).chunkType == tEXtSignature
#guard (chunkAt pngExportWithMetas.chunks 1).chunkType == iDATSignature
#guard (chunkAt pngExportWithMetas.chunks 2).chunkType == iENDSignature

/-- A greyscale (`Pixel8`) image round-trips through `IHDR`'s colour type. -/
def pngExportOnePixelGrey : Image Pixel8 := { width := 1, height := 1, data := #[42] }

def pngExportGreyHeader : PngIHdr :=
  match parseRawPngImage (Data.ByteString.unpack (encodePngUsing id pngExportOnePixelGrey)) with
  | .ok img => img.header
  | .error _ => pngExportOnePixelBytes.header

#guard pngExportGreyHeader.colourType == .greyscale
#guard pngExportGreyHeader.bitDepth == 8

/-- A 16-bit `PixelRGBA16` image declares bit depth `16`. -/
def pngExportOnePixelRgba16 : Image PixelRGBA16 :=
  { width := 1, height := 1, data := #[0x1111, 0x2222, 0x3333, 0x4444] }

def pngExportRgba16Header : PngIHdr :=
  match parseRawPngImage (Data.ByteString.unpack (encodePngUsing id pngExportOnePixelRgba16)) with
  | .ok img => img.header
  | .error _ => pngExportOnePixelBytes.header

#guard pngExportRgba16Header.colourType == .trueColourWithAlpha
#guard pngExportRgba16Header.bitDepth == 16

-- ── `encodePalettedPngUsing` (colour-indexed images) ──

/-- A 2-colour `PixelRGB8` palette (red, green). -/
def pngExportPaletteRgb : Image PixelRGB8 :=
  { width := 2, height := 1, data := #[255, 0, 0, 0, 255, 0] }

/-- A 2-pixel indexed image, each pixel referencing a valid palette entry. -/
def pngExportIndexedImg : Image Pixel8 := { width := 2, height := 1, data := #[0, 1] }

def pngExportPalettedResult : PngRawImage :=
  match encodePalettedPngUsing id pngExportPaletteRgb pngExportIndexedImg with
  | .ok bytes =>
      match parseRawPngImage (Data.ByteString.unpack bytes) with
      | .ok img => img
      | .error _ => pngExportOnePixelBytes
  | .error _ => pngExportOnePixelBytes

#guard pngExportPalettedResult.header.colourType == .indexedColor
#guard pngExportPalettedResult.chunks.length == 3
#guard (chunkAt pngExportPalettedResult.chunks 0).chunkType == pLTESignature
#guard (chunkAt pngExportPalettedResult.chunks 0).chunkData.toList == [255, 0, 0, 0, 255, 0]
#guard (chunkAt pngExportPalettedResult.chunks 1).chunkType == iDATSignature
#guard (chunkAt pngExportPalettedResult.chunks 2).chunkType == iENDSignature

-- An index referencing a palette entry that doesn't exist is rejected.
def pngExportBadIndexImg : Image Pixel8 := { width := 2, height := 1, data := #[0, 5] }

#guard
  (match encodePalettedPngUsing id pngExportPaletteRgb pngExportBadIndexImg with
   | .error _ => true
   | .ok _ => false)

-- A palette more than one pixel tall is rejected.
def pngExportBadPalette : Image PixelRGB8 :=
  { width := 1, height := 2, data := #[0, 0, 0, 255, 255, 255] }

#guard
  (match encodePalettedPngUsing id pngExportBadPalette pngExportIndexedImg with
   | .error _ => true
   | .ok _ => false)

/-- A `PixelRGBA8` palette splits into an opaque `PLTE` chunk plus a
    `tRNS` chunk holding each entry's alpha. -/
def pngExportPaletteRgba : Image PixelRGBA8 :=
  { width := 2, height := 1, data := #[255, 0, 0, 10, 0, 255, 0, 20] }

def pngExportPalettedAlphaResult : PngRawImage :=
  match encodePalettedPngUsing id pngExportPaletteRgba pngExportIndexedImg with
  | .ok bytes =>
      match parseRawPngImage (Data.ByteString.unpack bytes) with
      | .ok img => img
      | .error _ => pngExportOnePixelBytes
  | .error _ => pngExportOnePixelBytes

#guard pngExportPalettedAlphaResult.chunks.length == 4
#guard (chunkAt pngExportPalettedAlphaResult.chunks 0).chunkType == pLTESignature
#guard (chunkAt pngExportPalettedAlphaResult.chunks 0).chunkData.toList == [255, 0, 0, 0, 255, 0]
#guard (chunkAt pngExportPalettedAlphaResult.chunks 1).chunkType == tRNSSignature
#guard (chunkAt pngExportPalettedAlphaResult.chunks 1).chunkData.toList == [10, 20]
#guard (chunkAt pngExportPalettedAlphaResult.chunks 2).chunkType == iDATSignature
#guard (chunkAt pngExportPalettedAlphaResult.chunks 3).chunkType == iENDSignature

-- ── `encodeDynamicPngUsing` ──

#guard
  (match encodeDynamicPngUsing id (.rgb8 pngExportOnePixelRgb) with
   | .ok _ => true
   | .error _ => false)

#guard
  (match encodeDynamicPngUsing id (.y8 pngExportOnePixelGrey) with
   | .ok _ => true
   | .error _ => false)

/-- `PixelRGBF`/`PixelYCbCr8`/etc. have no PNG encoding, matching upstream's
    `encodeDynamicPng _ = Left "Unsupported image format for PNG export"`. -/
def pngExportUnsupported : DynamicImage := .rgbF { width := 1, height := 1, data := #[0.0, 0.0, 0.0] }

#guard
  (match encodeDynamicPngUsing id pngExportUnsupported with
   | .error _ => true
   | .ok _ => false)
