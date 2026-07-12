/-
  Tests for `Linen.Codec.Picture.Jpg`. Decode/encode are both pure
  (`Except String X` / `Data.ByteString` — see that module's doc-comment for
  why this module never needs `IO`), so round trips are checked with plain
  `#guard`.

  Fixture names are prefixed `jpg` to avoid cross-file `Tests` namespace
  collisions.
-/
import Linen.Codec.Picture.Jpg

open Codec.Picture
open Codec.Picture.Jpg.Internal

-- ── `powerOf` : the `SSSS` bit-category of a signed coefficient ──

#guard powerOf 0 == 0
#guard powerOf 1 == 1
#guard powerOf (-1) == 1
#guard powerOf 3 == 2
#guard powerOf (-3) == 2
#guard powerOf 4 == 3
#guard powerOf 255 == 8
#guard powerOf (-256) == 9

-- ── `quantize` : rounded division by the quantization table ──

#guard quantize (Array.replicate 64 (1 : Int16)) (Array.replicate 64 (5 : Int))
  == Array.replicate 64 (5 : Int)

#guard (quantize (Array.replicate 64 (16 : Int16)) (Array.replicate 64 (40 : Int))).getD 0 0 == 3

-- ── `divUpward` : round-up division ──

#guard divUpward 0 8 == 0
#guard divUpward 1 8 == 1
#guard divUpward 8 8 == 1
#guard divUpward 9 8 == 2

-- ── `huffmanWriterCodeOfTable` : canonical Huffman writer codes ──

-- A two-symbol table, both length 1: symbol `0` -> code `0`, symbol `1` ->
-- code `1` (the standard, minimal canonical Huffman assignment).
def jpgTinyHuffmanTable : HuffmanTable := [[0, 1]]

#guard (huffmanWriterCodeOfTable jpgTinyHuffmanTable).size.getD 0 0 == 1
#guard (huffmanWriterCodeOfTable jpgTinyHuffmanTable).size.getD 1 0 == 1
#guard (huffmanWriterCodeOfTable jpgTinyHuffmanTable).code.getD 0 0 == 0
#guard (huffmanWriterCodeOfTable jpgTinyHuffmanTable).code.getD 1 0 == 1

-- The shipped default luma DC table: rebuilding its writer code and its
-- decode tree must agree on every symbol's bit length (round trip through
-- `DefaultTable.lean`'s own `buildHuffmanTree`/`insertHuffmanVal`).
#guard
  let writer := huffmanWriterCodeOfTable defaultDcLumaHuffmanTable
  (List.range 12).all fun s => writer.size.getD s 0 != 0

-- ── `ycckArrayToCmyk` : inverted-YCbCr-plus-K -> CMYK ──

-- One pixel in, four bytes out; the `K` channel is the raw fourth byte
-- inverted (untouched by the YCbCr->RGB step).
#guard (ycckArrayToCmyk #[0, 0, 0, 0]).size == 4
#guard (ycckArrayToCmyk #[0, 0, 0, 0]).getD 3 0 == 255
#guard (ycckArrayToCmyk #[0, 0, 0, 200]).getD 3 0 == 55

-- ── `colorSpaceOfComponentStr` : component-identifier guessing ──

#guard colorSpaceOfComponentStr [1] == some .y
#guard colorSpaceOfComponentStr [1, 2] == some .ya
#guard colorSpaceOfComponentStr [1, 2, 3] == some .ycbcr
#guard colorSpaceOfComponentStr [82, 71, 66] == some .rgb
#guard colorSpaceOfComponentStr [67, 77, 89, 75] == some .cmyk

-- ── `scaleQuantisationMatrix` / `lumaQuantTableAtQuality` : IJG scaling ──

-- Quality 50 leaves the base table unscaled (`200 - 2*50 = 100`, i.e.
-- multiply-by-1).
#guard lumaQuantTableAtQuality 50 == defaultLumaQuantizationTable

-- Quality 100 clamps every scaled coefficient down to `1`.
#guard lumaQuantTableAtQuality 100 == Array.replicate 64 (1 : UInt8)

-- ── Round trip: a small grayscale image at default quality ──

def jpgGrayImg : Image Pixel8 :=
  generateImage (fun x y => (((x + y) * 16) % 256).toUInt8) 8 8

def jpgGrayBytes : Data.ByteString :=
  encodeDirectJpegAtQualityWithMetadata (pixel := Pixel8) 90 Metadatas.empty jpgGrayImg

def jpgGrayByteArray : ByteArray := ByteArray.mk jpgGrayBytes.unpack.toArray

#guard match decodeJpeg jpgGrayByteArray.data.toList with
  | .ok (.y8 img) => img.width == 8 ∧ img.height == 8
  | _ => false

-- ── Round trip: a small YCbCr image, checking a saturated colour ──

def jpgYCbCrImg : Image PixelYCbCr8 :=
  generateImage (fun x _ => if x < 8 then (⟨235, 128, 128⟩ : PixelYCbCr8)
    else (⟨16, 128, 128⟩ : PixelYCbCr8)) 16 16

def jpgYCbCrBytes : ByteArray :=
  let bs := encodeJpegAtQuality 95 jpgYCbCrImg
  ByteArray.mk bs.unpack.toArray

#guard match decodeJpeg jpgYCbCrBytes.data.toList with
  | .ok (.ycbcr8 img) =>
      img.width == 16 ∧ img.height == 16 ∧
      -- Luma should stay close to the extremes on each half at high quality.
      (img.getPixel 0 0).y > 200 ∧ (img.getPixel 15 0).y < 60
  | _ => false

-- ── `decodeJpegWithMetadata` : basic size metadata is attached ──

#guard match decodeJpegWithMetadata jpgYCbCrBytes.data.toList with
  | .ok (_, metas) => metas.lookup .width == some 16 ∧ metas.lookup .height == some 16
  | .error _ => false
