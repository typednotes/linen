/-
  Tests for `Linen.Codec.Picture.Png.Internal.Type` — CRC-32 known-value
  checks, chunk-signature byte constants, `PngFilter`/`PngImageType`/
  `PngInterlaceMethod`/`PngUnit` code round trips, a hand-built chunk-stream
  round trip through `parseChunks`/`parseRawPngImage`, and a `parsePalette`
  round trip.
-/
import Linen.Codec.Picture.Png.Internal.Type

open Codec.Picture

-- ── CRC-32 ──

-- The standard CRC-32 (zlib/PNG, polynomial 0xedb88320) test vector for the
-- ASCII string "123456789" is the well-known value 0xCBF43926.
#guard pngComputeCrc ["123456789".toUTF8] == 0xCBF43926

-- The CRC-32 of the empty buffer is 0.
#guard pngComputeCrc [ByteArray.empty] == 0

-- `mkRawChunk` bakes in a CRC that `parseOneChunk` accepts.
def pngTypeTestChunk : PngRawChunk := mkRawChunk iDATSignature (ByteArray.mk #[1, 2, 3, 4])

#guard pngTypeTestChunk.chunkCRC == pngComputeCrc [iDATSignature, ByteArray.mk #[1, 2, 3, 4]]

-- ── Chunk-signature byte constants ──

#guard pngSignature == ByteArray.mk #[137, 80, 78, 71, 13, 10, 26, 10]
#guard iHDRSignature == "IHDR".toUTF8
#guard pLTESignature == "PLTE".toUTF8
#guard iDATSignature == "IDAT".toUTF8
#guard iENDSignature == "IEND".toUTF8
#guard tRNSSignature == "tRNS".toUTF8
#guard gammaSignature == "gAMA".toUTF8
#guard pHYsSignature == "pHYs".toUTF8
#guard tEXtSignature == "tEXt".toUTF8
#guard zTXtSignature == "zTXt".toUTF8
#guard animationControlSignature == "acTL".toUTF8

-- ── `PngFilter` code round trip ──

#guard match pngFilterOfCode 0 with | .ok f => codeOfPngFilter f == 0 | .error _ => false
#guard match pngFilterOfCode 1 with | .ok f => codeOfPngFilter f == 1 | .error _ => false
#guard match pngFilterOfCode 2 with | .ok f => codeOfPngFilter f == 2 | .error _ => false
#guard match pngFilterOfCode 3 with | .ok f => codeOfPngFilter f == 3 | .error _ => false
#guard match pngFilterOfCode 4 with | .ok f => codeOfPngFilter f == 4 | .error _ => false
#guard match pngFilterOfCode 5 with | .ok _ => false | .error _ => true

-- ── `PngImageType` code round trip ──

#guard match imageTypeOfCode 0 with | .ok t => codeOfImageType t == 0 | .error _ => false
#guard match imageTypeOfCode 2 with | .ok t => codeOfImageType t == 2 | .error _ => false
#guard match imageTypeOfCode 3 with | .ok t => codeOfImageType t == 3 | .error _ => false
#guard match imageTypeOfCode 4 with | .ok t => codeOfImageType t == 4 | .error _ => false
#guard match imageTypeOfCode 6 with | .ok t => codeOfImageType t == 6 | .error _ => false
#guard match imageTypeOfCode 1 with | .ok _ => false | .error _ => true

-- ── `PngInterlaceMethod` code round trip ──

#guard match interlaceMethodOfCode 0 with | .ok m => codeOfInterlaceMethod m == 0 | .error _ => false
#guard match interlaceMethodOfCode 1 with | .ok m => codeOfInterlaceMethod m == 1 | .error _ => false
#guard match interlaceMethodOfCode 2 with | .ok _ => false | .error _ => true

-- ── `PngUnit` code round trip ──

#guard match parsePngUnit [0] with | .ok (u, _) => codeOfPngUnit u == 0 | .error _ => false
#guard match parsePngUnit [1] with | .ok (u, _) => codeOfPngUnit u == 1 | .error _ => false

-- ── `PngGamma` round trip ──

-- 100000 (0x000186A0), big-endian, decodes to gamma value 1.0.
#guard match parsePngGamma [0, 1, 0x86, 0xA0] with
  | .ok (g, _) => Float.abs (g.value - 1.0) < 1e-9
  | .error _ => false

-- ── Hand-built chunk-stream round trip ──

-- A minimal, valid one-pixel true-colour PNG image: the signature, an
-- `IHDR` chunk, and a terminating `IEND` chunk (no `IDAT`, which this
-- module never needs to decompress).
def pngTypeTestImage : PngRawImage :=
  { header :=
      { width := 1, height := 1, bitDepth := 8, colourType := .trueColour,
        compressionMethod := 0, filterMethod := 0, interlaceMethod := .noInterlace }
    chunks := [mkRawChunk iENDSignature ByteArray.empty] }

def pngTypeTestEncoded : List UInt8 := (putPngRawImage pngTypeTestImage).toStrictByteString.unpack

#guard match parseRawPngImage pngTypeTestEncoded with
  | .ok img =>
      img.header.width == 1 && img.header.height == 1 && img.header.colourType == .trueColour &&
      (match img.chunks with
        | [c] => c.chunkType == iENDSignature
        | _ => false)
  | .error _ => false

-- The CRC check applies to every chunk except `IHDR` — corrupting a
-- non-`IHDR` chunk's payload after the fact (leaving the CRC stale) must be
-- rejected by the generic chunk decoder.
def pngTypeTestCorruptedChunkBytes : List UInt8 :=
  let corrupted : PngRawChunk := { pngTypeTestChunk with chunkData := ByteArray.mk #[9, 9, 9, 9] }
  (putPngRawChunk corrupted).toStrictByteString.unpack

#guard match parseOneChunk pngTypeTestCorruptedChunkBytes with
  | .error _ => true
  | .ok _ => false

-- `IHDR`'s own decoder, by contrast, reads and discards its trailing CRC
-- without verifying it (the documented upstream asymmetry): corrupting the
-- CRC bytes at the end of an otherwise-valid `IHDR` chunk still parses.
#guard
  let ihdrBytes := (putPngIHdr pngTypeTestImage.header).toStrictByteString.unpack
  let corrupted := ihdrBytes.dropLast ++ [(ihdrBytes.getLast! : UInt8) ^^^ 0xFF]
  match parsePngIHdr corrupted with
  | .ok (hdr, _) => hdr.width == 1 && hdr.height == 1
  | .error _ => false

-- ── `parsePalette` round trip ──

def pngTypeTestPaletteChunk : PngRawChunk :=
  mkRawChunk pLTESignature (ByteArray.mk #[255, 0, 0, 0, 255, 0, 0, 0, 255])

#guard match parsePalette pngTypeTestPaletteChunk with
  | .ok pal =>
      pal.width == 3 && pal.height == 1 &&
      pal.getPixel 0 0 == (⟨255, 0, 0⟩ : PixelRGB8) &&
      pal.getPixel 1 0 == (⟨0, 255, 0⟩ : PixelRGB8) &&
      pal.getPixel 2 0 == (⟨0, 0, 255⟩ : PixelRGB8)
  | .error _ => false

-- An invalid (non-multiple-of-3) palette size is rejected.
#guard match parsePalette (mkRawChunk pLTESignature (ByteArray.mk #[1, 2, 3, 4])) with
  | .ok _ => false
  | .error _ => true

-- ── `chunksWithSig` ──

#guard chunksWithSig pngTypeTestImage iENDSignature == [ByteArray.empty]
#guard chunksWithSig pngTypeTestImage iDATSignature == []
