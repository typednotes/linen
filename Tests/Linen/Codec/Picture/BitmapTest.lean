/-
  Tests for `Linen.Codec.Picture.Bitmap` — checks `linePadding`, `sizeofPixelData`,
  the `Bitfield` make/extract round trip, `getBitfield`, `extractDpiOfMetadata`,
  and full encode→decode round trips for the `Pixel8`/`PixelRGB8`/`PixelRGBA8`
  `BmpEncodable` instances.
-/
import Linen.Codec.Picture.Bitmap

open Codec.Picture

-- ── `linePadding` ──

-- 8bpp, width 5 → 5 bytes/line, pad to 8 (next multiple of 4)
#guard linePadding 8 5 == 3
-- 24bpp, width 4 → 12 bytes/line, already a multiple of 4
#guard linePadding 24 4 == 0
-- 1bpp, width 10 → 2 bytes/line, pad to 4
#guard linePadding 1 10 == 2

-- ── `sizeofPixelData` ──

#guard sizeofPixelData 24 4 2 == 24
#guard sizeofPixelData 8 5 3 == 24
#guard sizeofPixelData 32 2 3 == 24

-- ── `Bitfield` make/extract round trip ──

-- an 8-bit-wide mask at bit 0 should extract unchanged (scale == 1)
#guard extractBitfield (makeBitfield 0x000000FF) 0x000000AB == 0xAB
-- a mask at bit 16 (as in the default 32-bit RGB bitfields) shifts down
#guard extractBitfield (makeBitfield 0x00FF0000) 0x00AB0000 == 0xAB
-- a narrower mask scales up to fill 8 bits: a 5-bit field's max value 31
-- scales by 255/31 ≈ 8.226, so the max value 31 maps to 255
#guard extractBitfield (makeBitfield 0x0000001F) 0x0000001F == 255
#guard extractBitfield (makeBitfield 0x0000001F) 0x00000000 == 0

-- ── `getBitfield` ──

#guard match getBitfield 0 with | .error _ => true | .ok _ => false
#guard match getBitfield 0x00FF0000 with | .ok _ => true | .error _ => false

-- ── `defaultBitfieldsRGB32`/`defaultBitfieldsRGB16` sanity ──

#guard (defaultBitfieldsRGB32.red.mask, defaultBitfieldsRGB32.green.mask, defaultBitfieldsRGB32.blue.mask)
  == (0x00FF0000, 0x0000FF00, 0x000000FF)
#guard (defaultBitfieldsRGB16.red.mask, defaultBitfieldsRGB16.green.mask, defaultBitfieldsRGB16.blue.mask)
  == (0x7C00, 0x03E0, 0x001F)

-- ── `extractDpiOfMetadata` ──

-- a metadata set with no dpi entries defaults to 0 dots-per-meter both ways
#guard extractDpiOfMetadata Metadatas.empty == (0, 0)
-- `mkDpiMetadata` stores a dpi value under `.dpiX`/`.dpiY`; `extractDpiOfMetadata`
-- converts it to dots-per-meter
#guard extractDpiOfMetadata (mkDpiMetadata 96) ==
  ((dotPerInchToDotsPerMeter 96).toUInt32, (dotPerInchToDotsPerMeter 96).toUInt32)

-- ── Encode → decode round trips ──

/-- A small 3×2 RGB test image, all pixels distinct so row/column order bugs show up. -/
def rgbImg : Image PixelRGB8 :=
  generateImage (fun x y => (⟨(x * 10 + 1).toUInt8, (y * 10 + 2).toUInt8, (x + y).toUInt8⟩ : PixelRGB8)) 3 2

def rgbBytes : ByteArray := ByteArray.mk (encodeBitmap rgbImg).unpack.toArray

#guard match decodeBitmap rgbBytes with
  | .ok (.rgb8 img) => img.width == 3 ∧ img.height == 2 ∧ img.getPixel 0 0 == rgbImg.getPixel 0 0
    ∧ img.getPixel 2 1 == rgbImg.getPixel 2 1
  | _ => false

/-- A 2×2 RGBA test image with a non-trivial alpha channel. -/
def rgbaImg : Image PixelRGBA8 :=
  generateImage (fun x y => (⟨(x * 50).toUInt8, (y * 50).toUInt8, 7, (x + y * 2 + 1).toUInt8⟩ : PixelRGBA8)) 2 2

def rgbaBytes : ByteArray := ByteArray.mk (encodeBitmap rgbaImg).unpack.toArray

#guard match decodeBitmap rgbaBytes with
  | .ok (.rgba8 img) => img.getPixel 0 0 == rgbaImg.getPixel 0 0 ∧ img.getPixel 1 1 == rgbaImg.getPixel 1 1
  | _ => false

/-- An 8-bit grayscale (indexed, default-palette) test image. -/
def y8Img : Image Pixel8 :=
  generateImage (fun x y => ((x + y * 3) % 256).toUInt8) 4 3

def y8Bytes : ByteArray := ByteArray.mk (encodeBitmap y8Img).unpack.toArray

-- the default palette is the identity grayscale ramp, so decoding (which
-- expands the palette via `palettedToTrueColor`) recovers the same grey value
-- in every channel
#guard match decodeBitmap y8Bytes with
  | .ok (.rgb8 img) =>
      let p := img.getPixel 2 1
      p.r == y8Img.getPixel 2 1 ∧ p.r == p.g ∧ p.g == p.b
  | _ => false
