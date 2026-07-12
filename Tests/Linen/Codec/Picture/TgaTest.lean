/-
  Tests for `Linen.Codec.Picture.Tga` — checks `isRleEncoded` and full
  encode→decode round trips for the `Pixel8`/`PixelRGB8`/`PixelRGBA8`
  `TgaSaveable` instances.
-/
import Linen.Codec.Picture.Tga

open Codec.Picture

-- ── `isRleEncoded` ──

#guard isRleEncoded (TgaImageType.trueColor true) == true
#guard isRleEncoded (TgaImageType.trueColor false) == false
#guard isRleEncoded (TgaImageType.colorMapped true) == true
#guard isRleEncoded (TgaImageType.monochrome false) == false

-- ── Encode → decode round trips ──

/-- A small 3×2 RGB test image, all pixels distinct so row/column order bugs show up. -/
def tgaRgbImg : Image PixelRGB8 :=
  generateImage (fun x y => (⟨(x * 10 + 1).toUInt8, (y * 10 + 2).toUInt8, (x + y).toUInt8⟩ : PixelRGB8)) 3 2

def tgaRgbBytes : ByteArray := ByteArray.mk (encodeTga tgaRgbImg).unpack.toArray

#guard match decodeTga tgaRgbBytes with
  | .ok (.rgb8 img) => img.width == 3 ∧ img.height == 2 ∧ img.getPixel 0 0 == tgaRgbImg.getPixel 0 0
    ∧ img.getPixel 2 1 == tgaRgbImg.getPixel 2 1
  | _ => false

/-- A 2×2 RGBA test image with a non-trivial alpha channel. -/
def tgaRgbaImg : Image PixelRGBA8 :=
  generateImage (fun x y => (⟨(x * 50).toUInt8, (y * 50).toUInt8, 7, (x + y * 2 + 1).toUInt8⟩ : PixelRGBA8)) 2 2

def tgaRgbaBytes : ByteArray := ByteArray.mk (encodeTga tgaRgbaImg).unpack.toArray

#guard match decodeTga tgaRgbaBytes with
  | .ok (.rgba8 img) => img.getPixel 0 0 == tgaRgbaImg.getPixel 0 0 ∧ img.getPixel 1 1 == tgaRgbaImg.getPixel 1 1
  | _ => false

/-- An 8-bit grayscale test image. -/
def tgaY8Img : Image Pixel8 :=
  generateImage (fun x y => ((x + y * 3) % 256).toUInt8) 4 3

def tgaY8Bytes : ByteArray := ByteArray.mk (encodeTga tgaY8Img).unpack.toArray

#guard match decodeTga tgaY8Bytes with
  | .ok (.y8 img) => img.width == 4 ∧ img.height == 3 ∧ img.getPixel 2 1 == tgaY8Img.getPixel 2 1
  | _ => false
