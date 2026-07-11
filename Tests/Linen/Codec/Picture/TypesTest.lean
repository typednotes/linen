/-
  Tests for `Linen.Codec.Picture.Types` — checks the `Pixel` instances'
  component conversions, `Image` pixel get/set and generation, colour
  conversions, and `DynamicImage` dispatch.
-/
import Linen.Codec.Picture.Types

open Codec.Picture

-- ── `Pixel` component conversions ──

#guard Pixel.toComponents (⟨10, 20, 30⟩ : PixelRGB8) == #[10, 20, 30]
#guard (Pixel.fromComponents #[10, 20, 30] : PixelRGB8) == ⟨10, 20, 30⟩
#guard Pixel.componentCount PixelRGB8 == 3
#guard Pixel.componentCount PixelRGBA8 == 4
#guard Pixel.pixelOpacity (⟨1, 2, 3⟩ : PixelRGB8) == 255
#guard Pixel.pixelOpacity (⟨1, 2, 3, 42⟩ : PixelRGBA8) == 42

#guard Pixel.mixWith (fun _ a b => a + b) (⟨1, 2, 3⟩ : PixelRGB8) ⟨10, 20, 30⟩
  == (⟨11, 22, 33⟩ : PixelRGB8)

#guard Pixel.colorMap (fun c => c + 1) (⟨1, 2, 3⟩ : PixelRGB8) == (⟨2, 3, 4⟩ : PixelRGB8)

-- ── `TransparentPixel` ──

#guard TransparentPixel.dropAlphaLayer (⟨1, 2, 3, 4⟩ : PixelRGBA8) == (⟨1, 2, 3⟩ : PixelRGB8)
#guard TransparentPixel.setOpacity (99 : Pixel8) (⟨1, 2, 3, 4⟩ : PixelRGBA8)
  == (⟨1, 2, 3, 99⟩ : PixelRGBA8)

-- ── `Image` get/set/generate ──

private def img2x2 : Image PixelRGB8 :=
  generateImage (fun x y => ⟨x.toUInt8, y.toUInt8, 0⟩) 2 2

#guard img2x2.getPixel 0 0 == (⟨0, 0, 0⟩ : PixelRGB8)
#guard img2x2.getPixel 1 0 == (⟨1, 0, 0⟩ : PixelRGB8)
#guard img2x2.getPixel 0 1 == (⟨0, 1, 0⟩ : PixelRGB8)
#guard img2x2.getPixel 1 1 == (⟨1, 1, 0⟩ : PixelRGB8)
#guard (img2x2.setPixel 1 1 ⟨9, 9, 9⟩).getPixel 1 1 == (⟨9, 9, 9⟩ : PixelRGB8)
#guard img2x2.stride == 6

#guard (pixelMap (ColorConvertible.promotePixel · : PixelRGB8 → PixelRGBA8) img2x2).getPixel 1 0
  == (⟨1, 0, 0, 255⟩ : PixelRGBA8)

#guard pixelFold (fun acc _ _ (p : PixelRGB8) => acc + p.r.toNat) 0 img2x2 == 2

-- ── `ColorConvertible` / `ColorSpaceConvertible` ──

#guard (ColorConvertible.promotePixel (200 : Pixel8) : Pixel16) == 51400
#guard (ColorConvertible.promotePixel (⟨5, 6, 7⟩ : PixelRGB8) : PixelRGBA8) == ⟨5, 6, 7, 255⟩

#guard (ColorSpaceConvertible.convertPixel (⟨255, 255, 255⟩ : PixelRGB8) : PixelYCbCr8)
  == ⟨255, 128, 128⟩

#guard (ColorSpaceConvertible.convertPixel (⟨0, 0, 0⟩ : PixelRGB8) : PixelYCbCr8)
  == ⟨0, 128, 128⟩

-- ── `ColorPlane` / `extractComponent` ──

#guard ColorPlane.toComponentIndex (α := PixelRGB8) (plane := PlaneGreen) == 1

#guard (extractComponent (α := PixelRGB8) (plane := PlaneGreen) img2x2).getPixel 1 1 == 1

-- ── `DynamicImage` ──

#guard dynamicMap (fun img => img.width) (.rgb8 img2x2) == 2

-- ── `LumaPlaneExtractable` ──

#guard LumaPlaneExtractable.computeLuma (⟨100, 200, 3⟩ : PixelYCbCr8) == 100
