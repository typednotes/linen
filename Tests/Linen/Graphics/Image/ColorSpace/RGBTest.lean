/-
  Tests for `Linen.Graphics.Image.ColorSpace.RGB` — the `RGB`/`RGBA` colour
  spaces, their `Pixel`/`ColorSpace`/`AlphaSpace` instances, and
  `PixelRGB`'s component-wise arithmetic instances.

  Fixture/example names are prefixed `csRGB` to avoid clashing with any other
  test file's identifiers in the shared `Tests` namespace (in particular
  `Tests.Linen.Graphics.Image.ColorSpace.YTest`, whose `ColorSpace`
  operations share the same imported names).
-/
import Linen.Graphics.Image.ColorSpace.RGB

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace channels toComponents fromComponents
  promote getPxC setPxC mapPxC liftPx liftPx2 foldlPx2 getAlpha addAlpha dropAlpha)
open Graphics.Image.ColorSpace.RGB

-- ── `RGB`/`PixelRGB` — `ColorSpace` operations ──

def csRGBPx : PixelRGB Int := ⟨3, 4, 5⟩

#guard (channels (cs := RGB) (e := Int)) == [RGB.red, RGB.green, RGB.blue]
#guard (toComponents (cs := RGB) (e := Int) csRGBPx) == (3, 4, 5)
#guard (fromComponents (cs := RGB) (e := Int) ((3, 4, 5) : Int × Int × Int)) == csRGBPx
#guard (promote (cs := RGB) (7 : Int)) == (⟨7, 7, 7⟩ : PixelRGB Int)
#guard getPxC (cs := RGB) (e := Int) csRGBPx RGB.red == 3
#guard getPxC (cs := RGB) (e := Int) csRGBPx RGB.green == 4
#guard getPxC (cs := RGB) (e := Int) csRGBPx RGB.blue == 5
#guard (setPxC (cs := RGB) (e := Int) csRGBPx RGB.red 9) == ⟨9, 4, 5⟩
#guard (setPxC (cs := RGB) (e := Int) csRGBPx RGB.green 9) == ⟨3, 9, 5⟩
#guard (setPxC (cs := RGB) (e := Int) csRGBPx RGB.blue 9) == ⟨3, 4, 9⟩
#guard (mapPxC (cs := RGB) (e := Int) (fun _ v => v + 1) csRGBPx) == ⟨4, 5, 6⟩
#guard (liftPx (cs := RGB) (e := Int) (· + 1) csRGBPx) == ⟨4, 5, 6⟩
#guard (liftPx2 (cs := RGB) (e := Int) (· + ·) csRGBPx ⟨1, 1, 1⟩) == ⟨4, 5, 6⟩
#guard (foldlPx2 (cs := RGB) (e := Int) (β := Int) (· + · + ·) 0 csRGBPx ⟨1, 1, 1⟩) == 15

-- ── `RGBA`/`PixelRGBA` — `ColorSpace` operations ──

def csRGBAPx : PixelRGBA Int := ⟨3, 4, 5, 6⟩

#guard (channels (cs := RGBA) (e := Int)) == [RGBA.red, RGBA.green, RGBA.blue, RGBA.alpha]
#guard (toComponents (cs := RGBA) (e := Int) csRGBAPx) == (3, 4, 5, 6)
#guard (fromComponents (cs := RGBA) (e := Int) ((3, 4, 5, 6) : Int × Int × Int × Int)) == csRGBAPx
#guard (promote (cs := RGBA) (7 : Int)) == (⟨7, 7, 7, 7⟩ : PixelRGBA Int)
#guard getPxC (cs := RGBA) (e := Int) csRGBAPx RGBA.red == 3
#guard getPxC (cs := RGBA) (e := Int) csRGBAPx RGBA.green == 4
#guard getPxC (cs := RGBA) (e := Int) csRGBAPx RGBA.blue == 5
#guard getPxC (cs := RGBA) (e := Int) csRGBAPx RGBA.alpha == 6
#guard (setPxC (cs := RGBA) (e := Int) csRGBAPx RGBA.red 9) == ⟨9, 4, 5, 6⟩
#guard (setPxC (cs := RGBA) (e := Int) csRGBAPx RGBA.alpha 9) == ⟨3, 4, 5, 9⟩
#guard (mapPxC (cs := RGBA) (e := Int) (fun _ v => v + 1) csRGBAPx) == ⟨4, 5, 6, 7⟩
#guard (liftPx (cs := RGBA) (e := Int) (· + 1) csRGBAPx) == ⟨4, 5, 6, 7⟩
#guard (liftPx2 (cs := RGBA) (e := Int) (· + ·) csRGBAPx ⟨1, 1, 1, 1⟩) == ⟨4, 5, 6, 7⟩
#guard (foldlPx2 (cs := RGBA) (e := Int) (β := Int) (· + · + ·) 0
  csRGBAPx ⟨1, 1, 1, 1⟩) == 22

-- ── `AlphaSpace` between `RGBA` and `RGB` ──

#guard getAlpha (cs := RGBA) (e := Int) csRGBAPx == 6
#guard addAlpha (cs := RGBA) (e := Int) (6 : Int) csRGBPx == csRGBAPx
#guard dropAlpha (cs := RGBA) (e := Int) csRGBAPx == csRGBPx

-- ── Component-wise arithmetic on `PixelRGB` ──

#guard csRGBPx + (⟨1, 1, 1⟩ : PixelRGB Int) == ⟨4, 5, 6⟩
#guard csRGBPx - (⟨1, 1, 1⟩ : PixelRGB Int) == ⟨2, 3, 4⟩
#guard csRGBPx * (⟨2, 2, 2⟩ : PixelRGB Int) == ⟨6, 8, 10⟩
#guard (⟨12, 12, 12⟩ : PixelRGB Int) / (⟨4, 3, 2⟩ : PixelRGB Int) == ⟨3, 4, 6⟩
#guard -csRGBPx == ⟨-3, -4, -5⟩
#guard (0 : PixelRGB Int) == ⟨0, 0, 0⟩
#guard (1 : PixelRGB Int) == ⟨1, 1, 1⟩
