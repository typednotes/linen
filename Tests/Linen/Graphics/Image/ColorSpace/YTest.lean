/-
  Tests for `Linen.Graphics.Image.ColorSpace.Y` — the `Y`/`YA` colour spaces,
  their `Pixel`/`ColorSpace`/`AlphaSpace` instances, and `PixelY`'s
  component-wise arithmetic instances.

  Fixture/example names are prefixed `csY` to avoid clashing with any other
  test file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace channels toComponents fromComponents
  promote getPxC setPxC mapPxC liftPx liftPx2 foldlPx2 getAlpha addAlpha dropAlpha)
open Graphics.Image.ColorSpace.Y

-- ── `Y`/`PixelY` — `ColorSpace` operations ──

#guard (channels (cs := Y) (e := Int)) == [Y.luma]
#guard (toComponents (cs := Y) (e := Int) (⟨(7 : Int)⟩ : PixelY Int)) == 7
#guard (fromComponents (cs := Y) (e := Int) (7 : Int)) == (⟨7⟩ : PixelY Int)
#guard (promote (cs := Y) (7 : Int)) == (⟨7⟩ : PixelY Int)
#guard getPxC (cs := Y) (e := Int) (⟨(7 : Int)⟩ : PixelY Int) Y.luma == 7
#guard (setPxC (cs := Y) (e := Int) (⟨(7 : Int)⟩ : PixelY Int) Y.luma 9) == ⟨9⟩
#guard (mapPxC (cs := Y) (e := Int) (fun _ v => v + 1) (⟨(7 : Int)⟩ : PixelY Int)) == ⟨8⟩
#guard (liftPx (cs := Y) (e := Int) (· + 1) (⟨(7 : Int)⟩ : PixelY Int)) == ⟨8⟩
#guard (liftPx2 (cs := Y) (e := Int) (· + ·) (⟨(3 : Int)⟩ : PixelY Int) ⟨4⟩) == ⟨7⟩
#guard (foldlPx2 (cs := Y) (e := Int) (β := Int) (· + · + ·) 0 (⟨(3 : Int)⟩ : PixelY Int) ⟨4⟩) == 7

-- ── `YA`/`PixelYA` — `ColorSpace` operations ──

#guard (channels (cs := YA) (e := Int)) == [YA.luma, YA.alpha]
#guard (toComponents (cs := YA) (e := Int) (⟨(3 : Int), 4⟩ : PixelYA Int)) == (3, 4)
#guard (fromComponents (cs := YA) (e := Int) ((3, 4) : Int × Int)) == (⟨3, 4⟩ : PixelYA Int)
#guard (promote (cs := YA) (5 : Int)) == (⟨5, 5⟩ : PixelYA Int)
#guard getPxC (cs := YA) (e := Int) (⟨(3 : Int), 4⟩ : PixelYA Int) YA.luma == 3
#guard getPxC (cs := YA) (e := Int) (⟨(3 : Int), 4⟩ : PixelYA Int) YA.alpha == 4
#guard (setPxC (cs := YA) (e := Int) (⟨(3 : Int), 4⟩ : PixelYA Int) YA.luma 9) == ⟨9, 4⟩
#guard (setPxC (cs := YA) (e := Int) (⟨(3 : Int), 4⟩ : PixelYA Int) YA.alpha 9) == ⟨3, 9⟩
#guard (mapPxC (cs := YA) (e := Int) (fun _ v => v + 1) (⟨(3 : Int), 4⟩ : PixelYA Int)) == ⟨4, 5⟩
#guard (liftPx (cs := YA) (e := Int) (· + 1) (⟨(3 : Int), 4⟩ : PixelYA Int)) == ⟨4, 5⟩
#guard (liftPx2 (cs := YA) (e := Int) (· + ·) (⟨(3 : Int), 4⟩ : PixelYA Int) ⟨1, 2⟩) == ⟨4, 6⟩
#guard (foldlPx2 (cs := YA) (e := Int) (β := Int) (· + · + ·) 0
  (⟨(3 : Int), 4⟩ : PixelYA Int) ⟨1, 2⟩) == 10

-- ── `AlphaSpace` between `YA` and `Y` ──

#guard getAlpha (cs := YA) (e := Int) (⟨(3 : Int), 4⟩ : PixelYA Int) == 4
#guard addAlpha (cs := YA) (e := Int) (4 : Int) (⟨(3 : Int)⟩ : PixelY Int) == (⟨3, 4⟩ : PixelYA Int)
#guard dropAlpha (cs := YA) (e := Int) (⟨(3 : Int), 4⟩ : PixelYA Int) == (⟨3⟩ : PixelY Int)

-- ── Component-wise arithmetic on `PixelY` ──

#guard (⟨(3 : Int)⟩ : PixelY Int) + ⟨4⟩ == ⟨7⟩
#guard (⟨(7 : Int)⟩ : PixelY Int) - ⟨4⟩ == ⟨3⟩
#guard (⟨(3 : Int)⟩ : PixelY Int) * ⟨4⟩ == ⟨12⟩
#guard (⟨(12 : Int)⟩ : PixelY Int) / ⟨4⟩ == ⟨3⟩
#guard -(⟨(3 : Int)⟩ : PixelY Int) == ⟨-3⟩
#guard (0 : PixelY Int) == ⟨0⟩
#guard (1 : PixelY Int) == ⟨1⟩
