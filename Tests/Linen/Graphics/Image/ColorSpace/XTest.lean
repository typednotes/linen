/-
  Tests for `Linen.Graphics.Image.ColorSpace.X` — the generic single-channel
  `X` colour space, its `Pixel`/`ColorSpace` instance, `PixelX`'s
  component-wise arithmetic instances, and the channel-separation helpers
  (`toPixelsX`/`fromPixelsX`/`toImagesX`/`fromImagesX`/`squashWith`/
  `squashWith2`).

  Fixture/example names are prefixed `csX` to avoid clashing with any other
  test file's identifiers in the shared `Tests` namespace. The channel-level
  helpers are exercised against `Y`'s `PixelY Int`/`Image Y Int` (an
  arbitrary already-ported single-channel colour space) since `X` itself,
  per its own module doc-comment, is not meant to be converted to or from.
-/
import Linen.Graphics.Image.ColorSpace.X
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface
  (Pixel ColorSpace channels toComponents fromComponents promote getPxC setPxC mapPxC liftPx
    liftPx2 foldlPx2 dims unsafeIndex makeImage)
open Graphics.Image.ColorSpace.X
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- ── `X`/`PixelX` — `ColorSpace` operations ──

#guard (channels (cs := X) (e := Int)) == [X.x]
#guard (toComponents (cs := X) (e := Int) (⟨(7 : Int)⟩ : PixelX Int)) == 7
#guard (fromComponents (cs := X) (e := Int) (7 : Int)) == (⟨7⟩ : PixelX Int)
#guard (promote (cs := X) (7 : Int)) == (⟨7⟩ : PixelX Int)
#guard getPxC (cs := X) (e := Int) (⟨(7 : Int)⟩ : PixelX Int) X.x == 7
#guard (setPxC (cs := X) (e := Int) (⟨(7 : Int)⟩ : PixelX Int) X.x 9) == ⟨9⟩
#guard (mapPxC (cs := X) (e := Int) (fun _ v => v + 1) (⟨(7 : Int)⟩ : PixelX Int)) == ⟨8⟩
#guard (liftPx (cs := X) (e := Int) (· + 1) (⟨(7 : Int)⟩ : PixelX Int)) == ⟨8⟩
#guard (liftPx2 (cs := X) (e := Int) (· + ·) (⟨(3 : Int)⟩ : PixelX Int) ⟨4⟩) == ⟨7⟩
#guard (foldlPx2 (cs := X) (e := Int) (β := Int) (· + · + ·) 0 (⟨(3 : Int)⟩ : PixelX Int) ⟨4⟩) == 7

-- ── Component-wise arithmetic on `PixelX` ──

#guard (⟨(3 : Int)⟩ : PixelX Int) + ⟨4⟩ == ⟨7⟩
#guard (⟨(7 : Int)⟩ : PixelX Int) - ⟨4⟩ == ⟨3⟩
#guard (⟨(3 : Int)⟩ : PixelX Int) * ⟨4⟩ == ⟨12⟩
#guard (⟨(12 : Int)⟩ : PixelX Int) / ⟨4⟩ == ⟨3⟩
#guard -(⟨(3 : Int)⟩ : PixelX Int) == ⟨-3⟩
#guard (0 : PixelX Int) == ⟨0⟩
#guard (1 : PixelX Int) == ⟨1⟩

-- ── `toPixelsX`/`fromPixelsX` — pixel-level channel separation ──

#guard toPixelsX (cs := Y) (e := Int) (⟨(5 : Int)⟩ : PixelY Int) == [(⟨5⟩ : PixelX Int)]
#guard (fromPixelsX (cs := Y) (e := Int) [(Y.luma, (⟨5⟩ : PixelX Int))]) == (⟨5⟩ : PixelY Int)

-- ── `toImagesX`/`fromImagesX`/`squashWith`/`squashWith2` — image-level ──

/-- A small `2×2` `Y`-colour-space test image: `[[0,1],[2,3]]`. -/
def csXTestImg : Graphics.Image.Interface.Image Y Int :=
  makeImage (2, 2) (fun (i, j) => (⟨i * 2 + j⟩ : PixelY Int))

#guard dims csXTestImg == (2, 2)

-- `toImagesX` on a single-channel colour space yields exactly one `X` image,
-- pixel-for-pixel identical (modulo the `PixelY` → `PixelX` wrapper change).
#guard (toImagesX (cs := Y) (e := Int) csXTestImg).length == 1
#guard unsafeIndex
  ((toImagesX (cs := Y) (e := Int) csXTestImg).getD 0 (makeImage (0, 0) (fun _ => default)))
  (1, 1) == (⟨3⟩ : PixelX Int)

-- `fromImagesX` inverts `toImagesX` for a single-channel colour space.
#guard
  (fromImagesX (cs := Y) (e := Int) [(Y.luma, (toImagesX (cs := Y) (e := Int) csXTestImg).getD 0
    (makeImage (0, 0) (fun _ => default)))] : Graphics.Image.Interface.Image Y Int)
  == csXTestImg

-- `squashWith` folds each pixel's single channel into an `X` pixel.
#guard unsafeIndex
  (squashWith (cs := Y) (e := Int) (b := Int) (· + ·) 100 csXTestImg) (1, 0) == (⟨102⟩ : PixelX Int)

-- `squashWith2` folds two images' pixels together into an `X` pixel.
#guard unsafeIndex
  (squashWith2 (cs := Y) (e := Int) (b := Int) (fun acc v1 v2 => acc + v1 + v2) 0
    csXTestImg csXTestImg) (0, 1) == (⟨2⟩ : PixelX Int)
