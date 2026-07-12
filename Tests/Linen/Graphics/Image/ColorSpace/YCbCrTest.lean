/-
  Tests for `Linen.Graphics.Image.ColorSpace.YCbCr` — the `YCbCr`/`YCbCrA`
  colour spaces, their `Pixel`/`ColorSpace`/`AlphaSpace` instances, and
  `PixelYCbCr`'s component-wise arithmetic instances.

  There is no `RGB ↔ YCbCr` conversion to test here: as documented in
  `Linen/Graphics/Image/ColorSpace/YCbCr.lean`'s own doc-comment, upstream's
  actual conversion (`ToRGB YCbCr`/`ToYCbCr RGB`) is not defined in
  `Graphics/Image/ColorSpace/YCbCr.hs` at all — it lives in
  `Graphics/Image/ColorSpace.hs` (module #12 in the plan), as one instance
  among a full conversion matrix spanning every colour space. That
  conversion — and its tests — is deferred to module #12's own port and test
  file, once every colour space it converts between exists.

  Fixture/example names are prefixed `csYCbCr`/`csYCbCrA` to avoid clashing
  with any other test file's identifiers in the shared `Tests` namespace (in
  particular `Tests.Linen.Graphics.Image.ColorSpace.YTest`/`RGBTest`/
  `HSITest`/`CMYKTest`, whose `ColorSpace` operations share the same imported
  names).
-/
import Linen.Graphics.Image.ColorSpace.YCbCr

open Graphics.Image.Interface (Pixel ColorSpace AlphaSpace channels toComponents fromComponents
  promote getPxC setPxC mapPxC liftPx liftPx2 foldlPx2 getAlpha addAlpha dropAlpha)
open Graphics.Image.ColorSpace.YCbCr

-- ── `YCbCr`/`PixelYCbCr` — `ColorSpace` operations ──

def csYCbCrPx : PixelYCbCr Int := ⟨3, 4, 5⟩

#guard (channels (cs := YCbCr) (e := Int)) == [YCbCr.luma, YCbCr.cb, YCbCr.cr]
#guard (toComponents (cs := YCbCr) (e := Int) csYCbCrPx) == (3, 4, 5)
#guard (fromComponents (cs := YCbCr) (e := Int) ((3, 4, 5) : Int × Int × Int)) == csYCbCrPx
#guard (promote (cs := YCbCr) (7 : Int)) == (⟨7, 7, 7⟩ : PixelYCbCr Int)
#guard getPxC (cs := YCbCr) (e := Int) csYCbCrPx YCbCr.luma == 3
#guard getPxC (cs := YCbCr) (e := Int) csYCbCrPx YCbCr.cb == 4
#guard getPxC (cs := YCbCr) (e := Int) csYCbCrPx YCbCr.cr == 5
#guard (setPxC (cs := YCbCr) (e := Int) csYCbCrPx YCbCr.luma 9) == ⟨9, 4, 5⟩
#guard (setPxC (cs := YCbCr) (e := Int) csYCbCrPx YCbCr.cb 9) == ⟨3, 9, 5⟩
#guard (setPxC (cs := YCbCr) (e := Int) csYCbCrPx YCbCr.cr 9) == ⟨3, 4, 9⟩
#guard (mapPxC (cs := YCbCr) (e := Int) (fun _ v => v + 1) csYCbCrPx) == ⟨4, 5, 6⟩
#guard (liftPx (cs := YCbCr) (e := Int) (· + 1) csYCbCrPx) == ⟨4, 5, 6⟩
#guard (liftPx2 (cs := YCbCr) (e := Int) (· + ·) csYCbCrPx ⟨1, 1, 1⟩) == ⟨4, 5, 6⟩
#guard (foldlPx2 (cs := YCbCr) (e := Int) (β := Int) (· + · + ·) 0 csYCbCrPx ⟨1, 1, 1⟩) == 15

-- ── `YCbCrA`/`PixelYCbCrA` — `ColorSpace` operations ──

def csYCbCrAPx : PixelYCbCrA Int := ⟨3, 4, 5, 6⟩

#guard (channels (cs := YCbCrA) (e := Int)) ==
  [YCbCrA.luma, YCbCrA.cb, YCbCrA.cr, YCbCrA.alpha]
#guard (toComponents (cs := YCbCrA) (e := Int) csYCbCrAPx) == (3, 4, 5, 6)
#guard (fromComponents (cs := YCbCrA) (e := Int)
  ((3, 4, 5, 6) : Int × Int × Int × Int)) == csYCbCrAPx
#guard (promote (cs := YCbCrA) (7 : Int)) == (⟨7, 7, 7, 7⟩ : PixelYCbCrA Int)
#guard getPxC (cs := YCbCrA) (e := Int) csYCbCrAPx YCbCrA.luma == 3
#guard getPxC (cs := YCbCrA) (e := Int) csYCbCrAPx YCbCrA.cb == 4
#guard getPxC (cs := YCbCrA) (e := Int) csYCbCrAPx YCbCrA.cr == 5
#guard getPxC (cs := YCbCrA) (e := Int) csYCbCrAPx YCbCrA.alpha == 6
#guard (setPxC (cs := YCbCrA) (e := Int) csYCbCrAPx YCbCrA.luma 9) == ⟨9, 4, 5, 6⟩
#guard (setPxC (cs := YCbCrA) (e := Int) csYCbCrAPx YCbCrA.alpha 9) == ⟨3, 4, 5, 9⟩
#guard (mapPxC (cs := YCbCrA) (e := Int) (fun _ v => v + 1) csYCbCrAPx) == ⟨4, 5, 6, 7⟩
#guard (liftPx (cs := YCbCrA) (e := Int) (· + 1) csYCbCrAPx) == ⟨4, 5, 6, 7⟩
#guard (liftPx2 (cs := YCbCrA) (e := Int) (· + ·) csYCbCrAPx ⟨1, 1, 1, 1⟩) == ⟨4, 5, 6, 7⟩
#guard (foldlPx2 (cs := YCbCrA) (e := Int) (β := Int) (· + · + ·) 0
  csYCbCrAPx ⟨1, 1, 1, 1⟩) == 22

-- ── `AlphaSpace` between `YCbCrA` and `YCbCr` ──

#guard getAlpha (cs := YCbCrA) (e := Int) csYCbCrAPx == 6
#guard addAlpha (cs := YCbCrA) (e := Int) (6 : Int) csYCbCrPx == csYCbCrAPx
#guard dropAlpha (cs := YCbCrA) (e := Int) csYCbCrAPx == csYCbCrPx

-- ── Component-wise arithmetic on `PixelYCbCr` ──

#guard csYCbCrPx + (⟨1, 1, 1⟩ : PixelYCbCr Int) == ⟨4, 5, 6⟩
#guard csYCbCrPx - (⟨1, 1, 1⟩ : PixelYCbCr Int) == ⟨2, 3, 4⟩
#guard csYCbCrPx * (⟨2, 2, 2⟩ : PixelYCbCr Int) == ⟨6, 8, 10⟩
#guard (⟨12, 12, 12⟩ : PixelYCbCr Int) / (⟨4, 3, 2⟩ : PixelYCbCr Int) == ⟨3, 4, 6⟩
#guard -csYCbCrPx == ⟨-3, -4, -5⟩
#guard (0 : PixelYCbCr Int) == ⟨0, 0, 0⟩
#guard (1 : PixelYCbCr Int) == ⟨1, 1, 1⟩
