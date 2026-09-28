/-
  Tests for `Linen.Graphics.Image.Processing.Interpolation` — the
  `Interpolation` class and its `Nearest`/`Bilinear`/`Bicubic` instances.

  Fixture names are prefixed `imgInterp` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.Processing.Interpolation
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (Border)
open Graphics.Image.Processing.Interpolation
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- ── Fixture: a single-channel `Float` image sampled from a linear function ──

-- `imgInterpGetPx (i, j) = i + 10*j`, defined for every *non-negative*
-- integer `(i, j)` (no actual array backing needed, since `interpolate` only
-- ever needs a lookup function). Every test below only ever calls this with
-- non-negative indices: any negative index is intercepted first by a
-- `Border` strategy (`edge`/`fill`), which never forwards an out-of-range
-- index to the lookup function. Because this function is linear in
-- `(i, j)`, both bilinear and bicubic interpolation are expected to
-- reproduce it exactly (up to floating-point rounding) at any fractional
-- coordinate.
def imgInterpGetPx (ij : Int × Int) : PixelY Float :=
  ⟨ij.1.toNat.toFloat + 10 * ij.2.toNat.toFloat⟩

-- `imgInterpGetPx`'s conceptual image dimensions, large enough that no test
-- coordinate below is anywhere near the south/east border.
def imgInterpDims : Int × Int := (1000, 1000)

-- Absolute difference between two `PixelY Float`s' single channel, for
-- tolerance-based `#guard`s on results that go through non-dyadic
-- (non-exactly-representable) fractional arithmetic.
def imgInterpDiff (p q : PixelY Float) : Float :=
  (p.y - q.y).abs

-- ── `Nearest` ──

-- Sampling exactly at an integer coordinate returns that pixel exactly.
#guard interpolate (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float)) imgInterpDims
  imgInterpGetPx (3.0, 4.0) == imgInterpGetPx (3, 4)

-- Sampling near — but not exactly at — an integer coordinate rounds to the
-- nearest one.
#guard interpolate (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float)) imgInterpDims
  imgInterpGetPx (3.4, 4.4) == imgInterpGetPx (3, 4)
#guard interpolate (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float)) imgInterpDims
  imgInterpGetPx (3.6, 4.6) == imgInterpGetPx (4, 5)

-- `Nearest` honours the supplied border-handling strategy for an
-- out-of-bounds rounded index: rounding `(-0.6, 1.0)` gives `(-1, 1)`, out of
-- bounds to the north, so `Border.edge` clamps the row to `0`.
#guard interpolate (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float)) (4, 4)
  imgInterpGetPx (-0.6, 1.0) == imgInterpGetPx (0, 1)

-- ── `Bilinear` ──

-- Bilinear interpolation of a linear function is exact. `0.25`/`0.75` are
-- exactly representable in binary floating point, so this is checked with
-- plain equality.
#guard interpolate (cs := Y) (e := Float) Bilinear.bilinear (Border.edge : Border (PixelY Float)) imgInterpDims
  imgInterpGetPx (2.25, 5.75) == PixelY.mk (2.25 + 10 * 5.75)

-- `Bilinear` honours the supplied border-handling strategy: sampling just
-- north of the image's northern edge with `Border.fill` blends the fixed
-- fill pixel into the result. Checked with a tolerance since `0.7`'s
-- arithmetic below is not exactly representable in binary floating point.
#guard imgInterpDiff
    (interpolate (cs := Y) (e := Float) Bilinear.bilinear (Border.fill (PixelY.mk 999.0)) (4, 4) imgInterpGetPx (-0.3, 0.0))
    (PixelY.mk 299.7) < 1e-9

-- ── `Bicubic` ──

-- Bicubic interpolation (Keys' cubic-convolution kernel is both a partition
-- of unity and exactly reproduces linear functions) of a linear function is
-- exact up to floating-point rounding; checked with a tolerance since the
-- kernel's arithmetic goes through non-dyadic fractions.
#guard imgInterpDiff
    (interpolate (cs := Y) (e := Float) (Bicubic.mk (-0.5)) (Border.edge : Border (PixelY Float)) imgInterpDims
      imgInterpGetPx (500.25, 500.75))
    (PixelY.mk (500.25 + 10 * 500.75)) < 1e-6
