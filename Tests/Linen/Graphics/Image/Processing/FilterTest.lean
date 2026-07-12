/-
  Tests for `Linen.Graphics.Image.Processing.Filter` — named filter kernels
  built on convolution/correlation.

  Fixture names are prefixed `filt` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace. Every fixture is a
  `PixelY Float` image, matching the pattern
  `Tests.Linen.Graphics.Image.Processing.ConvolutionTest` already establishes
  for the same `ColorSpace`-needing operations.
-/
import Linen.Graphics.Image.Processing.Filter
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (Border unsafeIndex fromLists)
open Graphics.Image.Processing.Filter
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- ── Fixtures ──

-- A constant 3×3 image of value `5`.
def filtConst5 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨5⟩, ⟨5⟩, ⟨5⟩], [⟨5⟩, ⟨5⟩, ⟨5⟩], [⟨5⟩, ⟨5⟩, ⟨5⟩]]

-- A constant 3×3 image of value `0`, the expected result of every
-- zero-sum-kernel filter (Sobel/Prewitt/Laplacian/LoG) applied to `filtConst5`.
def filtZero3 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩]]

-- A constant 5×5 image of value `1`, sized to match the 5×5 kernel filters.
def filtConst1_5 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists (List.replicate 5 (List.replicate 5 (⟨1⟩ : PixelY Float)))

-- A 3×5 step image: every row is `[0, 0, 5, 10, 10]`, a monotone rising step
-- crossing column 2, matching `ConvolutionTest`'s own `convStep` fixture.
def filtStep : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨0⟩, ⟨0⟩, ⟨5⟩, ⟨10⟩, ⟨10⟩],
             [⟨0⟩, ⟨0⟩, ⟨5⟩, ⟨10⟩, ⟨10⟩],
             [⟨0⟩, ⟨0⟩, ⟨5⟩, ⟨10⟩, ⟨10⟩]]

-- A trivial 1×1 image, for the "kernel larger than the image" edge case.
def filt1x1 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨7⟩]]

-- ── `meanFilter`: constant image is unchanged (average of 9 equal values) ──

#guard (meanFilter (cs := Y) (e := Float) Border.edge).applyFilter filtConst5 == filtConst5

-- `meanFilter`'s 3×3 kernel is larger than this 1×1 image; `Edge` border
-- resolution keeps every out-of-bounds read equal to the single pixel, so the
-- average of 9 copies of the same value is that value, unchanged.
#guard (meanFilter (cs := Y) (e := Float) Border.edge).applyFilter filt1x1 == filt1x1

-- ── `laplacianFilter`/`logFilter`: zero-sum kernels vanish on a constant image ──

#guard (laplacianFilter (cs := Y) (e := Float) Border.edge).applyFilter filtConst5 == filtZero3
#guard (logFilter (cs := Y) (e := Float) Border.edge).applyFilter filtConst5 == filtZero3

-- ── `gaussianSmoothingFilter`/`unsharpMaskingFilter`: rescaled kernels sum to `1` ──

-- The 5×5 kernel's entries sum to exactly `273`, its own rescaling factor, so
-- a constant image of `1` is unchanged.
#guard (gaussianSmoothingFilter (cs := Y) (e := Float) Border.edge).applyFilter filtConst1_5
  == filtConst1_5

-- The 5×5 kernel's entries sum to exactly `256`, its own rescaling factor, so
-- a constant image is unchanged.
#guard (unsharpMaskingFilter (cs := Y) (e := Float) Border.edge).applyFilter filtConst5
  == filtConst5

-- ── `sobelFilter`/`sobelOperator`: zero on a constant image, hand-computed on a step ──

#guard (sobelFilter (cs := Y) (e := Float) .horizontal Border.edge).applyFilter filtConst5
  == filtZero3
#guard (sobelFilter (cs := Y) (e := Float) .vertical Border.edge).applyFilter filtConst5
  == filtZero3

-- Horizontal Sobel at `(1, 2)`: neighbourhood columns `1, 2, 3` hold `0, 5,
-- 10` in every row, weighted by kernel rows `[-1,0,1]`, `[-2,0,2]`, `[-1,0,1]`:
-- `(10) + (20) + (10) = 40`.
#guard unsafeIndex
    ((sobelFilter (cs := Y) (e := Float) .horizontal Border.edge).applyFilter filtStep) (1, 2)
  == (⟨40⟩ : PixelY Float)

-- Vertical Sobel is zero everywhere on `filtStep`, since every row is identical.
#guard unsafeIndex
    ((sobelFilter (cs := Y) (e := Float) .vertical Border.edge).applyFilter filtStep) (1, 2)
  == (⟨0⟩ : PixelY Float)

-- `sobelOperator`'s gradient magnitude at `(1, 2)` is `sqrt(40² + 0²) = 40`.
#guard unsafeIndex (sobelOperator (cs := Y) filtStep) (1, 2) == (⟨40⟩ : PixelY Float)

-- ── `prewittFilter`/`prewittOperator`: hand-computed on the same step ──

-- Horizontal Prewitt at `(1, 2)`: row-kernel `[1,0,-1]` gives
-- `img[1] - img[3] = 0 - 10 = -10` per row, then column-kernel `[1,1,1]`
-- sums 3 identical rows: `-10 * 3 = -30`.
#guard unsafeIndex
    ((prewittFilter (cs := Y) (e := Float) .horizontal Border.edge).applyFilter filtStep) (1, 2)
  == (⟨-30⟩ : PixelY Float)

-- Vertical Prewitt is zero everywhere on `filtStep`, since every row is identical.
#guard unsafeIndex
    ((prewittFilter (cs := Y) (e := Float) .vertical Border.edge).applyFilter filtStep) (1, 2)
  == (⟨0⟩ : PixelY Float)

-- `prewittOperator`'s gradient magnitude at `(1, 2)` is `sqrt(30² + 0²) = 30`.
#guard unsafeIndex (prewittOperator (cs := Y) filtStep) (1, 2) == (⟨30⟩ : PixelY Float)

-- ── `gaussianLowPass`/`gaussianBlur`: trivial radius-0 kernel is the identity ──

-- With radius `0`, the kernel is a single `1`-valued pixel (normalised to
-- itself), so correlating with it (row-wise, then column-wise) leaves the
-- image unchanged.
#guard (gaussianLowPass (cs := Y) 0 1.0 Border.edge).applyFilter filtConst5 == filtConst5
