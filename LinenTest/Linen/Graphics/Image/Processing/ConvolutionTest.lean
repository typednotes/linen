/-
  Tests for `Linen.Graphics.Image.Processing.Convolution` — kernel
  correlation/convolution of an image.

  Fixture names are prefixed `conv` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace. Every fixture is a
  `PixelY Float` image, since `correlate`/`convolve` need a `ColorSpace`
  instance (for `promote`/`liftPx`), exactly the pattern
  `Tests.Linen.Graphics.Image.Processing.GeometricTest` already establishes
  for its own `ColorSpace`-needing operations.
-/
import Linen.Graphics.Image.Processing.Convolution
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (Border dims unsafeIndex fromLists)
open Graphics.Image.Processing.Convolution
open Graphics.Image.ColorSpace.X (PixelX)
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- ── Fixture: a 3×3 image, row `i` holding `[3*i+1, 3*i+2, 3*i+3]` ──

-- Rows: `[1, 2, 3]`, `[4, 5, 6]`, `[7, 8, 9]` — the classic 1..9 magic-square
-- layout, chosen so every hand-computed sum below is easy to check.
def convImg : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨1⟩, ⟨2⟩, ⟨3⟩], [⟨4⟩, ⟨5⟩, ⟨6⟩], [⟨7⟩, ⟨8⟩, ⟨9⟩]]

-- ── Identity/delta kernel — `correlate`/`convolve` return the source image ──

-- A 3×3 kernel with a single `1` at its centre and `0` everywhere else: every
-- non-centre term is multiplied by `0`, so the result is the source image
-- unchanged, at every pixel (including the border, since the border pixel's
-- value is always multiplied by a `0` weight and so never contributes).
def convDelta3 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Float) :=
  fromLists [[⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨1⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩]]

#guard correlate (cs := Y) (e := Float) Border.edge convDelta3 convImg == convImg
#guard convolve (cs := Y) (e := Float) Border.edge convDelta3 convImg == convImg

-- ── Box-sum kernel — hand-computable interior result ──

-- A 3×3 all-`1`s kernel is symmetric under 180° rotation, so `convolve` and
-- `correlate` agree on it. On the 3×3 fixture above, the only interior pixel
-- (the kernel's own size equals the image's, so only the very centre needs
-- no border pixels) is `(1, 1)`, whose correlation sum is the sum of every
-- pixel of `convImg`: `1+2+3+4+5+6+7+8+9 = 45`.
def convBox3 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Float) :=
  fromLists [[⟨1⟩, ⟨1⟩, ⟨1⟩], [⟨1⟩, ⟨1⟩, ⟨1⟩], [⟨1⟩, ⟨1⟩, ⟨1⟩]]

#guard unsafeIndex (correlate (cs := Y) (e := Float) Border.edge convBox3 convImg) (1, 1)
  == (⟨45⟩ : PixelY Float)
#guard unsafeIndex (convolve (cs := Y) (e := Float) Border.edge convBox3 convImg) (1, 1)
  == (⟨45⟩ : PixelY Float)

-- ── Sobel-style row-gradient kernel on a step pattern ──

-- A 3×5 step image: every row is `[0, 0, 5, 10, 10]`, a monotone rising step
-- crossing column 2. A `[-1, 0, 1]` row kernel (`kN2 = 1`) correlated at
-- column `j` computes `img[j+1] - img[j-1]`: a right-minus-left gradient.
def convStep : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨0⟩, ⟨0⟩, ⟨5⟩, ⟨10⟩, ⟨10⟩],
             [⟨0⟩, ⟨0⟩, ⟨5⟩, ⟨10⟩, ⟨10⟩],
             [⟨0⟩, ⟨0⟩, ⟨5⟩, ⟨10⟩, ⟨10⟩]]

def convGradKernel : List (PixelX Float) := [⟨-1⟩, ⟨0⟩, ⟨1⟩]

-- `convolveRows` pre-reverses the list once (upstream's `fromLists . (:[])
-- . reverse`) and `convolve` itself reverses the kernel a second time (via
-- `rotate180`), so the two reversals cancel: `convolveRows` convolves using
-- the kernel exactly as given, agreeing with correlating directly against
-- the same `[-1, 0, 1]` row kernel (below) — a positive gradient across the
-- rising step.
#guard unsafeIndex
    (convolveRows (cs := Y) (e := Float) Border.edge convGradKernel convStep) (1, 1)
  == (⟨5⟩ : PixelY Float)
#guard unsafeIndex
    (convolveRows (cs := Y) (e := Float) Border.edge convGradKernel convStep) (1, 2)
  == (⟨10⟩ : PixelY Float)
#guard unsafeIndex
    (convolveRows (cs := Y) (e := Float) Border.edge convGradKernel convStep) (1, 3)
  == (⟨5⟩ : PixelY Float)

-- Correlating directly (no extra rotation) gives the same, positive
-- gradient across the rising step.
def convGradKernelRow : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Float) :=
  fromLists [[⟨-1⟩, ⟨0⟩, ⟨1⟩]]

#guard unsafeIndex
    (correlate (cs := Y) (e := Float) Border.edge convGradKernelRow convStep) (1, 1)
  == (⟨5⟩ : PixelY Float)
#guard unsafeIndex
    (correlate (cs := Y) (e := Float) Border.edge convGradKernelRow convStep) (1, 2)
  == (⟨10⟩ : PixelY Float)
#guard unsafeIndex
    (correlate (cs := Y) (e := Float) Border.edge convGradKernelRow convStep) (1, 3)
  == (⟨5⟩ : PixelY Float)

-- ── `convolveCols`: the column-kernel counterpart ──

-- A 5×3 step image (each row constant, rising down the rows), correlated
-- with the same `[-1, 0, 1]` kernel along columns via `convolveCols`, agrees
-- with the row case by symmetry (same cancelling double-reversal as
-- `convolveRows`, see above).
def convStepT : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨0⟩, ⟨0⟩, ⟨0⟩],
             [⟨0⟩, ⟨0⟩, ⟨0⟩],
             [⟨5⟩, ⟨5⟩, ⟨5⟩],
             [⟨10⟩, ⟨10⟩, ⟨10⟩],
             [⟨10⟩, ⟨10⟩, ⟨10⟩]]

#guard unsafeIndex
    (convolveCols (cs := Y) (e := Float) Border.edge convGradKernel convStepT) (1, 1)
  == (⟨5⟩ : PixelY Float)
#guard unsafeIndex
    (convolveCols (cs := Y) (e := Float) Border.edge convGradKernel convStepT) (2, 1)
  == (⟨10⟩ : PixelY Float)
#guard unsafeIndex
    (convolveCols (cs := Y) (e := Float) Border.edge convGradKernel convStepT) (3, 1)
  == (⟨5⟩ : PixelY Float)
