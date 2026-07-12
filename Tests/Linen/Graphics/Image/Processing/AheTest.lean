/-
  Tests for `Linen.Graphics.Image.Processing.Ahe` — the local-rank
  ("adaptive histogram equalization") transform.

  Fixture names are prefixed `ahe` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.Processing.Ahe
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (fromLists unsafeIndex)
open Graphics.Image.Processing.Ahe (ahe)
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- ── Constant image: the Laplacian preprocessing pass is `0` everywhere ──

-- A constant 3×3 image (every pixel `7`). The Laplacian kernel's weights sum
-- to `0`, and every neighbour (including every `Edge`-clamped border
-- neighbour, which is itself always some copy of `7`) equals the centre
-- pixel, so the preprocessed image `ip` is `0` at every pixel; no pixel is
-- ever strictly greater than another (`0 > 0` is false everywhere), so every
-- rank — and hence every output pixel — is `0`.
def aheConst7 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨7⟩, ⟨7⟩, ⟨7⟩], [⟨7⟩, ⟨7⟩, ⟨7⟩], [⟨7⟩, ⟨7⟩, ⟨7⟩]]

def aheZero3 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY UInt16) :=
  fromLists [[⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩]]

#guard ahe aheConst7 3 3 0 == aheZero3

-- ── A 1×3 row, fully hand-computed through both the Laplacian pass and the ──
-- ── rank count (the `±5` neighbourhood always covers every one of the ──
-- ── image's `≤ 11` pixels along each axis, so the rank is a plain count ──
-- ── over the whole preprocessed row) ──

-- Row `[1, 2, 5]`. Hand-deriving the `Edge`-bordered Laplacian correlation
-- (kernel `[[0,-1,0],[-1,4,-1],[0,-1,0]]`, a single row so every vertical
-- neighbour clamps back to the same row) gives the preprocessed row
-- `ip = [-1, -2, 3]`:
--   `ip(0,0) = -img(0,0) - img(0,0) + 4*img(0,0) - img(0,1) - img(0,0) = -1`
--   `ip(0,1) = -img(0,1) - img(0,0) + 4*img(0,1) - img(0,2) - img(0,1) = -2`
--   `ip(0,2) = -img(0,2) - img(0,1) + 4*img(0,2) - img(0,2) - img(0,2) = 3`
-- The rank at each column is then the count of the other two `ip` entries
-- strictly less than its own: rank(0) = 1 (`-1 > -2`), rank(1) = 0
-- (`-2` is less than both others), rank(2) = 2 (`3` exceeds both `-1, -2`).
-- Scaling each rank by `255` gives the expected output row `[255, 0, 510]`.
def aheRow : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨1⟩, ⟨2⟩, ⟨5⟩]]

def aheRowResult : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY UInt16) :=
  fromLists [[⟨255⟩, ⟨0⟩, ⟨510⟩]]

#guard ahe aheRow 1 3 0 == aheRowResult

-- Cross-check individual pixels via `unsafeIndex`, independent of `==` on
-- the whole `Manifest`.
#guard unsafeIndex (ahe aheRow 1 3 0) (0, 0) == (⟨255⟩ : PixelY UInt16)
#guard unsafeIndex (ahe aheRow 1 3 0) (0, 1) == (⟨0⟩ : PixelY UInt16)
#guard unsafeIndex (ahe aheRow 1 3 0) (0, 2) == (⟨510⟩ : PixelY UInt16)

-- The dead `_neighborhoodFactor` argument does not affect the result.
#guard ahe aheRow 1 3 0 == ahe aheRow 1 3 999
