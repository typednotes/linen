/-
  Tests for `Linen.Graphics.Image.Processing.Hough` — the linear Hough
  transform.

  Fixture names are prefixed `hough` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.Processing.Hough
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (fromLists unsafeIndex dims)
open Graphics.Image.Processing.Hough (hough)
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- ── An all-off (uniform) image: every gradient is `0`, so no vote is ever ──
-- ── cast, `accBin` stays all-zero, and every output pixel is `255` (pure ──
-- ── white — "no line evidence anywhere"), per the module doc-comment's ──
-- ── `maxAcc = 0` reading. ──

def houghAllOff3 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩]]

def houghAllOffResult := hough houghAllOff3 4 4

#guard dims houghAllOffResult == (4, 4)
#guard unsafeIndex houghAllOffResult (0, 0) == (⟨255⟩ : PixelY UInt8)
#guard unsafeIndex houghAllOffResult (3, 3) == (⟨255⟩ : PixelY UInt8)
#guard unsafeIndex houghAllOffResult (2, 1) == (⟨255⟩ : PixelY UInt8)

-- A uniform (constant, nonzero) image behaves identically: every
-- forward-difference gradient is still `0`.
def houghConst7x3 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨7⟩, ⟨7⟩, ⟨7⟩], [⟨7⟩, ⟨7⟩, ⟨7⟩], [⟨7⟩, ⟨7⟩, ⟨7⟩]]

#guard hough houghConst7x3 4 4 == houghAllOffResult

-- ── A single straight edge (a bright column against a dark background): ──
-- ── every pixel along the edge casts a vote, so at least one accumulator ──
-- ── cell receives more votes than the all-off baseline, and the darkest ──
-- ── output cell is strictly less than `255`. ──

-- A 4×4 image split by a vertical edge at column 2: columns `0`-`1` are `0`,
-- columns `2`-`3` are `1`. Every row's forward-difference gradient at
-- `(x, 1)` is nonzero (`orig = 0`, `y' = image (x, 2) = 1`), so every one of
-- those `4` pixels casts `thetaSz + 1` votes, all landing somewhere in the
-- `(thetaSz+1) × (distSz+1)` accumulator: `accBin`'s maximum is therefore
-- strictly positive, and the corresponding output pixel is strictly less
-- than `255` (`255 - round(1.0 × 255) = 0`, the brightest possible vote
-- peak).
def houghEdge4 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists
    [[⟨0⟩, ⟨0⟩, ⟨1⟩, ⟨1⟩],
     [⟨0⟩, ⟨0⟩, ⟨1⟩, ⟨1⟩],
     [⟨0⟩, ⟨0⟩, ⟨1⟩, ⟨1⟩],
     [⟨0⟩, ⟨0⟩, ⟨1⟩, ⟨1⟩]]

def houghEdgeResult := hough houghEdge4 8 8

#guard dims houghEdgeResult == (8, 8)
-- The maximum-vote cell reaches full brightness inversion (`0`, the
-- darkest possible pixel), witnessing that at least one vote was cast.
#guard houghEdgeResult.elems.any (fun p => p.y == 0)
-- ... and it isn't the uniform "no votes" image.
#guard !(houghEdgeResult == hough houghAllOff3 8 8)
