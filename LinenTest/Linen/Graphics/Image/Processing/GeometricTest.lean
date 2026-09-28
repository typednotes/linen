/-
  Tests for `Linen.Graphics.Image.Processing.Geometric` — sampling,
  concatenation, canvas, flipping, rotation, and scaling operations on
  images.

  Fixture names are prefixed `geo` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace. The plain-`Int`-pixel
  operations (everything not needing a `ColorSpace`) are exercised against a
  `Manifest`-backed image of `Int`s, exactly the pattern
  `Tests.Linen.Graphics.Image.InterfaceTest` already establishes; the
  operations that do need a `ColorSpace` (`upsample`/`rotate`/`resize`/
  `scale`, via `promote`/`Interpolation.interpolate`) are exercised against a
  `PixelY Float` image instead.
-/
import Linen.Graphics.Image.Processing.Geometric
import Linen.Graphics.Image.Processing.Interpolation
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (Border dims unsafeIndex makeImage fromLists)
open Graphics.Image.Processing.Geometric
open Graphics.Image.Processing.Interpolation
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- ── Fixture: a 2×3 image of plain `Int` "pixels" ──

-- Row `i`, column `j` holds `10*i + j`: row 0 is `[0, 1, 2]`, row 1 is
-- `[10, 11, 12]`.
def geoImg : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 Int :=
  makeImage (2, 3) (fun (i, j) => 10 * i + j)

-- ── Sampling ──

#guard dims (downsample (fun i => i == 1) (fun _ => false) geoImg) == (1, 3)
#guard unsafeIndex (downsample (fun i => i == 1) (fun _ => false) geoImg) (0, 1) == 1

-- `downsampleRows` drops the (only) odd row, row 1.
#guard dims (downsampleRows geoImg) == (1, 3)
#guard unsafeIndex (downsampleRows geoImg) (0, 1) == 1

-- `downsampleCols` drops the odd column, column 1 — keeping columns 0 and 2.
#guard dims (downsampleCols geoImg) == (2, 2)
#guard unsafeIndex (downsampleCols geoImg) (1, 1) == 12

-- ── Fixture: a 2×2 `PixelY Float` image, for the `ColorSpace`-needing ops ──

def geoYImg : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨0⟩, ⟨1⟩], [⟨2⟩, ⟨3⟩]]

-- `upsampleRows` inserts a zero-valued row after each row.
#guard dims (upsampleRows (cs := Y) (e := Float) geoYImg) == (4, 2)
#guard unsafeIndex (upsampleRows (cs := Y) (e := Float) geoYImg) (0, 0) == (⟨0⟩ : PixelY Float)
#guard unsafeIndex (upsampleRows (cs := Y) (e := Float) geoYImg) (1, 0) == (⟨0⟩ : PixelY Float)
#guard unsafeIndex (upsampleRows (cs := Y) (e := Float) geoYImg) (2, 1) == (⟨3⟩ : PixelY Float)

-- `upsampleCols` inserts a zero-valued column after each column.
#guard dims (upsampleCols (cs := Y) (e := Float) geoYImg) == (2, 4)
#guard unsafeIndex (upsampleCols (cs := Y) (e := Float) geoYImg) (0, 1) == (⟨0⟩ : PixelY Float)
#guard unsafeIndex (upsampleCols (cs := Y) (e := Float) geoYImg) (1, 2) == (⟨3⟩ : PixelY Float)

-- ── Concatenation ──

#guard dims (leftToRight geoImg geoImg) == (2, 6)
#guard unsafeIndex (leftToRight geoImg geoImg) (1, 4) == unsafeIndex geoImg (1, 1)

#guard dims (topToBottom geoImg geoImg) == (4, 3)
#guard unsafeIndex (topToBottom geoImg geoImg) (3, 2) == unsafeIndex geoImg (1, 2)

-- ── Canvas ──

-- Shifting by `(1, 1)` moves every pixel one row/column towards the
-- bottom-right; the vacated north/west border is filled with `-1`.
#guard dims (translate (Border.fill (-1)) (1, 1) geoImg) == (2, 3)
#guard unsafeIndex (translate (Border.fill (-1)) (1, 1) geoImg) (0, 0) == -1
#guard unsafeIndex (translate (Border.fill (-1)) (1, 1) geoImg) (1, 1) == unsafeIndex geoImg (0, 0)
#guard unsafeIndex (translate (Border.fill (-1)) (1, 1) geoImg) (1, 2) == unsafeIndex geoImg (0, 1)

-- Growing the canvas to `(3, 4)` keeps the original pixels in place and
-- fills the newly out-of-bounds area with `-1`.
#guard dims (canvasSize (Border.fill (-1)) (3, 4) geoImg) == (3, 4)
#guard unsafeIndex (canvasSize (Border.fill (-1)) (3, 4) geoImg) (0, 0) == unsafeIndex geoImg (0, 0)
#guard unsafeIndex (canvasSize (Border.fill (-1)) (3, 4) geoImg) (2, 0) == -1
#guard unsafeIndex (canvasSize (Border.fill (-1)) (3, 4) geoImg) (0, 3) == -1

-- Cropping the 2×2 window starting at `(0, 1)` extracts columns 1 and 2 of
-- both rows.
#guard dims (crop (0, 1) (2, 2) geoImg) == (2, 2)
#guard unsafeIndex (crop (0, 1) (2, 2) geoImg) (0, 0) == unsafeIndex geoImg (0, 1)
#guard unsafeIndex (crop (0, 1) (2, 2) geoImg) (1, 1) == unsafeIndex geoImg (1, 2)

-- `superimpose`s a 1×2 image of `99`s over `geoImg`, starting at `(0, 1)`:
-- covers row 0, columns 1 and 2.
def geoOverlay : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 Int :=
  makeImage (1, 2) (fun _ => 99)

#guard dims (superimpose (0, 1) geoOverlay geoImg) == dims geoImg
#guard unsafeIndex (superimpose (0, 1) geoOverlay geoImg) (0, 1) == 99
#guard unsafeIndex (superimpose (0, 1) geoOverlay geoImg) (0, 2) == 99
#guard unsafeIndex (superimpose (0, 1) geoOverlay geoImg) (0, 0) == unsafeIndex geoImg (0, 0)
#guard unsafeIndex (superimpose (0, 1) geoOverlay geoImg) (1, 1) == unsafeIndex geoImg (1, 1)

-- ── Flipping ──

#guard dims (flipV geoImg) == dims geoImg
#guard unsafeIndex (flipV geoImg) (0, 0) == unsafeIndex geoImg (1, 0)
#guard unsafeIndex (flipV geoImg) (1, 2) == unsafeIndex geoImg (0, 2)

#guard dims (flipH geoImg) == dims geoImg
#guard unsafeIndex (flipH geoImg) (0, 0) == unsafeIndex geoImg (0, 2)
#guard unsafeIndex (flipH geoImg) (1, 1) == unsafeIndex geoImg (1, 1)

-- ── Rotation ──

-- `rotate90` swaps and reverses axes: `(i, j) ↦ geoImg (rows - 1 - j, i)`.
#guard dims (rotate90 geoImg) == (3, 2)
#guard unsafeIndex (rotate90 geoImg) (0, 0) == unsafeIndex geoImg (1, 0)
#guard unsafeIndex (rotate90 geoImg) (2, 1) == unsafeIndex geoImg (0, 2)

#guard dims (rotate180 geoImg) == dims geoImg
#guard unsafeIndex (rotate180 geoImg) (0, 0) == unsafeIndex geoImg (1, 2)
#guard unsafeIndex (rotate180 geoImg) (1, 2) == unsafeIndex geoImg (0, 0)

#guard dims (rotate270 geoImg) == (3, 2)
#guard unsafeIndex (rotate270 geoImg) (0, 0) == unsafeIndex geoImg (0, 2)
#guard unsafeIndex (rotate270 geoImg) (2, 1) == unsafeIndex geoImg (1, 0)

-- A `0`-radian `rotate` is the identity (up to floating-point rounding, here
-- exact since every sampled coordinate lands exactly on an integer — see the
-- module's doc-comment for why `Nearest` at `theta = 0` reproduces the
-- source exactly).
#guard dims (rotate (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float))
    (0 : Float) geoYImg) == dims geoYImg
#guard unsafeIndex
    (rotate (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float))
      (0 : Float) geoYImg) (1, 1)
  == unsafeIndex geoYImg (1, 1)

-- ── Scaling ──

-- Resizing the 2×2 image to 4×4 with `Nearest` doubles every pixel into a
-- 2×2 block.
#guard dims (resize (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float))
    (4, 4) geoYImg) == (4, 4)
#guard unsafeIndex
    (resize (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float))
      (4, 4) geoYImg) (0, 0)
  == unsafeIndex geoYImg (0, 0)
#guard unsafeIndex
    (resize (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float))
      (4, 4) geoYImg) (3, 3)
  == unsafeIndex geoYImg (1, 1)
#guard unsafeIndex
    (resize (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float))
      (4, 4) geoYImg) (1, 2)
  == unsafeIndex geoYImg (0, 1)

-- `scale (2.0, 2.0)` on a 2×2 image resizes to the same 4×4 result as above.
#guard dims (scale (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float))
    ((2.0 : Float), (2.0 : Float)) geoYImg) == (4, 4)
#guard unsafeIndex
    (scale (cs := Y) (e := Float) Nearest.nearest (Border.edge : Border (PixelY Float))
      ((2.0 : Float), (2.0 : Float)) geoYImg) (0, 0)
  == unsafeIndex geoYImg (0, 0)
