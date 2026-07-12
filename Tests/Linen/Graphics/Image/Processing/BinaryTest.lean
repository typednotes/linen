/-
  Tests for `Linen.Graphics.Image.Processing.Binary` — binary-image
  construction and morphology (erode/dilate/opening/closing).

  Fixture names are prefixed `bin` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.Processing.Binary
import Linen.Graphics.Image.ColorSpace.RGB

open Graphics.Image.Interface (fromLists dims)
open Graphics.Image.Processing.Binary
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.ColorSpace.Binary (Bit on off isOn zero one)
open Graphics.Image.ColorSpace.RGB (RGB PixelRGB)

-- ── Fixtures: a single foreground pixel and a cross-shaped structuring
-- element ──

-- A 5×5 binary image, `on` at `(2, 2)` (0-indexed, the very centre), `off`
-- everywhere else.
def binImg : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Bit) :=
  fromLists [[off, off, off, off, off],
             [off, off, off, off, off],
             [off, off, on, off, off],
             [off, off, off, off, off],
             [off, off, off, off, off]]

-- A 3×3 cross ("plus"-shaped) structuring element, centred at `(1, 1)`.
def binCross : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Bit) :=
  fromLists [[off, on, off],
             [on, on, on],
             [off, on, off]]

-- Dilating the single centre pixel with the cross places the cross itself
-- (translated to be centred on that pixel) into the result — every pixel
-- involved is interior to the 5×5 image, so no border strategy is exercised.
def binCrossAround22 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Bit) :=
  fromLists [[off, off, off, off, off],
             [off, off, on, off, off],
             [off, on, on, on, off],
             [off, off, on, off, off],
             [off, off, off, off, off]]

def binAllOff : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Bit) :=
  fromLists [[off, off, off, off, off],
             [off, off, off, off, off],
             [off, off, off, off, off],
             [off, off, off, off, off],
             [off, off, off, off, off]]

-- ── `dilate`/`erode`: hand-computed against the fixtures above ──

-- Dilation "spreads" the single foreground pixel into the cross's own shape
-- (the cross's 5 support cells, centred on that pixel).
#guard dilate binCross binImg == binCrossAround22

-- Erosion of a single pixel by a strictly larger structuring element is
-- always empty: no placement of the 5-cell cross can land entirely inside a
-- single foreground pixel.
#guard erode binCross binImg == binAllOff

-- ── `opening`/`closing`: sanity checks against the fixtures above ──

-- Opening = dilate (erode img): erosion already emptied the image, and
-- dilating an empty image stays empty.
#guard opening binCross binImg == binAllOff

-- Closing = erode (dilate img): dilating then eroding the single pixel with
-- the same structuring element recovers exactly the original single-pixel
-- image (the cross-shaped dilation result contains, and is contained by,
-- one exact placement of the cross centred back on the original pixel).
#guard closing binCross binImg == binImg

-- ── `toImageBinaryUsing`/`toImageBinaryUsing2`: predicate-based thresholding
-- ──

def binRGBImg : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelRGB UInt8) :=
  fromLists [[⟨0, 0, 0⟩, ⟨10, 10, 10⟩], [⟨255, 255, 255⟩, ⟨0, 0, 0⟩]]

-- `on` exactly where the source pixel is pure black, `off` elsewhere.
#guard toImageBinaryUsing (cs := RGB) (e := UInt8) (· == (⟨0, 0, 0⟩ : PixelRGB UInt8)) binRGBImg
  == fromLists [[on, off], [off, on]]

-- `toImageBinaryUsing2`, comparing an image against itself, is always `on`.
#guard toImageBinaryUsing2 (cs := RGB) (e := UInt8) (· == ·) binRGBImg binRGBImg
  == fromLists [[on, on], [on, on]]

-- ── `invert`/`zipAnd`/`zipOr`: pixel-wise, channel-preserving operators ──

def binRGBBit1 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelRGB Bit) :=
  fromLists [[⟨one, zero, one⟩, ⟨zero, zero, zero⟩]]

def binRGBBit2 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelRGB Bit) :=
  fromLists [[⟨one, one, zero⟩, ⟨one, one, one⟩]]

#guard invert (cs := RGB) binRGBBit1 == fromLists [[⟨zero, one, zero⟩, ⟨one, one, one⟩]]

#guard zipAnd (cs := RGB) binRGBBit1 binRGBBit2 == fromLists [[⟨one, zero, zero⟩, ⟨zero, zero, zero⟩]]
#guard zipOr (cs := RGB) binRGBBit1 binRGBBit2 == fromLists [[⟨one, one, one⟩, ⟨one, one, one⟩]]

-- ── `squashAnd`/`squashOr`/`disjunction`/`conjunction`: channel-collapsing
-- operators ──

-- `squashAnd`/`squashOr` `AND`/`OR` every channel of *both* pixels together
-- into one `X` channel: the first pixel `⟨1, 0, 1⟩ AND ⟨1, 1, 0⟩ = 0`
-- (channel 2 of the first operand is `0`); the second pixel `⟨0,0,0⟩ AND
-- ⟨1,1,1⟩ = 0` as well (every channel of the first operand is `0`).
#guard squashAnd (cs := RGB) binRGBBit1 binRGBBit2 == fromLists [[off, off]]
#guard squashOr (cs := RGB) binRGBBit1 binRGBBit2 == fromLists [[on, on]]

-- `disjunction`/`conjunction` do the same channel-collapsing fold on a
-- single image: `⟨1,0,1⟩`'s channels `OR` together to `1`, `AND` together to
-- `0`; `⟨0,0,0⟩`'s channels `OR`/`AND` together to `0` either way.
#guard disjunction (cs := RGB) binRGBBit1 == fromLists [[on, off]]
#guard conjunction (cs := RGB) binRGBBit1 == fromLists [[off, off]]

-- ── `or`/`and`: whole-image boolean reduction ──

-- `binCrossAround22` has some `on` pixels (disjunction true) but is not
-- entirely `on` (conjunction false); `binAllOff` is neither.
#guard or binCrossAround22 == true
#guard and binCrossAround22 == false
#guard or binAllOff == false
#guard and binAllOff == false

-- An all-`on` image is both a disjunction and a conjunction.
def binAllOn : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Bit) :=
  fromLists [[on, on], [on, on]]

#guard or binAllOn == true
#guard and binAllOn == true
