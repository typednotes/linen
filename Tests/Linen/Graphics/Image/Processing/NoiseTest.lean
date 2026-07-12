/-
  Tests for `Linen.Graphics.Image.Processing.Noise` — synthetic
  salt-and-pepper noise.

  Fixture names are prefixed `noise` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.Processing.Noise
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (fromLists unsafeIndex dims)
open Graphics.Image.Processing.Noise (randomCoords saltAndPepper)
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- ── `randomCoords`: determinism and exact count ──

-- The same generator seed always produces the same coordinate sequence.
#guard randomCoords (mkStdGen 42) 9 5 6 == randomCoords (mkStdGen 42) 9 5 6

-- Exactly `n` coordinates are generated, every one within bounds.
#guard (randomCoords (mkStdGen 7) 9 5 10).length == 10
#guard (randomCoords (mkStdGen 7) 9 5 10).all (fun (x, y) => x ≥ 0 && x ≤ 9 && y ≥ 0 && y ≤ 5)

-- A different seed generally produces a different sequence (checked against
-- one concrete pair of seeds rather than asserted as a universal law).
#guard randomCoords (mkStdGen 1) 9 5 6 != randomCoords (mkStdGen 2) 9 5 6

-- ── `saltAndPepper`: determinism ──

def noiseImg4x4 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists
    [[⟨0.2⟩, ⟨0.2⟩, ⟨0.2⟩, ⟨0.2⟩],
     [⟨0.2⟩, ⟨0.2⟩, ⟨0.2⟩, ⟨0.2⟩],
     [⟨0.2⟩, ⟨0.2⟩, ⟨0.2⟩, ⟨0.2⟩],
     [⟨0.2⟩, ⟨0.2⟩, ⟨0.2⟩, ⟨0.2⟩]]

-- Two calls with the same seed and the same inputs produce an identical
-- output image (the RNG is threaded purely, with no hidden global state).
#guard saltAndPepper noiseImg4x4 0.5 (mkStdGen 123) == saltAndPepper noiseImg4x4 0.5 (mkStdGen 123)

-- The output has the same dimensions as the input.
#guard dims (saltAndPepper noiseImg4x4 0.5 (mkStdGen 123)) == dims noiseImg4x4

-- ── `saltAndPepper`: the coordinate-parity write rule ──

-- With `noiseLevel = 0`, `noiseIntensity = round (0 * widthMax * heightMax)
-- = 0`, so exactly one coordinate (`noiseIntensity + 1 = 1`) is still drawn
-- and written — this is upstream's own behaviour (`take 1 …`), not a
-- "no-op at zero" edge case, so this test checks the one written pixel
-- follows the documented parity rule rather than asserting the image is
-- unchanged.
def noiseZeroLevelResult := saltAndPepper noiseImg4x4 0.0 (mkStdGen 5)
def noiseZeroLevelCoord := (randomCoords (mkStdGen 5) 3 3 1).head!

#guard
  let (x, y) := noiseZeroLevelCoord
  let expected : Float := if (x + y) % 2 == 0 then 0.0 else 1.0
  unsafeIndex noiseZeroLevelResult (x, y) == (⟨expected⟩ : PixelY Float)

-- Every pixel not among the drawn noise coordinates is copied unchanged
-- from the input.
def noiseUntouchedCoord : Int × Int := (3, 0)
#guard noiseZeroLevelCoord != noiseUntouchedCoord
#guard unsafeIndex noiseZeroLevelResult noiseUntouchedCoord
  == unsafeIndex noiseImg4x4 noiseUntouchedCoord
