/-
  Tests for `Linen.Data.Colour.RGB` — an RGB triple for an unspecified
  colour space.
-/
import Linen.Data.Colour.RGB

open Data.Colour
open Data.Colour.CIE (Chromaticity)

namespace Tests.Data.Colour.RGB

def triple : RGB Float := ⟨1, 2, 3⟩

/-! ### RGB -/

#guard RGB.map (· * 2) triple == (⟨2, 4, 6⟩ : RGB Float)
#guard RGB.uncurry (fun r g b => r + g + b) triple == 6
#guard RGB.curry (fun c => c.r + c.g + c.b) 1 2 3 == 6

/-! ### hue/saturation/lightness/value -/

#guard RGB.hslsv (⟨0, 0, 0⟩ : RGB Float) == (0, 0, 0, 0, 0)
#guard (RGB.hue (⟨1, 0, 0⟩ : RGB Float)) == 0
#guard (RGB.hue (⟨0, 1, 0⟩ : RGB Float)) == 120
#guard (RGB.hue (⟨0, 0, 1⟩ : RGB Float)) == 240

/-! ### mod1 -/

#guard mod1 0.25 == 0.25
#guard mod1 1.25 == 0.25
#guard mod1 (-0.25) == 0.75

/-! ### RGBGamut: rgb2xyz / xyz2rgb round-trip -/

def gamut : RGBGamut :=
  ⟨⟨Chromaticity.of 0.64 0.33, Chromaticity.of 0.3 0.6, Chromaticity.of 0.15 0.06⟩,
   Chromaticity.of 0.3127 0.329⟩

/-- Whether `m` is within `1e-9` of the 3×3 identity matrix, i.e. whether
    `xyz2rgb` and `rgb2xyz` invert each other up to `Float` rounding. -/
def approxIdentity (m : Matrix3) : Bool :=
  let eps := 1e-9
  (m.r0.x - 1).abs < eps && m.r0.y.abs < eps && m.r0.z.abs < eps &&
  m.r1.x.abs < eps && (m.r1.y - 1).abs < eps && m.r1.z.abs < eps &&
  m.r2.x.abs < eps && m.r2.y.abs < eps && (m.r2.z - 1).abs < eps

#guard approxIdentity (Matrix3.matrixMult gamut.xyz2rgb gamut.rgb2xyz)

end Tests.Data.Colour.RGB
