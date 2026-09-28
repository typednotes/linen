/-
  Tests for `Linen.Data.Colour.RGBSpace` — RGB colour coordinate systems.
-/
import Linen.Data.Colour.RGBSpace

open Data.Colour

namespace Tests.Data.Colour.RGBSpace

#guard SRGB.Linear.sRGBGamut.inGamut (SRGB.Linear.rgb 0.2 0.3 0.4) == true
#guard SRGB.Linear.sRGBGamut.inGamut (SRGB.Linear.rgb 2 0.3 0.4) == false

#guard TransferFunction.linear.transfer 0.5 == 0.5
#guard TransferFunction.linear.transferInverse 0.5 == 0.5
#guard (TransferFunction.power 2).transfer 3 == 9
#guard (TransferFunction.power 2).transferInverse 9 == 3
#guard (TransferFunction.inverse (TransferFunction.power 2)).transfer 9 == 3
#guard (TransferFunction.append TransferFunction.linear TransferFunction.linear).transfer 0.5 == 0.5

def linearSpace : RGBSpace := SRGB.Linear.sRGBGamut.linearRGBSpace

/-- `RGBGamut.rgb2xyz`/`xyz2rgb` invert each other up to `Float` rounding
    (see `Data.Colour.RGB`'s design note), so a linear space's round-trip
    through the same gamut is only *approximately* the identity. -/
def approxRGB (u v : RGB Float) : Bool :=
  let eps := 1e-9
  (u.r - v.r).abs < eps && (u.g - v.g).abs < eps && (u.b - v.b).abs < eps

#guard approxRGB (RGBSpace.toRGBUsingSpace linearSpace (SRGB.Linear.rgb 0.2 0.3 0.4)) ⟨0.2, 0.3, 0.4⟩
#guard approxRGB (SRGB.Linear.toRGB (RGBSpace.rgbUsingSpace linearSpace 0.2 0.3 0.4)) ⟨0.2, 0.3, 0.4⟩

end Tests.Data.Colour.RGBSpace
