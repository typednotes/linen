/-
  Tests for `Linen.Data.Colour.SRGB.Linear` — a linear colour space with
  sRGB's gamut.
-/
import Linen.Data.Colour.SRGB.Linear

open Data.Colour
open Data.Colour.SRGB.Linear

namespace Tests.Data.Colour.SRGB.Linear

#guard toRGB (rgb 0.1 0.2 0.3) == (⟨0.1, 0.2, 0.3⟩ : RGB Float)
#guard sRGBGamut.whitePoint == CIE.Illuminant.d65
#guard sRGBGamut.primaries.r.x == 0.64

end Tests.Data.Colour.SRGB.Linear
