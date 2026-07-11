/-
  Linen.Data.Colour.SRGB.Linear — a linear colour space with sRGB's gamut

  ## Haskell equivalent
  `Data.Colour.SRGB.Linear` from https://hackage.haskell.org/package/colour
-/
import Linen.Data.Colour.CIE.Illuminant
import Linen.Data.Colour.Internal
import Linen.Data.Colour.RGB

namespace Data.Colour.SRGB.Linear

open Data.Colour

/-- Constructs a `Colour` from RGB values, using the *linear* RGB colour
    space with the same gamut as sRGB. -/
def rgb (r g b : Float) : Colour := ⟨⟨r⟩, ⟨g⟩, ⟨b⟩⟩

/-- Returns RGB values, using the *linear* RGB colour space with the same
    gamut as sRGB. -/
def toRGB (c : Colour) : RGB Float := ⟨c.r.val, c.g.val, c.b.val⟩

/-- The gamut for the sRGB colour space. -/
def sRGBGamut : RGBGamut :=
  ⟨⟨CIE.Chromaticity.of 0.64 0.33, CIE.Chromaticity.of 0.30 0.60, CIE.Chromaticity.of 0.15 0.06⟩,
   CIE.Illuminant.d65⟩

end Data.Colour.SRGB.Linear
