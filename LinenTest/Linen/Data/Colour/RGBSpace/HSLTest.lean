/-
  Tests for `Linen.Data.Colour.RGBSpace.HSL` — HSL colours.
-/
import Linen.Data.Colour.RGBSpace.HSL

open Data.Colour
open Data.Colour.RGBSpace.HSL

namespace Tests.Data.Colour.RGBSpace.HSL

#guard hslView (⟨1, 0, 0⟩ : RGB Float) == (0, 1, 0.5)
#guard saturation (⟨1, 0, 0⟩ : RGB Float) == 1
#guard lightness (⟨1, 0, 0⟩ : RGB Float) == 0.5
#guard hsl 0 1 0.5 == (⟨1, 0, 0⟩ : RGB Float)
#guard hsl 0 0 0 == (⟨0, 0, 0⟩ : RGB Float)
#guard hsl 0 0 1 == (⟨1, 1, 1⟩ : RGB Float)

end Tests.Data.Colour.RGBSpace.HSL
