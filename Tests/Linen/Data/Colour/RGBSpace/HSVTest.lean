/-
  Tests for `Linen.Data.Colour.RGBSpace.HSV` — HSV colours.
-/
import Linen.Data.Colour.RGBSpace.HSV

open Data.Colour
open Data.Colour.RGBSpace.HSV

namespace Tests.Data.Colour.RGBSpace.HSV

#guard hsvView (⟨1, 0, 0⟩ : RGB Float) == (0, 1, 1)
#guard saturation (⟨1, 0, 0⟩ : RGB Float) == 1
#guard value (⟨1, 0, 0⟩ : RGB Float) == 1
#guard hsv 0 1 1 == (⟨1, 0, 0⟩ : RGB Float)
#guard hsv 0 0 0 == (⟨0, 0, 0⟩ : RGB Float)
#guard hsv 0 0 1 == (⟨1, 1, 1⟩ : RGB Float)
#guard hsv 360 1 1 == (⟨1, 0, 0⟩ : RGB Float)

end Tests.Data.Colour.RGBSpace.HSV
