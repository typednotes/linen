/-
  Tests for `Linen.Data.Colour.CIE.Illuminant` — standard illuminants
  defined by the CIE.
-/
import Linen.Data.Colour.CIE.Illuminant

open Data.Colour.CIE

namespace Tests.Data.Colour.CIE.Illuminant

#guard Illuminant.d65.x == 0.31271
#guard Illuminant.d65.y == 0.32902
#guard Illuminant.e.x == 1 / 3
#guard Illuminant.e.y == 1 / 3
#guard Illuminant.f12.x == 0.43695

end Tests.Data.Colour.CIE.Illuminant
