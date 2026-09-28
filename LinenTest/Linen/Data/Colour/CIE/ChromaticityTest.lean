/-
  Tests for `Linen.Data.Colour.CIE.Chromaticity` — CIE xy chromaticity
  coordinates.
-/
import Linen.Data.Colour.CIE.Chromaticity

open Data.Colour.CIE

namespace Tests.Data.Colour.CIE.Chromaticity

#guard (Chromaticity.of 0.25 0.5).x == 0.25
#guard (Chromaticity.of 0.25 0.5).y == 0.5
#guard (Chromaticity.of 0.25 0.5).z == 0.25
#guard Chromaticity.coords (Chromaticity.of 0.25 0.5) == (0.25, 0.5, 0.25)

end Tests.Data.Colour.CIE.Chromaticity
