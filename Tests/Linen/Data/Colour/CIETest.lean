/-
  Tests for `Linen.Data.Colour.CIE` — CIE colour operations.
-/
import Linen.Data.Colour.CIE
import Linen.Data.Colour.CIE.Illuminant

open Data.Colour
open Data.Colour.CIE

namespace Tests.Data.Colour.CIE

def approxTriple (u v : Float × Float × Float) : Bool :=
  let eps := 1e-9
  (u.1 - v.1).abs < eps && (u.2.1 - v.2.1).abs < eps && (u.2.2 - v.2.2).abs < eps

def approxColour (u v : Colour) : Bool :=
  let eps := 1e-9
  let a := SRGB.Linear.toRGB u
  let b := SRGB.Linear.toRGB v
  (a.r - b.r).abs < eps && (a.g - b.g).abs < eps && (a.b - b.b).abs < eps

-- `cieXYZ`/`cieXYZView` round-trip through an arbitrary XYZ triple.
#guard approxTriple (cieXYZView (cieXYZ 0.3 0.4 0.5)) (0.3, 0.4, 0.5)

-- The white point's own chromaticity has luminance 1 by construction.
#guard (luminance (chromaColour Illuminant.d65 1.0) - 1.0).abs < 1e-9

-- The white point's own lightness (`L*`) is 100.
#guard (lightness Illuminant.d65 (chromaColour Illuminant.d65 1.0) - 100).abs < 1e-6

-- `cieLAB`/`cieLABView` round-trip through the white point's own
-- coordinates: `(L*, a*, b*) = (100, 0, 0)`.
#guard approxColour (cieLAB Illuminant.d65 100 0 0) (chromaColour Illuminant.d65 1.0)
#guard approxTriple (cieLABView Illuminant.d65 (chromaColour Illuminant.d65 1.0)) (100, 0, 0)

end Tests.Data.Colour.CIE
