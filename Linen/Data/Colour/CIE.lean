/-
  Linen.Data.Colour.CIE — colour operations defined by the International
  Commission on Illumination (CIE)

  ## Haskell equivalent
  `Data.Colour.CIE` from https://hackage.haskell.org/package/colour

  ## Design
  Upstream's `AffineSpace Chromaticity` instance (`affineCombo` built from
  local `chromaAdd`/`chromaScale` helpers) is not exported by this module
  and is never used anywhere else in `colour`, so — unlike
  `Data.Colour.Internal`'s `AffineSpace Colour`/`AffineSpace AlphaColour`,
  which back real, exported `blend` call sites — it is dropped as dead
  code, matching the precedent set for `Data.Colour.RGB`'s unused
  `Applicative` instance.

  `toCIEXYZ`, a deprecated alias for `cieXYZView`, and `cieLuv` (CIELUV),
  which upstream itself keeps unexported and commented out, are both
  omitted.
-/
import Linen.Data.Colour
import Linen.Data.Colour.CIE.Chromaticity
import Linen.Data.Colour.Matrix
import Linen.Data.Colour.RGB
import Linen.Data.Colour.SRGB.Linear

namespace Data.Colour.CIE

open Data.Colour

/-- Constructs a `Colour` from XYZ coordinates for the 2° standard
    (colourimetric) observer. -/
def cieXYZ (x y z : Float) : Colour :=
  let v := Matrix3.mult SRGB.Linear.sRGBGamut.xyz2rgb ⟨x, y, z⟩
  SRGB.Linear.rgb v.x v.y v.z

/-- Returns the XYZ colour coordinates for the 2° standard (colourimetric)
    observer. -/
def cieXYZView (c : Colour) : Float × Float × Float :=
  let rgb := SRGB.Linear.toRGB c
  let v := Matrix3.mult SRGB.Linear.sRGBGamut.rgb2xyz ⟨rgb.r, rgb.g, rgb.b⟩
  (v.x, v.y, v.z)

/-- Returns the *Y* colour coordinate (luminance) for the 2° standard
    (colourimetric) observer. -/
def luminance (c : Colour) : Float := (cieXYZView c).2.1

/-- Constructs a colour from the given `Chromaticity` and `luminance`. -/
def chromaColour (ch : Chromaticity) (y : Float) : Colour :=
  let (chX, chY, chZ) := ch.coords
  let s := y / chY
  cieXYZ (s * chX) y (s * chZ)

/-- Returns the lightness of a colour with respect to a given white point.
    Lightness is a perceptually uniform measure. -/
def lightness (whiteCh : Chromaticity) (c : Colour) : Float :=
  let white := chromaColour whiteCh 1.0
  let y' := luminance c / luminance white
  if (6.0 / 29.0) ^ (3 : Float) < y' then 116 * y' ^ (1 / 3 : Float) - 16
  else (29.0 / 3.0) ^ (3 : Float) * y'

private def f (x : Float) : Float :=
  if (6.0 / 29.0) ^ (3 : Float) < x then x ^ (1 / 3 : Float)
  else 841 / 108 * x + 4 / 29

/-- Returns the CIELAB coordinates of a colour, which is a perceptually
    uniform colour space. The first coordinate is `lightness`. If you don't
    know what white point to use, use `Data.Colour.CIE.Illuminant.d65`. -/
def cieLABView (whiteCh : Chromaticity) (c : Colour) : Float × Float × Float :=
  let white := chromaColour whiteCh 1.0
  let (x, y, z) := cieXYZView c
  let (xn, yn, zn) := cieXYZView white
  let (fx, fy, fz) := (f (x / xn), f (y / yn), f (z / zn))
  (lightness whiteCh c, 500 * (fx - fy), 200 * (fy - fz))

/-- Returns the colour for given CIELAB coordinates, which is a
    perceptually uniform colour space. If you don't know what white point
    to use, use `Data.Colour.CIE.Illuminant.d65`. -/
def cieLAB (whiteCh : Chromaticity) (l a b : Float) : Colour :=
  let white := chromaColour whiteCh 1.0
  let (xn, yn, zn) := cieXYZView white
  let fy := (l + 16) / 116
  let fx := fy + a / 500
  let fz := fy - b / 200
  let delta := 6.0 / 29.0
  let transform (fa : Float) : Float :=
    if fa > delta then fa ^ (3 : Float) else (fa - 16 / 116) * 3 * delta ^ (2 : Float)
  cieXYZ (xn * transform fx) (yn * transform fy) (zn * transform fz)

end Data.Colour.CIE
