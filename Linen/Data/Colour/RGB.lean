/-
  Linen.Data.Colour.RGB — an RGB triple for an unspecified colour space

  ## Haskell equivalent
  `Data.Colour.RGB` from https://hackage.haskell.org/package/colour

  ## Design
  Unlike `Chan`/`Colour`/`Chromaticity`, `RGB` itself stays generic over its
  element type: downstream code maps it over both `Float` (colour
  coordinates) and `Bool` (`Data.Colour.RGBSpace`'s in-gamut test), so a
  `Float`-only specialization would lose a real use site rather than a
  merely-generic one. Upstream's `Applicative` instance is dropped: nothing
  downstream calls it (only `fmap`, i.e. `map`, is ever used).

  `RGBGamut`'s primaries and white point are `Data.Colour.CIE.Chromaticity`,
  already specialized to `Float`; upstream keeps them at `Rational` to
  compute `rgb2xyz`/`xyz2rgb` exactly before rounding to the working type.
  Doing the same arithmetic directly in `Float` loses about 1 part in
  10^15 of precision, far below the precision of any image data this
  library processes, so the exactness isn't worth reintroducing a second,
  `Rational`-flavoured `Matrix3`/`Vec3` alongside `Data.Colour.Matrix`'s.
-/
import Linen.Data.Colour.CIE.Chromaticity
import Linen.Data.Colour.Matrix

namespace Data.Colour

open CIE (Chromaticity)

/-- An RGB triple for an unspecified colour space. -/
structure RGB (α : Type) where
  r : α
  g : α
  b : α
  deriving Repr, BEq

namespace RGB

/-- Applies `f` to each channel. -/
def map (f : α → β) (c : RGB α) : RGB β := ⟨f c.r, f c.g, f c.b⟩

/-- Uncurries a function expecting three r, g, b parameters. -/
def uncurry (f : α → α → α → β) (c : RGB α) : β := f c.r c.g c.b

/-- Curries a function expecting one `RGB` parameter. -/
def curry (f : RGB α → β) (r g b : α) : β := f ⟨r, g, b⟩

end RGB

/-- A 3-D colour "cube" that contains all the colours displayable by an RGB
    device, normalized so that white has luminance 1. -/
structure RGBGamut where
  primaries : RGB Chromaticity
  whitePoint : Chromaticity
  deriving BEq

namespace RGB

/-- The matrix whose columns are the XYZ coordinates of the red, green, and
    blue primaries (not for export upstream either). -/
def primaryMatrix (p : RGB Chromaticity) : Matrix3 :=
  let (xr, yr, zr) := p.r.coords
  let (xg, yg, zg) := p.g.coords
  let (xb, yb, zb) := p.b.coords
  ⟨⟨xr, xg, xb⟩, ⟨yr, yg, yb⟩, ⟨zr, zg, zb⟩⟩

end RGB

namespace RGBGamut

/-- The matrix that converts a gamut's linear RGB coordinates to XYZ. -/
def rgb2xyz (space : RGBGamut) : Matrix3 :=
  let (xn, yn, zn) := space.whitePoint.coords
  let matrix := RGB.primaryMatrix space.primaries
  let as := Matrix3.mult (Matrix3.inverse matrix) ⟨xn / yn, 1, zn / yn⟩
  let mt := Matrix3.transpose matrix
  Matrix3.transpose ⟨Vec3.scale as.x mt.r0, Vec3.scale as.y mt.r1, Vec3.scale as.z mt.r2⟩

/-- The matrix that converts XYZ coordinates to a gamut's linear RGB. -/
def xyz2rgb (space : RGBGamut) : Matrix3 := Matrix3.inverse (rgb2xyz space)

end RGBGamut

namespace RGB

/-- Returns `(hue, saturation_hsl, lightness, saturation_hsv, value)` for an
    `RGB` triple. -/
def hslsv (c : RGB Float) : Float × Float × Float × Float × Float :=
  let mx := max c.r (max c.g c.b)
  let mn := min c.r (min c.g c.b)
  if mx == mn then (0, 0, mx, 0, mx)
  else
    let l := (mx + mn) / 2
    let s := if l <= 0.5 then (mx - mn) / (mx + mn) else (mx - mn) / (2 - (mx + mn))
    let s0 := (mx - mn) / mx
    let (_x, y, z, o) :=
      if c.r == mx then (c.r, c.g, c.b, (0 : Float))
      else if c.g == mx then (c.g, c.b, c.r, (1 : Float))
      else (c.b, c.r, c.g, (2 : Float))
    let h0 := 60 * (y - z) / (mx - mn) + 120 * o
    let h := if h0 < 0 then h0 + 360 else h0
    (h, s, l, s0, mx)

/-- The `hue` coordinate of an `RGB` value, in degrees, always in `[0, 360)`. -/
def hue (c : RGB Float) : Float := (hslsv c).1

end RGB

/-- The fractional part of `x`, always in `[0, 1)` (not for export upstream
    either). -/
def mod1 (x : Float) : Float := x - x.floor

end Data.Colour
