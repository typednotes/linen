/-
  Linen.Data.Colour.RGBSpace — RGB colour coordinate systems

  ## Haskell equivalent
  `Data.Colour.RGBSpace` from https://hackage.haskell.org/package/colour

  ## Design
  `TransferFunction`'s upstream `Semigroup`/`Monoid` instance is ported as
  the plain function `TransferFunction.append` (`mempty` becomes
  `TransferFunction.linear`); nothing downstream in `colour` actually
  composes transfer functions, but the operation is cheap and still useful
  to a `linen` caller building a custom RGB space, unlike the `RGB`
  `Applicative` instance dropped in `Data.Colour.RGB`, which had no
  observable behaviour beyond what `RGB.map` already provides.
-/
import Linen.Data.Colour.RGB
import Linen.Data.Colour.SRGB.Linear

namespace Data.Colour

/-- Returns RGB values, of a general `RGBGamut`, for a colour. -/
def RGBGamut.toRGBUsingGamut (gamut : RGBGamut) (c : Colour) : RGB Float :=
  let rgb0 := SRGB.Linear.toRGB c
  let matrix := Matrix3.matrixMult gamut.xyz2rgb SRGB.Linear.sRGBGamut.rgb2xyz
  let v := Matrix3.mult matrix ⟨rgb0.r, rgb0.g, rgb0.b⟩
  ⟨v.x, v.y, v.z⟩

/-- Constructs a `Colour` from red, green, and blue coordinates given in a
    general `RGBGamut`. -/
def RGBGamut.rgbUsingGamut (gamut : RGBGamut) (r g b : Float) : Colour :=
  let matrix := Matrix3.matrixMult SRGB.Linear.sRGBGamut.xyz2rgb gamut.rgb2xyz
  let v := Matrix3.mult matrix ⟨r, g, b⟩
  SRGB.Linear.rgb v.x v.y v.z

/-- Returns `true` if the given colour lies inside the given gamut. -/
def RGBGamut.inGamut (gamut : RGBGamut) (c : Colour) : Bool :=
  let test (x : Float) : Bool := 0 <= x && x <= 1
  let r := gamut.toRGBUsingGamut c
  test r.r && test r.g && test r.b

/-- A `transfer` function typically translates linear colour space
    coordinates into non-linear coordinates; `transferInverse` reverses
    this. It is required that
    `transfer ∘ transferInverse = id = transferInverse ∘ transfer`
    (up to floating-point rounding). `transferGamma` is informational only:
    `transfer` is expected to approximate `(· ^ transferGamma)`. -/
structure TransferFunction where
  transfer : Float → Float
  transferInverse : Float → Float
  transferGamma : Float

namespace TransferFunction

/-- The identity `TransferFunction`. -/
def linear : TransferFunction := ⟨id, id, 1⟩

/-- The `(· ^ gamma)` `TransferFunction`. -/
def power (gamma : Float) : TransferFunction := ⟨(· ^ gamma), (· ^ (1 / gamma)), gamma⟩

/-- Reverses a `TransferFunction`. -/
def inverse (f : TransferFunction) : TransferFunction := ⟨f.transferInverse, f.transfer, 1 / f.transferGamma⟩

/-- Composes two `TransferFunction`s, applying `g` first. -/
def append (f g : TransferFunction) : TransferFunction :=
  ⟨f.transfer ∘ g.transfer, g.transferInverse ∘ f.transferInverse, f.transferGamma * g.transferGamma⟩

end TransferFunction

/-- An RGB colour coordinate system for colours lying `RGBGamut.inGamut` of
    `gamut`. Linear coordinates are passed through `transferFunction` to
    produce non-linear `RGB` values. -/
structure RGBSpace where
  gamut : RGBGamut
  transferFunction : TransferFunction

/-- Produces a linear colour space from an `RGBGamut`. -/
def RGBGamut.linearRGBSpace (gamut : RGBGamut) : RGBSpace := ⟨gamut, TransferFunction.linear⟩

namespace RGBSpace

/-- Constructs a `Colour` from red, green, and blue coordinates given in a
    general `RGBSpace`. -/
def rgbUsingSpace (space : RGBSpace) (r g b : Float) : Colour :=
  let tinv := space.transferFunction.transferInverse
  space.gamut.rgbUsingGamut (tinv r) (tinv g) (tinv b)

/-- Returns the coordinates of a given `Colour` for a general `RGBSpace`. -/
def toRGBUsingSpace (space : RGBSpace) (c : Colour) : RGB Float :=
  RGB.map space.transferFunction.transfer (space.gamut.toRGBUsingGamut c)

end RGBSpace
end Data.Colour
