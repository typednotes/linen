/-
  Linen.Data.Colour.SRGB — specifies `Colour`s in accordance with the sRGB
  standard

  ## Haskell equivalent
  `Data.Colour.SRGB` from https://hackage.haskell.org/package/colour

  ## Design
  `Data.Colour.Internal.quantize` (generic over any `Integral`/`Bounded`
  target) is ported here concretely at `UInt8`, the only bound type this
  module (and every caller of `sRGB24`/`toSRGB24`) ever fixes it to; `Float`'s
  own `round`/`toUInt8` already clamp and round exactly as upstream's
  `quantize` does, so no separate clamping logic is needed.

  `sRGBBounded`, generic over any `Integral`/`Bounded` source type, is
  likewise specialized directly to `UInt8` as `sRGB24`, its only real use.

  `sRGB24reads`/`sRGB24read` (a `ReadS`-style parser that signals failure by
  returning `[]` or calling `error`) is ported as `sRGB24read?`, returning
  `Option Colour`, which is the idiomatic Lean shape for a parser that can
  fail.
-/
import Linen.Data.Colour.Internal
import Linen.Data.Colour.RGBSpace
import Linen.Data.Colour.SRGB.Linear

namespace Data.Colour.SRGB

open Data.Colour

/-! ── The sRGB transfer function ── -/

/-- The sRGB transfer function, approximating a gamma of about `1/2.2`. -/
def transferFunction (lin : Float) : Float :=
  if lin == 1 then 1
  else if lin <= 0.0031308 then 12.92 * lin
  else 1.055 * lin ^ (1 / 2.4) - 0.055

/-- The inverse of `transferFunction`. -/
def invTransferFunction (nonLin : Float) : Float :=
  if nonLin == 1 then 1
  else if nonLin <= 0.04045 then nonLin / 12.92
  else ((nonLin + 0.055) / 1.055) ^ 2.4

/-! ── Constructing colours ── -/

/-- Constructs a colour from an sRGB specification. Input components are
    expected to be in the range `[0, 1]`. -/
def sRGB (r g b : Float) : Colour :=
  SRGB.Linear.rgb (invTransferFunction r) (invTransferFunction g) (invTransferFunction b)

/-- Constructs a colour from a 24-bit (three 8-bit words) sRGB
    specification. -/
def sRGB24 (r g b : UInt8) : Colour :=
  sRGB (r.toFloat / 255) (g.toFloat / 255) (b.toFloat / 255)

/-! ── Reading off colours ── -/

/-- Returns the sRGB colour components in the range `[0, 1]`. -/
def toSRGB (c : Colour) : RGB Float :=
  RGB.map transferFunction (SRGB.Linear.toRGB c)

/-- Rounds a `Float` to the nearest `UInt8`, clamping out-of-range values. -/
def quantize (x : Float) : UInt8 := x.round.toUInt8

/-- Returns the approximate sRGB colour components in the range
    `[0, 255]`. Out-of-range values are clamped. -/
def toSRGB24 (c : Colour) : RGB UInt8 :=
  RGB.map (fun x => quantize (255 * x)) (toSRGB c)

/-! ── Hexadecimal notation ── -/

private def hexDigitChar (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + '0'.toNat) else Char.ofNat (n - 10 + 'a'.toNat)

private def hexByte (b : UInt8) : String :=
  let n := b.toNat
  String.ofList [hexDigitChar (n / 16), hexDigitChar (n % 16)]

private def hexDigitValue (c : Char) : Option Nat :=
  if '0' <= c && c <= '9' then some (c.toNat - '0'.toNat)
  else if 'a' <= c && c <= 'f' then some (c.toNat - 'a'.toNat + 10)
  else if 'A' <= c && c <= 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

private def hexByteValue (hi lo : Char) : Option UInt8 := do
  let hi ← hexDigitValue hi
  let lo ← hexDigitValue lo
  some (UInt8.ofNat (hi * 16 + lo))

/-- Shows a colour in hexadecimal form, e.g. `"#00aaff"`. -/
def sRGB24show (c : Colour) : String :=
  let rgb := toSRGB24 c
  "#" ++ hexByte rgb.r ++ hexByte rgb.g ++ hexByte rgb.b

/-- Reads a colour in hexadecimal form, e.g. `"#00aaff"` or `"00aaff"`. -/
def sRGB24read? (s : String) : Option Colour :=
  let cs := match s.toList with
    | '#' :: rest => rest
    | cs => cs
  match cs with
  | [r0, r1, g0, g1, b0, b1] => do
    let r ← hexByteValue r0 r1
    let g ← hexByteValue g0 g1
    let b ← hexByteValue b0 b1
    some (sRGB24 r g b)
  | _ => none

/-! ── The sRGB colour space ── -/

/-- The sRGB colour space. -/
def sRGBSpace : RGBSpace :=
  ⟨SRGB.Linear.sRGBGamut, ⟨transferFunction, invTransferFunction, 1 / 2.2⟩⟩

end Data.Colour.SRGB
