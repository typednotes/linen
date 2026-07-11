/-
  Linen.Data.Colour.RGBSpace.HSV — HSV (hue-saturation-value) colours

  ## Haskell equivalent
  `Data.Colour.RGBSpace.HSV` from https://hackage.haskell.org/package/colour
-/
import Linen.Data.Colour.RGB

namespace Data.Colour.RGBSpace.HSV

open Data.Colour

/-- Returns the `(hue, saturation, value)` coordinates of an `RGB`
    triple. -/
def hsvView (c : RGB Float) : Float × Float × Float :=
  let (h, _, _, s, v) := RGB.hslsv c
  (h, s, v)

/-- Returns the saturation coordinate (range `[0, 1]`) of an `RGB` triple,
    for the HSV system. Note: this differs from
    `Data.Colour.RGBSpace.HSL.saturation`. -/
def saturation (c : RGB Float) : Float := (RGB.hslsv c).2.2.2.1

/-- Returns the value coordinate (range `[0, 1]`) of an `RGB` triple, for
    the HSV system. -/
def value (c : RGB Float) : Float := (RGB.hslsv c).2.2.2.2

/-- Converts HSV (hue-saturation-value) coordinates to an `RGB` value.
    `h` is expected in degrees `[0, 360]`; `s` and `v` in `[0, 1]`. -/
def hsv (h s v : Float) : RGB Float :=
  let hiFloat := Float.floor (h / 60)
  let hi := (hiFloat - 6 * Float.floor (hiFloat / 6)).toUInt64.toNat
  let f := mod1 (h / 60)
  let p := v * (1 - s)
  let q := v * (1 - f * s)
  let t := v * (1 - (1 - f) * s)
  match hi with
  | 0 => ⟨v, t, p⟩
  | 1 => ⟨q, v, p⟩
  | 2 => ⟨p, v, t⟩
  | 3 => ⟨p, q, v⟩
  | 4 => ⟨t, p, v⟩
  | _ => ⟨v, p, q⟩

end Data.Colour.RGBSpace.HSV
