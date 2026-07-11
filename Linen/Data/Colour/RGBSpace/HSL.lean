/-
  Linen.Data.Colour.RGBSpace.HSL — HSL (hue-saturation-lightness) colours

  ## Haskell equivalent
  `Data.Colour.RGBSpace.HSL` from https://hackage.haskell.org/package/colour
-/
import Linen.Data.Colour.RGB

namespace Data.Colour.RGBSpace.HSL

open Data.Colour

/-- Returns the `(hue, saturation, lightness)` coordinates of an `RGB`
    triple. -/
def hslView (c : RGB Float) : Float × Float × Float :=
  let (h, s, l, _, _) := RGB.hslsv c
  (h, s, l)

/-- Returns the saturation coordinate (range `[0, 1]`) of an `RGB` triple,
    for the HSL system. Note: this differs from
    `Data.Colour.RGBSpace.HSV.saturation`. -/
def saturation (c : RGB Float) : Float := (RGB.hslsv c).2.1

/-- Returns the lightness coordinate (range `[0, 1]`) of an `RGB` triple,
    for the HSL system. -/
def lightness (c : RGB Float) : Float := (RGB.hslsv c).2.2.1

/-- Converts HSL (hue-saturation-lightness) coordinates to an `RGB` value.
    `h` is expected in degrees `[0, 360]`; `s` and `l` in `[0, 1]`. -/
def hsl (h s l : Float) : RGB Float :=
  let hk := h / 360
  let component (t : Float) : Float :=
    let q := if l < 0.5 then l * (1 + s) else l + s - l * s
    let p := 2 * l - q
    if t < 1 / 6 then p + (q - p) * 6 * t
    else if t < 1 / 2 then q
    else if t < 2 / 3 then p + (q - p) * 6 * (2 / 3 - t)
    else p
  ⟨component (mod1 (hk + 1 / 3)), component (mod1 hk), component (mod1 (hk - 1 / 3))⟩

end Data.Colour.RGBSpace.HSL
