/-
  Linen.Data.Colour.CIE.Illuminant — standard illuminants defined by the CIE

  ## Haskell equivalent
  `Data.Colour.CIE.Illuminant` from
  https://hackage.haskell.org/package/colour
-/
import Linen.Data.Colour.CIE.Chromaticity

namespace Data.Colour.CIE.Illuminant

/-- Incandescent / Tungsten. -/
def a : Chromaticity := .of 0.44757 0.40745

/-- {obsolete} Direct sunlight at noon. -/
def b : Chromaticity := .of 0.34842 0.35161

/-- {obsolete} Average / North sky Daylight. -/
def c : Chromaticity := .of 0.31006 0.31616

/-- Horizon Light. ICC profile PCS. -/
def d50 : Chromaticity := .of 0.34567 0.35850

/-- Mid-morning / Mid-afternoon Daylight. -/
def d55 : Chromaticity := .of 0.33242 0.34743

/-- Noon Daylight: Television, sRGB color space. -/
def d65 : Chromaticity := .of 0.31271 0.32902

/-- North sky Daylight. -/
def d75 : Chromaticity := .of 0.29902 0.31485

/-- Equal energy. -/
def e : Chromaticity := .of (1 / 3) (1 / 3)

/-- Daylight Fluorescent. -/
def f1 : Chromaticity := .of 0.31310 0.33727

/-- Cool White Fluorescent. -/
def f2 : Chromaticity := .of 0.37208 0.37529

/-- White Fluorescent. -/
def f3 : Chromaticity := .of 0.40910 0.39430

/-- Warm White Fluorescent. -/
def f4 : Chromaticity := .of 0.44018 0.40329

/-- Daylight Fluorescent. -/
def f5 : Chromaticity := .of 0.31379 0.34531

/-- Lite White Fluorescent. -/
def f6 : Chromaticity := .of 0.37790 0.38835

/-- D65 simulator, Daylight simulator. -/
def f7 : Chromaticity := .of 0.31292 0.32933

/-- D50 simulator, Sylvania F40 Design 50. -/
def f8 : Chromaticity := .of 0.34588 0.35875

/-- Cool White Deluxe Fluorescent. -/
def f9 : Chromaticity := .of 0.37417 0.37281

/-- Philips TL85, Ultralume 50. -/
def f10 : Chromaticity := .of 0.34609 0.35986

/-- Philips TL84, Ultralume 40. -/
def f11 : Chromaticity := .of 0.38052 0.37713

/-- Philips TL83, Ultralume 30. -/
def f12 : Chromaticity := .of 0.43695 0.40441

end Data.Colour.CIE.Illuminant
