import Linen.Graphics.Image.ColorSpace

/-!
Tests for `Linen.Graphics.Image.ColorSpace`: representative round-trip and
direct-conversion checks between `RGB` and each of `Y`/`HSI`/`CMYK`/`YCbCr`,
plus a couple of alpha-carrying and `Binary` conversions. Fixtures are named
with a `csTest…` prefix to avoid colliding with any of the individual
colour-space test modules' own fixtures (`Y`/`RGB`/`HSI`/`CMYK`/`YCbCr` each
already declare unprefixed names such as `y1`/`rgb1` in their own test
files).
-/

open Graphics.Image.ColorSpace
open Graphics.Image.ColorSpace.RGB (RGB RGBA PixelRGB PixelRGBA)
open Graphics.Image.ColorSpace.Y (Y YA PixelY PixelYA)
open Graphics.Image.ColorSpace.HSI (HSI HSIA PixelHSI PixelHSIA)
open Graphics.Image.ColorSpace.CMYK (CMYK PixelCMYK)
open Graphics.Image.ColorSpace.YCbCr (YCbCr PixelYCbCr)
open Graphics.Image.ColorSpace.Binary (Bit on off isOn)

-- A mid-grey and a saturated-red fixture, used throughout below.
def csTestGrey : PixelRGB Float := ⟨0.5, 0.5, 0.5⟩
def csTestRed : PixelRGB Float := ⟨0.8, 0.2, 0.2⟩

-- `RGB → Y`: BT.601 luma weights on a saturated red.
#guard eqTolPx (cs := Y) (e := Float) 0.0001
  (toPixelY (cs := RGB) (e := Float) csTestRed) (⟨0.299 * 0.8 + 0.587 * 0.2 + 0.114 * 0.2⟩ : PixelY Float)

-- `RGB → Y → RGB`: grey stays grey (Y round-trips to `R=G=B=Y`).
#guard eqTolPx (cs := RGB) (e := Float) 0.0001
  (toPixelRGB (cs := Y) (e := Float) (toPixelY (cs := RGB) (e := Float) csTestGrey)) csTestGrey

-- `RGB → HSI → RGB` round-trips within tolerance.
#guard eqTolPx (cs := RGB) (e := Float) 0.0001
  (toPixelRGB (cs := HSI) (e := Float) (toPixelHSI (cs := RGB) (e := Float) csTestRed)) csTestRed

-- A pure grey has zero saturation in `HSI`.
#guard eqTolPx (cs := HSI) (e := Float) 0.0001
  (toPixelHSI (cs := RGB) (e := Float) csTestGrey) (⟨0, 0, 0.5⟩ : PixelHSI Float)

-- `RGB → CMYK → RGB` round-trips within tolerance.
#guard eqTolPx (cs := RGB) (e := Float) 0.0001
  (toPixelRGB (cs := CMYK) (e := Float) (toPixelCMYK (cs := RGB) (e := Float) csTestRed)) csTestRed

-- Pure black converts to `K = 1` in `CMYK` (the `C`/`M`/`Y` channels are a
-- `0 / 0` indeterminate form at pure black, mirroring upstream's own
-- formula, so only `K` is checked here).
#guard (toPixelCMYK (cs := RGB) (e := Float) (⟨0, 0, 0⟩ : PixelRGB Float)).k == 1

-- `RGB → YCbCr → RGB` round-trips within tolerance.
#guard eqTolPx (cs := RGB) (e := Float) 0.0001
  (toPixelRGB (cs := YCbCr) (e := Float) (toPixelYCbCr (cs := RGB) (e := Float) csTestRed)) csTestRed

-- A mid-grey has `Cb = Cr = 0.5` in `YCbCr`.
#guard eqTolPx (cs := YCbCr) (e := Float) 0.0001
  (toPixelYCbCr (cs := RGB) (e := Float) csTestGrey) (⟨0.5, 0.5, 0.5⟩ : PixelYCbCr Float)

-- Alpha-carrying conversions: `RGBA → YA` preserves the alpha channel.
#guard eqTolPx (cs := YA) (e := Float) 0.0001
  (toPixelYA (cs := RGBA) (e := Float) (⟨0.8, 0.2, 0.2, 0.7⟩ : PixelRGBA Float))
  (⟨0.299 * 0.8 + 0.587 * 0.2 + 0.114 * 0.2, 0.7⟩ : PixelYA Float)

-- Alpha-carrying conversions: `HSIA → RGBA` preserves the alpha channel.
#guard (toPixelRGBA (cs := HSIA) (e := Float)
  (⟨0, 0, 0.5, 0.42⟩ : PixelHSIA Float)).a == 0.42

-- `toPixelBinary`/`fromPixelBinary`: an exactly-zero pixel is `on`, mapping
-- to Luma black; any nonzero pixel is `off`, mapping to Luma white.
#guard isOn (toPixelBinary (cs := RGB) (e := Float) (⟨0, 0, 0⟩ : PixelRGB Float)) == true
#guard isOn (toPixelBinary (cs := RGB) (e := Float) csTestRed) == false
#guard (fromPixelBinary (on : Graphics.Image.ColorSpace.X.PixelX Bit)).y == (0 : UInt8)
#guard (fromPixelBinary (off : Graphics.Image.ColorSpace.X.PixelX Bit)).y == (255 : UInt8)
