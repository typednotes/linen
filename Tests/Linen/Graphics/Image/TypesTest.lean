/-
  Tests for `Linen.Graphics.Image.Types` — the package-level re-export
  facade. Since upstream's own `Types.hs` turned out (per that module's own
  doc-comment) to contain no concrete `RGBImage`-style type aliases, these
  tests instead confirm the facade actually does its one real job: a single
  `import Linen.Graphics.Image.Types` is enough to construct an `Image cs e`
  for every already-ported colour space, and to reach `Border` too, with no
  further per-colour-space import needed.

  Fixture names are prefixed `imgTypes` to avoid clashing with any other
  test file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.Types

open Graphics.Image.Types (Image Border)
open Graphics.Image.Interface (makeImage dims unsafeIndex handleBorderIndex)
open Graphics.Image.ColorSpace.Y (Y PixelY)
open Graphics.Image.ColorSpace.RGB (RGB PixelRGB)
open Graphics.Image.ColorSpace.HSI (HSI PixelHSI)
open Graphics.Image.ColorSpace.CMYK (CMYK PixelCMYK)
open Graphics.Image.ColorSpace.YCbCr (YCbCr PixelYCbCr)
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.ColorSpace.Binary (Bit on off isOn)
open Data (Complex)

-- ── `Image Y Float` — single-channel luma ──

def imgTypesY : Image Y Float :=
  makeImage (1, 2) (fun (_, j) => (⟨if j == 0 then 0.25 else 0.75⟩ : PixelY Float))

#guard dims imgTypesY == (1, 2)
#guard (unsafeIndex imgTypesY (0, 1)).y == 0.75

-- ── `Image RGB Float` — three-channel colour ──

def imgTypesRGB : Image RGB Float :=
  makeImage (1, 1) (fun _ => (⟨0.1, 0.2, 0.3⟩ : PixelRGB Float))

#guard dims imgTypesRGB == (1, 1)
#guard (unsafeIndex imgTypesRGB (0, 0)).g == 0.2

-- ── `Image HSI Float` — hue/saturation/intensity ──

def imgTypesHSI : Image HSI Float :=
  makeImage (1, 1) (fun _ => (⟨0.0, 0.5, 0.9⟩ : PixelHSI Float))

#guard dims imgTypesHSI == (1, 1)
#guard (unsafeIndex imgTypesHSI (0, 0)).i == 0.9

-- ── `Image CMYK Float` — cyan/magenta/yellow/black ──

def imgTypesCMYK : Image CMYK Float :=
  makeImage (1, 1) (fun _ => (⟨0.1, 0.2, 0.3, 0.4⟩ : PixelCMYK Float))

#guard dims imgTypesCMYK == (1, 1)
#guard (unsafeIndex imgTypesCMYK (0, 0)).k == 0.4

-- ── `Image YCbCr Float` — luma/chroma ──

def imgTypesYCbCr : Image YCbCr Float :=
  makeImage (1, 1) (fun _ => (⟨0.5, -0.2, 0.1⟩ : PixelYCbCr Float))

#guard dims imgTypesYCbCr == (1, 1)
#guard (unsafeIndex imgTypesYCbCr (0, 0)).cb == -0.2

-- ── `Image X (Complex Float)` — the carrier `Complex.lean` builds on ──

def imgTypesComplex : Image X (Complex Float) :=
  makeImage (1, 1) (fun _ => (⟨⟨1.0, 2.0⟩⟩ : PixelX (Complex Float)))

#guard dims imgTypesComplex == (1, 1)
#guard (unsafeIndex imgTypesComplex (0, 0)).x.re == 1.0
#guard (unsafeIndex imgTypesComplex (0, 0)).x.im == 2.0

-- ── `Image X Bit` — binary pixels ──

def imgTypesBinary : Image X Bit :=
  makeImage (1, 2) (fun (_, j) => if j == 0 then on else off)

#guard dims imgTypesBinary == (1, 2)
#guard isOn (unsafeIndex imgTypesBinary (0, 0)) == true
#guard isOn (unsafeIndex imgTypesBinary (0, 1)) == false

-- ── `Border`, re-exported alongside `Image` ──

#guard handleBorderIndex (Border.fill (⟨0.0⟩ : PixelY Float)) (dims imgTypesY)
    (unsafeIndex imgTypesY) (5, 5) == (⟨0.0⟩ : PixelY Float)
#guard handleBorderIndex (px := PixelY Float) .edge (dims imgTypesY)
    (unsafeIndex imgTypesY) (5, 5) == unsafeIndex imgTypesY (0, 1)
