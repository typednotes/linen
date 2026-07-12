/-
  Tests for `Linen.Codec.Picture.ColorQuant` — checks `bitDiv3`, `dist2Px`,
  `nearestColorIdx`, the `rgbIntPack`/`rgbIntUnpack` round trip, `dither`,
  `isColorCountBelow`, and `palettize` under both the median-mean-cut and
  uniform algorithms.
-/
import Linen.Codec.Picture.ColorQuant

open Codec.Picture

-- ── `bitDiv3` ──

#guard bitDiv3 8 == (1, 1, 1)
#guard bitDiv3 2 == (1, 0, 0)
#guard bitDiv3 256 == (3, 3, 2)

-- ── `dist2Px` ──

#guard dist2Px ⟨1, 2, 3⟩ ⟨1, 2, 3⟩ == 0
#guard dist2Px ⟨0, 0, 0⟩ ⟨255, 255, 255⟩ == 195075

-- ── `nearestColorIdx` ──

#guard nearestColorIdx ⟨10, 10, 10⟩ #[⟨0, 0, 0⟩, ⟨255, 255, 255⟩] == 0
#guard nearestColorIdx ⟨250, 250, 250⟩ #[⟨0, 0, 0⟩, ⟨255, 255, 255⟩] == 1
#guard nearestColorIdx ⟨1, 2, 3⟩ #[⟨9, 9, 9⟩, ⟨1, 2, 3⟩, ⟨5, 5, 5⟩] == 1

-- ── `rgbIntPack`/`rgbIntUnpack` round trip ──

#guard rgbIntUnpack (rgbIntPack ⟨12, 34, 56⟩) == (⟨12, 34, 56⟩ : PixelRGB8)
#guard rgbIntUnpack (rgbIntPack ⟨255, 0, 128⟩) == (⟨255, 0, 128⟩ : PixelRGB8)

-- ── `dither` ──

-- at the origin, none of the three magic-number offsets have bit 4 set, so
-- the pixel passes through unchanged
#guard dither 0 0 ⟨10, 20, 30⟩ == (⟨10, 20, 30⟩ : PixelRGB8)

-- ── `isColorCountBelow` ──

def twoColorImg : Image PixelRGB8 :=
  generateImage (fun x _ => if x == 0 then (⟨255, 0, 0⟩ : PixelRGB8) else ⟨0, 255, 0⟩) 2 1

#guard (isColorCountBelow 4 twoColorImg).2
#guard (isColorCountBelow 4 twoColorImg).1.size' == 2
#guard !(isColorCountBelow 1 twoColorImg).2

-- ── `palettize`: median-mean-cut, below the colour-count threshold ──

def threeColorImg : Image PixelRGB8 :=
  generateImage
    (fun x _ => if x == 0 then (⟨255, 0, 0⟩ : PixelRGB8) else if x == 1 then ⟨0, 255, 0⟩ else ⟨0, 0, 255⟩)
    3 1

def threeColorResult := palettize defaultPaletteOptions threeColorImg

-- every original colour survives exactly (the palette has room for all
-- three, so `isColorCountBelow` shortcuts straight to an exact palette)
#guard threeColorResult.2.getPixel (threeColorResult.1.getPixel 0 0).toNat 0 == (⟨255, 0, 0⟩ : PixelRGB8)
#guard threeColorResult.2.getPixel (threeColorResult.1.getPixel 1 0).toNat 0 == (⟨0, 255, 0⟩ : PixelRGB8)
#guard threeColorResult.2.getPixel (threeColorResult.1.getPixel 2 0).toNat 0 == (⟨0, 0, 255⟩ : PixelRGB8)

-- ── `palettize`: median-mean-cut, above the colour-count threshold ──

-- `initCluster` only samples every 9th pixel (a 3×3 subsampling factor), so
-- the image needs enough pixels for that sampling to see more than one
-- colour; 5 colours cycled over 30 pixels is comfortably above that floor.
def colors5 : Array PixelRGB8 :=
  #[⟨255, 0, 0⟩, ⟨0, 255, 0⟩, ⟨0, 0, 255⟩, ⟨255, 255, 0⟩, ⟨0, 255, 255⟩]

def fiveColorImg : Image PixelRGB8 :=
  generateImage (fun x _ => colors5[x % 5]!) 30 1

def cutResult := palettize
    { paletteCreationMethod := .medianMeanCut, enableImageDithering := false, paletteColorCount := 2 }
    fiveColorImg

-- the palette is cut down to exactly `paletteColorCount` entries
#guard cutResult.2.width == 2
-- every pixel's index is a valid entry in that palette
#guard (List.range 30).all fun x => (cutResult.1.getPixel x 0).toNat < 2

-- ── `palettize`: uniform quantization ──

def uniformResult := palettize
    { paletteCreationMethod := .uniform, enableImageDithering := false, paletteColorCount := 8 }
    fiveColorImg

-- 8 colours, 3 bits, evenly split (1 bit per channel) → 2³ = 8 palette entries
#guard uniformResult.2.width == 8

-- (200, 10, 5) masks down to (128, 0, 0), the 5th (index 4) of the 8
-- lexicographically-enumerated (r, g, b) ∈ {0, 128}³ combinations
def uniformSample : Image PixelRGB8 := generateImage (fun _ _ => (⟨200, 10, 5⟩ : PixelRGB8)) 1 1
def uniformSampleResult := palettize
    { paletteCreationMethod := .uniform, enableImageDithering := false, paletteColorCount := 8 }
    uniformSample
#guard uniformSampleResult.1.getPixel 0 0 == 4
