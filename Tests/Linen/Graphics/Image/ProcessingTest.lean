/-
  Tests for `Linen.Graphics.Image.Processing` — the processing facade
  (`Geometric`/`Interpolation`/`Convolution`/`Filter` re-exports, plus its own
  `pixelGrid`).

  Fixture names are prefixed `procFacade` to avoid clashing with any of the
  individual `Processing.*` submodules' own test files (`GeometricTest`,
  `InterpolationTest`, `ConvolutionTest`, `FilterTest`), each of which already
  declares its own unprefixed fixtures in the shared `Tests` namespace. These
  are smoke tests only, confirming a representative call into each
  re-exported submodule is reachable through this one `import`; the
  submodules' own test files already cover their behaviour in depth.
-/
import Linen.Graphics.Image.Processing
import Linen.Graphics.Image.ColorSpace.Y

open Graphics.Image.Interface (Border fromLists unsafeIndex dims)
open Graphics.Image.Processing
open Graphics.Image.Processing.Geometric (flipV crop resize)
open Graphics.Image.Processing.Interpolation (Nearest)
open Graphics.Image.Processing.Convolution (correlate)
open Graphics.Image.Processing.Filter (meanFilter)
open Graphics.Image.ColorSpace.Y (Y PixelY)

-- A 3×3 step image, reused across every smoke test below.
def procFacadeImg : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists [[⟨0⟩, ⟨1⟩, ⟨2⟩], [⟨3⟩, ⟨4⟩, ⟨5⟩], [⟨6⟩, ⟨7⟩, ⟨8⟩]]

-- ── `Geometric` re-export: `flipV`/`crop` are reachable ──

#guard flipV procFacadeImg == fromLists [[⟨6⟩, ⟨7⟩, ⟨8⟩], [⟨3⟩, ⟨4⟩, ⟨5⟩], [⟨0⟩, ⟨1⟩, ⟨2⟩]]
#guard crop (1, 1) (2, 2) procFacadeImg == fromLists [[⟨4⟩, ⟨5⟩], [⟨7⟩, ⟨8⟩]]

-- ── `Interpolation` re-export: `Nearest` is reachable and usable via `resize` ──

#guard (resize (cs := Y) (e := Float) Nearest.nearest Border.edge (6, 6) procFacadeImg).elems.size == 36

-- ── `Convolution` re-export: `correlate` with an identity kernel is a no-op ──

def procFacadeIdKernel : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2
    (Graphics.Image.ColorSpace.X.PixelX Float) :=
  fromLists [[⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨1⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩]]

#guard correlate (cs := Y) (e := Float) Border.edge procFacadeIdKernel procFacadeImg == procFacadeImg

-- ── `Filter` re-export: `meanFilter` on a constant image is a no-op ──

def procFacadeConst : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelY Float) :=
  fromLists (List.replicate 3 (List.replicate 3 (⟨2⟩ : PixelY Float)))

#guard (meanFilter (cs := Y) (e := Float) Border.edge).applyFilter procFacadeConst == procFacadeConst

-- ── This module's own contribution: `pixelGrid` ──

-- Magnification factor `1` yields `succ 1 = 2`: a 1×1 image maps to a 3×3
-- grid image (`1 + 1*2` on each dimension).
#guard dims (pixelGrid (cs := Y) (e := Float) 1 (fromLists [[(⟨9⟩ : PixelY Float)]])) == (3, 3)

-- The original pixel survives at the grid-aligned interior position `(1, 1)`.
#guard unsafeIndex (pixelGrid (cs := Y) (e := Float) 1 (fromLists [[(⟨9⟩ : PixelY Float)]])) (1, 1)
  == (⟨9⟩ : PixelY Float)

-- Every border row/column (`i = 0` or `j = 0`) is the mid-grey grid line.
#guard unsafeIndex (pixelGrid (cs := Y) (e := Float) 1 (fromLists [[(⟨9⟩ : PixelY Float)]])) (0, 0)
  == (⟨0.5⟩ : PixelY Float)
