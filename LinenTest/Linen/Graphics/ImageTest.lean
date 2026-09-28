/-
  Tests for `Linen.Graphics.Image` — the top-level public facade of the
  whole `hip` library.

  Two groups of tests: (1) smoke tests confirming a representative call into
  each re-exported sub-module (`ColorSpace`, `IO`, `Interface`, `Types`,
  `Processing`, `Processing.Binary`, `Processing.Complex`/`Complex.Fourier`)
  is reachable through nothing but `import Linen.Graphics.Image` — these
  sub-modules' own test files already cover their behaviour in depth; (2)
  direct tests of the handful of genuine top-level definitions this module
  adds itself (`rows`/`cols`/`sum`/`product`/`maximum`/`minimum`/`normalize`/
  `eqTol`/`toLists`).

  Fixture names are prefixed `img` to avoid clashing with any other test
  file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image

open Graphics.Image
open Graphics.Image.Interface (Border fromLists unsafeIndex dims makeImage)
open Graphics.Image.Processing.Geometric (flipV)
open Graphics.Image.Processing.Filter (meanFilter)
open Graphics.Image.Processing.Binary (dilate erode)
open Graphics.Image.Processing.Complex (mkComplexImg realPartImg)
open Graphics.Image.Processing.Complex.Fourier (fft ifft)
open Graphics.Image.ColorSpace.Y (Y PixelY)
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.ColorSpace.Binary (Bit on off zero one)
open Graphics.Image.Types (Image)

-- ── A small fixture image, reused across the smoke tests below ──

def imgStep : Image Y Float :=
  fromLists [[⟨0⟩, ⟨1⟩, ⟨2⟩], [⟨3⟩, ⟨4⟩, ⟨5⟩], [⟨6⟩, ⟨7⟩, ⟨8⟩]]

-- ── Smoke test: `Processing` (transitively `Geometric`) reachable ──

#guard dims (flipV imgStep) == (3, 3)
#guard unsafeIndex (flipV imgStep) (0, 0) == unsafeIndex imgStep (2, 0)

-- ── Smoke test: `Processing.Filter` reachable ──

def imgConst5 : Image Y Float :=
  fromLists [[⟨5⟩, ⟨5⟩, ⟨5⟩], [⟨5⟩, ⟨5⟩, ⟨5⟩], [⟨5⟩, ⟨5⟩, ⟨5⟩]]

#guard (meanFilter (cs := Y) (e := Float) Border.edge).applyFilter imgConst5 == imgConst5

-- ── Smoke test: `Processing.Binary` reachable ──

def imgBinCross : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Bit) :=
  fromLists [[off, on, off], [on, on, on], [off, on, off]]

def imgBinCenter : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 (PixelX Bit) :=
  fromLists [[off, off, off], [off, on, off], [off, off, off]]

#guard dilate imgBinCross imgBinCenter == imgBinCross
#guard erode imgBinCross imgBinCross == imgBinCenter

-- ── Smoke test: `Processing.Complex`/`Complex.Fourier` reachable ──

def imgReal : Image Y Float :=
  fromLists [[⟨1⟩, ⟨2⟩], [⟨3⟩, ⟨4⟩]]

def imgZero : Image Y Float :=
  fromLists [[⟨0⟩, ⟨0⟩], [⟨0⟩, ⟨0⟩]]

#guard (realPartImg (cs := Y) (ifft (cs := Y) (fft (cs := Y) (mkComplexImg (cs := Y) imgReal imgZero)))).elems ==
  imgReal.elems

-- ── `Types`/`ColorSpace` reachable (already exercised above via
-- `Image`/pixel construction and colour-space `open`s); `IO` reachable via
-- its format-enumeration helpers ──

#guard Graphics.Image.IO.allInputFormats.length == 8

-- ── `rows`/`cols` ──

#guard rows imgStep == 3
#guard cols imgStep == 3

-- ── `sum`/`product` (see the module doc-comment for `product`'s literal
-- `fold (+) 1` transcription of upstream's own bug) ──

#guard sum imgStep == (⟨36⟩ : PixelY Float)
#guard product imgStep == (⟨37⟩ : PixelY Float)

-- ── `maximum`/`minimum` — need `[Max px]`/`[Min px]` (see the module
-- doc-comment: no colour-space pixel type in this port carries such an
-- instance yet), so exercised directly against a raw `Float`-pixel image
-- rather than through a `Pixel cs e px` instance ──

def imgFloats : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 Float :=
  fromLists [[0, 1, 2], [3, 4, 5], [6, 7, 8]]

#guard maximum imgFloats == 8
#guard minimum imgFloats == 0

-- ── `normalize` ──

#guard normalize (cs := Y) (e := Float) imgStep ==
  fromLists
    [ [(⟨0⟩ : PixelY Float), ⟨1/8⟩, ⟨2/8⟩]
    , [⟨3/8⟩, ⟨4/8⟩, ⟨5/8⟩]
    , [⟨6/8⟩, ⟨7/8⟩, (⟨1⟩ : PixelY Float)] ]

#guard normalize (cs := Y) (e := Float) imgConst5 == imgConst5

-- ── `eqTol` ──

#guard eqTol (cs := Y) (e := Float) 0.0 imgStep imgStep == true
#guard eqTol (cs := Y) (e := Float) 0.0 imgStep imgConst5 == false
#guard eqTol (cs := Y) (e := Float) 100.0 imgStep imgConst5 == true

-- ── `toLists` (inverse of `Interface.fromLists`) ──

#guard toLists imgStep ==
  [ [(⟨0⟩ : PixelY Float), ⟨1⟩, ⟨2⟩]
  , [⟨3⟩, ⟨4⟩, ⟨5⟩]
  , [⟨6⟩, ⟨7⟩, ⟨8⟩] ]
