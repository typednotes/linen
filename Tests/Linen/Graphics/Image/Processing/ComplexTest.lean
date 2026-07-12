/-
  Tests for `Linen.Graphics.Image.Processing.Complex` — the whole-image
  complex-pixel wrappers (`mkComplexImg`, `realPartImg`, `imagPartImg`,
  `conjugateImg`, `magnitudeImg`, `phaseImg`, `polarImg`, `mkPolarImg`,
  `cisImg`, and their `Float32` counterparts) plus a sanity check that
  `fft`/`ifft` remain reachable through this module's transitive import of
  `Processing.Complex.Fourier`.

  Fixture/example names are prefixed `pcx` (Processing.Complex) to avoid
  clashing with any other test file's identifiers in the shared `Tests`
  namespace (in particular `Tests.Linen.Graphics.Image.Processing.Complex.
  FourierTest`'s `fourier`-prefixed fixtures and
  `Tests.Linen.Graphics.Image.ColorSpace.YTest`'s `Y`/`PixelY`).
-/
import Linen.Graphics.Image.Processing.Complex
import Linen.Graphics.Image.ColorSpace.Y
import Linen.Graphics.Image.ColorSpace.Complex

open Graphics.Image.Interface (makeImage)
open Graphics.Image.Processing.Complex
open Graphics.Image.Processing.Complex.Fourier (fft ifft fftF32 ifftF32)
open Graphics.Image.ColorSpace.Y (Y PixelY)
open Data (Complex)

-- ── Fixtures ──
-- All images below are `1×1` (a single pixel) purely to keep every `elems`
-- array a one-element literal; none of the operations under test are
-- dimension-sensitive (they are `Interface.map`/`zipWith`, applied pixel by
-- pixel, unlike `Fourier.lean`'s `fft`/`ifft` which do need a power-of-two
-- image and are exercised on `2×2` fixtures below instead).

/-- A single-pixel real-valued image, value `c`. -/
private def pcxRealImg (c : Float) : Graphics.Image.Interface.Image Y Float :=
  makeImage (1, 1) (fun _ => (PixelY.mk c : PixelY Float))

/-- A single-pixel complex image, `⟨re, im⟩`. -/
private def pcxComplexImg (re im : Float) : Graphics.Image.Interface.Image Y (Complex Float) :=
  makeImage (1, 1) (fun _ => (PixelY.mk (⟨re, im⟩ : Complex Float) : PixelY (Complex Float)))

-- ── Rectangular form: `mkComplexImg`, `realPartImg`, `imagPartImg` ──

#guard (mkComplexImg (cs := Y) (pcxRealImg 3.0) (pcxRealImg 4.0)).elems ==
  #[(PixelY.mk (⟨3.0, 4.0⟩ : Complex Float) : PixelY (Complex Float))]

#guard (realPartImg (cs := Y) (pcxComplexImg 3.0 4.0)).elems == #[(PixelY.mk 3.0 : PixelY Float)]

#guard (imagPartImg (cs := Y) (pcxComplexImg 3.0 4.0)).elems == #[(PixelY.mk 4.0 : PixelY Float)]

-- ── Conjugate ──

#guard (conjugateImg (cs := Y) (pcxComplexImg 3.0 4.0)).elems ==
  #[(PixelY.mk (⟨3.0, -4.0⟩ : Complex Float) : PixelY (Complex Float))]

-- `conjugateImg` is its own inverse.
#guard (conjugateImg (cs := Y) (conjugateImg (cs := Y) (pcxComplexImg 3.0 4.0))).elems ==
  (pcxComplexImg 3.0 4.0).elems

-- ── Polar form (double precision) ──

-- `3-4-5` right triangle: magnitude 5.
#guard (magnitudeImg (cs := Y) (pcxComplexImg 3.0 4.0)).elems == #[(PixelY.mk 5.0 : PixelY Float)]

#guard (phaseImg (cs := Y) (pcxComplexImg 0.0 0.0)).elems == #[(PixelY.mk 0.0 : PixelY Float)]

#guard (polarImg (cs := Y) (pcxComplexImg 3.0 4.0)).1.elems ==
  (magnitudeImg (cs := Y) (pcxComplexImg 3.0 4.0)).elems
#guard (polarImg (cs := Y) (pcxComplexImg 3.0 4.0)).2.elems ==
  (phaseImg (cs := Y) (pcxComplexImg 3.0 4.0)).elems

-- `mkPolarImg`/`cisImg` round-trip a magnitude-1 image back to `cis θ`.
#guard (cisImg (cs := Y) (pcxRealImg 0.0)).elems ==
  #[(PixelY.mk (⟨1.0, 0.0⟩ : Complex Float) : PixelY (Complex Float))]

#guard (mkPolarImg (cs := Y) (pcxRealImg 1.0) (pcxRealImg 0.0)).elems ==
  (cisImg (cs := Y) (pcxRealImg 0.0)).elems

-- ── `Float32` counterparts ──

private def pcxRealImgF32 (c : Float32) : Graphics.Image.Interface.Image Y Float32 :=
  makeImage (1, 1) (fun _ => (PixelY.mk c : PixelY Float32))

private def pcxComplexImgF32 (re im : Float32) :
    Graphics.Image.Interface.Image Y (Complex Float32) :=
  makeImage (1, 1) (fun _ => (PixelY.mk (⟨re, im⟩ : Complex Float32) : PixelY (Complex Float32)))

#guard (magnitudeImgF32 (cs := Y) (pcxComplexImgF32 3.0 4.0)).elems ==
  #[(PixelY.mk (5.0 : Float32) : PixelY Float32)]

#guard (mkPolarImgF32 (cs := Y) (pcxRealImgF32 1.0) (pcxRealImgF32 0.0)).elems ==
  (cisImgF32 (cs := Y) (pcxRealImgF32 0.0)).elems

-- ── `fft`/`ifft` remain reachable via this module's transitive import ──
-- (Both dimensions must be powers of two of at least 2, so `2×2` fixtures
-- are used here instead of the `1×1` ones above.)

private def pcxComplexImg2 (re im : Float) : Graphics.Image.Interface.Image Y (Complex Float) :=
  makeImage (2, 2) (fun _ => (PixelY.mk (⟨re, im⟩ : Complex Float) : PixelY (Complex Float)))

private def pcxComplexImg2F32 (re im : Float32) :
    Graphics.Image.Interface.Image Y (Complex Float32) :=
  makeImage (2, 2) (fun _ => (PixelY.mk (⟨re, im⟩ : Complex Float32) : PixelY (Complex Float32)))

#guard (ifft (cs := Y) (fft (cs := Y) (pcxComplexImg2 3.0 4.0))).elems ==
  (pcxComplexImg2 3.0 4.0).elems

#guard (ifftF32 (cs := Y) (fftF32 (cs := Y) (pcxComplexImg2F32 3.0 4.0))).elems ==
  (pcxComplexImg2F32 3.0 4.0).elems
