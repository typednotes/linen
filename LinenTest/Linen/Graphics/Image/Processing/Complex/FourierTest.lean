/-
  Tests for `Linen.Graphics.Image.Processing.Complex.Fourier` — `fft`/`ifft`
  (and their `Float32` counterparts) and `isPowerOfTwo`.

  All image fixtures here use dimensions that are small powers of two (`2×2`
  and `4×4`), chosen so that every value below is exact in floating point
  (no `cos`/`sin`-rounding tolerance needed): a `2×2` transform never invokes
  a twiddle factor at all (its only combine step is the length-2 base case,
  pure addition/subtraction), and the `4×4` fixtures below only ever combine
  values that are `0` or equal to each other, so every twiddle multiplication
  either multiplies by `0` or contributes to a sum that upstream's own
  algorithm guarantees is exactly the (real, integer-valued) input scaled by
  a power of two — confirmed numerically (via `#eval`) before being written
  here as `#guard`s.

  Fixture/example names are prefixed `fourier` to avoid clashing with any
  other test file's identifiers in the shared `Tests` namespace (in
  particular `Tests.Linen.Graphics.Image.ColorSpace.YTest`, whose `Y`/
  `PixelY` this file also builds on top of).
-/
import Linen.Graphics.Image.Processing.Complex.Fourier
import Linen.Graphics.Image.ColorSpace.Y
import Linen.Graphics.Image.ColorSpace.Complex

open Graphics.Image.Interface (makeImage)
open Graphics.Image.Processing.Complex.Fourier
open Graphics.Image.ColorSpace.Y (Y PixelY)
open Graphics.Image.ColorSpace.Complex (magnitudeOf)
open Data (Complex)

-- ── `isPowerOfTwo` ──

#guard isPowerOfTwo 1
#guard isPowerOfTwo 2
#guard isPowerOfTwo 4
#guard isPowerOfTwo 1024
#guard !isPowerOfTwo 0
#guard !isPowerOfTwo 3
#guard !isPowerOfTwo 6

-- ── Fixtures: real-valued (zero imaginary part) pixels ──

private def fourierReal (r : Float) : Complex Float := ⟨r, 0⟩

/-- A `2×2` image of a single constant real value. -/
private def fourierConst2 (c : Float) : Graphics.Image.Interface.Image Y (Complex Float) :=
  makeImage (2, 2) (fun _ => (PixelY.mk (fourierReal c) : PixelY (Complex Float)))

/-- A `2×2` image with a single nonzero pixel at `(0, 0)`. -/
private def fourierImpulse2 (c : Float) : Graphics.Image.Interface.Image Y (Complex Float) :=
  makeImage (2, 2)
    (fun (i, j) =>
      (PixelY.mk (fourierReal (if i == 0 && j == 0 then c else 0)) : PixelY (Complex Float)))

/-- A `4×4` image of a single constant real value. -/
private def fourierConst4 (c : Float) : Graphics.Image.Interface.Image Y (Complex Float) :=
  makeImage (4, 4) (fun _ => (PixelY.mk (fourierReal c) : PixelY (Complex Float)))

/-- A `4×4` image with a single nonzero pixel at `(0, 0)`. -/
private def fourierImpulse4 (c : Float) : Graphics.Image.Interface.Image Y (Complex Float) :=
  makeImage (4, 4)
    (fun (i, j) =>
      (PixelY.mk (fourierReal (if i == 0 && j == 0 then c else 0)) : PixelY (Complex Float)))

-- ── A constant image's DFT is a single DC spike ──

#guard (fft (cs := Y) (fourierConst2 3.0)).elems ==
  #[(PixelY.mk (fourierReal 12.0) : PixelY (Complex Float)), PixelY.mk (fourierReal 0.0),
    PixelY.mk (fourierReal 0.0), PixelY.mk (fourierReal 0.0)]

#guard (fft (cs := Y) (fourierConst4 3.0)).elems.map (fun px => magnitudeOf px.y) ==
  #[48.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

-- ── A single-impulse image's DFT is a constant-magnitude (flat) image ──

#guard (fft (cs := Y) (fourierImpulse2 3.0)).elems ==
  #[(PixelY.mk (fourierReal 3.0) : PixelY (Complex Float)), PixelY.mk (fourierReal 3.0),
    PixelY.mk (fourierReal 3.0), PixelY.mk (fourierReal 3.0)]

#guard (fft (cs := Y) (fourierImpulse4 3.0)).elems.map (fun px => magnitudeOf px.y) ==
  #[3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0]

-- ── `ifft` inverts `fft` (up to no floating-point error at all, for these
-- exact fixtures) ──

#guard (ifft (cs := Y) (fft (cs := Y) (fourierConst2 3.0))).elems == (fourierConst2 3.0).elems
#guard (ifft (cs := Y) (fft (cs := Y) (fourierImpulse2 3.0))).elems == (fourierImpulse2 3.0).elems
#guard (fft (cs := Y) (ifft (cs := Y) (fourierConst2 3.0))).elems == (fourierConst2 3.0).elems

-- ── `Float32` counterparts ──

private def fourierRealF32 (r : Float32) : Complex Float32 := ⟨r, 0⟩

private def fourierConst2F32 (c : Float32) : Graphics.Image.Interface.Image Y (Complex Float32) :=
  makeImage (2, 2) (fun _ => (PixelY.mk (fourierRealF32 c) : PixelY (Complex Float32)))

#guard (fftF32 (cs := Y) (fourierConst2F32 3.0)).elems ==
  #[(PixelY.mk (fourierRealF32 12.0) : PixelY (Complex Float32)), PixelY.mk (fourierRealF32 0.0),
    PixelY.mk (fourierRealF32 0.0), PixelY.mk (fourierRealF32 0.0)]

#guard (ifftF32 (cs := Y) (fftF32 (cs := Y) (fourierConst2F32 3.0))).elems ==
  (fourierConst2F32 3.0).elems
