/-
  Linen.Graphics.Image.Processing.Complex.Fourier — 2-D fast Fourier
  transform (and its inverse) on complex-pixel images

  ## Haskell equivalent
  `Graphics.Image.Processing.Complex.Fourier` from
  https://hackage.haskell.org/package/hip (module #15 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`), on module #9's
  `Linen.Graphics.Image.ColorSpace.Complex` and module #14's
  `Linen.Graphics.Image.Processing.Geometric`. Checked directly against the
  tarball source (`hip-1.5.6.0/src/Graphics/Image/Processing/Complex/
  Fourier.hs`).

  ## The algorithm actually implemented upstream

  This is **not** a naive $O(n^2)$ double-sum DFT. Upstream's `fftGeneral`
  is a genuine radix-2, decimation-in-frequency Cooley–Tukey FFT: it
  recursively splits a length-`len` sequence (indexed by `offset`/`stride`
  into the original row) into two length-`len/2` sequences, recurses on
  each, then recombines via a butterfly (`combine`) that multiplies one half
  by a per-index *twiddle factor* (`cos α + i·sign·sin α`) and concatenates
  `evens + odds'` alongside `evens - odds'`. The base case is a length-2
  sequence (`len == 2`), combined directly by one addition and one
  subtraction. `fft2d` runs this 1-D transform along an image's columns
  (`fftGeneral`, which also transposes its own result), then again along
  what were originally the image's rows (transpose turns the second call's
  columns into the first call's rows) — the standard row-then-column
  separable construction of a 2-D FFT from two passes of a 1-D FFT. Upstream
  requires **both image dimensions to be powers of two**, checked once in
  `fft2d`, and calls `error` (this port's `panic!`) otherwise. This is
  exactly the recursive, log-depth algorithm the task brief anticipated as
  the "genuine FFT" case — so the port below gives it a real termination
  argument (see below), not a downgrade to $O(n^2)$.

  ## Termination: structural recursion on an explicit power-of-two exponent

  Upstream's `go len offset stride` recurses on `len`, halving it each step
  (`len \`div\` 2`), down to a base case at `len == 2`. Ported literally with
  `len : Int` this would need a `termination_by`/`decreasing_by` proof that
  `len / 2 < len` for `len > 1`. This port instead follows the task brief's
  alternative (b): `fftGo` recurses on an explicit `k : Nat` with the
  invariant `len = 2 ^ k`, `k ≥ 1` (upstream's `isPowerOfTwo`-checked
  dimension, log-transformed once by `fftGeneral` via `Nat.log2`). Matching
  on `k` as `0 | 1 | k' + 2` makes the recursive calls use `k' + 1`, a
  literal structural subterm of `k' + 2` — the same shape as the textbook
  two-step-down recursions (`Nat.fib`-style) Lean's equation compiler
  accepts as plain structural recursion, with **no `termination_by`/
  `decreasing_by` needed at all**. This is a faithful transcription of
  upstream's own recursion depth (`log₂ len` levels, base case at length 2),
  not a weakened algorithm: every `k' + 2` case still does exactly the
  butterfly combine (`twiddle`, `leftToRight`, pointwise `+`/`-`) upstream's
  `otherwise` branch does, and the `k == 1` case is upstream's `len == 2`
  base case verbatim.

  The `k == 0` case (`len = 1`) is *not* reachable from any valid call:
  `fftGeneral` only ever calls `fftGo` with `k = Nat.log2 n.toNat` after
  checking `n.toNat ≥ 2 ∧ 2 ^ k = n.toNat`, so `k ≥ 1` always holds at the
  top of the recursion, and every recursive step keeps `k ≥ 1` until the
  `k == 1` base case is hit directly (there is no path from `k = 1` down to
  `k = 0`; the pattern jumps straight from `k' + 2` to `k' + 1`, never
  producing `0` unless the top-level call itself passed `0`). It is filled
  in with `panic!` purely so `fftGo` is a total function, exactly the
  existing `Interface.lean`/`Geometric.lean` convention for "this upstream
  precondition failure is a programming error, not user-facing data" (see
  `checkDims`/`index`/`crop`'s own `panic!`s). Note that upstream's own
  `isPowerOfTwo 1 = True` (since `1 .&. 0 == 0`), yet `go 1 offset stride`
  itself would loop forever (`len \`div\` 2 = 0`, and the `otherwise` branch
  recurses on `0` indefinitely, never matching `len == 2`) — a latent
  non-termination bug in upstream's own acceptance check for 1×n / n×1
  images that this port does not reproduce: `fftGeneral` below explicitly
  requires `n.toNat ≥ 2` (not just "a power of two"), `panic!`ing with a
  message that says so, which is *stricter* than upstream's `isPowerOfTwo`
  but is the actual precondition the algorithm needs to terminate.

  ## Genericity: `cs` generic, `e` specialised to `Float`/`Float32`

  Upstream is generic over any component type `e` with `RealFloat e`, using
  `Applicative (Pixel cs)`-based `liftA`/`liftA2` (`cos`, `sin`, `(+:)`, `*`,
  `/`) to apply the whole FFT channel-by-channel for *any* colour space
  `cs`. Following `ColorSpace/Complex.lean`'s own established convention
  (`magnitudePx`/`cisPx`/… there, specialised to `Float`/`Float32` since
  `Interface.lean` provides no generic `Floating (Pixel cs e)`), this module
  keeps `cs` fully generic but specialises `e` to `Float` and, in a parallel
  set of `F32`-suffixed definitions, `Float32` — the same two floating
  component types every dual-precision module in this port already covers.

  ## `twiddle` — via `promote`, not a generic `Floating (Pixel cs e)`

  Upstream's `twiddle sign k n = cos alpha +: sign * sin alpha` broadcasts a
  *scalar* angle (`alpha`, depending only on `k`/`n`, not on any pixel data)
  across every channel via `Applicative (Pixel cs)`. Since the angle carries
  no per-channel information, this port computes it once as a plain
  `Complex Float` via `ColorSpace.Complex.cisOf` (`cisOf α = cos α + i·sin
  α`, and `cos`/`sin`'s evenness/oddness makes `cisOf (sign·α) = cos α +
  i·sign·sin α` exactly upstream's `twiddle`) and lifts it to a pixel with
  `ColorSpace.promote` — the same "no generic `Floating (Pixel cs e)`"
  workaround `Interface.lean`'s own doc-comment already explains, applied
  here to the one place this module would otherwise need it.

  Note that no definition below states `[Elevator (Complex Float)]`/
  `[Elevator (Complex Float32)]` explicitly, even though `promote` needs it:
  `ColorSpace cs (Complex Float) Components` already embeds exactly that
  instance as one of its own class parameters, and adding a *second*,
  separately-bound `[Elevator (Complex Float)]` hypothesis alongside it
  (redundant on paper) was found to make Lean's outParam-driven instance
  search for `ColorSpace cs (Complex Float) ?Components` fail outright at
  every call site in this file, even though the very same `ColorSpace`
  hypothesis is sitting unused in local context. Leaving `Elevator` to be
  resolved solely through the ambient `ColorSpace` instance (as every
  concrete call site does implicitly via the global `[Elevator e] →
  Elevator (Complex e)` instance from `ColorSpace/Complex.lean`) avoids the
  issue entirely; this is a Lean elaboration quirk around duplicate
  instance hypotheses of an `outParam`-indexed class, not a change in what
  is actually required mathematically.

  ## Pixel-level `+`/`-`/`*` via the per-colour-space instances

  `evens + odds'`/`evens - odds'` (images) and `twiddle * px` (pixels)
  upstream rest on the generic `Num (Pixel cs e)`/`Num (Image arr cs e)`
  instances `Interface.lean`'s doc-comment explains are *not* ported
  generically. This module instead takes `[Add pxC] [Sub pxC] [Mul pxC]` on
  the concrete complex-pixel type `pxC` directly as hypotheses — satisfied
  automatically by every colour space ported so far (`PixelY`/`PixelRGB`/…
  each declare `Add`/`Sub`/`Mul` component-wise, conditional on the matching
  instance for their component type, and `Complex Float`/`Complex Float32`
  supply exactly that via `Linen.Data.Complex`) — and uses
  `Interface.zipWith`/plain `+`/`-`/`*` at the image/pixel level instead of a
  hypothetical generic `Num (Image cs e)`.

  ## `ifft`'s `/ factor` — via a local `divComplexPx`, not a generic pixel `Div`

  Upstream's inverse transform divides every pixel by the real scalar
  `factor = fromIntegral (m * n)` through `Fractional (Pixel cs (Complex
  e))`. `Linen.Data.Complex` deliberately has no `Div` instance (dividing a
  complex number needs a nonzero-denominator side condition upstream itself
  ignores by using plain `Fractional`, i.e. IEEE-754 semantics with a
  Haskell partial-division `Fractional` instance that this port does not
  want to add generically to `Data.Complex`). Since `factor` here is always
  a real (zero-imaginary-part) `Float`, this module instead defines a small
  local `divComplexPx`, built the same way `ColorSpace/Complex.lean`'s own
  `buildPx` is: divide each channel's `Complex Float` real and imaginary
  parts by the real scalar directly. Faithful to upstream's actual values
  (dividing by a real `factor` is exactly `⟨z.re / factor, z.im / factor⟩`
  for every `z`), narrower in scope than a fully generic `Div (Complex e)`.

  ## `isPowerOfTwo` — restricted to positive `n`

  Ported as `n > 0 ∧ (n.toNat &&& (n.toNat - 1)) = 0` (`Nat` has a bitwise
  `&&&`; `Int` in Lean's core library does not, unlike Haskell's `Data.Bits
  Integer` instance upstream's `.&.` uses — see `Linen.Data.Bits`'s own
  doc-comment on this exact gap for fixed-width integers). Upstream's
  `n /= 0 && (n .&. (n-1)) == 0` is, in principle, defined for negative `n`
  too (Haskell's arbitrary-precision two's-complement `Integer`), but every
  caller here only ever passes a positive image dimension (`dims`'
  `Int × Int`, always positive per `Interface.checkDims`), so this port
  narrows the check to `n > 0` rather than reconstructing negative-`Int`
  two's-complement bit patterns nobody calls this with.

  ## `Int ↔ Float`/`Float32` conversions

  `intToFloat`/`intToFloat32` are local copies of the same helper
  `Geometric.lean` already defines (and, for the same reason that module
  gives, does not export): Lean's core library has no signed `Int → Float`
  conversion. `piD`/`piF32` are likewise local copies of `Geometric.lean`'s
  `piD` (there is no core `Float.pi`) and `Codec.Picture.Jpg.Internal.
  FastDct.piF32`'s single-precision counterpart.
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace.Complex
import Linen.Graphics.Image.Processing.Geometric

open Graphics.Image.Interface
  (Pixel ColorSpace Image dims makeImage unsafeIndex imap zipWith transpose)
open Graphics.Image.Processing.Geometric (leftToRight)
open Graphics.Image.ColorSpace.Complex (cisOf cisOfF32 buildPx)
open Data (Complex)

namespace Graphics.Image.Processing.Complex.Fourier

-- ── Direction of the transform ──

/-- Forward or inverse Fourier transform. Upstream's `Mode`. -/
private inductive Mode where
  /-- The forward transform. -/
  | forward
  /-- The inverse transform. -/
  | inverse

/-- The sign used inside the twiddle factor's exponential: `-1` for the
forward transform, `1` for the inverse. Upstream's `signOfMode`, specialised
to `Float`. -/
private def signOfMode : Mode → Float
  | .forward => -1
  | .inverse => 1

/-- `Float32` counterpart of `signOfMode`. -/
private def signOfModeF32 : Mode → Float32
  | .forward => -1
  | .inverse => 1

-- ── Power-of-two dimension check ──

/-- Check whether an `Int` is a positive power of two. Upstream's
`isPowerOfTwo` — see the module doc-comment for why this is restricted to
positive `n`. -/
def isPowerOfTwo (n : Int) : Bool :=
  n > 0 && (n.toNat &&& (n.toNat - 1)) == 0

-- ── `Int ↔ Float`/`Float32` conversions (local copies, see the module doc-comment) ──

private def intToFloat (n : Int) : Float :=
  if n < 0 then -((-n).toNat.toFloat) else n.toNat.toFloat

private def intToFloat32 (n : Int) : Float32 :=
  if n < 0 then -((-n).toNat.toFloat32) else n.toNat.toFloat32

/-- $\pi$, to `Float` (double) precision. Local copy of `Geometric.lean`'s
`piD` (see the module doc-comment). -/
private def piD : Float := 3.14159265358979323846

/-- `Float32` counterpart of `piD`. -/
private def piF32 : Float32 := 3.14159265358979323846

-- ── Twiddle factors ──

/-- The twiddle factor for index `j` of a length-`len` transform, lifted to a
pixel of colour space `cs` via `promote`. Upstream's `twiddle`, specialised
to `Float` — see the module doc-comment for why `promote` replaces a generic
`Floating (Pixel cs e)`. -/
private def twiddlePx {cs pxC ComponentsC : Type} [Pixel cs (Complex Float) pxC]
    [ColorSpace cs (Complex Float) ComponentsC]
    (sign : Float) (j len : Int) : pxC :=
  ColorSpace.promote (cs := cs) (e := Complex Float)
    (cisOf (sign * 2 * piD * intToFloat j / intToFloat len))

/-- `Float32` counterpart of `twiddlePx`. -/
private def twiddlePxF32 {cs pxC ComponentsC : Type} [Pixel cs (Complex Float32) pxC]
    [ColorSpace cs (Complex Float32) ComponentsC]
    (sign : Float32) (j len : Int) : pxC :=
  ColorSpace.promote (cs := cs) (e := Complex Float32)
    (cisOfF32 (sign * 2 * piF32 * intToFloat32 j / intToFloat32 len))

-- ── 1-D radix-2 FFT along an image's columns ──

/-- Radix-2, decimation-in-frequency FFT of length `2 ^ k` (`k ≥ 1`),
applied independently to every row of `img`, reading the current
subsequence at column indices `offset, offset + stride, offset + 2·stride,
…`. Upstream's `go`, recursing on the explicit exponent `k` in place of
`len` directly — see the module doc-comment for why this needs no
`termination_by`/`decreasing_by`, and why the `k = 0` case is unreachable
from any valid top-level call. -/
private def fftGo {cs pxC ComponentsC : Type} [Pixel cs (Complex Float) pxC] [Inhabited pxC]
    [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float) ComponentsC]
    (sign : Float) (img : Image cs (Complex Float)) (m : Int) :
    Nat → Int → Int → Image cs (Complex Float)
  | 0, _offset, _stride =>
    panic! "Graphics.Image.Processing.Complex.Fourier.fftGo: unreachable length-1 case \
      (every valid call keeps k ≥ 1, see the module doc-comment)"
  | 1, offset, stride =>
    makeImage (m, 2) (fun (i, j) =>
      if j == 0 then
        unsafeIndex img (i, offset) + unsafeIndex img (i, offset + stride)
      else
        unsafeIndex img (i, offset) - unsafeIndex img (i, offset + stride))
  | k + 2, offset, stride =>
    let len : Int := (2 : Int) ^ (k + 2)
    let evens := fftGo sign img m (k + 1) offset (stride * 2)
    let odds := fftGo sign img m (k + 1) (offset + stride) (stride * 2)
    let odds' := imap (fun (_, j) px => twiddlePx (cs := cs) sign j len * px) odds
    leftToRight (zipWith (· + ·) evens odds') (zipWith (· - ·) evens odds')

/-- `Float32` counterpart of `fftGo`. -/
private def fftGoF32 {cs pxC ComponentsC : Type} [Pixel cs (Complex Float32) pxC]
    [Inhabited pxC] [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float32) ComponentsC]
    (sign : Float32) (img : Image cs (Complex Float32)) (m : Int) :
    Nat → Int → Int → Image cs (Complex Float32)
  | 0, _offset, _stride =>
    panic! "Graphics.Image.Processing.Complex.Fourier.fftGoF32: unreachable length-1 case \
      (every valid call keeps k ≥ 1, see the module doc-comment)"
  | 1, offset, stride =>
    makeImage (m, 2) (fun (i, j) =>
      if j == 0 then
        unsafeIndex img (i, offset) + unsafeIndex img (i, offset + stride)
      else
        unsafeIndex img (i, offset) - unsafeIndex img (i, offset + stride))
  | k + 2, offset, stride =>
    let len : Int := (2 : Int) ^ (k + 2)
    let evens := fftGoF32 sign img m (k + 1) offset (stride * 2)
    let odds := fftGoF32 sign img m (k + 1) (offset + stride) (stride * 2)
    let odds' := imap (fun (_, j) px => twiddlePxF32 (cs := cs) sign j len * px) odds
    leftToRight (zipWith (· + ·) evens odds') (zipWith (· - ·) evens odds')

/-- Run `fftGo` over the whole width of `img`, then transpose the result so
a second call (on what were originally the image's rows) completes a 2-D
transform. `panic!`s (upstream's `error`, tightened — see the module
doc-comment) unless `img`'s column count is a power of two of at least 2.
Upstream's `fftGeneral`, specialised to `Float`. -/
private def fftGeneral {cs pxC ComponentsC : Type} [Pixel cs (Complex Float) pxC] [Inhabited pxC]
    [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float) ComponentsC]
    (sign : Float) (img : Image cs (Complex Float)) : Image cs (Complex Float) :=
  let (m, n) := dims img
  let k := Nat.log2 n.toNat
  if n.toNat < 2 || (2 : Nat) ^ k != n.toNat then
    panic! s!"Graphics.Image.Processing.Complex.Fourier.fft: number of columns {n} is not a \
      power of two of at least 2."
  else
    transpose (fftGo (cs := cs) sign img m k 0 1)

/-- `Float32` counterpart of `fftGeneral`. -/
private def fftGeneralF32 {cs pxC ComponentsC : Type} [Pixel cs (Complex Float32) pxC]
    [Inhabited pxC] [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float32) ComponentsC]
    (sign : Float32) (img : Image cs (Complex Float32)) : Image cs (Complex Float32) :=
  let (m, n) := dims img
  let k := Nat.log2 n.toNat
  if n.toNat < 2 || (2 : Nat) ^ k != n.toNat then
    panic! s!"Graphics.Image.Processing.Complex.Fourier.fft: number of columns {n} is not a \
      power of two of at least 2."
  else
    transpose (fftGoF32 (cs := cs) sign img m k 0 1)

-- ── 2-D transform ──

/-- Divide a complex pixel's every channel by a real scalar, channel by
channel. Local stand-in for upstream's `Fractional (Pixel cs (Complex e))`
`/` on the always-real `factor` — see the module doc-comment. -/
private def divComplexPx {cs pxC ComponentsC : Type} [Pixel cs (Complex Float) pxC]
    [ColorSpace cs (Complex Float) ComponentsC] (pz : pxC) (r : Float) : pxC :=
  buildPx (cs := cs) (e := Complex Float)
    (fun c =>
      let z := ColorSpace.getPxC (cs := cs) (e := Complex Float) pz c
      (⟨z.re / r, z.im / r⟩ : Complex Float))

/-- `Float32` counterpart of `divComplexPx`. -/
private def divComplexPxF32 {cs pxC ComponentsC : Type} [Pixel cs (Complex Float32) pxC]
    [ColorSpace cs (Complex Float32) ComponentsC] (pz : pxC) (r : Float32) : pxC :=
  buildPx (cs := cs) (e := Complex Float32)
    (fun c =>
      let z := ColorSpace.getPxC (cs := cs) (e := Complex Float32) pz c
      (⟨z.re / r, z.im / r⟩ : Complex Float32))

/-- Compute the 2-D DFT of an image (forward or inverse), by running
`fftGeneral` twice — once along columns, once (after the intervening
transpose) along what were originally the rows. `panic!`s (upstream's
`error`) unless both dimensions are powers of two. Upstream's `fft2d`,
specialised to `Float`. -/
private def fft2d {cs pxC ComponentsC : Type} [Pixel cs (Complex Float) pxC] [Inhabited pxC]
    [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float) ComponentsC]
    (mode : Mode) (img : Image cs (Complex Float)) : Image cs (Complex Float) :=
  let (m, n) := dims img
  if !(isPowerOfTwo m && isPowerOfTwo n) then
    panic! s!"Graphics.Image.Processing.Complex.Fourier.fft: array dimensions must be powers \
      of two, but the provided image has dimensions {(m, n)}."
  else
    let sign := signOfMode mode
    let transformed := fftGeneral (cs := cs) sign (fftGeneral (cs := cs) sign img)
    match mode with
    | .forward => transformed
    | .inverse =>
      let factor : Float := intToFloat (m * n)
      let scaleDown : pxC → pxC := fun pz => divComplexPx (cs := cs) pz factor
      Graphics.Image.Interface.map scaleDown transformed

/-- `Float32` counterpart of `fft2d`. -/
private def fft2dF32 {cs pxC ComponentsC : Type} [Pixel cs (Complex Float32) pxC]
    [Inhabited pxC] [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float32) ComponentsC]
    (mode : Mode) (img : Image cs (Complex Float32)) : Image cs (Complex Float32) :=
  let (m, n) := dims img
  if !(isPowerOfTwo m && isPowerOfTwo n) then
    panic! s!"Graphics.Image.Processing.Complex.Fourier.fft: array dimensions must be powers \
      of two, but the provided image has dimensions {(m, n)}."
  else
    let sign := signOfModeF32 mode
    let transformed := fftGeneralF32 (cs := cs) sign (fftGeneralF32 (cs := cs) sign img)
    match mode with
    | .forward => transformed
    | .inverse =>
      let factor : Float32 := intToFloat32 (m * n)
      let scaleDown : pxC → pxC := fun pz => divComplexPxF32 (cs := cs) pz factor
      Graphics.Image.Interface.map scaleDown transformed

-- ── Public API ──

/-- Fast Fourier Transform of an image whose dimensions are both powers of
two (of at least 2), `panic!`ing (upstream's `error`) otherwise. Upstream's
`fft`, specialised to `Float`.

```
#guard isPowerOfTwo 4 && isPowerOfTwo 2 && !isPowerOfTwo 3
```
-/
def fft {cs pxC ComponentsC : Type} [Pixel cs (Complex Float) pxC] [Inhabited pxC]
    [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float) ComponentsC]
    (img : Image cs (Complex Float)) : Image cs (Complex Float) :=
  fft2d (cs := cs) .forward img

/-- Inverse Fast Fourier Transform, the two-sided inverse of `fft` (up to
floating-point error). Upstream's `ifft`, specialised to `Float`. -/
def ifft {cs pxC ComponentsC : Type} [Pixel cs (Complex Float) pxC] [Inhabited pxC]
    [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float) ComponentsC]
    (img : Image cs (Complex Float)) : Image cs (Complex Float) :=
  fft2d (cs := cs) .inverse img

/-- `Float32` counterpart of `fft`. -/
def fftF32 {cs pxC ComponentsC : Type} [Pixel cs (Complex Float32) pxC] [Inhabited pxC]
    [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float32) ComponentsC]
    (img : Image cs (Complex Float32)) : Image cs (Complex Float32) :=
  fft2dF32 (cs := cs) .forward img

/-- `Float32` counterpart of `ifft`. -/
def ifftF32 {cs pxC ComponentsC : Type} [Pixel cs (Complex Float32) pxC] [Inhabited pxC]
    [Add pxC] [Sub pxC] [Mul pxC] [ColorSpace cs (Complex Float32) ComponentsC]
    (img : Image cs (Complex Float32)) : Image cs (Complex Float32) :=
  fft2dF32 (cs := cs) .inverse img

end Graphics.Image.Processing.Complex.Fourier
