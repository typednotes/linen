/-
  Linen.Graphics.Image.Processing.Complex — whole-image complex-pixel
  operations, plus the `fft`/`ifft` re-export

  ## Haskell equivalent
  `Graphics.Image.Processing.Complex` from
  https://hackage.haskell.org/package/hip (module #16 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`), on module #9's
  `Linen.Graphics.Image.ColorSpace.Complex` and module #15's
  `Linen.Graphics.Image.Processing.Complex.Fourier`. Checked directly against
  the tarball source (`hip-1.5.6.0/src/Graphics/Image/Processing/Complex.hs`),
  fetched because `raw.githubusercontent.com/lehins/hip/master/…` 404s (the
  repository's default branch layout has moved on since the 1.5.6.0 release).

  ## What upstream actually is

  A small facade, exactly as the task brief anticipated: its entire body is
  nine one-line definitions (`(!+!)`, `realPartI`, `imagPartI`, `mkPolarI`,
  `cisI`, `polarI`, `magnitudeI`, `phaseI`, `conjugateI`) that lift module
  #9's *pixel*-level complex operations to *image* level via `Applicative
  (Pixel cs)`-based `map`/`zipWith`, plus a bare re-export of module #15's
  `fft`/`ifft` (upstream's own export list literally ends in `fft, ifft` with
  no new code — the whole module contributes nothing to the FFT itself
  beyond re-exposing it alongside the pixel-lift wrappers). There is no
  image-level convolution-theorem `applyFilter` or similar: that guess from
  the task brief does not match the real source.

  ## Re-export strategy for `fft`/`ifft`

  Following `Linen.Graphics.Image.ColorSpace.lean`'s own established
  convention (see its doc-comment): Lean's `import` is already transitive, so
  `import Linen.Graphics.Image.Processing.Complex.Fourier` below already
  makes `Graphics.Image.Processing.Complex.Fourier.fft`/`.ifft` (and their
  `Float32` counterparts) reachable with no further re-declaration. This
  module's own new contribution — matching upstream's own file, whose body
  is otherwise almost entirely `map`/`zipWith` one-liners — is exactly the
  nine whole-image pixel-lift wrappers below.

  ## Rectangular form and conjugate: generic `[Elevator e]`, not `RealFloat e`

  Upstream types `(!+!)`/`realPartI`/`imagPartI`/`conjugateI` with a blanket
  `RealFloat e` constraint, exactly mirroring `ColorSpace/Complex.lean`'s own
  pixel-level `(+:)`/`realPart`/`imagPart`/`conjugate` (see that module's
  doc-comment for the full rationale). This port reuses the same, strictly
  weaker constraints those pixel-level functions already state
  (`[Elevator e]` for `mkComplexImg`/`realPartImg`/`imagPartImg`, `[Elevator e]
  [Neg e]` for `conjugateImg`) rather than reintroducing `RealFloat e`: these
  four image-level wrappers are `Interface.map`/`zipWith` applied to a pixel
  function that already carries the weaker constraint, so requiring more here
  would only narrow applicability with no benefit.

  ## Polar form: specialised to `Float`/`Float32`, doubled

  `mkPolarI`/`cisI`/`polarI`/`magnitudeI`/`phaseI` upstream need genuine
  `sqrt`/`atan2`/`cos`/`sin`, exactly as `ColorSpace/Complex.lean`'s
  pixel-level `mkPolar`/`cis`/`polar`/`magnitude`/`phase` do. Following that
  module's (and `Processing/Complex/Fourier.lean`'s) dual-precision
  convention, each of these five is ported twice: once specialised to `Float`
  and once, with an `F32` suffix, to `Float32`.

  ## Naming

  Upstream's infix `(!+!)` has no direct Lean spelling convenient at image
  level without introducing a new operator into this port's namespace for a
  single call site; it is ported as the prefix name `mkComplexImg`, matching
  `ColorSpace/Complex.lean`'s own `mkComplexPx` (the pixel-level function this
  wraps) with an `Img` suffix in place of `Px`, the same naming pattern this
  file uses throughout (`realPartImg`, `imagPartImg`, `conjugateImg`,
  `magnitudeImg`, `phaseImg`, `polarImg`, `mkPolarImg`, `cisImg`) for
  upstream's `realPartI`, `imagPartI`, `conjugateI`, `magnitudeI`, `phaseI`,
  `polarI`, `mkPolarI`, `cisI` respectively.
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace.Complex
import Linen.Graphics.Image.Processing.Complex.Fourier

open Graphics.Image.Interface (Pixel ColorSpace Image map zipWith)
open Graphics.Image.Interface.Elevator (Elevator)
open Graphics.Image.ColorSpace.Complex
  (mkComplexPx realPartPx imagPartPx conjugatePx magnitudePx phasePx polarPx mkPolarPx cisPx
    magnitudePxF32 phasePxF32 polarPxF32 mkPolarPxF32 cisPxF32)
open Data (Complex)

namespace Graphics.Image.Processing.Complex

-- ── Rectangular form: `mkComplexImg`, `realPartImg`, `imagPartImg` ──

/-- Construct a complex image from two images representing the real and
imaginary parts, pixel by pixel. Upstream's `(!+!)`. -/
def mkComplexImg {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components]
    {pxC : Type} [Pixel cs (Complex e) pxC] [Elevator (Complex e)] [Inhabited pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex e) ComponentsC]
    (img1 img2 : Image cs e) : Image cs (Complex e) :=
  zipWith (mkComplexPx (cs := cs) (e := e)) img1 img2

/-- Extract the real part of a complex image, pixel by pixel. Upstream's
`realPartI`. -/
def realPartImg {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components]
    {pxC : Type} [Pixel cs (Complex e) pxC] [Elevator (Complex e)]
    {ComponentsC : Type} [ColorSpace cs (Complex e) ComponentsC]
    (img : Image cs (Complex e)) : Image cs e :=
  map (realPartPx (cs := cs) (e := e)) img

/-- Extract the imaginary part of a complex image, pixel by pixel. Upstream's
`imagPartI`. -/
def imagPartImg {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components]
    {pxC : Type} [Pixel cs (Complex e) pxC] [Elevator (Complex e)]
    {ComponentsC : Type} [ColorSpace cs (Complex e) ComponentsC]
    (img : Image cs (Complex e)) : Image cs e :=
  map (imagPartPx (cs := cs) (e := e)) img

-- ── Conjugate ──

/-- The conjugate of a complex image, pixel by pixel. Upstream's
`conjugateI`. -/
def conjugateImg {cs e : Type} [Elevator e] [Neg e]
    {pxC : Type} [Pixel cs (Complex e) pxC] [Elevator (Complex e)]
    {ComponentsC : Type} [ColorSpace cs (Complex e) ComponentsC]
    (img : Image cs (Complex e)) : Image cs (Complex e) :=
  map (conjugatePx (cs := cs) (e := e)) img

-- ── Polar form, lifted to images (double precision) ──

/-- The nonnegative magnitude of a complex image, pixel by pixel. Upstream's
`magnitudeI` (double precision). -/
def magnitudeImg {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (img : Image cs (Complex Float)) : Image cs Float :=
  map (magnitudePx (cs := cs)) img

/-- The phase of a complex image, pixel by pixel. Upstream's `phaseI`
(double precision). -/
def phaseImg {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (img : Image cs (Complex Float)) : Image cs Float :=
  map (phasePx (cs := cs)) img

/-- A complex image's `(magnitude, phase)` pair. Upstream's `polarI` (double
precision). -/
def polarImg {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (img : Image cs (Complex Float)) : Image cs Float × Image cs Float :=
  (magnitudeImg (cs := cs) img, phaseImg (cs := cs) img)

/-- Form a complex image from polar components of magnitude and phase, pixel
by pixel. Upstream's `mkPolarI` (double precision). -/
def mkPolarImg {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC] [Elevator (Complex Float)] [Inhabited pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (imgR imgTheta : Image cs Float) : Image cs (Complex Float) :=
  zipWith (mkPolarPx (cs := cs)) imgR imgTheta

/-- A complex image with magnitude `1` and phase `θ` (per pixel), pixel by
pixel. Upstream's `cisI` (double precision). -/
def cisImg {cs : Type} {px : Type} [Pixel cs Float px] {Components : Type}
    [ColorSpace cs Float Components]
    {pxC : Type} [Pixel cs (Complex Float) pxC] [Elevator (Complex Float)]
    {ComponentsC : Type} [ColorSpace cs (Complex Float) ComponentsC]
    (imgTheta : Image cs Float) : Image cs (Complex Float) :=
  map (cisPx (cs := cs)) imgTheta

-- ── Polar form, lifted to images (single precision) ──

/-- `Float32` counterpart of `magnitudeImg`. -/
def magnitudeImgF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (img : Image cs (Complex Float32)) : Image cs Float32 :=
  map (magnitudePxF32 (cs := cs)) img

/-- `Float32` counterpart of `phaseImg`. -/
def phaseImgF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (img : Image cs (Complex Float32)) : Image cs Float32 :=
  map (phasePxF32 (cs := cs)) img

/-- `Float32` counterpart of `polarImg`. -/
def polarImgF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (img : Image cs (Complex Float32)) : Image cs Float32 × Image cs Float32 :=
  (magnitudeImgF32 (cs := cs) img, phaseImgF32 (cs := cs) img)

/-- `Float32` counterpart of `mkPolarImg`. -/
def mkPolarImgF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC] [Elevator (Complex Float32)] [Inhabited pxC]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (imgR imgTheta : Image cs Float32) : Image cs (Complex Float32) :=
  zipWith (mkPolarPxF32 (cs := cs)) imgR imgTheta

/-- `Float32` counterpart of `cisImg`. -/
def cisImgF32 {cs : Type} {px : Type} [Pixel cs Float32 px] {Components : Type}
    [ColorSpace cs Float32 Components]
    {pxC : Type} [Pixel cs (Complex Float32) pxC] [Elevator (Complex Float32)]
    {ComponentsC : Type} [ColorSpace cs (Complex Float32) ComponentsC]
    (imgTheta : Image cs Float32) : Image cs (Complex Float32) :=
  map (cisPxF32 (cs := cs)) imgTheta

end Graphics.Image.Processing.Complex
