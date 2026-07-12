/-
  Linen.Graphics.Image.Processing.Filter — named filter kernels built on
  convolution/correlation

  ## Haskell equivalent
  `Graphics.Image.Processing.Filter` from https://hackage.haskell.org/package/hip
  (module #18 of the `hip` import plan, see `docs/imports/hip/dependencies.md`),
  on module #12 (`Linen.Graphics.Image.ColorSpace`, via `Linen.Graphics.Image.
  ColorSpace.X` directly, matching upstream's own `import Graphics.Image.
  ColorSpace (X)`) and module #17 (`Linen.Graphics.Image.Processing.
  Convolution`). Read directly against the tarball source
  (`hip-1.5.6.0/src/Graphics/Image/Processing/Filter.hs`).

  ## The `Filter` type

  Upstream's `data Filter arr cs e = Filter { applyFilter :: Image arr cs e ->
  Image arr cs e }` is a thin wrapper around an image-to-image function, with
  `arr` already collapsed away by `Interface.lean`'s representation-collapse
  decision (see that module's doc-comment) — so this port drops the `arr`
  parameter along with it, exactly as `Convolution.lean` already does for
  `correlate`/`convolve`. `Filter cs e` becomes a one-field structure wrapping
  `Image cs e → Image cs e`, with `applyFilter` as its (record-projection)
  accessor, matching upstream's own accessor name and use (`applyFilter
  someFilter img`).

  ## Numeric kernel literals: `Num e` polymorphism vs. per-literal `OfNat e n`

  Every named filter below whose kernel is given as literal integers
  (`sobelFilter`, `prewittFilter`, `laplacianFilter`, `logFilter`,
  `gaussianSmoothingFilter`, `meanFilter`, `unsharpMaskingFilter`) relies,
  upstream, on Haskell's `Num` class: a single `fromInteger :: Integer -> e`
  method that accepts *any* integer literal generically for *any* `Num e`
  instance. Lean's numeral literals instead resolve through `OfNat e n`,
  indexed by the specific literal `n` — there is no single class covering
  "every integer literal" generically the way `Num.fromInteger` does. Rather
  than fabricate one (which would need a `Nat`/`Int`-embedding operation no
  other module in this port introduces) or narrow these filters to a single
  concrete `e` (which would be an unfaithful narrowing of upstream's genuine
  `Array arr cs e` genericity — none of these functions actually need
  `Floating`/`Fractional e` upstream, only `sobelOperator`/`prewittOperator`
  and the Gaussian filters do, see below), each such filter's signature
  instead lists the *exact* set of `[OfNat e n]` instances its own kernel's
  literal magnitudes need, plus `[Neg e]` where a literal is negative — the
  same style of explicit, per-call-site instance threading `Interpolation.
  lean`'s `interpolate` already uses for `[Add e] [Sub e] [Mul e]` in place of
  upstream's generic derived `Num (Pixel cs e)`. This adds no real burden at
  any concrete call site: every component type ported so far (`UInt8`/…/
  `Int`/`Float`/`Float32`) already has a *generic* `OfNat _ n` instance for
  every literal `n` from Lean's own core library, so these constraints are
  discharged automatically the moment a concrete `e` is chosen.

  ## `Floating e`/`Fractional e`: specialising to `Float`

  Upstream types `gaussianLowPass`/`gaussianBlur` over `(Floating e,
  Fractional e)` and `sobelOperator`/`prewittOperator` over `Floating e` (for
  `exp`/`sqrt`). In practice, across this whole port, only `Float`/`Float32`
  carry anything resembling `Floating`'s operations (`Linen.Graphics.Image.
  Interface.Elevator`'s `Elevator` class is a precision-scaling class, not a
  `Floating`-style arithmetic one, and no generic `Floating e` class has been
  introduced anywhere in this port) — exactly mirroring upstream itself, where
  no `Floating` instance exists for any of `Word8`/`Word16`/`Word32`/`Word64`/
  `Int` either, so `gaussianLowPass`/`gaussianBlur`/`sobelOperator`/
  `prewittOperator` were already, in practice, usable only at `e := Double`
  upstream. This port makes that existing restriction explicit by fixing
  `e := Float` in these four functions' signatures, rather than inventing an
  unused generic `Floating e` class purely to keep a type variable that no
  real instantiation other than `Float` could ever satisfy anyway.

  ## `Direction`

  Upstream's `Direction = Vertical | Horizontal` (used by `sobelFilter`/
  `prewittFilter` to pick which axis' gradient kernel to build) ports directly
  as an inductive with the same two constructors, Lean-cased.

  ## `BangPatterns` strictness annotations

  As in `Convolution.lean`, every `!`-prefixed argument/binding upstream
  (`!r`, `!sigma`, `!gV`, `!kernel`, `!img`, …) is a GHC strictness hint with
  no Lean surface-syntax counterpart and is simply absent from the port.
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace.X
import Linen.Graphics.Image.Processing.Convolution

open Graphics.Image.Interface
  (Pixel ColorSpace Border Image fromLists transpose map zipWith liftPx liftPx2)
open Graphics.Image.Interface.Elevator (Elevator)
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.Processing.Convolution (correlate convolveRows convolveCols)

namespace Graphics.Image.Processing.Filter

-- ── `Filter` — a named, ready-to-apply image transformation ──

/-- A filter that can be applied to an image via `applyFilter`. Upstream's
`Filter arr cs e`, with `arr` dropped per the module doc-comment's
representation-collapse note. -/
structure Filter (cs e : Type) {px : outParam Type} [Pixel cs e px] where
  /-- Apply a filter to an image. Upstream's `applyFilter` record accessor. -/
  applyFilter : Image cs e → Image cs e

/-- Direction used by `sobelFilter`/`prewittFilter` to pick an axis' gradient
kernel. Upstream's `Direction`. -/
inductive Direction where
  /-- The vertical-gradient kernel. -/
  | vertical
  /-- The horizontal-gradient kernel. -/
  | horizontal
deriving BEq, Repr, Inhabited

-- ── `Double`/`Int` conversion helpers (mirroring `Interpolation.lean`/`Geometric.lean`) ──

/-- Convert an `Int` to `Float`, exactly. Same helper as `Interpolation.lean`/
`Geometric.lean` (Lean's core library has no direct `Int.toFloat`). -/
private def intToFloat (n : Int) : Float :=
  if n < 0 then
    -((-n).toNat.toFloat)
  else
    n.toNat.toFloat

/-- Convert an already non-negative, integral-valued `Float` (e.g.
`Float.ceil` of a non-negative input) to `Nat`, exactly. Used by
`gaussianBlur` to derive a radius from `sigma`, which is assumed non-negative
(a standard deviation), matching upstream's own implicit assumption. -/
private def floatToNat (x : Float) : Nat :=
  x.toUInt64.toNat

-- ── Gaussian ──

/-- Build a Gaussian low-pass filter: a separable Gaussian blur applied first
along rows, then along columns. Upstream's `gaussianLowPass`; specialised to
`e := Float`, see the module doc-comment. -/
def gaussianLowPass {cs px Components : Type} [Pixel cs Float px]
    [ColorSpace cs Float Components] [Add px] [Mul Float] [OfNat Float 0] [Inhabited px]
    (r : Nat) (sigma : Float) (border : Border px) : Filter cs Float where
  applyFilter img :=
    let n := 2 * r + 1
    let sigma2sq := 2 * sigma * sigma
    let raw := (List.range n).map
      (fun j => Float.exp (-((intToFloat (Int.ofNat j - Int.ofNat r)) ^ 2) / sigma2sq))
    let weight := raw.foldl (· + ·) 0.0
    let row : List (PixelX Float) := raw.map (fun v => ⟨v / weight⟩)
    let gV : Image X Float := fromLists [row]
    let gV' : Image X Float := transpose gV
    correlate border gV' (correlate border gV img)

/-- Build a Gaussian blur filter: the radius is derived from the standard
deviation as `⌈2 * sigma⌉` and `Edge` border resolution is used. For a custom
radius/border strategy, use `gaussianLowPass` directly. Upstream's
`gaussianBlur`; specialised to `e := Float`, see the module doc-comment. -/
def gaussianBlur {cs px Components : Type} [Pixel cs Float px] [ColorSpace cs Float Components]
    [Add px] [Mul Float] [OfNat Float 0] [Inhabited px]
    (sigma : Float) : Filter cs Float :=
  gaussianLowPass (floatToNat (2 * sigma).ceil) sigma .edge

-- ── Sobel ──

/-- The Sobel edge-detection kernel for one direction, correlated against the
image. Upstream's `sobelFilter`. -/
def sobelFilter {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0] [OfNat e 1]
    [OfNat e 2] [Neg e]
    (dir : Direction) (border : Border px) : Filter cs e where
  applyFilter img :=
    let kernel : Image X e :=
      match dir with
      | .vertical =>
        fromLists ([[-1, -2, -1], [0, 0, 0], [1, 2, 1]] : List (List (PixelX e)))
      | .horizontal =>
        fromLists ([[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]] : List (List (PixelX e)))
    correlate border kernel img

/-- The Sobel operator: the gradient magnitude combining both the horizontal
and vertical Sobel filters, using `Edge` border resolution. Specialised to
`e := Float` (needs `sqrt`), see the module doc-comment. Upstream's
`sobelOperator`. -/
def sobelOperator {cs px Components : Type} [Pixel cs Float px] [ColorSpace cs Float Components]
    [Add px] [Mul Float] [OfNat Float 0] [OfNat Float 1] [OfNat Float 2] [Neg Float]
    [Inhabited px]
    (img : Image cs Float) : Image cs Float :=
  let sobelX := (sobelFilter .horizontal .edge).applyFilter img
  let sobelY := (sobelFilter .vertical .edge).applyFilter img
  zipWith (liftPx2 (cs := cs) (e := Float) (fun a b => Float.sqrt (a * a + b * b))) sobelX sobelY

-- ── Prewitt ──

/-- The Prewitt edge-detection kernel for one direction, convolved against the
image via separate row/column vector kernels. Upstream's `prewittFilter`. -/
def prewittFilter {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0] [OfNat e 1] [Neg e]
    (dir : Direction) (border : Border px) : Filter cs e where
  applyFilter img :=
    let rcV : List (PixelX e) × List (PixelX e) :=
      match dir with
      | .vertical => ([1, 1, 1], [1, 0, -1])
      | .horizontal => ([1, 0, -1], [1, 1, 1])
    convolveCols border rcV.2 (convolveRows border rcV.1 img)

/-- The Prewitt operator: the gradient magnitude combining both the
horizontal and vertical Prewitt filters, using `Edge` border resolution.
Specialised to `e := Float` (needs `sqrt`), see the module doc-comment.
Upstream's `prewittOperator`. -/
def prewittOperator {cs px Components : Type} [Pixel cs Float px] [ColorSpace cs Float Components]
    [Add px] [Mul Float] [OfNat Float 0] [OfNat Float 1] [Neg Float] [Inhabited px]
    (img : Image cs Float) : Image cs Float :=
  let prewittX := (prewittFilter .horizontal .edge).applyFilter img
  let prewittY := (prewittFilter .vertical .edge).applyFilter img
  zipWith
    (liftPx2 (cs := cs) (e := Float) (fun a b => Float.sqrt (a * a + b * b))) prewittX prewittY

-- ── Laplacian ──

/-- The Laplacian edge-detection filter: a single kernel (unlike Sobel/
Prewitt) approximating the second-order derivative of the image, including
diagonals. Upstream's `laplacianFilter`. -/
def laplacianFilter {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0] [OfNat e 1] [OfNat e 8]
    [Neg e]
    (border : Border px) : Filter cs e where
  applyFilter img :=
    let kernel : Image X e :=
      fromLists ([[-1, -1, -1], [-1, 8, -1], [-1, -1, -1]] : List (List (PixelX e)))
    correlate border kernel img

-- ── Laplacian of Gaussian ──

/-- The Laplacian-of-Gaussian (LoG) filter: a 9×9 kernel approximating a
Gaussian smoothing pass followed by a Laplacian derivative in a single pass,
reducing sensitivity to noise. Upstream's `logFilter`. -/
def logFilter {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0] [OfNat e 1]
    [OfNat e 2] [OfNat e 3] [OfNat e 4] [OfNat e 5] [OfNat e 12] [OfNat e 24] [OfNat e 40]
    [Neg e]
    (border : Border px) : Filter cs e where
  applyFilter img :=
    let kernel : Image X e :=
      fromLists
        ([ [0, 1, 1, 2, 2, 2, 1, 1, 0]
         , [1, 2, 4, 5, 5, 5, 4, 2, 1]
         , [1, 4, 5, 3, 0, 3, 5, 4, 1]
         , [2, 5, 3, -12, -24, -12, 3, 5, 2]
         , [2, 5, 0, -24, -40, -24, 0, 5, 2]
         , [2, 5, 3, -12, -24, -12, 3, 5, 2]
         , [1, 4, 5, 3, 0, 3, 5, 4, 1]
         , [1, 2, 4, 5, 5, 5, 4, 2, 1]
         , [0, 1, 1, 2, 2, 2, 1, 1, 0] ] : List (List (PixelX e)))
    correlate border kernel img

-- ── Gaussian smoothing ──

/-- A discrete approximation to the Gaussian smoothing operator, via a fixed
5×5 kernel rescaled by the sum of its own entries (`273`). Upstream's
`gaussianSmoothingFilter`. -/
def gaussianSmoothingFilter {cs e px Components : Type} [Pixel cs e px] [Inhabited px]
    [Inhabited e] [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0]
    [OfNat e 1] [OfNat e 4] [OfNat e 7] [OfNat e 16] [OfNat e 26] [OfNat e 41] [OfNat e 273]
    [Div e]
    (border : Border px) : Filter cs e where
  applyFilter img :=
    let kernel : Image X e :=
      fromLists
        ([ [1, 4, 7, 4, 1]
         , [4, 16, 26, 16, 4]
         , [7, 26, 41, 26, 7]
         , [4, 16, 26, 16, 4]
         , [1, 4, 7, 4, 1] ] : List (List (PixelX e)))
    map (liftPx (cs := cs) (e := e) (· / (273 : e))) (correlate border kernel img)

-- ── Mean ──

/-- The mean filter: replaces each pixel with the average of its 3×3
neighbourhood (including itself). Upstream's `meanFilter`. -/
def meanFilter {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0] [OfNat e 1]
    [OfNat e 9] [Div e]
    (border : Border px) : Filter cs e where
  applyFilter img :=
    let kernel : Image X e :=
      fromLists ([[1, 1, 1], [1, 1, 1], [1, 1, 1]] : List (List (PixelX e)))
    map (liftPx (cs := cs) (e := e) (· / (9 : e))) (correlate border kernel img)

-- ── Unsharp masking ──

/-- The unsharp-masking sharpening filter: subtracts a smoothed ("unsharp")
version of the image from the original via a single rescaled 5×5 kernel.
Upstream's `unsharpMaskingFilter`. -/
def unsharpMaskingFilter {cs e px Components : Type} [Pixel cs e px] [Inhabited px]
    [Inhabited e] [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0]
    [OfNat e 1] [OfNat e 4] [OfNat e 6] [OfNat e 16] [OfNat e 24] [OfNat e 256] [OfNat e 476]
    [Neg e] [Div e]
    (border : Border px) : Filter cs e where
  applyFilter img :=
    let kernel : Image X e :=
      fromLists
        ([ [-1, -4, -6, -4, -1]
         , [-4, -16, -24, -16, -4]
         , [-6, -24, 476, -24, -6]
         , [-4, -16, -24, -16, -4]
         , [-1, -4, -6, -4, -1] ] : List (List (PixelX e)))
    map (liftPx (cs := cs) (e := e) (· / (256 : e))) (correlate border kernel img)

end Graphics.Image.Processing.Filter
