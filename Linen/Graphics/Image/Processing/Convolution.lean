/-
  Linen.Graphics.Image.Processing.Convolution — kernel convolution/correlation
  of an image

  ## Haskell equivalent
  `Graphics.Image.Processing.Convolution` from
  https://hackage.haskell.org/package/hip (module #17 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`), on module #1
  (`Linen.Graphics.Image.Utils`), #12 (`Linen.Graphics.Image.ColorSpace`, for
  the single-channel `X`/`PixelX` kernel carrier), and #14
  (`Linen.Graphics.Image.Processing.Geometric`, for `rotate180`). Read
  directly against the tarball source
  (`hip-1.5.6.0/src/Graphics/Image/Processing/Convolution.hs`).

  ## Kernel representation

  Upstream represents a convolution/correlation kernel as an ordinary
  `Image arr X e`: a single-channel image (colour space `X`, the generic
  "unlabeled channel" carrier from module #10) whose pixels are read with
  `getX`. This port carries that over directly: a kernel here is a
  `Graphics.Image.Interface.Image X e`, i.e. `Graphics.Image.ColorSpace.X.
  PixelX e` pixels, with each kernel value read via `PixelX.x` (this port's
  name for upstream's `getX` record accessor — see `X.lean`'s own
  doc-comment on that naming).

  ## Centering convention

  Upstream centres a kernel of size `(kM, kN)` at `(kM \`div\` 2, kN \`div\`
  2)` (`kM2`/`kN2` below), using Haskell's `div` (floored integer division).
  This is carried over unchanged; Lean's `Int./` on non-negative operands
  (kernel dimensions are always positive, since `Interface.checkDims`-backed
  image construction rules out non-positive extents) agrees with Haskell's
  `div`, so no sign-correction is needed, matching `Interface.lean`'s own
  note on `Int`'s Euclidean `/`/`%`. For an odd-sized kernel this centres
  exactly on the middle cell (e.g. `3 \`div\` 2 = 1`); for an even-sized
  kernel it centres one cell short of the true middle (e.g. `4 \`div\` 2 =
  2`, the *third* of four cells) — this asymmetry is upstream's own
  documented behaviour (`kM2`/`kN2` are plain `div`, with no separate
  even-size code path in the original source), not a simplification
  introduced here.

  ## Border handling

  `correlate`'s `border : Border (Pixel cs e)` argument — a border-resolution
  strategy for the *source image*'s colour space `cs`, not the kernel's `X` —
  is used only for the outer window of the result where the sliding kernel
  would read out-of-bounds source pixels; the interior (`Interface.
  makeImageWindowed`'s inner region, of size `(m - kM2 * 2, n - kN2 * 2)`)
  reads the source directly via `unsafeIndex`, exactly mirroring upstream's
  `getStencil (I.unsafeIndex imgM)` (inner) vs. `getStencil (borderIndex
  border imgM)` (border) split.

  ## The stencil sum

  Upstream's `getStencil` is `Graphics.Image.Utils.loop`, nested two deep
  (rows then columns of the kernel), accumulating `acc + liftPx (* getX
  kernelPx) srcPx` at each step — a scalar-times-pixel product (every channel
  of the source pixel scaled by the kernel's single value at that offset,
  via `[Mul e]`) folded with pixel addition (`[Add px]`). This is ported as
  a literal transcription using this port's own `Graphics.Image.Utils.loop`
  (whose own doc-comment names this exact call site, alongside the
  now-dropped `Interface.Vector.Generic`, as the reason `loop` was narrowed
  to a bounded increasing walk rather than upstream's fully general,
  conditionally-terminating combinator) — no new termination-proof machinery
  is needed, since both loops are already bounded by the kernel's own
  (runtime, but finite) dimensions.

  The zero-valued accumulator seed (upstream's bare `0`, resolved via the
  generic, not-ported `Num (Pixel cs e)` instance — see `Interface.lean`'s
  and `Geometric.lean`'s doc-comments on that same deferral) is built here as
  `promote (0 : e)`, requiring `[OfNat e 0]` in place of upstream's `Num e`,
  exactly the substitution `X.lean`'s `fromPixelsX`/`Geometric.lean`'s
  `upsample` already use for the identical situation.

  ## `convolve`/`convolveRows`/`convolveCols`

  `convolve` is upstream's `correlate out . rotate180`: correlating with a
  180°-rotated kernel, using `Geometric.rotate180` directly (no new rotation
  logic). `convolveRows`/`convolveCols` build a one-row/one-column kernel
  from a plain list of `PixelX e` values via `Interface.fromLists`, reversed
  first exactly as upstream (`fromLists . (:[]) . reverse` /
  `fromLists . P.map (:[]) . reverse`) — `convolve` itself performs the
  second, 180°-rotation reversal, so `convolveRows`/`convolveCols` only need
  to supply the single reversal upstream's own helpers apply before that.

  ## `BangPatterns` strictness annotations

  Every `!`-prefixed argument/let-binding in the upstream source
  (`!border`, `!kernel`, `!img`, `!imgM`, `!sz@(m, n)`, …) is a GHC strictness
  hint with no Lean surface-syntax counterpart (Lean's `let`/function
  arguments are already evaluated eagerly along every path this module
  actually takes); these are simply absent from the port, per the
  package-wide convention already used throughout this port (see, e.g.,
  `Graphics.Image.Utils`'s own doc-comment on `compose₂`/`compose₂!`).
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace.X
import Linen.Graphics.Image.Processing.Geometric
import Linen.Graphics.Image.Utils

open Graphics.Image.Interface
  (Pixel ColorSpace Border borderIndex unsafeIndex dims promote liftPx makeImageWindowed fromLists
    Image)
open Graphics.Image.Interface.Elevator (Elevator)
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.Processing.Geometric (rotate180)

namespace Graphics.Image.Processing.Convolution

-- ── Correlation ──

/-- Correlate an image with a kernel: at every pixel, sum the source image's
neighbourhood (of the kernel's own size, centred per the module doc-comment)
weighted channel-wise by the corresponding kernel value. A border-resolution
strategy is required for the source pixels the sliding kernel reads outside
the image, near its edges. Upstream's `correlate`. -/
def correlate {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0]
    (border : Border px) (kernel : Image X e) (img : Image cs e) : Image cs e :=
  let (m, n) := dims img
  let (kM, kN) := dims kernel
  let kM2 := kM / 2
  let kN2 := kN / 2
  let zeroPx : px := promote (cs := cs) (e := e) (0 : e)
  let getStencil (getImgPx : Int × Int → px) (ij : Int × Int) : px :=
    let (i, j) := ij
    Graphics.Image.Utils.loop 0 kM.toNat zeroPx (fun iK acc0 =>
      let iD := i + Int.ofNat iK - kM2
      Graphics.Image.Utils.loop 0 kN.toNat acc0 (fun jK acc1 =>
        let jD := j + Int.ofNat jK - kN2
        let kv := (unsafeIndex kernel (Int.ofNat iK, Int.ofNat jK)).x
        acc1 + liftPx (cs := cs) (e := e) (fun v => v * kv) (getImgPx (iD, jD))))
  makeImageWindowed (m, n) (kM2, kN2) (m - kM2 * 2, n - kN2 * 2)
    (getStencil (unsafeIndex img))
    (getStencil (borderIndex border img))

-- ── Convolution ──

/-- Convolution of an image using a kernel: correlation with the kernel
rotated 180°. A border-resolution strategy is required for the source pixels
read outside the image, near its edges.

Example using the [Sobel operator](https://en.wikipedia.org/wiki/Sobel_operator):
```
let frogX := convolve .edge (fromLists [[⟨-1⟩, ⟨0⟩, ⟨1⟩], [⟨-2⟩, ⟨0⟩, ⟨2⟩], [⟨-1⟩, ⟨0⟩, ⟨1⟩]]) frog
let frogY := convolve .edge (fromLists [[⟨-1⟩, ⟨-2⟩, ⟨-1⟩], [⟨0⟩, ⟨0⟩, ⟨0⟩], [⟨1⟩, ⟨2⟩, ⟨1⟩]]) frog
```
Upstream's `convolve`. -/
def convolve {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0]
    (border : Border px) (kernel : Image X e) (img : Image cs e) : Image cs e :=
  correlate border (rotate180 kernel) img

/-- Convolve an image's rows with a vector kernel represented by a list of
`X`-pixels. Upstream's `convolveRows`. -/
def convolveRows {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0]
    (border : Border px) (ps : List (PixelX e)) (img : Image cs e) : Image cs e :=
  convolve border (fromLists [ps.reverse]) img

/-- Convolve an image's columns with a vector kernel represented by a list of
`X`-pixels. Upstream's `convolveCols`. -/
def convolveCols {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Inhabited e]
    [Elevator e] [ColorSpace cs e Components] [Mul e] [Add px] [OfNat e 0]
    (border : Border px) (ps : List (PixelX e)) (img : Image cs e) : Image cs e :=
  convolve border (fromLists (ps.reverse.map (fun p => [p]))) img

end Graphics.Image.Processing.Convolution
