/-
  Linen.Graphics.Image.Processing.Geometric — resampling, cropping, flipping,
  rotation, and resizing of images

  ## Haskell equivalent
  `Graphics.Image.Processing.Geometric` from
  https://hackage.haskell.org/package/hip (module #14 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`), on module #13's
  `Linen.Graphics.Image.Processing.Interpolation`.

  ## Scope

  Every transform ported here produces an image whose dimensions are known
  *before* any pixel is generated (`downsample`/`upsample`'s new row/column
  counts, `leftToRight`/`topToBottom`'s summed dimensions, `crop`/`canvasSize`/
  `resize`'s explicit target size, `rotate`'s trigonometrically-derived
  bounding box, …), so every pixel-generation loop below is a plain bounded
  traversal via `Linen.Graphics.Image.Interface`'s `traverse`/`traverse2`/
  `backpermute`/`makeImage` — none of it needs any new termination-proof
  machinery beyond what those already provide.

  ## Upstream partiality

  Upstream signals invalid input with plain `error` calls in four places:
  `upsample` on a negative `(pre, post)` pair, `leftToRight`/`topToBottom` on
  mismatched row/column counts, `crop` on an out-of-range window, and `scale`
  on a non-positive scaling factor. This port keeps the same convention
  `Linen.Graphics.Image.Interface` itself already uses for its own partial
  operations (`checkDims`, `zipWith`, `izipWith`, `index` — see that module's
  doc-comment): a `panic!` with a descriptive message, rather than threading
  `Except`/`Option` through every caller for input that is a programming
  error in practice (a request to concatenate two differently-sized images,
  to crop a window outside the source, …), exactly mirroring the shape of
  upstream's own `error` sites one-for-one.

  ## `promote 0`/pixel arithmetic

  `upsample`'s zero-valued inserted pixel (upstream's bare `0 :: Pixel cs e`,
  resting on the generic derived `Num (Pixel cs e)` instance that `Interface.
  lean`'s doc-comment explains is *not* ported generically) is built here as
  `promote (fromFloat 0)` instead — `Elevator.fromFloat 0` is the zero value of
  every component type ported so far, and `promote` replicates it across every
  channel, exactly the pixel `fromInteger 0` would have produced. `rotate`/
  `resize`/`scale` need no pixel arithmetic of their own: they only forward to
  `Interpolation.interpolate`, which already carries the `[Add e] [Sub e]
  [Mul e]` constraints it needs (see that module's doc-comment).

  ## `Double` → `Int`/`Int` → `Double` conversions

  `rotate`/`resize`/`scale` need the same `Float ↔ Int` conversions
  `Interpolation.lean` already introduced (`floatToInt`/`intToFloat`) for
  exactly the same reason (no signed `Float → Int` in Lean's core library).
  Those two helpers are `private` to that module, so this module defines its
  own local copies rather than exporting private internals across module
  boundaries.

  `angle0to2pi`/`sin'`/`cos'` are ported as literal transcriptions of
  upstream's own helpers; Lean's core library has no `Float.pi` constant
  (checked directly — `Init/Data/Float.lean` defines no such value), so this
  module defines its own `piD`, the same pattern `Linen.Codec.Picture.Jpg.
  Internal.FastDct.piF32` already uses for the single-precision case.

  ## Sampling predicates

  `downsample`'s row/column predicates and `upsample`'s row/column
  before/after-insertion functions are ported as plain `Int → Bool`/
  `Int → Int × Int` functions taking a signed index, exactly as upstream
  (`Int -> Bool`/`Int -> (Int, Int)`), matching every index type already used
  throughout `Interface.lean`. `upsample`'s row/column spreading is
  implemented directly (build the `Array (Option Int)` mapping each new row/
  column position to either `some` source index or `none` for an inserted
  row/column) rather than porting upstream's `Data.Vector.unfoldr`-based state
  machine literally — the two are behaviourally identical (same bounded,
  one-pass construction), and the direct form reads more plainly as "for each
  source row, emit `pre` blanks, the row itself, then `post` blanks".
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.Processing.Interpolation

open Graphics.Image.Interface
  (Pixel ColorSpace Border handleBorderIndex promote dims traverse traverse2 backpermute
    transpose Image)
open Graphics.Image.Interface.Elevator (Elevator fromFloat)
open Graphics.Image.Processing.Interpolation (Interpolation interpolate)

namespace Graphics.Image.Processing.Geometric

-- ── `Float`/`Int` conversion helpers (local copies, see the module doc-comment) ──

/-- Convert an already integral-valued `Float` (e.g. the result of
`Float.floor`/`Float.round`/`Float.ceil`) to `Int`, exactly. -/
private def floatToInt (x : Float) : Int :=
  if x < 0 then
    -(Int.ofNat (-x).toUInt64.toNat)
  else
    Int.ofNat x.toUInt64.toNat

/-- Convert an `Int` to `Float`, exactly (within `Int64`'s range, which every
image coordinate/dimension here stays well within). -/
private def intToFloat (n : Int) : Float :=
  if n < 0 then
    -((-n).toNat.toFloat)
  else
    n.toNat.toFloat

/-- $\pi$, to `Float` (double) precision. Lean's core library defines no
`Float.pi` constant; matches the value already used by `Linen.Codec.Picture.
Jpg.Internal.FastDct.piF32` for the single-precision case. -/
private def piD : Float := 3.14159265358979323846

/-- Put an angle into the $[0, 2\pi)$ range. Upstream's `angle0to2pi`. -/
private def angle0to2pi (f : Float) : Float :=
  f - 2 * piD * intToFloat (floatToInt (f / (2 * piD)).floor)

/-- Make sure `sin' pi == 0` instead of a tiny nonzero floating-point residue.
Upstream's `sin'`. -/
private def sin' (a : Float) : Float :=
  let zero0 := 10 * piD.sin
  let sinA := a.sin
  if sinA.abs <= zero0 then 0 else sinA

/-- Make sure `cos' (pi/2) == 0`/`cos' (3*pi/2) == 0` instead of a tiny
nonzero floating-point residue. Upstream's `cos'`. -/
private def cos' (a : Float) : Float :=
  sin' (a + piD / 2)

-- ── Sampling ──

/-- Test whether an `Int` is odd. Used by `downsampleRows`/`downsampleCols`/
`upsampleRows`/`upsampleCols` below, matching upstream's use of `Prelude`'s
`odd`. -/
private def oddI (k : Int) : Bool :=
  k % 2 != 0

/-- Downsample an image: drop every row/column whose index satisfies the
corresponding predicate. Upstream's `downsample`. -/
def downsample {px} [Inhabited px] (mPred nPred : Int → Bool)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let (m, n) := dims img
  let rowsIx := ((List.range m.toNat).map Int.ofNat).filter (fun i => !mPred i) |>.toArray
  let colsIx := ((List.range n.toNat).map Int.ofNat).filter (fun j => !nPred j) |>.toArray
  traverse img (fun _ => (Int.ofNat rowsIx.size, Int.ofNat colsIx.size))
    (fun getPx (i, j) =>
      getPx (rowsIx.getD i.toNat 0, colsIx.getD j.toNat 0))

/-- Downsample an image by discarding every odd row. Upstream's
`downsampleRows`. -/
def downsampleRows {px} [Inhabited px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  downsample oddI (fun _ => false) img

/-- Downsample an image by discarding every odd column. Upstream's
`downsampleCols`. -/
def downsampleCols {px} [Inhabited px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  downsample (fun _ => false) oddI img

/-- For a source axis of length `len`, with `add k` giving the `(before,
after)` count of blanks to insert around source index `k`, build the mapping
from every new-axis position to either `some` source index (a copied
row/column) or `none` (an inserted blank). `panic!`s on a negative
`before`/`after` count, matching upstream's `error`. Used by `upsample`. -/
private def buildSpreadIndex (add : Int → Int × Int) (len : Int) : Array (Option Int) :=
  Id.run do
    let mut acc : Array (Option Int) := #[]
    for k in [0:len.toNat] do
      let kk := Int.ofNat k
      let (pre, post) := add kk
      if pre < 0 || post < 0 then
        panic! s!"Graphics.Image.Processing.Geometric.upsample: negative values are not \
          accepted: ({pre}, {post})"
      for _ in [0:pre.toNat] do
        acc := acc.push none
      acc := acc.push (some kk)
      for _ in [0:post.toNat] do
        acc := acc.push none
    pure acc

/-- Upsample an image by inserting rows/columns of zero-valued pixels.
`mAdd`/`nAdd` give the `(before, after)` count of rows/columns to insert
around each row/column index. Upstream's `upsample`. -/
def upsample {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Elevator e]
    [ColorSpace cs e Components] (mAdd nAdd : Int → Int × Int) (img : Image cs e) : Image cs e :=
  let (m, n) := dims img
  let rowsIx := buildSpreadIndex mAdd m
  let colsIx := buildSpreadIndex nAdd n
  let zeroPx := promote (cs := cs) (e := e) (fromFloat (0 : Float))
  traverse img (fun _ => (Int.ofNat rowsIx.size, Int.ofNat colsIx.size))
    (fun getPx (i, j) =>
      match rowsIx.getD i.toNat none, colsIx.getD j.toNat none with
      | some i', some j' => getPx (i', j')
      | _, _ => zeroPx)

/-- Upsample an image by inserting a row of zero-valued pixels after each
row. Upstream's `upsampleRows`. -/
def upsampleRows {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Elevator e]
    [ColorSpace cs e Components] (img : Image cs e) : Image cs e :=
  upsample (fun _ => (0, 1)) (fun _ => (0, 0)) img

/-- Upsample an image by inserting a column of zero-valued pixels after each
column. Upstream's `upsampleCols`. -/
def upsampleCols {cs e px Components : Type} [Pixel cs e px] [Inhabited px] [Elevator e]
    [ColorSpace cs e Components] (img : Image cs e) : Image cs e :=
  upsample (fun _ => (0, 0)) (fun _ => (0, 1)) img

-- ── Concatenation ──

/-- Concatenate two images side by side. Both must have the same number of
rows, or this `panic!`s (upstream's `error`). Upstream's `leftToRight`. -/
def leftToRight {px} [Inhabited px]
    (img1 img2 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let (_, n1) := dims img1
  traverse2 img1 img2
    (fun d1 d2 =>
      let (m1, _) := d1
      let (m2, n2) := d2
      if m1 == m2 then
        (m1, n1 + n2)
      else
        panic! s!"Graphics.Image.Processing.Geometric.leftToRight: images must agree in number \
          of rows, but received rows {m1} and {m2}")
    (fun getPx1 getPx2 (i, j) => if j < n1 then getPx1 (i, j) else getPx2 (i, j - n1))

/-- Concatenate two images top to bottom. Both must have the same number of
columns, or this `panic!`s (upstream's `error`). Upstream's `topToBottom`. -/
def topToBottom {px} [Inhabited px]
    (img1 img2 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let (m1, _) := dims img1
  traverse2 img1 img2
    (fun d1 d2 =>
      let (_, n1) := d1
      let (m2, n2) := d2
      if n1 == n2 then
        (m1 + m2, n1)
      else
        panic! s!"Graphics.Image.Processing.Geometric.topToBottom: images must agree in number \
          of columns, but received columns {n1} and {n2}")
    (fun getPx1 getPx2 (i, j) => if i < m1 then getPx1 (i, j) else getPx2 (i - m1, j))

-- ── Canvas ──

/-- Shift an image towards its bottom-right corner by `(deltaM, deltaN)`
rows and columns, using a border-resolution strategy for the vacated area.
Upstream's `translate`. -/
def translate {px} [Inhabited px] (atBorder : Border px) (delta : Int × Int)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  traverse img id
    (fun getPx (i, j) => handleBorderIndex atBorder (dims img) getPx (i - delta.1, j - delta.2))

/-- Change the size of an image's canvas, using a border-resolution strategy
for any newly out-of-bounds area. Upstream's `canvasSize`. -/
def canvasSize {px} [Inhabited px] (atBorder : Border px) (newDims : Int × Int)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  traverse img (fun _ => newDims) (fun getPx ix => handleBorderIndex atBorder (dims img) getPx ix)

/-- Crop a sub-image with `m` rows and `n` columns, starting at `(i0, j0)`.
`panic!`s (upstream's `error`) if the starting index or the resulting window
falls outside the source image. Upstream's `crop`. -/
def crop {px} [Inhabited px] (ix0 sz : Int × Int)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let (i0, j0) := ix0
  let (m', n') := sz
  let (m, n) := dims img
  if i0 < 0 || j0 < 0 || i0 >= m || j0 >= n then
    panic! s!"Graphics.Image.Processing.Geometric.crop: starting index {ix0} is outside the \
      source image dimensions {(m, n)}"
  else if i0 + m' > m || j0 + n' > n then
    panic! s!"Graphics.Image.Processing.Geometric.crop: result dimensions {sz} plus the offset \
      {ix0} exceed the source image dimensions {(m, n)}"
  else
    backpermute sz (fun (i, j) => (i + i0, j + j0)) img

/-- Place one image on top of a source image, starting at `(i0, j0)` within
the source. Upstream's `superimpose`. -/
def superimpose {px} [Inhabited px] (ix0 : Int × Int)
    (imgA imgB : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let (i0, j0) := ix0
  let (m, n) := dims imgA
  traverse2 imgB imgA (fun d _ => d)
    (fun getPxB getPxA (i, j) =>
      let (i', j') := (i - i0, j - j0)
      if i' >= 0 && j' >= 0 && i' < m && j' < n then getPxA (i', j') else getPxB (i, j))

-- ── Flipping ──

/-- Backpermute an image's indices using a function of the image's own
dimensions. Shared by `flipV`/`flipH`. Upstream's `flipUsing`. -/
private def flipUsing {px} [Inhabited px] (getNewIndex : Int × Int → Int × Int → Int × Int)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let sz := dims img
  backpermute sz (getNewIndex sz) img

/-- Flip an image vertically. Upstream's `flipV`. -/
def flipV {px} [Inhabited px] (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  flipUsing (fun (m, _) (i, j) => (m - 1 - i, j)) img

/-- Flip an image horizontally. Upstream's `flipH`. -/
def flipH {px} [Inhabited px] (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  flipUsing (fun (_, n) (i, j) => (i, n - 1 - j)) img

-- ── Rotation ──

/-- Rotate an image clockwise by 90°. Upstream's `rotate90`. -/
def rotate90 {px} [Inhabited px] (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  transpose (flipV img)

/-- Rotate an image by 180°. Upstream's `rotate180`. -/
def rotate180 {px} [Inhabited px] (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  flipUsing (fun (m, n) (i, j) => (m - 1 - i, n - 1 - j)) img

/-- Rotate an image clockwise by 270°. Upstream's `rotate270`. -/
def rotate270 {px} [Inhabited px] (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  transpose (flipH img)

/-- Rotate an image clockwise by an angle `theta'` in radians, using an
interpolation method and a border-handling strategy for out-of-bounds
neighbours. The result image's dimensions are computed up front from the
rotated bounding box. Upstream's `rotate`. -/
def rotate {cs e px Components method : Type} [Pixel cs e px] [Inhabited px] [Elevator e]
    [ColorSpace cs e Components] [Add e] [Sub e] [Mul e] [Interpolation method]
    (m : method) (border : Border px) (theta' : Float) (img : Image cs e) : Image cs e :=
  let theta := angle0to2pi (-theta')
  let sz := dims img
  let (rows, cols) := sz
  let mD := intToFloat rows
  let nD := intToFloat cols
  let sinTheta := sin' theta
  let cosTheta := cos' theta
  let sinThetaAbs := sinTheta.abs
  let cosThetaAbs := cosTheta.abs
  let mD' := mD * cosThetaAbs + nD * sinThetaAbs
  let nD' := nD * cosThetaAbs + mD * sinThetaAbs
  let (iDelta, jDelta) :=
    if sinTheta >= 0 then
      if cosTheta >= 0 then (nD * sinTheta, 0) else (mD', -nD * cosTheta)
    else
      if cosTheta >= 0 then (0, -mD * sinTheta) else (-mD * cosTheta, nD')
  traverse img (fun _ => (floatToInt mD'.ceil, floatToInt nD'.ceil))
    (fun getPx (i, j) =>
      let iD := intToFloat i - iDelta + 0.5
      let jD := intToFloat j - jDelta + 0.5
      let i' := iD * cosTheta + jD * sinTheta - 0.5
      let j' := jD * cosTheta - iD * sinTheta - 0.5
      interpolate (cs := cs) (e := e) (px := px) (Components := Components) m border sz getPx
        (i', j'))

-- ── Scaling ──

/-- Resize an image to the given dimensions using an interpolation method and
a border-handling strategy for out-of-bounds neighbours. Upstream's
`resize`. -/
def resize {cs e px Components method : Type} [Pixel cs e px] [Inhabited px] [Elevator e]
    [ColorSpace cs e Components] [Add e] [Sub e] [Mul e] [Interpolation method]
    (m : method) (border : Border px) (sz' : Int × Int) (img : Image cs e) : Image cs e :=
  let sz := dims img
  let (rows, cols) := sz
  let fM := intToFloat sz'.1 / intToFloat rows
  let fN := intToFloat sz'.2 / intToFloat cols
  traverse img (fun _ => sz')
    (fun getPx (i, j) =>
      interpolate (cs := cs) (e := e) (px := px) (Components := Components) m border sz getPx
        ((intToFloat i + 0.5) / fM - 0.5, (intToFloat j + 0.5) / fN - 0.5))

/-- Scale an image by positive `(rowFactor, colFactor)` factors, using an
interpolation method and a border-handling strategy. `panic!`s (upstream's
`error`) if either factor is not strictly positive. Upstream's `scale`. -/
def scale {cs e px Components method : Type} [Pixel cs e px] [Inhabited px] [Elevator e]
    [ColorSpace cs e Components] [Add e] [Sub e] [Mul e] [Interpolation method]
    (m : method) (border : Border px) (factors : Float × Float) (img : Image cs e) : Image cs e :=
  let (fM, fN) := factors
  if fM <= 0 || fN <= 0 then
    panic! "Graphics.Image.Processing.Geometric.scale: scaling factor must be greater than 0."
  else
    let (rows, cols) := dims img
    resize m border (floatToInt (fM * intToFloat rows).round, floatToInt (fN * intToFloat cols).round) img

end Graphics.Image.Processing.Geometric
