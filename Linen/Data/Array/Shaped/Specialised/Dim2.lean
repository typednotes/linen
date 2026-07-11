/-
  Linen.Data.Array.Shaped.Specialised.Dim2 — functions specialised for
  rank-2 arrays

  Ported from Haskell's `Data.Array.Repa.Specialised.Dim2` (package `repa`).
-/

import Linen.Data.Array.Shaped.Index
import Linen.Data.Array.Shaped.Repr.Partitioned
import Linen.Data.Array.Shaped.Repr.Undefined

namespace Data.Array.Shaped

/-- Check if an index lies outside the given extent. Unlike `inRange` (see
    `Index.lean`), this is a short-circuited test that checks the lowest
    dimension first. -/
def isOutside2 (ex ix : DIM2) : Bool :=
  match ex, ix with
  | _ :. yLen :. xLen, _ :. yy :. xx =>
    if xx < 0 then true
    else if xx >= xLen then true
    else if yy < 0 then true
    else if yy >= yLen then true
    else false

/-- Check if an index lies inside the given extent. -/
def isInside2 (ex ix : DIM2) : Bool :=
  !isOutside2 ex ix

/-- Given the extent of an array, clamp the components of an index so they
    lie within the given array. Outlying indices are clamped to the index
    of the nearest border element. -/
def clampToBorder2 (ex ix : DIM2) : DIM2 :=
  match ex, ix with
  | _ :. yLen :. xLen, sh :. j :. i =>
    let x := if i < 0 then 0 else if i >= xLen then xLen - 1 else i
    let y := if j < 0 then 0 else if j >= yLen then yLen - 1 else j
    sh :. y :. x

/-- Make a 2D partitioned array from two others: one to produce the elements
    in the internal region, and one to produce elements in the border
    region. The two arrays must have the same extent, and the border must be
    the same width on all sides.

    Upstream nests five `Partitioned` regions (top/bottom/left/right border
    strips plus the internal region) over an `Undefined` fallback; ported
    faithfully, dropping only the `error` call's message formatting
    difference (`panic!` here vs. GHC's `error` there). -/
def makeBordered2 {arr1 arr2 a} [Inhabited a] [Source arr1] [Source arr2]
    [Inhabited (arr1 DIM2 a)] [Inhabited (arr2 DIM2 a)]
    (sh : DIM2) (bWidth : Int) (arrInternal : arr1 DIM2 a) (arrBorder : arr2 DIM2 a) :
    Partitioned arr1
      (Partitioned arr2 (Partitioned arr2 (Partitioned arr2 (Partitioned arr2 Undefined))))
      DIM2 a :=
  match sh with
  | _ :. aHeight :. aWidth =>
    if !(Source.extent arrInternal == Source.extent arrBorder) then
      panic! "Linen.Data.Array.Shaped.Specialised.Dim2.makeBordered2: internal and border arrays have different extents"
    else
      let inX := bWidth
      let inY := bWidth
      let inW := aWidth - 2 * bWidth
      let inH := aHeight - 2 * bWidth
      let inInternal : DIM2 → Bool := fun ix => match ix with
        | Z.Z :. y :. x => x >= inX && x < inX + inW && y >= inY && y < inY + inH
      let inBorder : DIM2 → Bool := fun ix => !inInternal ix
      ⟨sh, ⟨ix2 inY inX, ix2 inH inW, inInternal⟩, arrInternal,
        ⟨sh, ⟨ix2 0 0, ix2 bWidth aWidth, inBorder⟩, arrBorder,
          ⟨sh, ⟨ix2 (inY + inH) 0, ix2 bWidth aWidth, inBorder⟩, arrBorder,
            ⟨sh, ⟨ix2 inY 0, ix2 inH bWidth, inBorder⟩, arrBorder,
              ⟨sh, ⟨ix2 inY (inX + inW), ix2 inH bWidth, inBorder⟩, arrBorder, ⟨sh⟩⟩⟩⟩⟩⟩

end Data.Array.Shaped
