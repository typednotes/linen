/-
  Linen.Data.Array.Shaped.Operators.IndexSpace — index-space transformations

  Ported from Haskell's `Data.Array.Repa.Operators.IndexSpace` (package
  `repa`): `reshape`, `append`, `transpose`, `extract`, `backpermute`,
  `backpermuteDft`, `extend`, and `slice`, all producing a `Delayed` result.

  Upstream's infix `(++)` (an alias for `append`) is dropped: it clashes
  with `List.append`'s own `++` notation and carries no behavior beyond
  `append` itself.
-/

import Linen.Data.Array.Shaped.Operators.Traversal
import Linen.Data.Array.Shaped.Slice

namespace Data.Array.Shaped

/-- Impose a new shape on the elements of an array. `panic!`s if the new
    extent is not the same size as the original. -/
def reshape {arr sh1 sh2 e} [Shape sh1] [Shape sh2] [Inhabited sh2] [Inhabited e] [Source arr]
    (sh2' : sh2) (a : arr sh1 e) : Delayed sh2 e :=
  if Shape.size sh2' == Shape.size (Source.extent a) then
    fromFunction sh2' (fun ix => unsafeIndex a (Shape.fromIndex (Source.extent a) (Shape.toIndex sh2' ix)))
  else
    panic! "Linen.Data.Array.Shaped.Operators.IndexSpace.reshape: reshaped array will not match size of the original"

/-- Append two arrays along their outermost dimension. -/
def append {arr1 arr2 sh e} [Shape sh] [Inhabited sh] [Inhabited e] [Source arr1] [Source arr2]
    (a1 : arr1 (Snoc sh Int) e) (a2 : arr2 (Snoc sh Int) e) : Delayed (Snoc sh Int) e :=
  let n := match Source.extent a1 with | _ :. n => n
  unsafeTraverse2 a1 a2
    (fun sh1 sh2 => match sh1, sh2 with
      | sh1' :. i, sh2' :. j => Shape.intersectDim sh1' sh2' :. (i + j))
    (fun f1 f2 ix => match ix with
      | sh' :. i => if i < n then f1 (sh' :. i) else f2 (sh' :. (i - n)))

/-- Transpose the lowest two dimensions of an array. Transposing an array
    twice yields the original. -/
def transpose {arr sh e} [Shape sh] [Inhabited sh] [Inhabited e] [Source arr]
    (a : arr (Snoc (Snoc sh Int) Int) e) : Delayed (Snoc (Snoc sh Int) Int) e :=
  unsafeTraverse a
    (fun ix => match ix with | sh' :. m :. n => sh' :. n :. m)
    (fun f ix => match ix with | sh' :. i :. j => f (sh' :. j :. i))

/-- Extract a sub-range of elements from an array, given a starting index
    and the size of the result. -/
def extract {arr sh e} [Shape sh] [Inhabited e] [Source arr]
    (start sz : sh) (a : arr sh e) : Delayed sh e :=
  fromFunction sz (fun ix => unsafeIndex a (Shape.addDim start ix))

/-- Backwards permutation of an array's elements. -/
def backpermute {arr sh1 sh2 e} [Shape sh1] [Inhabited e] [Source arr]
    (newExtent : sh2) (perm : sh2 → sh1) (a : arr sh1 e) : Delayed sh2 e :=
  traverse a (fun _ => newExtent) (fun f ix => f (perm ix))

/-- Backwards permutation of an array's elements, without bounds checking. -/
def unsafeBackpermute {arr sh1 sh2 e} [Shape sh1] [Inhabited e] [Source arr]
    (newExtent : sh2) (perm : sh2 → sh1) (a : arr sh1 e) : Delayed sh2 e :=
  unsafeTraverse a (fun _ => newExtent) (fun f ix => f (perm ix))

/-- Default backwards permutation of an array's elements. If the index
    function returns `none`, the value at that index is taken from the
    default array instead. -/
def backpermuteDft {arr1 arr2 sh1 sh2 e}
    [Shape sh1] [Shape sh2] [Inhabited e] [Source arr1] [Source arr2]
    (dft : arr2 sh2 e) (fnIndex : sh2 → Option sh1) (src : arr1 sh1 e) : Delayed sh2 e :=
  fromFunction (Source.extent dft) (fun ix =>
    match fnIndex ix with
    | some ix' => index src ix'
    | none => index dft ix)

/-- Default backwards permutation of an array's elements, without bounds
    checking. -/
def unsafeBackpermuteDft {arr1 arr2 sh1 sh2 e}
    [Shape sh1] [Shape sh2] [Inhabited e] [Source arr1] [Source arr2]
    (dft : arr2 sh2 e) (fnIndex : sh2 → Option sh1) (src : arr1 sh1 e) : Delayed sh2 e :=
  fromFunction (Source.extent dft) (fun ix =>
    match fnIndex ix with
    | some ix' => unsafeIndex src ix'
    | none => unsafeIndex dft ix)

/-- Extend an array according to a given slice specification: e.g.
    `extend (Any.Any :. (5 : Int) :. All.All) arr` replicates the rows of
    `arr`. -/
def extend {arr ss full slice e} [Shape slice] [Inhabited e] [Source arr] [Slice ss full slice]
    (sl : ss) (a : arr slice e) : Delayed full e :=
  backpermute (Slice.fullOfSlice sl (Source.extent a)) (Slice.sliceOfFull sl) a

/-- Extend an array according to a given slice specification, without
    bounds checking. -/
def unsafeExtend {arr ss full slice e} [Shape slice] [Inhabited e] [Source arr] [Slice ss full slice]
    (sl : ss) (a : arr slice e) : Delayed full e :=
  unsafeBackpermute (Slice.fullOfSlice sl (Source.extent a)) (Slice.sliceOfFull sl) a

/-- Take a slice from an array according to a given specification: e.g.
    `slice arr (Any.Any :. (5 : Int) :. All.All)` takes a row from a
    matrix. -/
def slice {arr ss full slice e} [Shape full] [Inhabited e] [Source arr] [Slice ss full slice]
    (a : arr full e) (sl : ss) : Delayed slice e :=
  backpermute (Slice.sliceOfFull sl (Source.extent a)) (Slice.fullOfSlice sl) a

/-- Take a slice from an array according to a given specification, without
    bounds checking. -/
def unsafeSlice {arr ss full slice e} [Shape full] [Inhabited e] [Source arr] [Slice ss full slice]
    (a : arr full e) (sl : ss) : Delayed slice e :=
  unsafeBackpermute (Slice.sliceOfFull sl (Source.extent a)) (Slice.fullOfSlice sl) a

end Data.Array.Shaped
