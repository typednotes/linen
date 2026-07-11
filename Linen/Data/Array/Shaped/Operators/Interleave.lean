/-
  Linen.Data.Array.Shaped.Operators.Interleave — interleaving array elements

  Ported from Haskell's `Data.Array.Repa.Operators.Interleave` (package
  `repa`). Interleaves the elements of two to four arrays of identical
  extent along the lowest dimension:

  $$
  \begin{pmatrix} a_1 & a_2 \\ a_3 & a_4 \end{pmatrix},
  \begin{pmatrix} b_1 & b_2 \\ b_3 & b_4 \end{pmatrix}
  \;\leadsto\;
  \begin{pmatrix} a_1 & b_1 & a_2 & b_2 \\ a_3 & b_3 & a_4 & b_4 \end{pmatrix}
  $$
-/

import Linen.Data.Array.Shaped.Index
import Linen.Data.Array.Shaped.Operators.Traversal

namespace Data.Array.Shaped

/-- Interleave the elements of two arrays. Both input arrays must have the
    same extent, else `panic!`. The lowest dimension of the result is
    twice the size of the inputs. -/
def interleave2 {arr1 arr2 sh e} [BEq sh] [Shape sh] [Inhabited sh] [Inhabited e]
    [Source arr1] [Source arr2]
    (a1 : arr1 (Snoc sh Int) e) (a2 : arr2 (Snoc sh Int) e) : Delayed (Snoc sh Int) e :=
  unsafeTraverse2 a1 a2
    (fun dim1 dim2 =>
      if dim1 == dim2 then
        match dim1 with | sh' :. len => sh' :. (len * 2)
      else
        panic! "Linen.Data.Array.Shaped.Operators.Interleave.interleave2: arrays must have same extent")
    (fun get1 get2 ix => match ix with
      | sh' :. i =>
        if i.tmod 2 == 0 then get1 (sh' :. i.tdiv 2) else get2 (sh' :. i.tdiv 2))

/-- Interleave the elements of three arrays. -/
def interleave3 {arr1 arr2 arr3 sh e} [BEq sh] [Shape sh] [Inhabited sh] [Inhabited e]
    [Source arr1] [Source arr2] [Source arr3]
    (a1 : arr1 (Snoc sh Int) e) (a2 : arr2 (Snoc sh Int) e) (a3 : arr3 (Snoc sh Int) e) :
    Delayed (Snoc sh Int) e :=
  unsafeTraverse3 a1 a2 a3
    (fun dim1 dim2 dim3 =>
      if dim1 == dim2 && dim1 == dim3 then
        match dim1 with | sh' :. len => sh' :. (len * 3)
      else
        panic! "Linen.Data.Array.Shaped.Operators.Interleave.interleave3: arrays must have same extent")
    (fun get1 get2 get3 ix => match ix with
      | sh' :. i =>
        match i.tmod 3 with
        | 0 => get1 (sh' :. i.tdiv 3)
        | 1 => get2 (sh' :. i.tdiv 3)
        | _ => get3 (sh' :. i.tdiv 3))

/-- Interleave the elements of four arrays. -/
def interleave4 {arr1 arr2 arr3 arr4 sh e} [BEq sh] [Shape sh] [Inhabited sh] [Inhabited e]
    [Source arr1] [Source arr2] [Source arr3] [Source arr4]
    (a1 : arr1 (Snoc sh Int) e) (a2 : arr2 (Snoc sh Int) e) (a3 : arr3 (Snoc sh Int) e)
    (a4 : arr4 (Snoc sh Int) e) : Delayed (Snoc sh Int) e :=
  unsafeTraverse4 a1 a2 a3 a4
    (fun dim1 dim2 dim3 dim4 =>
      if dim1 == dim2 && dim1 == dim3 && dim1 == dim4 then
        match dim1 with | sh' :. len => sh' :. (len * 4)
      else
        panic! "Linen.Data.Array.Shaped.Operators.Interleave.interleave4: arrays must have same extent")
    (fun get1 get2 get3 get4 ix => match ix with
      | sh' :. i =>
        match i.tmod 4 with
        | 0 => get1 (sh' :. i.tdiv 4)
        | 1 => get2 (sh' :. i.tdiv 4)
        | 2 => get3 (sh' :. i.tdiv 4)
        | _ => get4 (sh' :. i.tdiv 4))

end Data.Array.Shaped
