/-
  Tests for `Linen.Data.Array.Shaped.Operators.IndexSpace` — `reshape`,
  `append`, `transpose`, `extract`, `backpermute`, `backpermuteDft`,
  `extend`, and `slice`.
-/
import Linen.Data.Array.Shaped.Operators.IndexSpace
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Operators.IndexSpace

private def flat : Manifest DIM1 Nat := Manifest.fromList (ix1 6) [1, 2, 3, 4, 5, 6]

#guard toList (reshape (ix2 2 3) flat) == [1, 2, 3, 4, 5, 6]

private def a1 : Manifest DIM1 Nat := Manifest.fromList (ix1 2) [1, 2]
private def a2 : Manifest DIM1 Nat := Manifest.fromList (ix1 3) [3, 4, 5]

#guard toList (append a1 a2) == [1, 2, 3, 4, 5]

private def mat : Manifest DIM2 Nat := Manifest.fromList (ix2 2 3) [1, 2, 3, 4, 5, 6]

#guard toList (transpose mat) == [1, 4, 2, 5, 3, 6]
#guard (Source.extent (transpose mat)) == ix2 3 2

#guard toList (extract (ix2 0 1) (ix2 2 2) mat) == [2, 3, 5, 6]

#guard toList (backpermute (ix1 3) (fun ix => match ix with | Z.Z :. i => ix1 (2 - i)) a2) == [5, 4, 3]

private def dft : Manifest DIM1 Nat := Manifest.fromList (ix1 3) [0, 0, 0]

#guard toList (backpermuteDft dft (fun ix => match ix with
    | Z.Z :. i => if i == 1 then some (ix1 0) else none) a1) == [0, 1, 0]

-- Extend a row vector into a 2x3 matrix by replicating it twice.
#guard toList (extend (Any.Any (sh := Z) :. (2 : Int) :. All.All) a2) == [3, 4, 5, 3, 4, 5]

-- Slice out row 1 of `mat`.
#guard toList (slice mat (Any.Any (sh := Z) :. (1 : Int) :. All.All)) == [4, 5, 6]

end Tests.Data.Array.Shaped.Operators.IndexSpace
