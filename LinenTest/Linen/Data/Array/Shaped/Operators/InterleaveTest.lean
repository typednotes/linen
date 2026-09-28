/-
  Tests for `Linen.Data.Array.Shaped.Operators.Interleave` — `interleave2`
  and `interleave3`.
-/
import Linen.Data.Array.Shaped.Operators.Interleave
import Linen.Data.Array.Shaped.Repr.Manifest

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Operators.Interleave

private def a1 : Manifest DIM1 Nat := Manifest.fromList (ix1 2) [1, 3]
private def a2 : Manifest DIM1 Nat := Manifest.fromList (ix1 2) [2, 4]

#guard toList (interleave2 a1 a2) == [1, 2, 3, 4]

private def a3 : Manifest DIM1 Nat := Manifest.fromList (ix1 2) [5, 6]

#guard toList (interleave3 a1 a2 a3) == [1, 2, 5, 3, 4, 6]

end Tests.Data.Array.Shaped.Operators.Interleave
