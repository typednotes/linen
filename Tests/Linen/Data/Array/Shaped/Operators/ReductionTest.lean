/-
  Tests for `Linen.Data.Array.Shaped.Operators.Reduction` — `fold`,
  `foldAll`, `sum`, `sumAll`, and `equalsS`.
-/
import Linen.Data.Array.Shaped.Operators.Reduction
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Operators.Reduction

private def mat : Manifest DIM2 Nat := Manifest.fromList (ix2 2 2) [1, 2, 3, 4]

#guard toList (fold (· + ·) 0 mat) == [3, 7]
#guard toList (sum mat) == [3, 7]

#guard foldAll (· + ·) 0 mat == 10
#guard sumAll mat == 10

private def mat' : Manifest DIM2 Nat := Manifest.fromList (ix2 2 2) [1, 2, 3, 4]
private def matDiff : Manifest DIM2 Nat := Manifest.fromList (ix2 2 2) [1, 2, 3, 5]

#guard equalsS mat mat' == true
#guard equalsS mat matDiff == false

end Tests.Data.Array.Shaped.Operators.Reduction
