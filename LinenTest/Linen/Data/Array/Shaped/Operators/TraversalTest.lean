/-
  Tests for `Linen.Data.Array.Shaped.Operators.Traversal` — `traverse`,
  `traverse2`, and their `unsafe` counterparts.
-/
import Linen.Data.Array.Shaped.Operators.Traversal
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Operators.Traversal

private def arr : Manifest DIM1 Nat := Manifest.fromList (ix1 4) [1, 2, 3, 4]

-- Reverse the array by transforming the lookup index.
private def reversed : Delayed DIM1 Nat :=
  traverse arr id (fun get ix => match ix with
    | Z.Z :. i => get (ix1 (3 - i)))

#guard toList reversed == [4, 3, 2, 1]

private def reversedUnsafe : Delayed DIM1 Nat :=
  unsafeTraverse arr id (fun get ix => match ix with
    | Z.Z :. i => get (ix1 (3 - i)))

#guard toList reversedUnsafe == [4, 3, 2, 1]

private def arr2 : Manifest DIM1 Nat := Manifest.fromList (ix1 4) [10, 20, 30, 40]

private def summed : Delayed DIM1 Nat :=
  traverse2 arr arr2 (fun sh _ => sh) (fun getA getB ix => getA ix + getB ix)

#guard toList summed == [11, 22, 33, 44]

end Tests.Data.Array.Shaped.Operators.Traversal
