/-
  Tests for `Linen.Data.Array.Shaped.Repr.Manifest` — the `Manifest` array
  representation, `fromList`, `computeS`, `copyS`, `zip`, and `unzip`.
-/
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Repr.Manifest

private def arr : Manifest DIM2 Nat :=
  Manifest.fromList (ix2 2 3) [1, 2, 3, 4, 5, 6]

#guard index arr (ix2 0 0) == 1
#guard index arr (ix2 1 2) == 6
#guard toList arr == [1, 2, 3, 4, 5, 6]

private def darr : Delayed DIM2 Nat :=
  fromFunction (ix2 2 3) (fun ix => match ix with
    | Z.Z :. y :. x => (y * 3 + x + 1).toNat)

#guard computeS darr == arr
#guard copyS arr == arr

private def arrB : Manifest DIM2 Nat :=
  Manifest.fromList (ix2 2 3) [10, 20, 30, 40, 50, 60]

#guard (Manifest.zip arr arrB).elems.toList == [(1, 10), (2, 20), (3, 30), (4, 40), (5, 50), (6, 60)]
#guard (Manifest.unzip (Manifest.zip arr arrB)).fst == arr
#guard (Manifest.unzip (Manifest.zip arr arrB)).snd == arrB

end Tests.Data.Array.Shaped.Repr.Manifest
