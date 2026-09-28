/-
  Tests for `Linen.Data.Array.Shaped.Repr.Delayed` — the `Delayed` array
  representation, `fromFunction`, `toFunction`, and `delay`.
-/
import Linen.Data.Array.Shaped.Repr.Delayed
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Repr.Delayed

private def arr : Delayed DIM2 Nat :=
  fromFunction (ix2 2 3) (fun ix => match ix with
    | Z.Z :. y :. x => (y * 3 + x).toNat)

#guard index arr (ix2 0 0) == 0
#guard index arr (ix2 1 2) == 5
#guard toList arr == [0, 1, 2, 3, 4, 5]

private def darr : Delayed DIM2 Nat := delay arr

#guard (toFunction arr).fst == ix2 2 3
#guard index darr (ix2 1 1) == 4

end Tests.Data.Array.Shaped.Repr.Delayed
