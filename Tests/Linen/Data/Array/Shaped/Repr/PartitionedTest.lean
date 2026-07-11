/-
  Tests for `Linen.Data.Array.Shaped.Repr.Partitioned` — the `Partitioned`
  array representation and `Range.inRange`.
-/
import Linen.Data.Array.Shaped.Repr.Partitioned
import Linen.Data.Array.Shaped.Repr.Manifest
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Repr.Partitioned

-- Left half (x < 2) reads from `interior`; right half reads from `border`.
private def interior : Manifest DIM2 Nat := Manifest.fromList (ix2 2 4) (List.replicate 8 1)
private def border : Manifest DIM2 Nat := Manifest.fromList (ix2 2 4) (List.replicate 8 9)

private def range : Range DIM2 :=
  ⟨ix2 0 0, ix2 2 2, fun ix => match ix with | Z.Z :. _y :. x => x < 2⟩

#guard range.inRange (ix2 0 0) == true
#guard range.inRange (ix2 0 3) == false

private def parr : Partitioned Manifest Manifest DIM2 Nat :=
  ⟨ix2 2 4, range, interior, border⟩

#guard index parr (ix2 0 0) == 1
#guard index parr (ix2 1 1) == 1
#guard index parr (ix2 0 2) == 9
#guard toList parr == [1, 1, 9, 9, 1, 1, 9, 9]

end Tests.Data.Array.Shaped.Repr.Partitioned
