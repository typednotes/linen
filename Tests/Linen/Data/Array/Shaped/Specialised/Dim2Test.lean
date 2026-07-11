/-
  Tests for `Linen.Data.Array.Shaped.Specialised.Dim2` — `isInside2`,
  `isOutside2`, `clampToBorder2`, and `makeBordered2`.
-/
import Linen.Data.Array.Shaped.Specialised.Dim2
import Linen.Data.Array.Shaped.Repr.Manifest

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Specialised.Dim2

private def ex : DIM2 := ix2 3 4

#guard isInside2 ex (ix2 1 1) == true
#guard isInside2 ex (ix2 3 1) == false
#guard isOutside2 ex (ix2 (-1) 1) == true
#guard isOutside2 ex (ix2 1 4) == true
#guard isOutside2 ex (ix2 1 1) == false

#guard clampToBorder2 ex (ix2 1 1) == ix2 1 1
#guard clampToBorder2 ex (ix2 (-1) 1) == ix2 0 1
#guard clampToBorder2 ex (ix2 1 (-1)) == ix2 1 0
#guard clampToBorder2 ex (ix2 5 1) == ix2 2 1
#guard clampToBorder2 ex (ix2 1 9) == ix2 1 3

-- A 4x4 image with a border width of 1: the interior is the single
-- center-most 2x2 block, everything else is border.
private def internal : Manifest DIM2 Nat := Manifest.fromList (ix2 4 4) (List.replicate 16 1)
private def border : Manifest DIM2 Nat := Manifest.fromList (ix2 4 4) (List.replicate 16 9)

private def bordered := makeBordered2 (ix2 4 4) 1 internal border

#guard index bordered (ix2 1 1) == 1
#guard index bordered (ix2 2 2) == 1
#guard index bordered (ix2 0 0) == 9
#guard index bordered (ix2 3 3) == 9
#guard toList bordered ==
  [9, 9, 9, 9,
   9, 1, 1, 9,
   9, 1, 1, 9,
   9, 9, 9, 9]

end Tests.Data.Array.Shaped.Specialised.Dim2
