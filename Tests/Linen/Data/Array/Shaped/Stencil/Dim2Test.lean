/-
  Tests for `Linen.Data.Array.Shaped.Stencil.Dim2` — `mapStencil2` and
  `forStencil2`.
-/
import Linen.Data.Array.Shaped.Stencil.Dim2
import Linen.Data.Array.Shaped.Repr.Manifest

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Stencil.Dim2

-- A 3x3 array:
--   1 2 3
--   4 5 6
--   7 8 9
private def img : Manifest DIM2 Int := Manifest.fromList (ix2 3 3) [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- Sums every neighbour in a 3x3 window, treating out-of-bounds cells as 0.
private def sumStencil : Stencil DIM2 Int := makeStencil2 3 3 (fun _ => some 1)

private def summed := mapStencil2 (Boundary.const 0) sumStencil img

#guard toList summed ==
  [12, 21, 16,
   27, 45, 33,
   24, 39, 28]

#guard toList (forStencil2 (Boundary.const 0) img sumStencil) == toList summed

-- With clamped boundaries, the corner sees its own value four times
-- (itself plus the three clamped-in-bounds duplicates within the window).
private def summedClamp := mapStencil2 Boundary.clamp sumStencil img
#guard index summedClamp (ix2 0 0) == 4 * 1 + 2 * 2 + 2 * 4 + 5

end Tests.Data.Array.Shaped.Stencil.Dim2
