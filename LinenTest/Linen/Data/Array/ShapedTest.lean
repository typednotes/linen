/-
  Tests for `Linen.Data.Array.Shaped` — an end-to-end smoke test exercising
  the root aggregator: build a `Manifest`, `map` it, apply a stencil, and
  reduce the result.
-/
import Linen.Data.Array.Shaped

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped

private def img : Manifest DIM2 Int := Manifest.fromList (ix2 2 2) [1, 2, 3, 4]

private def doubled := map (· * 2) img

#guard toList doubled == [2, 4, 6, 8]
#guard sumAll doubled == 20

private def edgeStencil : Stencil DIM2 Int := makeStencil2 1 1 (fun _ => some 1)

#guard toList (mapStencil2 (Boundary.const 0) edgeStencil img) == [1, 2, 3, 4]

end Tests.Data.Array.Shaped
