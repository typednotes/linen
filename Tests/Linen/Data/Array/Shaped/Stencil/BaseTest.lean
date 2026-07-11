/-
  Tests for `Linen.Data.Array.Shaped.Stencil.Base` — `makeStencil` and
  `makeStencil2`.
-/
import Linen.Data.Array.Shaped.Stencil.Base

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Stencil.Base

-- A 1x3 averaging-by-sum stencil: coefficient 1 at each of the three
-- horizontal offsets, undefined (skipped) elsewhere.
private def avg : Stencil DIM2 Int :=
  makeStencil2 1 3 (fun ix => match ix with
    | Z.Z :. 0 :. x => if x >= -1 && x <= 1 then some 1 else none
    | _ => none)

#guard avg.zero == 0
#guard avg.acc (ix2 0 (-1)) 5 avg.zero == 5
#guard avg.acc (ix2 0 2) 5 avg.zero == avg.zero

end Tests.Data.Array.Shaped.Stencil.Base
