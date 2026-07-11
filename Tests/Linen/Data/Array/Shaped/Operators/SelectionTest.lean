/-
  Tests for `Linen.Data.Array.Shaped.Operators.Selection` — `select`.
-/
import Linen.Data.Array.Shaped.Operators.Selection
import Linen.Data.Array.Shaped.Repr.Manifest

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Operators.Selection

-- Squares of the even numbers below 10.
#guard toList (select (fun i => i % 2 == 0) (fun i => i * i) 10) == [0, 4, 16, 36, 64]
#guard (Source.extent (select (fun i => i % 2 == 0) (fun i => i * i) 10)) == ix1 5

end Tests.Data.Array.Shaped.Operators.Selection
