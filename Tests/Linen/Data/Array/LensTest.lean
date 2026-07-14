/-
  Tests for `Linen.Data.Array.Lens`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Setter
import Linen.Data.Array.Lens

open Control.Lens

namespace Tests.Linen.Data.Array.Lens

#guard preview (ix 1) (#[10, 20, 30] : Array Nat) = some 20
#guard preview (ix 5) (#[10, 20, 30] : Array Nat) = none
#guard over (ix 1) (· + 1) (#[10, 20, 30] : Array Nat) = #[10, 21, 30]
#guard over (ix 5) (· + 1) (#[10, 20, 30] : Array Nat) = #[10, 20, 30]

end Tests.Linen.Data.Array.Lens
