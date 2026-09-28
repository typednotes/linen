/-
  Tests for `Linen.Control.Lens.Each`.
-/
import Linen.Control.Lens.Each
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Setter

open Control.Lens

namespace Tests.Linen.Control.Lens.Each

-- ── `List` / `Option` / `Array` — via `Data.Traversable.traverse` ──

#guard toListOf each ([1, 2, 3] : List Nat) = [1, 2, 3]
#guard toListOf each ([] : List Nat) = []
#guard toListOf each (some 5 : Option Nat) = [5]
#guard toListOf each (none : Option Nat) = []
#guard toListOf each (#[1, 2, 3] : Array Nat) = [1, 2, 3]

#guard (over each (· + 1) ([1, 2, 3] : List Nat)) = [2, 3, 4]
#guard (over each (· + 1) (some 5 : Option Nat)) = some 6
#guard (over each (· + 1) (#[1, 2, 3] : Array Nat)) = #[2, 3, 4]

-- ── tuples — up to a 4-tuple ─────────────────────

#guard toListOf each ((1, 2) : Nat × Nat) = [1, 2]
#guard toListOf each ((1, 2, 3) : Nat × Nat × Nat) = [1, 2, 3]
#guard toListOf each ((1, 2, 3, 4) : Nat × Nat × Nat × Nat) = [1, 2, 3, 4]

#guard (over each (· + 1) ((1, 2) : Nat × Nat)) = (2, 3)
#guard (over each (· + 1) ((1, 2, 3) : Nat × Nat × Nat)) = (2, 3, 4)
#guard (over each (· + 1) ((1, 2, 3, 4) : Nat × Nat × Nat × Nat)) = (2, 3, 4, 5)

end Tests.Linen.Control.Lens.Each
