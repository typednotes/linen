/-
  Tests for `Linen.Control.Monad`.

  Covers the monad combinators not already in Lean core: `join`,
  `replicateM`, and `replicateM_`.
-/
import Linen.Control.Monad

open Control.Monad

namespace Tests.Control.Monad

-- `join` flattens one monadic layer.
#guard join (some (some 3)) == some 3
#guard join (some (none : Option Nat)) == none
#guard join (none : Option (Option Nat)) == none

-- `replicateM` collects `n` copies of the result.
#guard replicateM 3 (some 7) == some [7, 7, 7]
#guard replicateM 0 (some 7) == some []
#guard replicateM 2 (none : Option Nat) == none

-- `replicateM_` runs the action `n` times, discarding results.
#guard replicateM_ 3 (some 7) == some ()
#guard replicateM_ 2 (none : Option Nat) == none

-- `join_pure` law holds for the `Id` monad (checked by reduction).
example (x : Id Nat) : join (pure x) = x := join_pure x

end Tests.Control.Monad
