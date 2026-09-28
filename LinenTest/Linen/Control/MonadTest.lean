/-
  Tests for `Linen.Control.Monad`.

  Covers the monad combinators not already in Lean core: `join`,
  `replicateM`, `replicateM_`, `when`, and `unless`.
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

-- `when`/`unless` run the action only on true/false respectively; we use
-- `Option` so the effect is observable (`none` action ⇒ `none` iff it ran).
#guard («when» true  (none : Option Unit)).isNone
#guard («when» false (none : Option Unit)).isSome
#guard («unless» false (none : Option Unit)).isNone
#guard («unless» true  (none : Option Unit)).isSome

-- Reduction laws (checked at compile time).
example (a : Id Unit) : «when» true a = a := when_true a
example (a : Id Unit) : «unless» true a = pure () := unless_true a

end Tests.Control.Monad
