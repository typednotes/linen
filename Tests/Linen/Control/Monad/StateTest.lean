/-
  Tests for `Linen.Control.Monad.State`.

  Covers the Haskell `mtl` names built on Lean's `StateT`/`get`/`set`/
  `modifyGet`: `get`, `put`, `modify`, `gets`, `runStateT`, `evalStateT`,
  `execStateT`, `runState`, `evalState`, `execState`.
-/
import Linen.Control.Monad.State

open Control.Monad.State

namespace Tests.Control.Monad.State

-- `get` returns the state unchanged, paired with the state.
#guard runState (get : State Nat Nat) 5 == (5, 5)

-- `put` replaces the state.
#guard runState (put 9 : State Nat Unit) 5 == ((), 9)

-- `modify` transforms the state, discarding the old value.
#guard runState (modify (· + 1) : State Nat Unit) 5 == ((), 6)

-- `gets` projects a function over the state.
#guard runState (gets (· * 2) : State Nat Nat) 5 == (10, 5)

-- `evalState`/`execState` keep only the value / only the final state.
#guard evalState (do let _ ← modify (· + 1); gets (· * 10) : State Nat Nat) 5 == 60
#guard execState (do let _ ← modify (· + 1); gets (· * 10) : State Nat Nat) 5 == 6

-- `runStateT`/`evalStateT`/`execStateT` agree with the pure variants under `Id`.
#guard Id.run (runStateT (modify (· + 1) : StateT Nat Id Unit) 5) == ((), 6)
#guard Id.run (evalStateT (gets (· * 2) : StateT Nat Id Nat) 5) == 10
#guard Id.run (execStateT (put 9 : StateT Nat Id Unit) 5) == 9

-- Reduction laws (checked at compile time).
example (a : Nat) (s : Nat) : runState (pure a : State Nat Nat) s = (a, s) := runState_pure a s
example (a : Nat) (s : Nat) : evalState (pure a : State Nat Nat) s = a := evalState_pure a s
example (a : Nat) (s : Nat) : execState (pure a : State Nat Nat) s = s := execState_pure a s
example (s : Nat) : runState (get : State Nat Nat) s = (s, s) := runState_get s
example (s s' : Nat) : execState (put s' : State Nat Unit) s = s' := execState_put s s'

end Tests.Control.Monad.State
