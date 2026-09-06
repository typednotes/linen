/-
  Tests for `Linen.Control.Monad.Freer.State`.

  Covers the state effect over `Eff`: `get`, `put`, `modify`, `gets`,
  `runState`, `evalState`, `execState` — and running the state effect together
  with the reader effect in a single row, which is the point of an effect row.
-/
import Linen.Control.Monad.Freer.State
import Linen.Control.Monad.Freer.Reader

open Data.OpenUnion Control.Monad.Freer Control.Monad.Freer.State

namespace Tests.Control.Monad.Freer.State

-- `get` returns the current state, leaving it unchanged.
#guard Eff.run (runState 5 (get : Eff [State Nat] Nat)) == (5, 5)

-- `put` replaces the state.
#guard Eff.run (runState 5 (do put 9; get : Eff [State Nat] Nat)) == (9, 9)

-- `modify` applies a function to the state.
#guard Eff.run (runState 5 (do modify (· * 3); get : Eff [State Nat] Nat)) == (15, 15)

-- `gets` projects the state.
#guard Eff.run (runState 5 (gets (· + 1) : Eff [State Nat] Nat)) == (6, 5)

-- State threads across a sequence of operations.
#guard Eff.run (runState 0 (do
    modify (· + 1)
    modify (· + 10)
    modify (· + 100)
    get : Eff [State Nat] Nat)) == (111, 111)

-- The value and the final state can differ.
#guard Eff.run (runState 1 (do
    let before ← get
    put (before + 41)
    pure before : Eff [State Nat] Nat)) == (1, 42)

-- `evalState` keeps only the value; `execState` only the final state.
#guard Eff.run (evalState 1 (do put 8; get : Eff [State Nat] Nat)) == 8
#guard Eff.run (execState 1 (do put 8; pure 0 : Eff [State Nat] Nat)) == 8

-- Non-numeric state works the same way.
#guard Eff.run (execState "" (do
    modify (· ++ "a")
    modify (· ++ "b") : Eff [State String] Unit)) == "ab"

-- ── Reader and State together in one row ────────────────────────────────────

-- Two effects in a single row, each eliminated by its own handler. The
-- handlers compose in either nesting order, since neither uses the other.
#guard Eff.run (runState 10 (Reader.runReader 5 (do
    let r ← Reader.ask
    put (r * 2)
    get : Eff [Reader.Reader Nat, State Nat] Nat))) == (10, 10)

-- Reading the environment repeatedly while accumulating into the state.
#guard Eff.run (runState 0 (Reader.runReader 3 (do
    let step ← Reader.ask
    modify (fun n : Nat => n + step)
    modify (fun n : Nat => n + step)
    get : Eff [Reader.Reader Nat, State Nat] Nat))) == (6, 6)

-- Handling state first, reader second: same result, different row order.
#guard Eff.run (Reader.runReader 4 (runState 100 (do
    let bonus ← Reader.ask
    modify (fun n : Nat => n + bonus)
    get : Eff [State Nat, Reader.Reader Nat] Nat))) == (104, 104)

-- ── The row bounds what the computation may do ──────────────────────────────

-- This program may read an environment and mutate a counter — and nothing
-- else. No filesystem, no network, no arbitrary `IO`, enforced by the type.
example : Eff [Reader.Reader Nat, State Nat] Unit := do
  let step ← Reader.ask
  modify (fun n : Nat => n + step)

end Tests.Control.Monad.Freer.State
