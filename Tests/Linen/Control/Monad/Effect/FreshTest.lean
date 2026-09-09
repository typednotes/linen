/-
  Tests for `Linen.Control.Monad.Effect.Fresh`.

  Covers `fresh`, `runFresh` and `runFresh0`.
-/
import Linen.Control.Monad.Effect.Fresh
import Linen.Control.Monad.Effect.State

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.Fresh

namespace Tests.Control.Monad.Effect.Fresh

-- The first request from 0 is 0.
#guard Eff.run (runFresh0 (fresh : Eff [Fresh] Nat)) == 0

-- Successive requests are distinct and increasing.
#guard Eff.run (runFresh0 (do
    let a ← fresh
    let b ← fresh
    let c ← fresh
    pure [a, b, c] : Eff [Fresh] (List Nat))) == [0, 1, 2]

-- The counter starts wherever `runFresh` is told to.
#guard Eff.run (runFresh 100 (do
    let a ← fresh
    let b ← fresh
    pure (a, b) : Eff [Fresh] (Nat × Nat))) == (100, 101)

-- A computation that asks for nothing is unaffected.
#guard Eff.run (runFresh0 (pure 7 : Eff [Fresh] Nat)) == 7

-- Names really are unique: no value repeats across many requests.
#guard (Eff.run (runFresh0 (do
    let xs ← (List.range 10).mapM (fun _ => fresh)
    pure xs : Eff [Fresh] (List Nat)))).eraseDups.length == 10

-- ── Composing with another effect ───────────────────────────────────────────

-- Fresh names alongside state: label each state value with a fresh id.
#guard Eff.run (State.runState ([] : List (Nat × Nat)) (runFresh0 (do
    let i ← fresh
    State.modify (fun xs : List (Nat × Nat) => xs ++ [(i, 10)])
    let j ← fresh
    State.modify (fun xs : List (Nat × Nat) => xs ++ [(j, 20)])
    : Eff [Fresh, State.State (List (Nat × Nat))] Unit)))
  == ((), [(0, 10), (1, 20)])

-- ── The row records that a computation mints names ──────────────────────────

example : Eff [Fresh] (Nat × Nat) := do
  let a ← fresh
  let b ← fresh
  pure (a, b)

end Tests.Control.Monad.Effect.Fresh
