/-
  Tests for `Linen.Control.Monad.Effect.Writer`.

  Covers `tell`, `runWriter` (explicit monoid), `runWriterAppend`, `execWriter`
  and `evalWriter`.
-/
import Linen.Control.Monad.Effect.Writer
import Linen.Control.Monad.Effect.State

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.Writer

namespace Tests.Control.Monad.Effect.Writer

-- Nothing told: the accumulator stays at the unit.
#guard Eff.run (runWriter ([] : List Nat) (· ++ ·)
    (pure 1 : Eff [Writer (List Nat)] Nat)) == (1, [])

-- Each `tell` appends, in order.
#guard Eff.run (runWriter ([] : List Nat) (· ++ ·) (do
    tell [1]
    tell [2]
    tell [3]
    pure 0 : Eff [Writer (List Nat)] Nat)) == (0, [1, 2, 3])

-- The value and the output are independent.
#guard Eff.run (runWriter ([] : List String) (· ++ ·) (do
    tell ["a"]
    tell ["b"]
    pure 7 : Eff [Writer (List String)] Nat)) == (7, ["a", "b"])

-- A non-list monoid: string concatenation.
#guard Eff.run (runWriter "" (· ++ ·) (do
    tell "he"
    tell "llo"
    pure () : Eff [Writer String] Unit)) == ((), "hello")

-- A numeric monoid — `(0, +)` is a perfectly good choice of monoid, which is
-- exactly what taking the operations explicitly buys over a fixed typeclass.
#guard Eff.run (runWriter 0 (· + ·) (do
    tell 3
    tell 4
    pure () : Eff [Writer Nat] Unit)) == ((), 7)

-- `runWriterAppend` picks up `[Append ω] [Inhabited ω]` instead.
#guard Eff.run (runWriterAppend (do
    tell ["x"]
    tell ["y"]
    pure 1 : Eff [Writer (List String)] Nat)) == (1, ["x", "y"])

-- `execWriter` keeps only the output; `evalWriter` only the value.
#guard Eff.run (execWriter ([] : List Nat) (· ++ ·) (do
    tell [1]; tell [2]; pure 9 : Eff [Writer (List Nat)] Nat)) == [1, 2]
#guard Eff.run (evalWriter ([] : List Nat) (· ++ ·) (do
    tell [1]; tell [2]; pure 9 : Eff [Writer (List Nat)] Nat)) == 9

-- ── Composing with another effect ───────────────────────────────────────────

-- Writer and State in one row: log each state transition as it happens.
#guard Eff.run (State.runState 0 (runWriterAppend (do
    State.put 1
    tell ["set 1"]
    State.modify (fun n : Nat => n + 10)
    tell ["added 10"]
    State.get : Eff [Writer (List String), State.State Nat] Nat)))
  == ((11, ["set 1", "added 10"]), 11)

-- ── The row records that a computation logs ─────────────────────────────────

-- This computation may accumulate output and nothing else — no state, no IO.
example : Eff [Writer (List String)] Unit := do
  tell ["starting"]
  tell ["done"]

end Tests.Control.Monad.Effect.Writer
