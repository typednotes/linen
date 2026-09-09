/-
  Tests for `Linen.Control.Monad.Effect.Error`.

  Covers `throwError`, `runError`, `catchError`, `orElseValue`, and that errors
  compose with other effects in one row.
-/
import Linen.Control.Monad.Effect.Error
import Linen.Control.Monad.Effect.State

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.Error

namespace Tests.Control.Monad.Effect.Error

-- A computation that never throws succeeds.
#guard (Eff.run (runError (pure 3 : Eff [Error String] Nat))) matches .ok 3

-- A throw aborts with the error.
#guard (Eff.run (runError (do throwError "boom"; pure 3 : Eff [Error String] Nat)))
         matches .error "boom"

-- Everything after a throw is discarded: the continuation is unreachable, so
-- the `put`-like side effect below never happens and the first error wins.
#guard (Eff.run (runError (do
    throwError "first"
    throwError "second"
    pure 0 : Eff [Error String] Nat))) matches .error "first"

-- The error type need not be a string.
#guard (Eff.run (runError (do throwError 404; pure 0 : Eff [Error Nat] Nat)))
         matches .error 404

-- ── catchError ──────────────────────────────────────────────────────────────

-- A caught error is recovered from.
#guard (Eff.run (runError (catchError (do throwError "x"; pure 1)
                            (fun _ => pure 42) : Eff [Error String] Nat)))
         matches .ok 42

-- The handler sees the thrown value.
#guard (Eff.run (runError (catchError (do throwError 7; pure 0)
                            (fun n => pure (n * 2)) : Eff [Error Nat] Nat)))
         matches .ok 14

-- A computation that does not throw is unaffected by a surrounding catch.
#guard (Eff.run (runError (catchError (pure 5)
                            (fun _ => pure 0) : Eff [Error String] Nat)))
         matches .ok 5

-- `catchError` keeps the effect in the row, so a later throw still propagates.
#guard (Eff.run (runError (do
    let a ← catchError (pure 1) (fun _ => pure 0)
    throwError "later"
    pure a : Eff [Error String] Nat))) matches .error "later"

-- An error raised by the handler itself propagates.
#guard (Eff.run (runError (catchError (do throwError "inner"; pure 0)
                            (fun _ => throwError "from handler")
                            : Eff [Error String] Nat)))
         matches .error "from handler"

-- `orElseValue` substitutes a fallback.
#guard (Eff.run (runError (orElseValue (do throwError "x"; pure 1) 99
                            : Eff [Error String] Nat))) matches .ok 99
#guard (Eff.run (runError (orElseValue (pure 1) 99 : Eff [Error String] Nat)))
         matches .ok 1

-- ── Composing with another effect ───────────────────────────────────────────

-- Error and State in one row. State updates made before the throw are visible
-- in the final state, because `runState` is the outer handler here.
#guard (Eff.run (State.runState 0 (runError (do
    State.put 5
    throwError "stop"
    State.put 100 : Eff [Error String, State.State Nat] Unit))))
         matches (.error "stop", 5)

-- Without a throw, both effects run to completion.
#guard (Eff.run (State.runState 0 (runError (do
    State.put 5
    State.modify (fun n : Nat => n + 1)
    State.get : Eff [Error String, State.State Nat] Nat))))
         matches (.ok 6, 6)

-- ── The row records that a computation may fail ─────────────────────────────

-- The signature says: reads an environment-free config, may fail with a
-- `String`, and can do nothing else.
example : Eff [Error String] Nat := do
  let n ← pure 10
  if n > 5 then throwError "too large" else pure n

end Tests.Control.Monad.Effect.Error
