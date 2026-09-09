/-
  Tests for `Linen.Control.Monad.Effect.Trace`.

  Covers `trace`, `runTracePure`, `ignoreTrace` and the `IO`-printing
  `runTrace`.
-/
import Linen.Control.Monad.Effect.Trace
import Linen.Control.Monad.Effect.State

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.Trace

namespace Tests.Control.Monad.Effect.Trace

-- Nothing traced: no messages.
#guard Eff.run (runTracePure (pure 1 : Eff [Trace] Nat)) == (1, [])

-- Messages are collected in the order emitted.
#guard Eff.run (runTracePure (do
    trace "first"
    trace "second"
    trace "third"
    pure 0 : Eff [Trace] Nat)) == (0, ["first", "second", "third"])

-- The value is unaffected by tracing.
#guard Eff.run (runTracePure (do
    trace "computing"
    pure (2 + 2) : Eff [Trace] Nat)) == (4, ["computing"])

-- `ignoreTrace` discards the messages.
#guard Eff.run (ignoreTrace (do
    trace "noise"
    pure 5 : Eff [Trace] Nat)) == 5

-- Tracing interleaved with computation records intermediate values.
#guard Eff.run (runTracePure (do
    let a := 3
    trace s!"a = {a}"
    let b := a * 2
    trace s!"b = {b}"
    pure b : Eff [Trace] Nat)) == (6, ["a = 3", "b = 6"])

-- ── Composing with another effect ───────────────────────────────────────────

-- Trace what the state does, as it does it.
#guard Eff.run (State.runState 0 (runTracePure (do
    State.put 1
    trace "put 1"
    State.modify (fun n : Nat => n + 41)
    trace "added 41"
    State.get : Eff [Trace, State.State Nat] Nat)))
  == ((42, ["put 1", "added 41"]), 42)

-- ── The row records that a computation logs ─────────────────────────────────

-- The point of the effect: `Eff [Trace] α` announces that this computation
-- logs, and a row without `Trace` provably does not — the discipline ambient
-- `IO.println` cannot offer.
example : Eff [Trace] Nat := do
  trace "starting"
  pure 1

-- ── The IO handler ──────────────────────────────────────────────────────────

-- The captured `info` block below is `runTrace`'s actual stdout, so this
-- asserts that the IO handler really printed both messages, in order.
/--
info: one
two
---
info: (2, ["one", "two"])
-/
#guard_msgs in
#eval show IO (Nat × List String) from do
  -- `runTrace` prints; run it here only to confirm it executes, and check the
  -- messages via the pure handler so the test asserts on values, not stdout.
  let n ← runTrace (do
    trace "one"
    trace "two"
    pure 2 : Eff [Trace] Nat)
  pure (n, (Eff.run (runTracePure (do
    trace "one"
    trace "two"
    pure 2 : Eff [Trace] Nat))).2)

end Tests.Control.Monad.Effect.Trace
