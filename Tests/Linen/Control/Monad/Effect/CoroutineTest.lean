/-
  Tests for `Linen.Control.Monad.Effect.Coroutine`.

  Covers `yield`, `yield'`, `runC`, `interposeC`, `replyC` and the `Status`
  accessors — including a step-bounded driver, since running a coroutine to
  completion is not total in general.
-/
import Linen.Control.Monad.Effect.Coroutine
import Linen.Control.Monad.Effect.State

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.Coroutine

namespace Tests.Control.Monad.Effect.Coroutine

-- ── A bounded driver ────────────────────────────────────────────────────────

/-- Run a coroutine for at most `n` steps, answering each yield with `reply`.

    Recurses structurally on the step budget, not on the coroutine: a coroutine
    may yield forever, so no total driver can exist without a bound. The bound is
    part of this helper's specification rather than a dodge — `none` means "still
    running after `n` steps". -/
def driveFor {a b r : Type} :
    Nat → Status [] a b r → (a → b) → List a × Option r
  | _,     .done x,          _     => ([], some x)
  | 0,     .continue _ _,    _     => ([], none)
  | n + 1, .continue v cont, reply =>
    let (rest, res) := driveFor n (Eff.run (cont (reply v))) reply
    (v :: rest, res)

-- ── Status of a finished vs. suspended coroutine ────────────────────────────

-- A computation that never yields is immediately done.
#guard (Eff.run (runC (pure 5 : Eff [Yield Nat Nat] Nat))).isDone
#guard (Eff.run (runC (pure 5 : Eff [Yield Nat Nat] Nat))).result? == some 5
#guard (Eff.run (runC (pure 5 : Eff [Yield Nat Nat] Nat))).yielded? == none

-- One that yields is suspended, reporting the yielded value.
#guard !(Eff.run (runC (yield' 1 : Eff [Yield Nat Nat] Nat))).isDone
#guard (Eff.run (runC (yield' 1 : Eff [Yield Nat Nat] Nat))).yielded? == some 1
#guard (Eff.run (runC (yield' 1 : Eff [Yield Nat Nat] Nat))).result? == none

-- ── Driving a coroutine to completion ───────────────────────────────────────

-- Two yields, each answered by multiplying by ten; the results are summed.
#guard (driveFor 10 (Eff.run (runC (do
            let x ← yield' 1
            let y ← yield' 2
            pure (x + y) : Eff [Yield Nat Nat] Nat))) (· * 10))
       == ([1, 2], some 30)

-- `yield`'s embedded function transforms the reply before the coroutine sees
-- it: here the reply is doubled on the way in, on top of the driver's ×10.
#guard (driveFor 10 (Eff.run (runC (do
            let x ← yield 1 (fun n : Nat => n * 2)
            pure x : Eff [Yield Nat Nat] Nat))) (· * 10))
       == ([1], some 20)

-- The coroutine's own values determine what it yields next.
#guard (driveFor 10 (Eff.run (runC (do
            let a ← yield' 1
            let b ← yield' (a + 1)
            pure (a + b) : Eff [Yield Nat Nat] Nat))) (· * 10))
       == ([1, 11], some 120)

-- A budget too small leaves the coroutine unfinished — `none`, with the values
-- yielded so far.
#guard (driveFor 1 (Eff.run (runC (do
            let x ← yield' 1
            let y ← yield' 2
            pure (x + y) : Eff [Yield Nat Nat] Nat))) (· * 10))
       == ([1], none)

-- A coroutine that yields more times than the budget allows still reports
-- partial progress rather than diverging.
#guard (driveFor 3 (Eff.run (runC (do
            let _ : Nat ← yield' 1
            let _ : Nat ← yield' 2
            let _ : Nat ← yield' 3
            let _ : Nat ← yield' 4
            let _ : Nat ← yield' 5
            pure 0 : Eff [Yield Nat Nat] Nat))) (fun _ => 0))
       == ([1, 2, 3], none)

-- ── Yielding a different type than is replied ───────────────────────────────

-- The yielded and replied types are independent: yield a `String`, get a `Nat`.
#guard (driveFor 10 (Eff.run (runC (do
            let n ← yield' "how long?"
            pure (n + 1) : Eff [Yield String Nat] Nat)))
          (fun s => s.length))
       == (["how long?"], some 10)

-- ── interposeC keeps `Yield` in the row ─────────────────────────────────────

-- `interposeC` reports the status *without* removing `Yield` from the row, so
-- the result still carries the effect and has to be interpreted rather than
-- `runC`'d. It suspends at the first yield, just as `runC` would.
#guard (Eff.run (interpret
          (fun | .yield v f => .protect (f (v * 10)))
          (interposeC (a := Nat) (b := Nat) (do
            let x ← yield' 1
            pure (x + 1) : Eff [Yield Nat Nat] Nat)))).yielded? == some 1

#guard !(Eff.run (interpret
          (fun | .yield v f => .protect (f (v * 10)))
          (interposeC (a := Nat) (b := Nat) (do
            let x ← yield' 1
            pure (x + 1) : Eff [Yield Nat Nat] Nat)))).isDone

-- ── Composing with another effect ───────────────────────────────────────────

-- A coroutine that also carries state: the state lives in the row rather than
-- in the coroutine's own frame, so it survives suspension.
--
-- Note the explicit `Eff.bindH`: `Status` lives in `Type 1` while the rest of
-- this computation is at `Type 0`, and `do`-notation goes through the
-- homogeneous `Monad.bind`. Crossing payload universes is exactly what the
-- heterogeneous bind is for.
#guard Eff.run (State.runState 0 (Eff.bindH
    (runC (do
      let x ← yield' 1
      pure x : Eff [Yield Nat Nat, State.State Nat] Nat))
    (fun st => do
      State.put ((Status.yielded? st).getD 99)
      State.get))) == (1, 1)

end Tests.Control.Monad.Effect.Coroutine
