/-
  Examples.STM — `Control.Monad.STM` and `Control.Concurrent.STM.{TVar,TMVar,TQueue}`
  end-to-end.

  * `demoTVarCounter` — ten concurrent tasks each `atomically` increment a
    shared `TVar Nat` a thousand times; the demo checks the final count is
    exactly `10 * 1000` (no lost updates under the global-mutex-serialized
    transactions).
  * `demoTMVarHandoff` — a producer/consumer pair rendezvous through an empty
    `TMVar`: the consumer's `takeTMVar` retries (blocks) until the producer's
    `putTMVar` supplies a value, for each of a few messages in turn.
  * `demoTQueueFifo` — values pushed with `writeTQueue` come back out via
    `readTQueue` in the same order (checking the two-list representation
    doesn't reorder anything), and `tryReadTQueue` on an empty queue reports
    `none` without blocking.
  * `demoOrElseCheck` — `STM.orElse` falls through to its second argument when
    the first `retry`s (via `check false`), and commits the first when it
    doesn't.

  Args: (none) -- runs every check below and exits non-zero on any mismatch
-/
import Linen.Control.Monad.STM
import Linen.Control.Concurrent.STM.TVar
import Linen.Control.Concurrent.STM.TMVar
import Linen.Control.Concurrent.STM.TQueue
import Linen.Control.Concurrent

open Control.Monad
open Control.Concurrent.STM

namespace Examples.STM

/-! ### `TVar`: concurrent counter increments -/

def demoTVarCounter : IO Bool := do
  IO.println "── ten tasks, 1000 atomically-incremented TVar writes each ──"
  let counter ← TVar.newTVarIO (0 : Nat)
  let tasks ← (List.range 10).mapM fun _ =>
    IO.asTask (prio := .dedicated) do
      for _ in [0:1000] do
        atomically (TVar.modifyTVar' counter (· + 1))
  for t in tasks do
    match t.get with
    | .ok () => pure ()
    | .error e => throw e
  let final ← atomically (TVar.readTVar counter)
  IO.println s!"  final count: {final}"
  pure (final == 10 * 1000)

/-! ### `TMVar`: blocking producer/consumer rendezvous -/

def demoTMVarHandoff : IO Bool := do
  IO.println "── producer/consumer handoff through an empty TMVar ──"
  let slot ← TMVar.newEmptyTMVarIO (α := String)
  let received ← IO.mkRef (#[] : Array String)
  let messages := ["ping", "pong", "done"]
  let consumer ← IO.asTask (prio := .dedicated) do
    for _ in messages do
      let msg ← atomically (TMVar.takeTMVar slot)
      received.modify (·.push msg)
  for msg in messages do
    atomically (TMVar.putTMVar slot msg)
  match consumer.get with
  | .ok () => pure ()
  | .error e => throw e
  let seen ← received.get
  IO.println s!"  received, in order: {seen.toList}"
  pure (seen.toList == messages)

/-! ### `TQueue`: FIFO ordering -/

def demoTQueueFifo : IO Bool := do
  IO.println "── FIFO order through writeTQueue / readTQueue ──"
  let q ← atomically (TQueue.newTQueue (α := Nat))
  for n in [1, 2, 3, 4, 5] do
    atomically (TQueue.writeTQueue q n)
  let mut out : List Nat := []
  for _ in [0:5] do
    out := (← atomically (TQueue.readTQueue q)) :: out
  let empty ← atomically (TQueue.tryReadTQueue q)
  IO.println s!"  dequeued, in order: {out.reverse}, tryRead on empty: {empty}"
  pure (out.reverse == [1, 2, 3, 4, 5] && empty == none)

/-! ### `orElse` / `check`: composable alternative transactions -/

def demoOrElseCheck : IO Bool := do
  IO.println "── orElse falls through to its alternative on retry ──"
  let firstTaken := STM.orElse (do STM.check false; pure "first") (pure "second")
  let r1 ← atomically firstTaken
  let secondTaken := STM.orElse (pure "first") (do STM.check false; pure "second")
  let r2 ← atomically secondTaken
  IO.println s!"  retry-then-alternative: {r1}, succeed-without-retry: {r2}"
  pure (r1 == "second" && r2 == "first")

def run (_args : List String) : IO Unit := do
  let okCounter ← demoTVarCounter
  IO.println ""
  let okHandoff ← demoTMVarHandoff
  IO.println ""
  let okFifo ← demoTQueueFifo
  IO.println ""
  let okOrElse ← demoOrElseCheck
  IO.println ""
  if okCounter && okHandoff && okFifo && okOrElse then
    IO.println "stm demo done · all checks passed"
  else
    throw (IO.userError "stm demo done · some checks failed")

end Examples.STM
