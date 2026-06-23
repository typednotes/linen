/-
  Benchmark: N IO-bound waiters, each doing ONE IO-bound wait, run two ways.

  Every waiter blocks on a single shared event that fires after `T` ms. We fork
  `N` of them, fire the event, join them all, and report wall-clock ms.

  * Green (cooperative) — `forkGreen` + `Green.await`. Awaiting frees the pool
    worker via `BaseIO.bindTask`, so the `N` waiters are cheap heap objects
    living on Lean's fixed ~#cores worker pool. Scales to 100 000+.

  * Lean Task (blocking) — `IO.asTask` + `IO.wait`. The wait *holds* its worker;
    to avoid deadlock Lean's runtime spawns extra OS threads, roughly one per
    blocked waiter. This is the dangerous side: forking tens of thousands of
    blocked tasks can exhaust the kernel and panic the machine, so we only run
    it up to `taskCap` (default 2000). Green has no such limit.

  Run: lake exe bench [T_ms] [taskCap]   (defaults: 25 2000)
-/
import Linen.Control.Concurrent

open Control.Concurrent

/-- Fork `n` green threads, each awaiting the shared event; fire it after `T`
ms; join all. Cooperative `await` frees the worker, so OS threads stay ≈ #cores
no matter how large `n` is. Returns wall-clock ms. -/
def runGreen (n T : Nat) : IO Nat := do
  let ev ← IO.Promise.new
  let t0 ← IO.monoMsNow
  let mut tids : Array ThreadId := Array.mkEmpty n
  for _ in [0:n] do
    tids := tids.push (← forkGreen (do let _ ← Green.Green.await ev.result!))
  let _fire ← IO.asTask (prio := .dedicated) (do IO.sleep T.toUInt32; ev.resolve ())
  for tid in tids do waitThread tid
  let t1 ← IO.monoMsNow
  pure (t1 - t0)

/-- Spawn `n` Lean tasks, each blocking on the shared event with `IO.wait`; fire
it after `T` ms; join all. Each blocked wait holds a worker, so the runtime
grows the OS-thread count to ≈ `n`. Returns wall-clock ms. -/
def runTasks (n T : Nat) : IO Nat := do
  let ev ← IO.Promise.new
  let t0 ← IO.monoMsNow
  let mut tasks : Array (Task (Except IO.Error Unit)) := Array.mkEmpty n
  for _ in [0:n] do
    tasks := tasks.push (← IO.asTask (do let _ ← IO.wait ev.result!))
  let _fire ← IO.asTask (prio := .dedicated) (do IO.sleep T.toUInt32; ev.resolve ())
  for t in tasks do let _ ← IO.wait t
  let t1 ← IO.monoMsNow
  pure (t1 - t0)

def main (args : List String) : IO Unit := do
  let arg := fun (i d : Nat) => (args[i]?.bind String.toNat?).getD d
  let T       := arg 0 25
  let taskCap := arg 1 2000
  let sizes := [200, 1000, 2000, 10000, 100000]
  -- Pass 1: Green at every size, on a fresh ~#cores pool (no blocking yet, so
  -- the pool never grows — these are honest green numbers).
  let mut greens : Array Nat := Array.mkEmpty sizes.length
  for n in sizes do
    greens := greens.push (← runGreen n T)
  -- Pass 2: Lean Tasks up to the safe cap (this grows the OS-thread pool).
  let mut tasks : Array (Option Nat) := Array.mkEmpty sizes.length
  for n in sizes do
    if n ≤ taskCap then
      let ms ← runTasks n T
      tasks := tasks.push (some ms)
    else
      tasks := tasks.push none
  -- Report
  IO.println s!"N IO-bound waiters · 1 wait each · event fires after {T} ms"
  IO.println s!"(Lean Task side capped at {taskCap}: each blocked wait costs an OS thread)\n"
  IO.println s!"        N | Green (await) | Lean Task (IO.wait)"
  IO.println s!"  --------+---------------+--------------------"
  for (n, g, b) in sizes.zip (greens.toList.zip tasks.toList) do
    match b with
    | some ms => IO.println s!"  {n} | Green {g} ms | Task {ms} ms"
    | none    => IO.println s!"  {n} | Green {g} ms | Task skipped (would need ~{n} OS threads)"
  IO.println s!"\n→ Green waiters are heap objects on a ~#cores pool, so 100000 of them are fine."
  IO.println s!"  Lean Tasks that block cost ~one OS thread each — they can't safely scale."
