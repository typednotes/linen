/-
  Green-thread scheduler

  An M:N scheduler inspired by GHC's RTS: many lightweight green threads are
  multiplexed onto Lean's built-in task thread pool instead of spawning a
  dedicated OS thread per fork.

  ## Design

  * `schedule` submits work to Lean's thread pool via `IO.asTask` (default
    priority) — O(1), uses the bounded thread pool instead of spawning a
    dedicated OS thread per fork.
  * The thread pool has a fixed number of workers (≈ CPU cores).
  * When a green thread blocks (e.g. on `IO.wait`), it occupies a pool thread —
    the same trade-off as GHC, where a blocked thread occupies a capability.

  ## Differences from GHC

  * **No stack switching:** Lean has no setjmp/longjmp, so a green thread that
    calls `IO.wait` blocks its worker OS thread.
  * **No preemption:** green threads run to completion or until they
    voluntarily yield / block.
  * **Cooperative cancellation:** via `Std.CancellationToken`, not async
    exceptions.

  ## Guarantees (documented)

  * FIFO fairness depends on Lean's task scheduler implementation.
-/

import Std.Sync.CancellationToken

namespace Control.Concurrent.Scheduler

/-! ### GreenThread -/

/-- A positive natural number. -/
def PosNat := { n : Nat // n > 0 }

instance : Nonempty PosNat := ⟨⟨1, by omega⟩⟩
instance : BEq PosNat where beq a b := a.val == b.val
instance : Hashable PosNat where hash p := hash p.val
instance : ToString PosNat where toString p := toString p.val
instance : Repr PosNat where reprPrec p n := reprPrec p.val n

/-- A lightweight green thread queued for execution.

* `id` — unique identifier (monotonic, $\ge 1$).
* `action` — the `IO Unit` closure to run.
* `token` — cooperative cancellation. -/
structure GreenThread where
  id      : PosNat
  action  : IO Unit
  token   : Std.CancellationToken

/-! ### Scheduling -/

/-- Submit a green thread to Lean's thread pool. Returns the `Task` that
resolves when the action completes.

Uses `IO.asTask` with **default** priority (pooled, not dedicated).
The work is queued onto the bounded thread pool instead of spawning a new OS
thread. Millions of tasks can be queued — they are just closures on the heap
until a pool worker picks them up. -/
def schedule (thread : GreenThread) : IO (Task (Except IO.Error Unit)) := do
  IO.asTask do
    let cancelled ← thread.token.isCancelled
    if cancelled then
      throw (IO.Error.userError "thread cancelled")
    else
      thread.action

end Control.Concurrent.Scheduler
