/-
  Thread management — fair green threads

  Provides `ThreadId`, `forkIO`, `forkFinally`, `forkGreen`, `killThread`,
  `threadDelay`, `yield`, and `waitThread`, modelled after Haskell's
  `Control.Concurrent`.

  ## One execution model: fair green threads

  All forking goes through `Linen.Control.Concurrent.Green` — the fair
  green-thread monad whose `await`s free the pool worker via `BaseIO.bindTask`
  (M:N scheduling, inspired by GHC's capability model). `forkIO` simply lifts a
  plain `IO Unit` into `Green` and forks it the same way.

  A forked computation is always *started* on a pool worker, never run in the
  caller: `spawn` bounces it onto a worker through an empty task before running
  the `Green` chain there. A cancellation pre-check makes a thread that is
  killed before it starts fail fast.

  ## Differences from GHC

  * **Cancellation is cooperative.** `killThread` sets a `CancellationToken`;
    a CPU-bound thread that never checks the token will not be interrupted.
  * **No preemption.** Green threads run to completion or until they
    voluntarily yield/block.
  * **No stack switching.** A `forkIO` action that calls `IO.wait` blocks its
    worker; use `forkGreen` with `Green.await`/`Green.takeMVar`/… to suspend
    without blocking.

  ## Type-level guarantees

  * `ThreadId.id` is unique by construction (monotonic counter).
  * `forkFinally` guarantees the finaliser runs regardless of success/failure.
-/

import Linen.Control.Concurrent.MVar
import Linen.Control.Concurrent.Green
import Std.Sync.CancellationToken

namespace Control.Concurrent

/-! ### PosNat -/

/-- A positive natural number, used for `ThreadId.id` to encode at the type
level that thread IDs are always $\ge 1$.

$$\text{PosNat} := \{n : \mathbb{N} \mid n > 0\}$$ -/
def PosNat := { n : Nat // n > 0 }

instance : Nonempty PosNat := ⟨⟨1, by omega⟩⟩
instance : BEq PosNat where beq a b := a.val == b.val
instance : Hashable PosNat where hash p := hash p.val
instance : ToString PosNat where toString p := toString p.val
instance : Repr PosNat where reprPrec p n := reprPrec p.val n

/-! ### ThreadId -/

/-- Global monotonic counter for unique thread IDs. Starts at $1$. -/
private initialize nextThreadId : IO.Ref PosNat ← IO.mkRef ⟨1, by omega⟩

/-- A handle to a forked concurrent thread.

* `id : PosNat` — unique identifier, $\ge 1$ by construction (monotonic counter).
* `task` — the underlying `Task` so we can `IO.wait` on it.
* `cancelToken` — cooperative cancellation via `Std.CancellationToken`.

The `id` field is never reused within a process (monotonic counter). -/
structure ThreadId where
  private mk ::
  id : PosNat
  private task : Task (Except IO.Error Unit)
  private cancelToken : Std.CancellationToken

instance : BEq ThreadId where beq a b := a.id == b.id
instance : Hashable ThreadId where hash t := hash t.id
instance : ToString ThreadId where toString t := s!"ThreadId({t.id})"
instance : Repr ThreadId where reprPrec t _ := s!"ThreadId({t.id})"

/-- Allocate a fresh unique thread ID (internal).

Atomically increments the global counter and returns the previous value,
which is $\ge 1$ since the counter starts at $1$ and only increases. -/
private def freshThreadId : BaseIO PosNat :=
  nextThreadId.modifyGet fun ⟨n, h⟩ => (⟨n, h⟩, ⟨n + 1, by omega⟩)

/-! ### Forking — every thread runs as a fair green thread -/

/-- Start a `Green` computation on Lean's thread pool, returning the `Task` for
its result.

The work is *bounced* onto a pool worker via an empty task (`BaseIO.asTask`)
so its synchronous prefix never runs in the caller. A cancellation pre-check
makes a thread that is killed before it starts fail fast. `await`s inside
`action` free the worker via `BaseIO.bindTask` (fair M:N scheduling). -/
private def spawn (action : Green.Green Unit) (token : Std.CancellationToken)
    : BaseIO (Task (Except IO.Error Unit)) := do
  let starter ← BaseIO.asTask (pure ())
  BaseIO.bindTask starter fun _ =>
    Green.Green.run (do Green.Green.checkCancelled; action) token

/-- Fork a fair green thread. The `Green` computation never blocks pool threads
when awaiting — suspensions use `BaseIO.bindTask` to register continuations,
freeing the pool thread for other work.

$$\text{forkGreen} : \text{Green}\ \text{Unit} \to \text{IO}\ \text{ThreadId}$$

Use `Green.await`, `Green.takeMVar`, etc. inside the action to suspend without
blocking. See `Linen.Control.Concurrent.Green` for the termination, liveness,
and fairness guarantees. -/
def forkGreen (action : Green.Green Unit) : IO ThreadId := do
  let tid ← freshThreadId
  let token ← Std.CancellationToken.new
  let task ← spawn action token
  pure { id := tid, task := task, cancelToken := token }

/-- Fork a new green thread running a plain `IO` action. The action is lifted
into `Green` and forked via `forkGreen`, so it is started on a pool worker — no
dedicated OS thread is spawned, and millions of green threads can be active.

$$\text{forkIO} : \text{IO}\ \text{Unit} \to \text{IO}\ \text{ThreadId}$$

```
let tid ← forkIO do
  IO.println "hello from green thread"
```

The returned `ThreadId` can be used with `killThread` for cooperative
cancellation, or with `waitThread` to join. -/
def forkIO (action : IO Unit) : IO ThreadId :=
  forkGreen (liftM action)

/-- Fork a green thread that calls `finally` with the outcome, whether the
action succeeded or threw.

$$\text{forkFinally} : \text{IO}\ \alpha \to (\text{Except}\ \text{IO.Error}\ \alpha \to \text{IO}\ \text{Unit}) \to \text{IO}\ \text{ThreadId}$$

Modelled after Haskell's `forkFinally`. -/
def forkFinally {α : Type} (action : IO α) (finally_ : Except IO.Error α → IO Unit) : IO ThreadId := do
  forkIO do
    try
      let a ← action
      finally_ (.ok a)
    catch e =>
      finally_ (.error e)

/-! ### Thread control -/

/-- Cooperatively cancel a thread. Sets the thread's `CancellationToken`.

$$\text{killThread} : \text{ThreadId} \to \text{BaseIO}\ \text{Unit}$$

**Note:** Unlike GHC's `killThread`, this is cooperative. The target thread
must check `Std.CancellationToken.isCancelled` or use cancellation-aware
primitives to actually stop. -/
def killThread (tid : ThreadId) : BaseIO Unit :=
  tid.cancelToken.cancel .cancel

/-- Suspend the current thread for at least $\mu s$ microseconds.

$$\text{threadDelay} : \mathbb{N} \to \text{BaseIO}\ \text{Unit}$$

Maps to `IO.sleep` (millisecond granularity, so we round up:
$\text{ms} = \lceil \mu s / 1000 \rceil$). -/
def threadDelay (μs : Nat) : BaseIO Unit :=
  IO.sleep (((μs + 999) / 1000).toUInt32)

/-- Yield execution to other threads. Equivalent to `IO.sleep 0`.

$$\text{yield} : \text{BaseIO}\ \text{Unit}$$ -/
def yield : BaseIO Unit := IO.sleep 0

/-! ### Waiting -/

/-- Await a thread's completion inside a `Green` computation, without blocking
the pool thread.

$$\text{waitThreadGreen} : \text{ThreadId} \to \text{Green.Green}\ \text{Unit}$$ -/
def waitThreadGreen (tid : ThreadId) : Green.Green Unit :=
  Green.Green.await tid.task >>= fun
    | .ok ()   => pure ()
    | .error e => throw e

/-- Wait for a thread to finish and return its result. Re-throws if the
thread threw an exception.

$$\text{waitThread} : \text{ThreadId} \to \text{IO}\ \text{Unit}$$ -/
def waitThread (tid : ThreadId) : IO Unit := do
  match ← IO.wait tid.task with
  | .ok () => pure ()
  | .error e => throw e

end Control.Concurrent
