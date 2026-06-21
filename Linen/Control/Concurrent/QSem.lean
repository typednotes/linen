/-
  Quantity semaphore

  A simple quantity semaphore: at most $n$ resources can be acquired
  concurrently. Modelled after Haskell's `Control.Concurrent.QSem`. Built
  entirely on Lean stdlib primitives (`Std.Mutex`, `Std.Queue`, `IO.Promise`,
  `Task`).

  ## Typing guarantees

  * `count : Nat` — non-negative by construction (cannot underflow).
  * All blocking is promise-based (`IO.Promise`).
  * FIFO fairness: waiters served in `Std.Queue` order.

  ## Invariant (documented, enforced by construction)

  $$\text{count} > 0 \implies \text{waiters} = \emptyset$$

  If resources are available, no one should be waiting. This is maintained
  by the `signal` implementation which wakes a waiter before incrementing.
-/

import Init.Data.Queue
import Std.Sync.Mutex

namespace Control.Concurrent

/-- Internal state of a `QSem`. -/
private structure QSemState where
  /-- Available resource count. Non-negative by `Nat` type. -/
  count : Nat
  /-- FIFO queue of tasks waiting to acquire. -/
  waiters : Std.Queue (IO.Promise (Option Unit))

/-- A quantity semaphore. Create with `QSem.new n` to allow up to $n$
concurrent acquisitions.

$$\text{QSem.new} : \text{Nat} \to \text{BaseIO}\ \text{QSem}$$

Modelled after Haskell's `QSem`. -/
structure QSem where
  private mk ::
  private state : Std.Mutex QSemState

namespace QSem

/-- Create a new semaphore with the given initial count $n$.

$$\text{new} : \text{Nat} \to \text{BaseIO}\ \text{QSem}$$ -/
def new (initial : Nat) : BaseIO QSem := do
  pure ⟨← Std.Mutex.new { count := initial, waiters := ∅ : QSemState }⟩

/-- Acquire one unit of the semaphore. If no units are available,
the caller becomes a dormant promise until `signal` releases one.

$$\text{wait} : \text{QSem} \to \text{BaseIO}\ (\text{Task}\ \text{Unit})$$

Never blocks an OS thread. FIFO fairness. -/
def wait (sem : QSem) : BaseIO (Task Unit) :=
  sem.state.atomically do
    let st ← get
    if st.count > 0 then
      set { st with count := st.count - 1 }
      pure (Task.pure ())
    else
      let promise ← IO.Promise.new
      set { st with waiters := st.waiters.enqueue promise }
      pure (promise.result?.map (sync := true) fun
        | some (some ()) => ()
        | _ => panic! "QSem.wait: promise dropped")

/-- Release one unit of the semaphore. If there are waiting tasks,
the first one in FIFO order is woken.

$$\text{signal} : \text{QSem} \to \text{BaseIO}\ \text{Unit}$$

**No-lost-wakeup guarantee:** either a waiter is woken or the count is
incremented. Both branches are mutually exclusive. -/
def signal (sem : QSem) : BaseIO Unit :=
  sem.state.atomically do
    let st ← get
    match st.waiters.dequeue? with
    | some (promise, remaining) =>
      set { st with waiters := remaining }
      promise.resolve (some ())
    | none =>
      set { st with count := st.count + 1 }

/-- Acquire one unit, run an action, then release — even if the action throws.
Guarantees the semaphore is released via `try`/`finally`.

$$\text{withSem} : \text{QSem} \to \text{IO}\ \alpha \to \text{IO}\ \alpha$$ -/
def withSem (sem : QSem) (action : IO α) : IO α := do
  IO.wait (← sem.wait)
  try
    action
  finally
    sem.signal

end QSem
end Control.Concurrent
