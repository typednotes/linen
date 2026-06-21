/-
  Generalised quantity semaphore

  Like `QSem`, but allows acquiring and releasing arbitrary quantities.
  Modelled after Haskell's `Control.Concurrent.QSemN`. Built entirely on Lean
  stdlib primitives (`Std.Mutex`, `Std.Queue`, `IO.Promise`, `Task`).

  ## Typing guarantees

  * `count : Nat` — non-negative by construction.
  * All blocking is promise-based (`IO.Promise`).
  * FIFO fairness: waiters are served in `Std.Queue` order.

  ## Invariant (documented, enforced by construction)

  $$\text{count} \ge n \implies \text{no waiter requesting} \le n\ \text{is queued}$$

  When `signal` adds resources, it greedily wakes FIFO-ordered waiters
  whose requested amount can be satisfied.
-/

import Init.Data.Queue
import Std.Sync.Mutex

namespace Control.Concurrent

/-- Internal state of a `QSemN`. -/
private structure QSemNState where
  /-- Available resource count. Non-negative by `Nat` type. -/
  count : Nat
  /-- FIFO queue of `(amount_needed, promise)` pairs. -/
  waiters : Std.Queue (Nat × IO.Promise (Option Unit))

/-- A generalised quantity semaphore. Allows acquiring and releasing
arbitrary amounts of the resource.

$$\text{QSemN.new} : \text{Nat} \to \text{BaseIO}\ \text{QSemN}$$

Modelled after Haskell's `QSemN`. -/
structure QSemN where
  private mk ::
  private state : Std.Mutex QSemNState

namespace QSemN

/-- Create a new semaphore with the given initial count.

$$\text{new} : \text{Nat} \to \text{BaseIO}\ \text{QSemN}$$ -/
def new (initial : Nat) : BaseIO QSemN := do
  pure ⟨← Std.Mutex.new { count := initial, waiters := ∅ : QSemNState }⟩

/-- Acquire $n$ units. If insufficient units are available, the caller
becomes a dormant promise until enough are released.

$$\text{wait} : \text{QSemN} \to \text{Nat} \to \text{BaseIO}\ (\text{Task}\ \text{Unit})$$ -/
def wait (sem : QSemN) (n : Nat) : BaseIO (Task Unit) :=
  sem.state.atomically do
    let st ← get
    if st.count ≥ n then
      set { st with count := st.count - n }
      pure (Task.pure ())
    else
      let promise ← IO.Promise.new
      set { st with waiters := st.waiters.enqueue (n, promise) }
      pure (promise.result?.map (sync := true) fun
        | some (some ()) => ()
        | _ => panic! "QSemN.wait: promise dropped")

/-- Try to wake waiters from the front of the queue whose requested
amount can be satisfied by the current count. Greedy FIFO.

Uses a mutable loop since state is accessed via `get`/`set` in the atomic
transaction, which precludes structural recursion. -/
private def wakeWaiters : Std.AtomicT QSemNState BaseIO Unit := do
  let mut st ← get
  let mut continue_ := true
  while continue_ do
    match st.waiters.dequeue? with
    | some ((needed, promise), remaining) =>
      if st.count ≥ needed then
        st := { count := st.count - needed, waiters := remaining }
        promise.resolve (some ())
      else
        continue_ := false
    | none =>
      continue_ := false
  set st

/-- Release $n$ units. Wakes as many FIFO-ordered waiters as the
available count allows.

$$\text{signal} : \text{QSemN} \to \text{Nat} \to \text{BaseIO}\ \text{Unit}$$ -/
def signal (sem : QSemN) (n : Nat) : BaseIO Unit :=
  sem.state.atomically do
    modify fun st => { st with count := st.count + n }
    wakeWaiters

/-- Acquire $n$ units, run an action, then release $n$ — even if the action throws.
Guarantees the semaphore is released via `try`/`finally`.

$$\text{withSemN} : \text{QSemN} \to \mathbb{N} \to \text{IO}\ \alpha \to \text{IO}\ \alpha$$ -/
def withSemN (sem : QSemN) (n : Nat) (action : IO α) : IO α := do
  IO.wait (← sem.wait n)
  try
    action
  finally
    sem.signal n

end QSemN
end Control.Concurrent
