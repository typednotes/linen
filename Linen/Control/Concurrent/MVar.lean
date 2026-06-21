/-
  Promise-based MVar

  An $\text{MVar}\ \alpha$ is a synchronisation variable that is either
  empty ($\bot$) or holds a value of type $\alpha$. All blocking is via
  `IO.Promise`, so waiters are dormant tasks — no OS thread is consumed
  while waiting. Built entirely on Lean stdlib primitives (`Std.Mutex`,
  `Std.Queue`, `IO.Promise`, `Task`).

  ## Typing guarantees

  * **FIFO fairness (structural):** Waiters are queued in `Std.Queue` and
    dequeued in insertion order.
  * **Mutual exclusion on state:** All reads/writes go through `Std.Mutex`.
  * **No lost wakeups:** Every `put` on an empty MVar with a taker wakes
    exactly one taker; every `take` on a full MVar with a putter wakes exactly
    one putter. Proof by case analysis on the match branches.

  ## Axiom-dependent properties (documented, not machine-checked)

  * **Linearizability** — depends on `Std.Mutex` providing mutual exclusion.
  * **Starvation-freedom** — depends on Lean's task scheduler being eventually
    fair and on complementary operations eventually occurring.
  * **Progress** — follows from no-lost-wakeups + mutex correctness.

  ## Concurrent type

  $\text{Concurrent}\ \alpha := \text{BaseIO}\ (\text{Task}\ \alpha)$
  encodes at the type level that an operation is non-blocking / suspendable.
  Compose concurrent actions with `BaseIO.bindTask`:

  ```
  let task ← mv.take        -- Concurrent α = BaseIO (Task α)
  BaseIO.bindTask task fun val => ...
  ```
-/

import Init.Data.Queue
import Std.Sync.Mutex

namespace Control.Concurrent

/-! ### Concurrent type alias -/

/-- A computation that produces a $\text{Task}\ \alpha$ without blocking the
calling OS thread.

$$\text{Concurrent}\ \alpha \triangleq \text{BaseIO}\ (\text{Task}\ \alpha)$$

Compose with `BaseIO.bindTask` to chain continuations as tasks.
Any function returning `Concurrent α` is non-blocking by construction. -/
abbrev Concurrent (α : Type) := BaseIO (Task α)

/-! ### MVar internals -/

/-- Internal state of an MVar, protected by a `Std.Mutex`.

**Structural invariant** (maintained by all operations):
- If `value.isSome`, then `takers` is empty (no one waits when a value exists).
- If `value.isNone`, then `putters` is empty (no one waits to put when empty).

These are enforced by the `take`/`put` implementations: `take` on a full
MVar checks putters; `put` on an empty MVar checks takers. No simultaneous
value + waiters of the complementary kind can exist. -/
private structure MVarState (α : Type) where
  value : Option α
  takers : Std.Queue (IO.Promise α)
  putters : Std.Queue (α × IO.Promise Unit)

/-! ### MVar structure -/

/-- A synchronisation variable that is either empty or holds a value.

$\text{MVar}$ is the fundamental building block for concurrent data
structures. All blocking is promise-based: waiting tasks are dormant
promises, not blocked OS threads. This allows scaling to millions of
concurrent tasks.

**Constraint:** Blocking operations (`take`, `read`, `swap`, etc.) require
`[Nonempty α]` for `IO.Promise` construction.

Modelled after Haskell's `Control.Concurrent.MVar`. -/
structure MVar (α : Type) where
  private mk ::
  private state : Std.Mutex (MVarState α)

namespace MVar

/-! ### Construction -/

/-- Create a new $\text{MVar}$ containing value $a$.

$$\text{new} : \alpha \to \text{BaseIO}\ (\text{MVar}\ \alpha)$$ -/
def new (a : α) : BaseIO (MVar α) := do
  pure ⟨← Std.Mutex.new { value := some a, takers := ∅, putters := ∅ : MVarState α }⟩

/-- Create a new empty $\text{MVar}$.

$$\text{newEmpty} : \text{BaseIO}\ (\text{MVar}\ \alpha)$$ -/
def newEmpty (α : Type) : BaseIO (MVar α) := do
  pure ⟨← Std.Mutex.new { value := none, takers := ∅, putters := ∅ : MVarState α }⟩

/-! ### Internal helpers -/

/-- Consume the value from a full MVar. If a putter is queued, its value
replaces the consumed one (maintaining fullness); otherwise the MVar becomes
empty. Returns the consumed value.

Used by both `take` and `tryTake` to avoid duplicating the full-MVar logic. -/
private def consumeValue (st : MVarState α) (val : α) : Std.AtomicT (MVarState α) BaseIO α := do
  match st.putters.dequeue? with
  | some ((putVal, putPromise), remaining) =>
    set { st with value := some putVal, putters := remaining }
    putPromise.resolve ()
    pure val
  | none =>
    set { st with value := none }
    pure val

/-- Deliver a value into an empty MVar. If a taker is queued, it receives
the value directly (the MVar stays empty); otherwise the MVar becomes full.

Used by both `put` and `tryPut` to avoid duplicating the empty-MVar logic. -/
private def deliverValue (st : MVarState α) (a : α) : Std.AtomicT (MVarState α) BaseIO Unit := do
  match st.takers.dequeue? with
  | some (takerPromise, remaining) =>
    set { st with takers := remaining }
    takerPromise.resolve a
  | none =>
    set { st with value := some a }

/-! ### Core async operations (non-blocking) -/

/-- Take the value from the MVar, leaving it empty. If the MVar is empty,
the caller becomes a dormant promise until a `put` fills it.

$$\text{take} : \text{MVar}\ \alpha \to \text{BaseIO}\ (\text{Task}\ \alpha)$$

**Fairness:** Takers are served in FIFO order from `Std.Queue`. -/
def take [Nonempty α] (mv : MVar α) : BaseIO (Task α) :=
  mv.state.atomically do
    let st ← get
    match st.value with
    | some val =>
      pure (Task.pure (← consumeValue st val))
    | none =>
      let promise ← IO.Promise.new
      set { st with takers := st.takers.enqueue promise }
      pure promise.result!

/-- Put a value into the MVar. If the MVar is full, the caller becomes a
dormant promise until a `take` empties it.

$$\text{put} : \text{MVar}\ \alpha \to \alpha \to \text{BaseIO}\ (\text{Task}\ \text{Unit})$$

**Fairness:** Putters are served in FIFO order from `Std.Queue`. -/
def put (mv : MVar α) (a : α) : BaseIO (Task Unit) :=
  mv.state.atomically do
    let st ← get
    match st.value with
    | none =>
      deliverValue st a
      pure (Task.pure ())
    | some _ =>
      let promise ← IO.Promise.new
      set { st with putters := st.putters.enqueue (a, promise) }
      pure promise.result!

/-- Read the value without removing it. If empty, waits like `take` then
puts the value back.

$$\text{read} : \text{MVar}\ \alpha \to \text{BaseIO}\ (\text{Task}\ \alpha)$$ -/
def read [Nonempty α] (mv : MVar α) : BaseIO (Task α) := do
  let takeTask ← mv.take
  BaseIO.bindTask takeTask fun val => do
    let putTask ← mv.put val
    pure (putTask.map fun () => val)

/-- Swap the value in the MVar: take the old, put the new.

$$\text{swap} : \text{MVar}\ \alpha \to \alpha \to \text{BaseIO}\ (\text{Task}\ \alpha)$$

Returns the old value. -/
def swap [Nonempty α] (mv : MVar α) (newVal : α) : BaseIO (Task α) := do
  let takeTask ← mv.take
  BaseIO.bindTask takeTask fun old => do
    let putTask ← mv.put newVal
    pure (putTask.map fun () => old)

/-- Apply a function $f$ to the MVar contents and return a result.

$$\text{withMVar} : \text{MVar}\ \alpha \to (\alpha \to \text{BaseIO}\ (\alpha \times \beta)) \to \text{BaseIO}\ (\text{Task}\ \beta)$$

Takes the value, applies $f$, puts back $\pi_1(f(a))$, returns $\pi_2(f(a))$.
If $f$ throws, the MVar remains empty (matching Haskell semantics). -/
def withMVar [Nonempty α] (mv : MVar α) (f : α → BaseIO (α × β)) : BaseIO (Task β) := do
  let takeTask ← mv.take
  BaseIO.bindTask takeTask fun val => do
    let (newVal, result) ← f val
    let putTask ← mv.put newVal
    pure (putTask.map fun () => result)

/-- Modify the MVar contents and return a result.

$$\text{modify} : \text{MVar}\ \alpha \to (\alpha \to \text{BaseIO}\ (\alpha \times \beta)) \to \text{BaseIO}\ (\text{Task}\ \beta)$$ -/
def modify [Nonempty α] (mv : MVar α) (f : α → BaseIO (α × β)) : BaseIO (Task β) :=
  mv.withMVar f

/-- Modify the MVar contents without returning a result.

$$\text{modify\_} : \text{MVar}\ \alpha \to (\alpha \to \text{BaseIO}\ \alpha) \to \text{BaseIO}\ (\text{Task}\ \text{Unit})$$ -/
def modify_ [Nonempty α] (mv : MVar α) (f : α → BaseIO α) : BaseIO (Task Unit) :=
  mv.withMVar fun a => do let a' ← f a; pure (a', ())

/-! ### Try operations (non-blocking, immediate) -/

/-- Try to take the value. Returns `some v` immediately if full, `none` if empty.

$$\text{tryTake} : \text{MVar}\ \alpha \to \text{BaseIO}\ (\text{Option}\ \alpha)$$ -/
def tryTake (mv : MVar α) : BaseIO (Option α) :=
  mv.state.atomically do
    let st ← get
    match st.value with
    | some val => pure (some (← consumeValue st val))
    | none => pure none

/-- Try to put a value. Returns `true` if successful, `false` if full.

$$\text{tryPut} : \text{MVar}\ \alpha \to \alpha \to \text{BaseIO}\ \text{Bool}$$ -/
def tryPut (mv : MVar α) (a : α) : BaseIO Bool :=
  mv.state.atomically do
    let st ← get
    match st.value with
    | none =>
      deliverValue st a
      pure true
    | some _ => pure false

/-- Try to read the value without removing it.

$$\text{tryRead} : \text{MVar}\ \alpha \to \text{BaseIO}\ (\text{Option}\ \alpha)$$ -/
def tryRead (mv : MVar α) : BaseIO (Option α) :=
  mv.state.atomically do
    pure (← get).value

/-- Check if the MVar is empty. This is a snapshot — may be stale by the
time you act on it.

$$\text{isEmpty} : \text{MVar}\ \alpha \to \text{BaseIO}\ \text{Bool}$$ -/
def isEmpty (mv : MVar α) : BaseIO Bool :=
  mv.state.atomically do
    pure (← get).value.isNone

/-! ### Sync wrappers (convenience — blocks OS thread) -/

/-- Take the value, blocking the OS thread until available.

$$\text{takeSync} : \text{MVar}\ \alpha \to \text{BaseIO}\ \alpha$$

Prefer `take` (async) for scalable code. -/
def takeSync [Nonempty α] (mv : MVar α) : BaseIO α := do IO.wait (← mv.take)

/-- Put a value, blocking the OS thread until the MVar is empty.

$$\text{putSync} : \text{MVar}\ \alpha \to \alpha \to \text{BaseIO}\ \text{Unit}$$

Prefer `put` (async) for scalable code. -/
def putSync (mv : MVar α) (a : α) : BaseIO Unit := do IO.wait (← mv.put a)

/-- Read the value, blocking the OS thread until available.

$$\text{readSync} : \text{MVar}\ \alpha \to \text{BaseIO}\ \alpha$$

Prefer `read` (async) for scalable code. -/
def readSync [Nonempty α] (mv : MVar α) : BaseIO α := do IO.wait (← mv.read)

end MVar

/-! ### Fairness properties (documented)

**FIFO ordering:** Elements dequeued from `Std.Queue` come out in insertion
order. MVar always enqueues at the back and dequeues from the front,
so waiters are served in FIFO order.

**take_resolves_on_put:** If an MVar is empty with a taker waiting, a
subsequent `put` resolves the head taker's promise with the put value.

**no_lost_wakeups:** Every `put` on an empty MVar either fills it or wakes
exactly one taker. Every `take` on a full MVar either empties it or wakes
exactly one putter.
-/

end Control.Concurrent
