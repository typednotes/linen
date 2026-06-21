/-
  Fair green-thread monad

  A `Green α` computation runs on Lean's thread pool without ever blocking
  a pool thread while waiting. When a green thread awaits a `Task`, the
  pool thread is freed via `BaseIO.bindTask` and the continuation resumes
  on any available worker when the task completes.

  ## Design — built directly on `BaseIO.bindTask`

  The core type is `Greenlet α`, an inductive that is either immediately
  available (`.now a`) or pending (`.later (Task α)`). `GreenBase α` wraps
  `BaseIO (Greenlet α)` with a custom `bind` that uses `BaseIO.bindTask`
  for the `.later` case — never `IO.wait`.

  ## Guarantees (documented; rest on `BaseIO.bindTask`'s extern contract)

  * **Termination:** `GreenBase.bind` is O(1) beyond its arguments — the
    `.later` branch registers a `bindTask` callback and returns immediately;
    `bindTask` introduces no recursion.
  * **Liveness:** `bindTask t cont` invokes `cont` exactly once when `t`
    resolves, so `await t >>= f` eventually calls `f` iff `t` resolves. For
    MVar-based suspension this also relies on MVar's no-lost-wakeup property.
  * **Fairness (no pool starvation):** the `.later` branch calls
    `BaseIO.bindTask` (non-blocking callback registration), not `IO.wait`
    (blocking), so the pool thread returns to the pool in O(1).
-/

import Linen.Control.Concurrent.MVar
import Linen.Control.Concurrent.Chan
import Linen.Control.Concurrent.QSem
import Linen.Control.Concurrent.QSemN
import Std.Sync.CancellationToken

namespace Control.Concurrent.Green

/-! ### Greenlet — the result type -/

/-- A computation result that is either immediately available or pending.

- `.now a` — value available, continuation runs synchronously.
- `.later t` — value pending; `bind` uses `BaseIO.bindTask` to register
  the continuation without blocking. -/
inductive Greenlet (α : Type) where
  | now   : α → Greenlet α
  | later : Task α → Greenlet α

namespace Greenlet

@[inline]
def toTask : Greenlet α → Task α
  | .now a   => Task.pure a
  | .later t => t

@[inline]
def map (f : α → β) : Greenlet α → Greenlet β
  | .now a   => .now (f a)
  | .later t => .later (t.map f)

end Greenlet

/-! ### GreenBase — the non-blocking IO monad -/

/-- The core green-thread monad (no error handling, no cancellation).

Wraps `BaseIO (Greenlet α)` with a custom `bind` that yields pool
threads on `.later` via `BaseIO.bindTask`. -/
structure GreenBase (α : Type) where
  /-- Run the green computation, producing a `Greenlet`. -/
  run : BaseIO (Greenlet α)

namespace GreenBase

@[inline]
protected def pure (a : α) : GreenBase α :=
  ⟨pure (Greenlet.now a)⟩

/-- Bind that yields the pool thread on `.later`.

**Termination:** O(1) beyond its arguments. `bindTask` registers a
callback and returns immediately.

**Fairness:** The `.later` branch calls `BaseIO.bindTask`, not `IO.wait`.
The pool thread is freed. -/
@[inline]
protected def bind (x : GreenBase α) (f : α → GreenBase β) : GreenBase β :=
  ⟨do match ← x.run with
      | .now a => (f a).run
      | .later t =>
        let task ← BaseIO.bindTask t fun a => (f a).run |>.map Greenlet.toTask
        pure (Greenlet.later task)⟩

@[inline]
protected def map (f : α → β) (x : GreenBase α) : GreenBase β :=
  ⟨(·.map f) <$> x.run⟩

instance : Monad GreenBase where
  pure := GreenBase.pure
  bind := GreenBase.bind

instance : Functor GreenBase where
  map := GreenBase.map

instance : MonadLift BaseIO GreenBase where
  monadLift x := ⟨Greenlet.now <$> x⟩

/-- Await a `Task` without blocking. The pool thread is freed.

$$\text{await} : \text{Task}\ \alpha \to \text{GreenBase}\ \alpha$$ -/
@[inline]
def await (t : Task α) : GreenBase α :=
  ⟨pure (Greenlet.later t)⟩

/-- Await a `Concurrent α` operation without blocking.

$$\text{awaitConcurrent} : \text{Concurrent}\ \alpha \to \text{GreenBase}\ \alpha$$ -/
@[inline]
def awaitConcurrent (op : Concurrent α) : GreenBase α :=
  ⟨do let task ← (op : BaseIO (Task α)); pure (Greenlet.later task)⟩

/-- Convert to a `Task` for interop with `IO.wait`. -/
@[inline]
def asTask (x : GreenBase α) : BaseIO (Task α) :=
  Greenlet.toTask <$> x.run

end GreenBase

/-! ### Green — the full green-thread monad -/

/-- A fair green-thread computation with cancellation and error handling.

$$\text{Green}\ \alpha := \text{CancellationToken} \to \text{GreenBase}\ (\text{Except}\ \text{IO.Error}\ \alpha)$$

- **Fair:** awaiting a `Task` frees the pool thread (via `GreenBase.bind`).
- **Cancellable:** takes a `CancellationToken` for cooperative cancellation.
- **Error-handling:** exceptions propagate via `Except`. -/
def Green (α : Type) := Std.CancellationToken → GreenBase (Except IO.Error α)

namespace Green

@[inline]
protected def pure (a : α) : Green α :=
  fun _token => GreenBase.pure (.ok a)

@[inline]
protected def bind (x : Green α) (f : α → Green β) : Green β :=
  fun token => GreenBase.bind (x token) fun
    | .ok a => f a token
    | .error e => GreenBase.pure (.error e)

@[inline]
protected def map (f : α → β) (x : Green α) : Green β :=
  fun token => GreenBase.map (·.map f) (x token)

@[inline]
protected def throw (e : IO.Error) : Green α :=
  fun _token => GreenBase.pure (.error e)

@[inline]
protected def tryCatch (x : Green α) (h : IO.Error → Green α) : Green α :=
  fun token => GreenBase.bind (x token) fun
    | .ok a => GreenBase.pure (.ok a)
    | .error e => h e token

instance : Monad Green where
  pure := Green.pure
  bind := Green.bind

instance : Functor Green where
  map := Green.map

instance : MonadExcept IO.Error Green where
  throw := Green.throw
  tryCatch := Green.tryCatch

instance : MonadFinally Green where
  tryFinally' x f := fun token =>
    GreenBase.bind (x token) fun
      | .ok a => GreenBase.bind (f (some a) token) fun
          | .ok b => GreenBase.pure (.ok (a, b))
          | .error e => GreenBase.pure (.error e)
      | .error e => GreenBase.bind (f none token) fun
          | .ok _ => GreenBase.pure (.error e)
          | .error e2 => GreenBase.pure (.error e2)

instance : MonadLift IO Green where
  monadLift action := fun _token => ⟨do
    match ← action.toBaseIO with
    | .ok a => pure (Greenlet.now (.ok a))
    | .error e => pure (Greenlet.now (.error e))⟩

/-! ### Core operations -/

/-- Await a `Task` without blocking the pool thread. -/
@[inline]
def await (t : Task α) : Green α :=
  fun _token => GreenBase.map .ok (GreenBase.await t)

/-- Await a `Concurrent α` operation without blocking the pool thread. -/
@[inline]
def awaitConcurrent (op : Concurrent α) : Green α :=
  fun _token => GreenBase.map .ok (GreenBase.awaitConcurrent op)

/-- Check cancellation and throw if cancelled. -/
@[inline]
def checkCancelled : Green Unit := fun token => ⟨do
  if ← token.isCancelled then
    pure (Greenlet.now (.error (IO.Error.userError "green thread cancelled")))
  else
    pure (Greenlet.now (.ok ()))⟩

/-! ### MVar integration -/

@[inline] def takeMVar [Nonempty α] (mv : MVar α) : Green α := awaitConcurrent mv.take
@[inline] def putMVar (mv : MVar α) (a : α) : Green Unit := awaitConcurrent (mv.put a)
@[inline] def readMVar [Nonempty α] (mv : MVar α) : Green α := awaitConcurrent mv.read

/-! ### Chan integration -/

@[inline] def readChan [Nonempty α] (ch : Chan α) : Green α := awaitConcurrent ch.read

/-! ### QSem integration -/

@[inline] def waitSem (sem : QSem) : Green Unit := awaitConcurrent sem.wait
@[inline] def waitSemN (sem : QSemN) (n : Nat) : Green Unit := awaitConcurrent (sem.wait n)

/-! ### Running Green computations -/

/-- Run a `Green` computation, returning a `Task` that resolves when the
entire continuation chain completes. No pool thread is blocked during
any `await`. -/
def run (action : Green α) (token : Std.CancellationToken)
    : BaseIO (Task (Except IO.Error α)) :=
  (action token).asTask

/-- Run a `Green` computation and block until it completes (top-level only). -/
def block (action : Green α) (token : Std.CancellationToken) : IO α := do
  let task ← run action token
  match ← IO.wait task with
  | .ok a => pure a
  | .error e => throw e

end Green
end Control.Concurrent.Green
