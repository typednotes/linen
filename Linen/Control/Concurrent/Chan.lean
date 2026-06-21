/-
  Unbounded FIFO channel

  Modelled after Haskell's `Control.Concurrent.Chan`.

  ## Design

  Internally uses a `Std.Queue` buffer protected by a `Std.Mutex`, with a
  queue of reader promises for when the buffer is empty. A shared write-side
  mutex holds the subscriber list, enabling `dup`. Built entirely on Lean
  stdlib primitives (`Std.Mutex`, `Std.Queue`, `IO.Promise`, `Task`).

  ## Typing guarantees

  * `read` returns `BaseIO (Task α)` — non-blocking by type.
  * FIFO ordering follows from `Std.Queue`.
  * `dup` shares writes: all subscribers see future writes.

  ## Invariant (maintained by `write`/`read`)

  $$\text{buffer.isEmpty} = \text{false} \implies \text{waiters.isEmpty} = \text{true}$$

  If there are buffered values, no reader should be waiting.
-/

import Init.Data.Queue
import Std.Sync.Mutex

namespace Control.Concurrent

/-- Internal state of a single channel endpoint (reader side). -/
private structure ChanReadState (α : Type) where
  buffer : Std.Queue α
  waiters : Std.Queue (IO.Promise α)

/-- Internal state of the shared write side. -/
private structure ChanWriteState (α : Type) where
  subscribers : Array (Std.Mutex (ChanReadState α))

/-- An unbounded FIFO channel, modelled after Haskell's `Chan`.

Supports `dup`: duplicated channels share writes but read independently.
All blocking uses promises (non-blocking by type via `BaseIO (Task α)`).

$$\text{Chan.new} : \text{BaseIO}\ (\text{Chan}\ \alpha)$$ -/
structure Chan (α : Type) where
  private mk ::
  private readState : Std.Mutex (ChanReadState α)
  private writeState : Std.Mutex (ChanWriteState α)

namespace Chan

/-- Create a new empty channel.

$$\text{new} : \text{BaseIO}\ (\text{Chan}\ \alpha)$$ -/
def new (α : Type) : BaseIO (Chan α) := do
  let rs ← Std.Mutex.new ({ buffer := ∅, waiters := ∅ } : ChanReadState α)
  let ws ← Std.Mutex.new ({ subscribers := #[rs] } : ChanWriteState α)
  pure ⟨rs, ws⟩

/-- Push a value to a single subscriber's read state.
If there is a waiting reader, resolve their promise directly (maintaining the
invariant $\text{buffer} \ne \emptyset \implies \text{waiters} = \emptyset$);
otherwise buffer the value. -/
private def pushToSubscriber (sub : Std.Mutex (ChanReadState α)) (val : α) : BaseIO Unit :=
  sub.atomically do
    let st ← get
    match st.waiters.dequeue? with
    | some (promise, remaining) =>
      set { st with waiters := remaining }
      promise.resolve val
    | none =>
      set { st with buffer := st.buffer.enqueue val }

/-- Write a value to the channel. The value is delivered to all current
subscribers (the original channel and any `dup`s).

$$\text{write} : \text{Chan}\ \alpha \to \alpha \to \text{BaseIO}\ \text{Unit}$$ -/
def write (ch : Chan α) (val : α) : BaseIO Unit := do
  let subs ← ch.writeState.atomically do return (← get).subscribers
  for sub in subs do
    pushToSubscriber sub val

/-- Read the next value from the channel. If the channel is empty, the
caller becomes a dormant promise until a writer delivers a value.

$$\text{read} : \text{Chan}\ \alpha \to \text{BaseIO}\ (\text{Task}\ \alpha)$$

Never blocks an OS thread. -/
def read [Nonempty α] (ch : Chan α) : BaseIO (Task α) :=
  ch.readState.atomically do
    let st ← get
    match st.buffer.dequeue? with
    | some (val, remaining) =>
      set { st with buffer := remaining }
      pure (Task.pure val)
    | none =>
      let promise ← IO.Promise.new
      set { st with waiters := st.waiters.enqueue promise }
      pure promise.result!

/-- Duplicate a channel. The new channel will receive all future writes
to the original channel, but has its own independent read position.

Values written before `dup` that haven't been read yet are NOT visible
on the new channel (matching Haskell's `dupChan` semantics).

$$\text{dup} : \text{Chan}\ \alpha \to \text{BaseIO}\ (\text{Chan}\ \alpha)$$ -/
def dup (ch : Chan α) : BaseIO (Chan α) := do
  let newRs ← Std.Mutex.new ({ buffer := ∅, waiters := ∅ } : ChanReadState α)
  ch.writeState.atomically do
    modify fun st => { st with subscribers := st.subscribers.push newRs }
  pure ⟨newRs, ch.writeState⟩

/-- Try to read without blocking. Returns `none` if the channel is empty.

$$\text{tryRead} : \text{Chan}\ \alpha \to \text{BaseIO}\ (\text{Option}\ \alpha)$$ -/
def tryRead (ch : Chan α) : BaseIO (Option α) :=
  ch.readState.atomically do
    let st ← get
    match st.buffer.dequeue? with
    | some (val, remaining) =>
      set { st with buffer := remaining }
      pure (some val)
    | none => pure none

end Chan
end Control.Concurrent
