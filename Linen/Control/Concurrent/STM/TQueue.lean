/-
  Control.Concurrent.STM.TQueue — Transactional FIFO queues

  Unbounded FIFO queue for STM. Uses a two-list amortized queue internally.
-/

import Linen.Control.Concurrent.STM.TVar

namespace Control.Concurrent.STM

open Control.Monad

/-- A transactional FIFO queue.
    $$\text{TQueue}(\alpha) = \text{TVar}(\text{List}(\alpha)) \times \text{TVar}(\text{List}(\alpha))$$
    Uses a two-list representation: `write` for enqueue, `read` for dequeue. -/
structure TQueue (α : Type) where
  /-- Read end (front of queue, items in order). -/
  readEnd : TVar (List α)
  /-- Write end (back of queue, items in reverse). -/
  writeEnd : TVar (List α)

namespace TQueue

/-- Create a new empty TQueue in STM. -/
@[inline] def newTQueue : STM (TQueue α) := do
  let r ← TVar.newTVar ([] : List α)
  let w ← TVar.newTVar ([] : List α)
  pure ⟨r, w⟩

/-- Create a new empty TQueue in IO. -/
@[inline] def newTQueueIO : IO (TQueue α) := do
  let r ← TVar.newTVarIO ([] : List α)
  let w ← TVar.newTVarIO ([] : List α)
  pure ⟨r, w⟩

/-- Write a value to the back of the queue.
    $$\text{writeTQueue} : \text{TQueue}(\alpha) \to \alpha \to \text{STM}(\text{Unit})$$ -/
@[inline] def writeTQueue (q : TQueue α) (val : α) : STM Unit :=
  TVar.modifyTVar' q.writeEnd (val :: ·)

/-- Read a value from the front of the queue. Retries if empty.
    $$\text{readTQueue} : \text{TQueue}(\alpha) \to \text{STM}(\alpha)$$ -/
def readTQueue (q : TQueue α) : STM α := do
  let xs ← TVar.readTVar q.readEnd
  match xs with
  | x :: xs' =>
    TVar.writeTVar q.readEnd xs'
    pure x
  | [] => do
    let ys ← TVar.readTVar q.writeEnd
    match ys.reverse with
    | [] => STM.retry
    | y :: ys' =>
      TVar.writeTVar q.writeEnd []
      TVar.writeTVar q.readEnd ys'
      pure y

/-- Try to read without blocking. Returns `none` if empty.
    $$\text{tryReadTQueue} : \text{TQueue}(\alpha) \to \text{STM}(\text{Option}(\alpha))$$ -/
def tryReadTQueue (q : TQueue α) : STM (Option α) := do
  let xs ← TVar.readTVar q.readEnd
  match xs with
  | x :: xs' =>
    TVar.writeTVar q.readEnd xs'
    pure (some x)
  | [] => do
    let ys ← TVar.readTVar q.writeEnd
    match ys.reverse with
    | [] => pure none
    | y :: ys' =>
      TVar.writeTVar q.writeEnd []
      TVar.writeTVar q.readEnd ys'
      pure (some y)

/-- Check if the queue is empty.
    $$\text{isEmptyTQueue} : \text{TQueue}(\alpha) \to \text{STM}(\text{Bool})$$ -/
def isEmptyTQueue (q : TQueue α) : STM Bool := do
  let xs ← TVar.readTVar q.readEnd
  match xs with
  | _ :: _ => pure false
  | [] => do
    let ys ← TVar.readTVar q.writeEnd
    pure ys.isEmpty

/-- Peek at the front without removing. Retries if empty. -/
def peekTQueue (q : TQueue α) : STM α := do
  let xs ← TVar.readTVar q.readEnd
  match xs with
  | x :: _ => pure x
  | [] => do
    let ys ← TVar.readTVar q.writeEnd
    match ys.reverse with
    | [] => STM.retry
    | y :: ys' =>
      TVar.writeTVar q.writeEnd []
      TVar.writeTVar q.readEnd (y :: ys')
      pure y

end TQueue
end Control.Concurrent.STM
