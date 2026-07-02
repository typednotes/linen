/-
  Control.Concurrent.STM.TMVar — Transactional MVars

  A `TMVar` is a synchronising variable in STM, analogous to `MVar` in IO.
  It can be empty or full.
-/

import Linen.Control.Concurrent.STM.TVar

namespace Control.Concurrent.STM

open Control.Monad

/-- A transactional synchronization variable. Can be empty or contain a value.
    $$\text{TMVar}(\alpha) = \text{TVar}(\text{Option}(\alpha))$$ -/
def TMVar (α : Type) : Type := TVar (Option α)

namespace TMVar

/-- Create a new full TMVar.
    $$\text{newTMVar} : \alpha \to \text{STM}(\text{TMVar}(\alpha))$$ -/
@[inline] def newTMVar (val : α) : STM (TMVar α) := TVar.newTVar (some val)

/-- Create a new empty TMVar.
    $$\text{newEmptyTMVar} : \text{STM}(\text{TMVar}(\alpha))$$ -/
@[inline] def newEmptyTMVar : STM (TMVar α) := TVar.newTVar none

/-- Create a new full TMVar in IO.
    $$\text{newTMVarIO} : \alpha \to \text{IO}(\text{TMVar}(\alpha))$$ -/
@[inline] def newTMVarIO (val : α) : IO (TMVar α) := TVar.newTVarIO (some val)

/-- Create a new empty TMVar in IO.
    $$\text{newEmptyTMVarIO} : \text{IO}(\text{TMVar}(\alpha))$$ -/
@[inline] def newEmptyTMVarIO : IO (TMVar α) := TVar.newTVarIO none

/-- Take the value from a TMVar, leaving it empty. Retries if empty.
    $$\text{takeTMVar} : \text{TMVar}(\alpha) \to \text{STM}(\alpha)$$ -/
@[inline] def takeTMVar (tv : TMVar α) : STM α := do
  let val ← TVar.readTVar tv
  match val with
  | some v =>
    TVar.writeTVar tv none
    pure v
  | none => STM.retry

/-- Put a value into a TMVar. Retries if full.
    $$\text{putTMVar} : \text{TMVar}(\alpha) \to \alpha \to \text{STM}(\text{Unit})$$ -/
@[inline] def putTMVar (tv : TMVar α) (val : α) : STM Unit := do
  let current ← TVar.readTVar tv
  match current with
  | some _ => STM.retry
  | none => TVar.writeTVar tv (some val)

/-- Read the value without removing it. Retries if empty.
    $$\text{readTMVar} : \text{TMVar}(\alpha) \to \text{STM}(\alpha)$$ -/
@[inline] def readTMVar (tv : TMVar α) : STM α := do
  let val ← TVar.readTVar tv
  match val with
  | some v => pure v
  | none => STM.retry

/-- Try to take the value, returning `none` if empty (non-blocking).
    $$\text{tryTakeTMVar} : \text{TMVar}(\alpha) \to \text{STM}(\text{Option}(\alpha))$$ -/
@[inline] def tryTakeTMVar (tv : TMVar α) : STM (Option α) := do
  let val ← TVar.readTVar tv
  match val with
  | some v =>
    TVar.writeTVar tv none
    pure (some v)
  | none => pure none

/-- Try to put a value, returning `false` if full (non-blocking).
    $$\text{tryPutTMVar} : \text{TMVar}(\alpha) \to \alpha \to \text{STM}(\text{Bool})$$ -/
@[inline] def tryPutTMVar (tv : TMVar α) (val : α) : STM Bool := do
  let current ← TVar.readTVar tv
  match current with
  | some _ => pure false
  | none =>
    TVar.writeTVar tv (some val)
    pure true

/-- Check if the TMVar is empty.
    $$\text{isEmptyTMVar} : \text{TMVar}(\alpha) \to \text{STM}(\text{Bool})$$ -/
@[inline] def isEmptyTMVar (tv : TMVar α) : STM Bool := do
  let val ← TVar.readTVar tv
  pure val.isNone

end TMVar
end Control.Concurrent.STM
