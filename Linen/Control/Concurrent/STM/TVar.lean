/-
  Control.Concurrent.STM.TVar — Transactional mutable variables

  A `TVar` is a mutable variable that can be read and written within
  STM transactions.

  ## Design

  Backed by `IO.Ref`. Thread safety is provided by the global STM mutex
  in `atomically`.
-/

import Linen.Control.Monad.STM

namespace Control.Concurrent.STM

open Control.Monad

/-- A transactional variable holding a value of type `α`.
    $$\text{TVar}(\alpha) = \text{IO.Ref}(\alpha)$$ -/
def TVar (α : Type) : Type := IO.Ref α

namespace TVar

/-- Create a new TVar with an initial value (in IO).
    $$\text{newTVarIO} : \alpha \to \text{IO}(\text{TVar}(\alpha))$$ -/
@[inline] def newTVarIO (val : α) : IO (TVar α) := IO.mkRef val

/-- Create a new TVar within an STM transaction.
    $$\text{newTVar} : \alpha \to \text{STM}(\text{TVar}(\alpha))$$ -/
@[inline] def newTVar (val : α) : STM (TVar α) := show BaseIO _ from do
  let ref ← ST.mkRef val
  pure (.success ref)

/-- Read the current value of a TVar.
    $$\text{readTVar} : \text{TVar}(\alpha) \to \text{STM}(\alpha)$$ -/
@[inline] def readTVar (tv : TVar α) : STM α := show BaseIO _ from do
  let v ← tv.get
  pure (.success v)

/-- Write a new value to a TVar.
    $$\text{writeTVar} : \text{TVar}(\alpha) \to \alpha \to \text{STM}(\text{Unit})$$ -/
@[inline] def writeTVar (tv : TVar α) (val : α) : STM Unit := show BaseIO _ from do
  tv.set val
  pure (.success ())

/-- Modify a TVar's value with a strict function.
    $$\text{modifyTVar'} : \text{TVar}(\alpha) \to (\alpha \to \alpha) \to \text{STM}(\text{Unit})$$ -/
@[inline] def modifyTVar' (tv : TVar α) (f : α → α) : STM Unit := show BaseIO _ from do
  tv.modify f
  pure (.success ())

end TVar
end Control.Concurrent.STM
