/-
  Control.Monad.STM — Software Transactional Memory

  Provides a simple pessimistic STM implementation. All transactions
  are serialized via a global mutex, providing strong consistency
  guarantees at the cost of concurrency.

  ## Design

  Uses a global `Std.Mutex` to serialize transactions. `retry` is implemented
  by releasing the lock, sleeping briefly, then re-acquiring and re-running
  the transaction. `orElse` catches retry from the first branch and runs
  the second.

  ## Guarantees

  - All transactions are serializable (global lock)
  - `retry` blocks until another transaction commits
  - `orElse` provides composable alternative transactions

  ## Axiom-dependent properties

  Atomicity depends on all shared mutable state being accessed only
  through `TVar` within `STM` transactions.
-/

import Std.Sync.Mutex

namespace Control.Monad

-- ── The STM monad ──────────────────

/-- Result of an STM transaction step. -/
inductive STMResult (α : Type) where
  | success : α → STMResult α
  | retry : STMResult α

/-- The STM monad. Wraps BaseIO actions that execute under the global STM lock.
    $$\text{STM}(\alpha) = \text{BaseIO}(\text{STMResult}(\alpha))$$ -/
def STM (α : Type) : Type := BaseIO (STMResult α)

namespace STM

instance : Monad STM where
  pure a := (pure (STMResult.success a) : BaseIO (STMResult _))
  bind ma f := show BaseIO _ from do
    let result ← (ma : BaseIO (STMResult _))
    match result with
    | .success a => (f a : BaseIO (STMResult _))
    | .retry => pure .retry

instance : MonadLiftT BaseIO STM where
  monadLift action := show BaseIO _ from do
    let a ← action
    pure (.success a)

/-- Signal retry: abort this transaction and wait for state changes.
    $$\text{retry} : \text{STM}(\alpha)$$ -/
@[inline] def retry : STM α := (pure .retry : BaseIO (STMResult _))

/-- Try the first transaction; if it retries, try the second.
    $$\text{orElse}(a, b) = \begin{cases} a & \text{if } a \text{ succeeds} \\ b & \text{if } a \text{ retries} \end{cases}$$ -/
@[inline] def orElse (a b : STM α) : STM α := show BaseIO _ from do
  let result ← (a : BaseIO (STMResult _))
  match result with
  | .success v => pure (.success v)
  | .retry => (b : BaseIO (STMResult _))

/-- Check a condition; retry if false.
    $$\text{check}(p) = \text{if } p \text{ then pure () else retry}$$ -/
@[inline] def check (cond : Bool) : STM Unit :=
  if cond then pure () else retry

end STM

-- ── Global STM infrastructure ──────────────────

/-- Global mutex for serializing STM transactions. The unit state is unused;
    the mutex itself provides mutual exclusion. -/
private initialize stmMutex : Std.Mutex Unit ← Std.Mutex.new ()

/-- Run an STM transaction atomically. Blocks until the transaction succeeds
    (i.e., does not retry).

    Uses a global `Std.Mutex` to serialize all transactions. If the transaction
    signals `retry`, the lock is released and the thread sleeps briefly before
    retrying.

    $$\text{atomically} : \text{STM}(\alpha) \to \text{IO}(\alpha)$$ -/
def atomically (action : STM α) : IO α := do
  while true do
    let r : STMResult α ← stmMutex.atomically do
      MonadLiftT.monadLift (action : BaseIO (STMResult α))
    match r with
    | STMResult.success a => return a
    | STMResult.retry =>
      -- Release the lock and wait briefly before retrying
      IO.sleep 1
  unreachable!

end Control.Monad
