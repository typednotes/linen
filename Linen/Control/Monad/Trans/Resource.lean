/-
  `Control.Monad.Trans.Resource` — deterministic resource cleanup

  Port of Haskell's `resourcet` package (`Control.Monad.Trans.Resource`).
  `ResourceT` tracks resources (file handles, connections, etc.) acquired
  during a computation and guarantees they are cleaned up when the
  computation completes, even on exceptions.

  ## Guarantees

  - All registered cleanup actions run when `runResourceT` completes (via
    `try/finally`), even on exceptions.
  - `ReleaseKey` is single-use: releasing twice is a no-op.
  - Cleanup order is LIFO (last allocated = first released).
-/

import Std.Sync.Mutex

namespace Control.Monad.Trans.Resource

-- ── Cleanup registry ─────────────────────────────────────────

/-- Opaque key for a registered cleanup action.
    Single-use: calling `release` twice is a no-op. -/
structure ReleaseKey where
  private mk ::
    id : Nat

private abbrev CleanupMap := Array (Nat × IO Unit)

-- Each key is unique even across nested runs. An empty option means the
-- interpreter has already taken ownership of the pending cleanup actions.
private initialize nextKey : Std.Mutex Nat ← Std.Mutex.new 0

/-- A computation with a cleanup registry. Use `allocate`, `release` and
`runResourceT` to manage a dynamically sized collection of resources. -/
abbrev ResourceT (m : Type → Type) (α : Type) := ReaderT (Std.Mutex (Option CleanupMap)) m α

/-- Lift an `IO` action into `ResourceT m` whenever `m` itself can. -/
instance [MonadLift IO m] : MonadLift IO (ResourceT m) where
  monadLift io := fun _ => MonadLift.monadLift io

/-- Acquire a resource and register cleanup. The key can release it early
inside the same run. If the registry has closed, release the acquisition
immediately and fail. -/
def allocate (acquire : IO α) (release : α → IO Unit) : ResourceT IO (ReleaseKey × α) :=
  fun registry => do
    let value ← acquire
    let key ← nextKey.atomically (m := IO) do
      let key ← get
      set (key + 1)
      pure key
    let registered ← registry.atomically (m := IO) do
      let some pending ← get | return false
      set (some (pending.push (key, release value)))
      return true
    unless registered do
      release value
      throw (IO.userError "resource registry has closed")
    return (⟨key⟩, value)

/-- Release a resource early. Keys already used or from a different run
leave the current registry unchanged. -/
def release (key : ReleaseKey) : ResourceT IO Unit :=
  fun registry => do
    let action ← registry.atomically (m := IO) do
      let some pending ← get | return none
      let some index := pending.findIdx? (fun (id, _) => id == key.id) | return none
      if h : index < pending.size then
        let (_, action) := pending[index]
        set (some (pending.eraseIdx index h))
        return some action
      else
        return none
    if let some cleanup := action then cleanup

/-- Run a computation and clean up remaining resources in reverse order.
Cleanup continues if an individual finalizer throws. -/
def runResourceT (action : ResourceT IO α) : IO α := do
  let registry ← Std.Mutex.new (some (#[] : CleanupMap))
  try
    action registry
  finally
    let actions ← registry.atomically (m := IO) do
      let pending ← get
      set (none : Option CleanupMap)
      pure (pending.getD #[])
    for (_, cleanup) in actions.toList.reverse do
      try cleanup
      catch _ => pure ()

-- ── Proofs ─────────────────────────────────────────────────

/-- `ReleaseKey` equality is by id. -/
theorem releaseKey_eq (a b : ReleaseKey) : a = b ↔ a.id = b.id := by
  constructor
  · intro h; subst h; rfl
  · intro h; cases a; cases b; simp at h; subst h; rfl

end Control.Monad.Trans.Resource
