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

namespace Control.Monad.Trans.Resource

-- ── Cleanup registry ─────────────────────────────────────────

/-- Opaque key for a registered cleanup action.
    Single-use: calling `release` twice is a no-op. -/
structure ReleaseKey where
  private mk ::
    id : Nat

/-- Internal cleanup registry. -/
private abbrev CleanupMap := Array (Nat × IO Unit)

-- ── The monad ────────────────────────────────────────────────

/-- Resource management monad transformer: a `ReaderT` over a mutable
    cleanup registry, so `Monad`/`bind` come from `ReaderT` for free.

    $$\text{ResourceT}\ m\ \alpha = \text{IO.Ref CleanupMap} \to m\ \alpha$$ -/
abbrev ResourceT (m : Type → Type) (α : Type) := ReaderT (IO.Ref CleanupMap) m α

/-- Lift an `IO` action into `ResourceT m` whenever `m` itself can. -/
instance [MonadLift IO m] : MonadLift IO (ResourceT m) where
  monadLift io := fun _ => MonadLift.monadLift io

-- ── Registering and releasing resources ───────────────────────

/-- Register a resource with its cleanup action.
    Returns a `ReleaseKey` that can be used to release early. -/
def allocate (acquire : IO α) (release : α → IO Unit) : ResourceT IO (ReleaseKey × α) :=
  fun ref => do
    let a ← acquire
    let map ← ref.get
    let key := map.size
    ref.set (map.push (key, release a))
    return (⟨key⟩, a)

/-- Release a resource early. No-op if already released. -/
def release (key : ReleaseKey) : ResourceT IO Unit :=
  fun ref => do
    let map ← ref.get
    match map.findIdx? (fun (k, _) => k == key.id) with
    | some idx =>
      if h : idx < map.size then
        let (_, action) := map[idx]
        ref.set (map.eraseIdx idx)
        action
      else pure ()
    | none => pure ()

/-- Run a `ResourceT` computation. All registered cleanup actions
    execute on completion in LIFO order, even on exceptions. -/
def runResourceT (action : ResourceT IO α) : IO α := do
  let ref ← IO.mkRef (#[] : CleanupMap)
  try
    action ref
  finally
    let map ← ref.get
    let sz := map.size
    for i in [:sz] do
      let idx := sz - 1 - i
      if h : idx < map.size then
        let (_, cleanup) := map[idx]
        try cleanup
        catch _ => pure ()

-- ── Proofs ─────────────────────────────────────────────────

/-- `ReleaseKey` equality is by id. -/
theorem releaseKey_eq (a b : ReleaseKey) : a = b ↔ a.id = b.id := by
  constructor
  · intro h; subst h; rfl
  · intro h; cases a; cases b; simp at h; subst h; rfl

end Control.Monad.Trans.Resource
