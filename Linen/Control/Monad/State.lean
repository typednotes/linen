/-
  `StateT` / `State` — Haskell `mtl`-compatible names

  Lean core already defines `StateT`, plus the generic `get`/`set`/`modify`/
  `modifyGet` (via `MonadState`) that work directly over `StateT` — those
  are used as-is, not re-wrapped. This module adds only what core lacks:
  the Haskell `mtl` name `put` (core's name is `set`), the `gets` projection,
  the `State` type alias, and the `run`/`eval`/`exec` family of runners.

  ## Haskell source

  https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Strict.html
-/

namespace Control.Monad.State

/-- The `State` monad: `StateT` over `Id`.

    $$\text{State}\ \sigma\ \alpha = \text{StateT}\ \sigma\ \text{Id}\ \alpha = \sigma \to (\alpha \times \sigma)$$ -/
abbrev State (σ : Type) (α : Type) := StateT σ Id α

/-- Replace the state with a new value. Alias for Lean's `set`
    (Haskell `mtl` name).

    $$\text{put}(\sigma) : \text{StateT}\ \sigma\ m\ \text{Unit}$$ -/
@[inline] def put [Monad m] (s : σ) : StateT σ m Unit :=
  set s

/-- Get a projection of the current state.

    $$\text{gets}(f) = f \mathbin{<\!\$\!>} \text{get}$$ -/
@[inline] def gets [Monad m] (f : σ → α) : StateT σ m α :=
  f <$> get

/-- Run a `StateT` computation with an initial state. Alias for `StateT.run`.

    $$\text{runStateT}(ma, s_0) : m\ (\alpha \times \sigma)$$ -/
@[inline] def runStateT [Monad m] (ma : StateT σ m α) (s : σ) : m (α × σ) :=
  ma.run s

/-- Run a `StateT` computation, returning only the final value.

    $$\text{evalStateT}(ma, s_0) : m\ \alpha$$ -/
@[inline] def evalStateT [Functor m] [Monad m] (ma : StateT σ m α) (s : σ) : m α :=
  Prod.fst <$> ma.run s

/-- Run a `StateT` computation, returning only the final state.

    $$\text{execStateT}(ma, s_0) : m\ \sigma$$ -/
@[inline] def execStateT [Functor m] [Monad m] (ma : StateT σ m α) (s : σ) : m σ :=
  Prod.snd <$> ma.run s

/-- Run a pure `State` computation with an initial state.

    $$\text{runState}(ma, s_0) : \alpha \times \sigma$$ -/
@[inline] def runState (ma : State σ α) (s : σ) : α × σ :=
  ma.run s

/-- Run a pure `State` computation, returning only the final value.

    $$\text{evalState}(ma, s_0) : \alpha$$ -/
@[inline] def evalState (ma : State σ α) (s : σ) : α :=
  (ma.run s).fst

/-- Run a pure `State` computation, returning only the final state.

    $$\text{execState}(ma, s_0) : \sigma$$ -/
@[inline] def execState (ma : State σ α) (s : σ) : σ :=
  (ma.run s).snd

-- ── Proofs ──────────────────────────────────────────

/-- `runState` of `pure a` returns `(a, s)`.
    $$\text{runState}(\text{pure}\ a, s) = (a, s)$$ -/
theorem runState_pure (a : α) (s : σ) : runState (pure a : State σ α) s = (a, s) := rfl

/-- `evalState` of `pure a` returns `a`.
    $$\text{evalState}(\text{pure}\ a, s) = a$$ -/
theorem evalState_pure (a : α) (s : σ) : evalState (pure a : State σ α) s = a := rfl

/-- `execState` of `pure a` returns the initial state unchanged.
    $$\text{execState}(\text{pure}\ a, s) = s$$ -/
theorem execState_pure (a : α) (s : σ) : execState (pure a : State σ α) s = s := rfl

/-- `get` returns the current state: `runState get s = (s, s)`.
    $$\text{runState}(\text{get}, s) = (s, s)$$ -/
theorem runState_get (s : σ) : runState (get : State σ σ) s = (s, s) := rfl

/-- `put` replaces the state: `execState (put s') s = s'`.
    $$\text{execState}(\text{put}(s'), s) = s'$$ -/
theorem execState_put (s s' : σ) : execState (put s' : State σ Unit) s = s' := rfl

end Control.Monad.State
