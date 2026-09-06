/-
  `Control.Monad.Freer.State` — the state effect over `Eff`

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-State.html
  module #4 of the `FreerSimple` import (see
  `docs/imports/FreerSimple/dependencies.md`).

  ## Relationship to `Control.Monad.State`

  As with `Control.Monad.Freer.Reader`, this does **not** replace `linen`'s
  mtl-style `Control.Monad.State` (`StateT`/`get`/`put`), which remains the
  recommended API for ordinary state threading. This module illustrates the
  effect-row mechanism and composes with other row effects in one `Eff`
  computation.
-/
import Linen.Control.Monad.Freer

namespace Control.Monad.Freer.State

open Data.OpenUnion Control.Monad.Freer

-- ── The effect ──────────────────────────────────────────────────────────────

/-- The state effect: read the current state, or replace it. -/
inductive State (σ : Type) : Type → Type where
  /-- Read the current state. -/
  | get : State σ σ
  /-- Replace the current state. -/
  | put : σ → State σ Unit

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Read the current state. -/
def get {σ : Type} {effs : List (Type → Type)} [Member (State σ) effs] :
    Eff effs σ :=
  send (.get : State σ σ)

/-- Replace the current state. -/
def put {σ : Type} {effs : List (Type → Type)} [Member (State σ) effs]
    (s : σ) : Eff effs Unit :=
  send (.put s : State σ Unit)

/-- Apply a function to the current state.

    $$\text{modify}(f) = \text{get} \bind (\text{put} \circ f)$$ -/
def modify {σ : Type} {effs : List (Type → Type)} [Member (State σ) effs]
    (f : σ → σ) : Eff effs Unit := do
  put (f (← get))

/-- Read a projection of the current state. -/
def gets {σ α : Type} {effs : List (Type → Type)} [Member (State σ) effs]
    (f : σ → α) : Eff effs α :=
  f <$> (get : Eff effs σ)

-- ── Handler ─────────────────────────────────────────────────────────────────

/-- Thread `σ` through the computation, returning the value alongside the final
    state and removing the state effect from the row.

    Written as a direct recursion rather than via `interpret`, because the
    handler itself carries state between requests — `interpret`'s handler is
    stateless. Structurally recursive on the `Eff` argument. -/
def runState {σ : Type} {effs : List (Type → Type)} {α : Type} :
    σ → Eff (State σ :: effs) α → Eff effs (α × σ)
  | s, .protect a  => .protect (a, s)
  | s, .impure u k => match u with
    | .here e   => match (e : State σ _) with
      | .get    => runState s (k s)
      | .put s' => runState s' (k ())
    | .there u' => .impure u' (fun b => runState s (k b))

/-- Run a stateful computation, keeping only its value. -/
def evalState {σ : Type} {effs : List (Type → Type)} {α : Type}
    (s : σ) (m : Eff (State σ :: effs) α) : Eff effs α :=
  Prod.fst <$> runState s m

/-- Run a stateful computation, keeping only its final state. -/
def execState {σ : Type} {effs : List (Type → Type)} {α : Type}
    (s : σ) (m : Eff (State σ :: effs) α) : Eff effs σ :=
  Prod.snd <$> runState s m

end Control.Monad.Freer.State
