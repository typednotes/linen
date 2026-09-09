/-
  `Control.Monad.Effect.Reader` — the reader effect over `Eff`

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-Reader.html
  module #3 of the `FreerSimple` import (see
  `docs/imports/FreerSimple/dependencies.md`).

  ## Relationship to `Control.Monad.Reader`

  This is **not** a replacement for `Control.Monad.Reader`, `linen`'s mtl-style
  `ReaderT`/`ask`/`local` port — that remains the recommended API for ordinary
  environment threading, and is cheaper besides. This module exists to
  illustrate the effect-row mechanism on a familiar effect, and to be composed
  with *other* row effects in a single `Eff` computation. Use it when the point
  is that the row bounds what a computation may do; use
  `Control.Monad.Reader` when the point is just to thread an environment.
-/
import Linen.Control.Monad.Effect

namespace Control.Monad.Effect.Reader

open Data.OpenUnion Control.Monad.Effect

-- ── The effect ──────────────────────────────────────────────────────────────

/-- The reader effect: a single request, for the environment of type `ρ`. -/
inductive Reader (ρ : Type) : Type → Type where
  /-- Ask for the environment. -/
  | ask : Reader ρ ρ
  deriving DecidableEq

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Retrieve the environment.

    `send`-wrapping smart constructor, hand-written because Lean has no
    Template Haskell to stand in for upstream's `makeEffect`. -/
def ask {ρ : Type} {effs : List (Type → Type)} [Member (Reader ρ) effs] :
    Eff effs ρ :=
  send (.ask : Reader ρ ρ)

/-- Retrieve a projection of the environment.

    $$\text{asks}(f) = f \mathbin{<\!\$\!>} \text{ask}$$ -/
def asks {ρ α : Type} {effs : List (Type → Type)} [Member (Reader ρ) effs]
    (f : ρ → α) : Eff effs α :=
  f <$> (ask : Eff effs ρ)

-- ── Handler ─────────────────────────────────────────────────────────────────

/-- Answer every `ask` with `env`, removing the reader effect from the row. -/
def runReader {ρ : Type} {effs : List (Type → Type)} {α : Type} (env : ρ) :
    Eff (Reader ρ :: effs) α → Eff effs α :=
  interpret (fun | .ask => .protect env)

/-- Run a computation against a transformed environment (upstream's `local`).

    The inner computation is handled first against `f env`, so the modification
    is scoped to it and invisible afterwards. -/
def withReader {ρ : Type} {effs : List (Type → Type)} {α : Type}
    (f : ρ → ρ) (env : ρ) : Eff (Reader ρ :: effs) α → Eff effs α :=
  runReader (f env)

end Control.Monad.Effect.Reader
