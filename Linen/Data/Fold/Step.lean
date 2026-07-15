/-
  Linen.Data.Fold.Step — the fold `Step` state machine

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Fold.Step`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Fold/Step.hs),
  module #8 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A fold's step function returns a `Step s b`: `Partial s` is an intermediate
  fold state (the step may be called again, or `extract`ed to a result), and
  `Done b` is a terminal result that ends the fold. `first` maps the state,
  `second`/`fmap` maps the result.

  ## Substitutions / deviations

  - **`Fuse` annotation dropped** — GHC-plugin marker with no Lean analogue
    (see the plan's `fusion-plugin-types` drop).
  - **`Bifunctor` is `Linen.Data.Bifunctor`** (the already-ported class), whose
    method names are `bimap`/`mapFst`/`mapSnd`; upstream's `first`/`second`
    map to `mapFst`/`mapSnd`.
-/

import Linen.Data.Bifunctor

namespace Data.Fold

-- ── The fold Step state machine ─────────────────────────────────────────────

/-- One step of a fold: `Partial s` carries an intermediate state; `Done b` is
    a terminal result that stops the fold. -/
inductive Step (s b : Type u) where
  | Partial : s → Step s b
  | Done : b → Step s b
  deriving Repr, Inhabited

namespace Step

/-- Map over the state (`Partial`) and the result (`Done`) at once. -/
@[inline] def bimap (f : s → t) (g : b → c) : Step s b → Step t c
  | .Partial a => .Partial (f a)
  | .Done b => .Done (g b)

/-- Map over the state only (upstream `first`). -/
@[inline] def mapFst (f : s → t) : Step s b → Step t b
  | .Partial a => .Partial (f a)
  | .Done x => .Done x

/-- Map over the result only (upstream `second`, also `fmap`). -/
@[inline] def mapSnd (g : b → c) : Step s b → Step s c
  | .Partial x => .Partial x
  | .Done a => .Done (g a)

/-- `Bifunctor`: `mapFst`/`mapSnd` are streamly's `first`/`second`. -/
instance : Data.Bifunctor Step where
  bimap := bimap
  mapFst := mapFst
  mapSnd := mapSnd

/-- `Functor` over the result: `fmap = second`. -/
instance : Functor (Step s) where
  map := mapSnd

/-- Map a monadic function over the result `b` in `Step s b`. -/
@[inline] def mapMStep [Applicative m] (f : a → m b) : Step s a → m (Step s b)
  | .Partial s => pure (.Partial s)
  | .Done b => .Done <$> f b

/-- If `Partial`, map the state; if `Done`, run the next step. -/
@[inline] def chainStepM [Applicative m]
    (f : s₁ → m s₂) (g : a → m (Step s₂ b)) : Step s₁ a → m (Step s₂ b)
  | .Partial s => Step.Partial <$> f s
  | .Done b => g b

end Step
end Data.Fold
