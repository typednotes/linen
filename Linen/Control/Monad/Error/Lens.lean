/-
  Linen.Control.Monad.Error.Lens — `throwing`, `throwing_`, `catching`,
  `catching_`, `handling`, `handling_`, `trying`

  Port of Hackage's `lens-5.3.6`'s `Control.Monad.Error.Lens` (fetched and
  read via the real source, not recalled from memory). Upstream combines
  `mtl`'s `MonadError e m` class with a `Prism`/`Getter`-shaped focus on part
  of the error type `e`, so a large sum-of-errors type can be thrown/caught
  through a single constructor's `Prism` without pattern-matching on the
  whole sum by hand. Upstream's real signatures:

  ```
  throwing  :: MonadError e m => AReview e t -> t -> m r
  throwing_ :: MonadError e m => AReview e () -> m r

  catching  :: MonadError e m => Getting (First a) e a -> m r -> (a -> m r) -> m r
  catching_ :: MonadError e m => Getting (First a) e a -> m r -> m r -> m r

  handling  :: MonadError e m => Getting (First a) e a -> (a -> m r) -> m r -> m r
  handling_ :: MonadError e m => Getting (First a) e a -> m r -> m r -> m r

  trying    :: MonadError e m => Getting (First a) e a -> m r -> m (Either a r)
  ```

  **Deviation (`Getting (First a) e a` → `Prism' e a`).** Upstream's real
  `catching`/`handling`/`trying` accept *any* `Fold`/`Getter`/`Prism` (run as
  a `Getting`), since Haskell's profunctor encoding lets a `Prism` be used
  wherever a `Getting` is expected with no ceremony. `linen`'s own `Prism`
  (`Linen.Control.Lens.Prism`) is instead encoded over the dedicated arrow
  profunctor `Control.Fun` (a `structure`, not the bare `α → β` `Getting`
  itself is built from), so a `Prism` in this codebase does **not**
  type-check where a `Getting` is expected — the two are structurally
  different function types even though both are "the same optic" at the
  value level. Rather than force every call site to hand-roll a `Getting`
  (which would need its own `Choice Function`-style bridging this batch's
  scope does not otherwise call for), this port narrows these three
  combinators' first argument to `Prism' e a` directly, extracting the match
  function via `Linen.Control.Lens.Prism.withPrism` — exactly matching every
  realistic error-lens use (one prism per error case, e.g. this batch's own
  `Linen.Control.Exception.Lens` per-`IO.Error`-constructor prisms), and
  losing no expressiveness for that use.

  **Substitution (`MonadError e m` → Lean's `MonadExcept ε m`).** `linen` has
  not ported `mtl`'s `MonadError` class as a standalone typeclass — Lean's own
  core prelude (`Init/Prelude.lean`) already ships an equivalent
  `MonadExcept ε m` class (`throw`, `tryCatch`, both exported unqualified),
  which every monad in this codebase that can fail already gets for free
  (`IO`, `ExceptT`, …, see `Linen.Control.Monad.Except`'s own doc comment).
  This module is written directly against `MonadExcept` instead of adding a
  second, redundant class.

  **Scope note (`trying_`, `Handler`, `catches`, `catchJust`).** Upstream's
  `Control.Monad.Error.Lens` module itself only exports the seven
  combinators above (`Handler`/`catches` are `Control.Exception.Lens`'s own
  exports, ported there instead — see `Linen.Control.Exception.Lens`, which
  re-exports everything below via `open`/`export` so `IO`-flavoured callers
  never need to import this module directly).
-/

import Linen.Control.Lens.Prism
import Linen.Control.Lens.Review

open Control.Lens

namespace Control.Monad.Error.Lens

-- ── throwing / throwing_ ─────────────────────────

/-- `throwing :: MonadError e m => AReview e t -> t -> m r`: throw an error
    built from `t` via the `AReview`'s constructor — `throwing l t = reviews
    l throwError t`. -/
@[inline] def throwing {E T R : Type u} {M : Type u → Type u} [MonadExcept E M]
    (l : AReview E T) (t : T) : M R :=
  reviews l throw t

/-- `throwing_ :: MonadError e m => AReview e () -> m r`: like `throwing`,
    specialized to a nullary (`Unit`-tagged) error constructor — `throwing_ l
    = reviews l throwError ()`. -/
@[inline] def throwing_ {E R : Type u} {M : Type u → Type u} [MonadExcept E M]
    (l : AReview E PUnit) : M R :=
  throwing l ⟨⟩

-- ── catching / catching_ ─────────────────────────

/-- `catching :: MonadError e m => Getting (First a) e a -> m r -> (a -> m r)
    -> m r`: run `m`, and if it throws an error the `Prism' e a` recognises,
    hand the focused value to the handler `h`; any other error is rethrown
    unchanged — `catching l m h = m \`catchError\` \e -> maybe (throwError
    e) h (preview l e)`, with `preview` here played by `withPrism`'s match
    function directly (see the module doc comment's deviation note). -/
@[inline] def catching {E A R : Type u} {M : Type u → Type u} [Monad M] [MonadExcept E M]
    (l : Prism' E A) (m : M R) (h : A → M R) : M R :=
  tryCatch m fun e => withPrism l (fun _ seta => match seta e with
    | Sum.inr a => h a
    | Sum.inl _ => throw e)

/-- `catching_ :: MonadError e m => Getting (First a) e a -> m r -> m r -> m
    r`: like `catching`, but the handler ignores the focused value —
    `catching_ l m b = m \`catchError\` \e -> maybe (throwError e) (const b)
    (preview l e)`. -/
@[inline] def catching_ {E A R : Type u} {M : Type u → Type u} [Monad M] [MonadExcept E M]
    (l : Prism' E A) (m : M R) (b : M R) : M R :=
  catching l m (fun _ => b)

-- ── handling / handling_ ─────────────────────────

/-- `handling :: MonadError e m => Getting (First a) e a -> (a -> m r) -> m r
    -> m r`: `catching` with its handler and action arguments flipped —
    `handling = flip . catching` (uncurried to match `catching`'s own
    argument order). -/
@[inline] def handling {E A R : Type u} {M : Type u → Type u} [Monad M] [MonadExcept E M]
    (l : Prism' E A) (h : A → M R) (m : M R) : M R :=
  catching l m h

/-- `handling_ :: MonadError e m => Getting (First a) e a -> m r -> m r -> m
    r`: `catching_` with its handler and action arguments flipped. -/
@[inline] def handling_ {E A R : Type u} {M : Type u → Type u} [Monad M] [MonadExcept E M]
    (l : Prism' E A) (b : M R) (m : M R) : M R :=
  catching_ l m b

-- ── trying ───────────────────────────────────────

/-- `trying :: MonadError e m => Getting (First a) e a -> m r -> m (Either a
    r)`: run `m`, catching any error the `Prism' e a` recognises and
    returning it as `Except.error` instead of rethrowing — `trying l m =
    catching l (Right <$> m) (pure . Left)`. -/
@[inline] def trying {E A R : Type u} {M : Type u → Type u} [Monad M] [MonadExcept E M]
    (l : Prism' E A) (m : M R) : M (Except A R) :=
  catching l (Except.ok <$> m) (fun a => pure (Except.error a))

end Control.Monad.Error.Lens
