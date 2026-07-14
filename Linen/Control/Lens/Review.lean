/-
  Linen.Control.Lens.Review — `Review`, `AReview`, `unto`, `un`, `re`,
  `review`, `reviews`, `(#)`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Review` (fetched and read via
  Hackage's rendered Haddock and source: the signatures below were pulled
  from the real source, not recalled from memory). A `Review t b` is the
  "write-only" dual of a `Getter`: instead of extracting an `a` out of an
  `s`, it builds a `t` out of a bare `b`, never inspecting anything else —
  the direction `_Left`/`_Just`/etc. (`Linen.Control.Lens.Prism`) go when
  used "backwards". `Review`/`AReview` are actually declared in upstream's
  `Control.Lens.Type` (this module only re-exports them there), but this
  batch places them here, alongside the rest of their supporting
  combinators, following the `Iso` → `Prism` → `Review` ordering already set
  by `Linen.Control.Lens.Internal.{Iso,Prism,Review}`.

  Upstream's real signatures:

  ```
  type Review t b = forall p f. (Choice p, Bifunctor p, Settable f) => Optic' p f t b
  type AReview t b = Optic' Tagged Identity t b

  unto   :: (Profunctor p, Bifunctor p, Functor f) => (b -> t) -> Optic p f s t a b
  un     :: (Profunctor p, Bifunctor p, Functor f) => Getting a s a -> Optic' p f a s
  re     :: AReview t b -> Getter b t
  review :: MonadReader b m => AReview t b -> m t
  (#)    :: AReview t b -> b -> t
  reviews :: MonadReader b m => AReview t b -> (t -> r) -> m r
  reuse   :: MonadState b m => AReview t b -> m t
  reuses  :: MonadState b m => AReview t b -> (t -> r) -> m r
  ```

  `unto` reuses `Linen.Control.Lens.Internal.Review`'s `retagged` directly:
  `unto f = first absurd . lmap absurd . rmap (fmap f)` is exactly `retagged`
  (the `first absurd . lmap absurd` part) applied after `rmap (fmap f)`.

  A `Prism`/`Iso` is always runnable as an `AReview` (Choice/Profunctor both
  being weaker than what `Tagged`'s trivial `Profunctor` instance needs,
  since `Tagged`'s `lmap` never inspects its phantom argument) — see
  `Linen.Control.Lens.Prism.prism`/`Linen.Control.Lens.Iso.iso`'s own callers
  for that use, no separate coercion is ported here since every concrete
  `Prism`/`Iso` builder in this batch is *already* polymorphic enough to be
  called directly at `P := Tagged`.

  **Scope note (`review`/`reviews`/`re`, `MonadReader`).** For the same
  reason `Linen.Control.Lens.Getter`'s own scope note gives for
  `view`/`views`/`iview`, upstream's real `review`/`reviews` are generalized
  over an arbitrary reader monad `m` via an mtl-style `MonadReader` class that
  `linen` has not ported (only the concrete `Linen.Control.Monad.Reader`
  monad itself exists). This port keeps the essential degenerate case at `m
  := Id` — i.e. `review`/`reviews` take the "environment" `b` as a plain
  explicit argument rather than reading it from an ambient reader monad,
  matching upstream's own definition before its `MonadReader` wrapping. Under
  this specialization `review` and `(#)` become the exact same function
  (upstream's own `(#) = review`'s `m := Id` degenerate case coincide
  already), so `(#)` is defined as `review`'s infix alias.

  **Scope note (`reuse`/`reuses`).** Need an mtl-style `MonadState`, which
  `linen` has likewise not ported (only the concrete `StateT` monad itself) —
  skipped, following `Linen.Control.Lens.Getter`'s identical precedent for
  `use`/`uses`.

  **Scope note (`reviewing`).** Upstream's `reviewing :: (Bifunctor p,
  Functor f) => Optic Tagged Identity s t a b -> Optic' p f t b` generalizes
  an already-built `AReview`-shaped optic to run at any `Bifunctor p` (rather
  than only the concrete `Tagged`); it has no call site anywhere in this
  batch's requested scope (`Review`, `AReview`, `un`, `unto`, `re`, `(#)`,
  `review`, `reviews`), so it is left unported alongside `Reviewable`'s and
  `retagged`'s upstream re-export (both already live directly in
  `Linen.Control.Lens.Internal.Review` and need no re-export step in Lean). -/

import Linen.Control.Lens.Getter
import Linen.Control.Lens.Internal.Review
import Linen.Control.Lens.Internal.Setter
import Linen.Control.Profunctor.Choice
import Linen.Control.Profunctor.Types
import Linen.Data.Bifunctor

open Control Control.Profunctor Control.Lens.Internal

namespace Control.Lens

-- ── Bifunctor Tagged ────────────────────────────

/-- `Tagged` is a `Bifunctor`: its phantom first argument carries no data, so
    `bimap` only ever needs to touch the second — `bimap _ g = mapSnd g`,
    matching `Tagged`'s existing `Profunctor` instance
    (`Linen.Control.Profunctor.Types`), which likewise ignores its first
    argument entirely. Needed to instantiate `Review`/`unto`/`un`'s
    `Bifunctor p` constraint at `p := Tagged`. -/
instance : Data.Bifunctor Tagged where
  bimap _ g t := ⟨g t.unTagged⟩

-- ── Review / AReview ────────────────────────────

/-- `Review t b := ∀ p f, (Choice p, Bifunctor p, Settable f) => Optic' p f t
    b`: the "write-only" dual of a `Getter` — it can build a `t` out of a
    bare `b`, but never inspects an `s`/`a` to do so. Like `Iso`/`Prism`
    (`Linen.Control.Lens.{Iso,Prism}`), genuinely profunctor-generalized,
    matching upstream verbatim. -/
abbrev Review (T B : Type u) :=
  ∀ {P : Type u → Type u → Type u} [Choice P] [Data.Bifunctor P] {F : Type u → Type u} [Settable F],
    Optic' P F T B

/-- `AReview t b := Optic' Tagged Identity t b`: a `Review` already run at the
    concrete profunctor `Tagged` and the identity functor — the shape
    `review`/`reviews`/`re`/`(#)` actually run against. Since Lean's `Id` is
    definitionally the identity (see `Linen.Control.Lens.Iso`'s module doc
    comment for the same point), this unfolds to plain `Tagged b b -> Tagged
    t t` with no `Identity`-stripping step needed anywhere below. -/
abbrev AReview (T B : Type u) := Optic' Tagged Id T B

-- ── unto / un ────────────────────────────────────

/-- `unto :: (Profunctor p, Bifunctor p, Functor f) => (b -> t) -> Optic p f s
    t a b`: build an arbitrary optic that only knows how to go "backwards",
    from a plain `b -> t` function — `unto f = first absurd . lmap absurd .
    rmap (fmap f)`, i.e. `retagged` (`Linen.Control.Lens.Internal.Review`)
    applied after mapping `f` over the profunctor's second argument. -/
@[inline] def unto {S T A B : Type u} (f : B → T) :
    ∀ {P : Type u → Type u → Type u} [Profunctor P] [Data.Bifunctor P] {F : Type u → Type u} [Functor F],
      Optic P F S T A B :=
  fun {P} [Profunctor P] [Data.Bifunctor P] {F} [Functor F] p =>
    Internal.retagged (Profunctor.rmap (Functor.map f) p)

/-- `un :: (Profunctor p, Bifunctor p, Functor f) => Getting a s a -> Optic' p
    f a s`: turn a `Getter`/`Lens`/`Traversal`/`Fold` around, producing an
    optic that builds an `s` out of a bare `a` by running the original
    getter's *view* function backwards — `un = unto . view`. -/
@[inline] def un {S A : Type u} (l : Getting A S A) :
    ∀ {P : Type u → Type u → Type u} [Profunctor P] [Data.Bifunctor P] {F : Type u → Type u} [Functor F],
      Optic' P F A S :=
  unto (view l)

-- ── re ───────────────────────────────────────────

/-- `re :: AReview t b -> Getter b t`: view an `AReview` "backwards", as a
    plain `Getter` from `b` to `t` — `re p = to (runIdentity #. unTagged . p
    .# Identity)`, simplified here since `Tagged`/`Id` need no
    `unTagged`/`runIdentity`-stripping ceremony beyond a single
    `.unTagged` projection (`Id` being definitionally transparent, as in
    `Linen.Control.Lens.Iso`'s `withIso`). -/
@[inline] def re {T B : Type u} (l : AReview T B) : Getter B T :=
  to (fun b => (l (⟨b⟩ : Tagged B B)).unTagged)

-- ── review / reviews / (#) ──────────────────────

/-- `review :: MonadReader b m => AReview t b -> m t`: build a `t` out of a
    `b`, running an `AReview` at the concrete `Tagged`/`Id` profunctor/functor
    pair — `review p = asks (runIdentity #. unTagged . p .# Identity)`. See
    the module doc comment for why this lands directly at `b -> t` rather
    than an arbitrary `MonadReader b m => m t`. -/
@[inline] def review {T B : Type u} (l : AReview T B) (b : B) : T :=
  (l (⟨b⟩ : Tagged B B)).unTagged

-- `(#) :: AReview t b -> b -> t`: infix alias for `review` (its `m := Id`
-- degenerate form and `(#)` coincide already upstream — see the module doc
-- comment).
@[inherit_doc review] infixr:8 " # " => review

/-- `reviews :: MonadReader b m => AReview t b -> (t -> r) -> m r`: like
    `review`, but post-processes the built value with `f` — `reviews p tr =
    asks (tr . runIdentity #. unTagged . p .# Identity)`. See the module doc
    comment for why this lands directly at `b -> r`. -/
@[inline] def reviews {T B R : Type u} (l : AReview T B) (f : T → R) (b : B) : R :=
  f (review l b)

end Control.Lens
