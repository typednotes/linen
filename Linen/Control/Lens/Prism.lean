/-
  Linen.Control.Lens.Prism — `Prism`, `Prism'`, `prism`, `prism'`,
  `withPrism`, `clonePrism`, `isoAsPrism`, `_Left`, `_Right`, `_Just`,
  `_Nothing`, `only`, `nearly`, `outside`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Prism` (fetched and read via
  Hackage's rendered Haddock and source: the signatures below were pulled
  from the real source, not recalled from memory). A `Prism s t a b` is a
  `Traversal` that focuses on at most one `a` and additionally knows how to
  *build* a `t` from a `b` alone (via `review`, `Linen.Control.Lens.Review`)
  without ever having seen an `s` — the optic behind pattern-matching
  constructors like `Left`/`Right`/`Just`.

  Matching upstream's real, profunctor-generalized signature verbatim: `type
  Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s
  (f t)`. Like `Iso` (`Linen.Control.Lens.Iso`, whose module doc comment
  explains the general principle), `Prism` needs this generalization —
  `withPrism` runs a `Prism` at the concrete profunctor `Market a b`
  (`Linen.Control.Lens.Internal.Prism`) to recover both the "build" and
  "match" halves it packages.

  **Scope note (`below`).** Upstream's `below :: Traversable f => APrism' s a
  -> Prism' (f s) (f a)` needs `Traversable f`'s `traverse` run against
  `Either s`'s `Applicative` instance (Haskell's short-circuiting-on-`Left`
  applicative for `Either e`) to detect whether *every* element of the
  container matched. `linen` has ported no `Applicative (Sum e)` instance
  anywhere (this batch is restricted to touching only
  `Iso.lean`/`Prism.lean`/`Review.lean` and the two import lists, so one
  cannot be added to `Sum`'s home module here either). `below` is therefore
  skipped, following this codebase's standing precedent of skipping a
  combinator that needs a class instance genuinely out of scope for the
  file at hand. -/

import Linen.Control.Lens.Iso
import Linen.Control.Lens.Internal.Prism
import Linen.Control.Profunctor.Choice
import Linen.Control.Profunctor.Rep

open Control Control.Profunctor Control.Lens.Internal

namespace Control.Lens

-- ── Prism ───────────────────────────────────────

/-- `Prism s t a b := ∀ p f, (Choice p, Applicative f) => p a (f b) -> p s (f
    t)`: a `Traversal` that focuses on at most one `a`, and can additionally
    build a `t` from a bare `b` (see `Linen.Control.Lens.Review`'s `review`/
    `(#)`) without needing an `s` to start from. -/
abbrev Prism (S T A B : Type u) :=
  ∀ {P : Type u → Type u → Type u} [Choice P] {F : Type u → Type u} [Applicative F],
    Optic P F S T A B

/-- `Prism' s a := Prism s s a a`. -/
abbrev Prism' (S A : Type u) := Prism S S A A

-- ── isoAsPrism ──────────────────────────────────

/-- Every `Iso` is (trivially) a `Prism` — an `Iso`'s `∀ p f, (Profunctor p,
    Functor f)` is weaker than a `Prism`'s `∀ p f, (Choice p, Applicative
    f)`, and `Choice`/`Applicative` already extend `Profunctor`/`Functor`, so
    any `Iso` value type-checks directly at the stronger constraint with no
    further work. Upstream gets this subtyping for free from `Iso`/`Prism`
    both being rank-2-polymorphic type *synonyms* (any term typeable at the
    weaker constraint is automatically well-typed at the stronger one, no
    coercion needed); Lean's `Iso`/`Prism` are `abbrev`s over genuinely
    different `∀`-quantified Pi-types, so an explicit (still zero-cost)
    wrapper is needed to witness the same fact. -/
@[inline] def isoAsPrism {S T A B : Type u} (l : Iso S T A B) : Prism S T A B :=
  fun {P} [Choice P] {F} [Applicative F] p => l p

-- ── prism / prism' ──────────────────────────────

/-- `prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b`: build a
    `Prism` out of a "build" function and a "match" function that either
    recognizes an `s` as an `a` (`.inr`) or gives up with an already-rebuilt
    `t` (`.inl`) — `prism bt seta = dimap seta (either pure (fmap bt)) .
    right'`. -/
@[inline] def prism {S T A B : Type u} (bt : B → T) (seta : S → T ⊕ A) : Prism S T A B :=
  fun {P} [Choice P] {F} [Applicative F] p =>
    Profunctor.dimap seta (fun x => x.elim (fun t => (pure t : F T)) (fun fb => bt <$> fb))
      (Choice.right' p)

/-- `prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b`: `prism`
    specialized to the common case where the container's type doesn't
    change, with the "match" function returning an `Option` rather than an
    `Either` — `prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma
    s))`. -/
@[inline] def prism' {S A B : Type u} (bs : B → S) (sma : S → Option A) : Prism S S A B :=
  prism bs (fun s => match sma s with
    | none => .inl s
    | some a => .inr a)

-- ── withPrism / clonePrism ──────────────────────

/-- `withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) ->
    r`: run a `Prism` at the concrete profunctor `Market a b`, recovering
    both the "build" and "match" halves it packages — `withPrism ai k = case
    ai (Market Identity Right) of Market bt seta -> k bt seta`, simplified
    here since Lean's `Id` needs no `runIdentity`/`coerce` detour to strip
    (see `Iso.withIso`'s identical simplification). -/
@[inline] def withPrism {S T A B R : Type u} (l : Prism S T A B)
    (k : (B → T) → (S → T ⊕ A) → R) : R :=
  let m : Market A B S T := l (P := Market A B) (F := Id) ⟨id, Sum.inr⟩
  k m.bt m.seta

/-- `clonePrism :: APrism s t a b -> Prism s t a b`: rebuild a fresh, fully
    polymorphic `Prism` out of one already run at a concrete profunctor —
    `clonePrism k = withPrism k prism`. -/
@[inline] def clonePrism {S T A B : Type u} (l : Prism S T A B) : Prism S T A B :=
  withPrism l (fun bt seta => prism bt seta)

-- ── _Left / _Right / _Just / _Nothing ───────────

/-- `_Left :: Prism (Either a c) (Either b c) a b`: focus on the `Left` case
    of a `Sum` — `_Left = prism Left $ either Right (Left . Right)`. -/
@[inline] def _Left {A B C : Type u} : Prism (A ⊕ C) (B ⊕ C) A B :=
  prism Sum.inl (fun s => match s with
    | .inl a => .inr a
    | .inr c => .inl (.inr c))

/-- `_Right :: Prism (Either c a) (Either c b) a b`: focus on the `Right`
    case of a `Sum` — `_Right = prism Right $ either (Left . Left) Right`. -/
@[inline] def _Right {A B C : Type u} : Prism (C ⊕ A) (C ⊕ B) A B :=
  prism Sum.inr (fun s => match s with
    | .inl c => .inl (.inl c)
    | .inr a => .inr a)

/-- `_Just :: Prism (Maybe a) (Maybe b) a b`: focus on the `Just` case of an
    `Option` — `_Just = prism Just $ maybe (Left Nothing) Right`. -/
@[inline] def _Just {A B : Type u} : Prism (Option A) (Option B) A B :=
  prism Option.some (fun s => match s with
    | none => .inl none
    | some a => .inr a)

/-- `_Nothing :: Prism' (Maybe a) ()`: focus on the `Nothing` case of an
    `Option` — `_Nothing = prism' (const Nothing) $ maybe (Just ()) (const
    Nothing)`.

    Fixed at a concrete `Type` (rather than the ambient `Type u` used
    elsewhere in this module), since `Unit` itself is a concrete `Type 0`
    type and `Prism'` requires all four of its indices to share one universe
    — the same accommodation `Linen.Control.Lens.Lens`'s `united` already
    makes, for the same reason. -/
@[inline] def _Nothing {A : Type} : Prism' (Option A) Unit :=
  prism' (fun _ => none) (fun s => match s with
    | none => some ()
    | some _ => none)

-- ── only / nearly ───────────────────────────────

/-- `only :: Eq a => a -> Prism' a ()`: a `Prism'` that matches exactly one
    value — `only a = prism' (\() -> a) $ guard . (a ==)`. Fixed at a
    concrete `Type`; see `_Nothing`'s doc comment for why. -/
@[inline] def only {A : Type} [DecidableEq A] (a : A) : Prism' A Unit :=
  prism' (fun _ => a) (fun x => if x = a then some () else none)

/-- `nearly :: a -> (a -> Bool) -> Prism' a ()`: like `only`, but matches
    every value satisfying an arbitrary predicate rather than exact equality
    — `nearly a p = prism' (\() -> a) $ guard . p`. Fixed at a concrete
    `Type`; see `_Nothing`'s doc comment for why. -/
@[inline] def nearly {A : Type} (a : A) (p : A → Bool) : Prism' A Unit :=
  prism' (fun _ => a) (fun x => if p x then some () else none)

-- ── outside ─────────────────────────────────────

/-- `outside :: Representable p => APrism s t a b -> Lens (p t r) (p s r) (p b
    r) (p a r)`: turn a `Prism` "inside-out" against any `Representable`
    profunctor `p`, producing a `Lens` on functions (or function-like
    values) *out of* `s`/`t`, indexed by whether the `Prism` matched. -/
@[inline] def outside {S T A B R : Type u} {P : Type u → Type u → Type u} {Rep : Type u → Type u}
    [Functor Rep] [Strong P] [Sieve P Rep] [Representable P Rep]
    (l : Prism S T A B) : Lens (P T R) (P S R) (P B R) (P A R) :=
  withPrism l (fun bt seta =>
    show Lens (P T R) (P S R) (P B R) (P A R) from
      fun {G : Type u → Type u} [Functor G] (f : P B R → G (P A R)) (ft : P T R) =>
        (fun pa => Representable.tabulate (fun s => (seta s).elim (Sieve.sieve ft) (Sieve.sieve pa)))
          <$> f (Profunctor.lmap bt ft))

end Control.Lens
