/-
  Linen.Control.Lens.Reified — `ReifiedLens`, `ReifiedLens'`, `ReifiedIso`,
  `ReifiedIso'`, `ReifiedGetter`, `ReifiedFold`, `ReifiedSetter`,
  `ReifiedSetter'`, `ReifiedTraversal`, `ReifiedTraversal'`,
  `ReifiedPrism`, `ReifiedPrism'`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Reified` (fetched and read
  via the real source, `src/Control/Lens/Reified.hs`, not recalled from
  memory). Every optic in this library (`Lens`, `Iso`, `Getter`, …) is,
  underneath, a rank-2-polymorphic function — `∀ f, C f => (a -> f b) -> s
  -> f t` for some constraint `C`. Such a function cannot be stored
  directly as a field of an ordinary container (a `List (Lens S T A B)`,
  say): both Haskell and Lean's own record/container types are, by
  default, insufficiently polymorphic to hold a value whose *own* type is
  itself universally quantified over an arbitrary functor. This module's
  entire purpose, upstream and here, is to fix that with one `newtype`
  (here, `structure`) wrapper per optic shape — "reifying" the
  rank-2-polymorphic function into an ordinary piece of data that can be
  freely stored, passed around, and pattern-matched on.

  **Scope (this port's coverage).** Only the shapes with a call site
  requested for this batch are ported: `ReifiedLens`/`ReifiedLens'`,
  `ReifiedIso`/`ReifiedIso'`, `ReifiedGetter`, `ReifiedFold`,
  `ReifiedSetter`/`ReifiedSetter'`, `ReifiedTraversal`/`ReifiedTraversal'`,
  and `ReifiedPrism`/`ReifiedPrism'`.

  **Scope note (`ReifiedAction`/`ReifiedMonadicFold`).** Upstream's real
  source, as read from `src/Control/Lens/Reified.hs`, does not actually
  define these two — they do not exist even in upstream lens-5.3.6 (an
  `Action`/monadic-fold optic family was removed from `lens` well before
  this version). Confirmed here by also searching this codebase (`grep -r
  Action Linen/Control/Lens/`) for any already-ported `Action`/monadic-fold
  optic to reify: there is none. Both are therefore skipped, for the same
  reason cited by whoever scoped this batch: no upstream type, and no
  `linen` type, to reify.

  **Scope note (`ReifiedReview`, `ReifiedIndexed*`).** Upstream also
  defines `ReifiedReview` and the indexed variants
  `ReifiedIndexedLens`/`ReifiedIndexedTraversal`/`ReifiedIndexedGetter`/
  `ReifiedIndexedFold`/`ReifiedIndexedSetter`. None of these were requested
  for this batch (see the scope list above) and none has a call site here;
  they are left for whichever later batch needs to store a `Review` or an
  indexed optic in a container.

  **Scope note (instances).** Upstream gives `ReifiedGetter`/`ReifiedFold`
  a wide range of instances beyond `Functor`/`Applicative`/`Monad`:
  `Distributive`, `Comonad`/`ComonadApply`/`Extend`, `Apply`/`Bind`
  (`semigroupoids`), `MonadReader`, `Alternative`/`MonadPlus`/`Alt`/`Plus`,
  `Semigroup`/`Monoid`, and a whole family of `Profunctor`/`Category`/
  `Arrow`/`ArrowChoice`/`ArrowApply`/`ArrowLoop`/`Choice`/`Strong`/`Closed`/
  `Cosieve`/`Corepresentable`/`Sieve`/`Representable`/`Costrong`/
  `Conjoined` instances. `linen` has ported none of `Distributive`,
  `Comonad`, `Apply`/`Bind` (`semigroupoids`), an mtl-style `MonadReader`
  class, `Alternative`/`MonadPlus`/`Alt`/`Plus`, a bare `Semigroup`/
  `Monoid` class pair (this codebase's own convention, e.g. `Focusing`'s
  `Pure`/`Seq` instances in `Linen.Control.Lens.Internal.Zoom`, is
  `[Append α] [Inhabited α]` in place of `Monoid`), `Category`/`Arrow*`, or
  any but the one already-`Fun`-wrapped `Profunctor` instance
  (`Linen.Control.Lens.Getter`'s `to`/`like` doc comments explain why
  `linen`'s function-arrow `Profunctor` instance lives on the nominal
  wrapper `Control.Fun`, not bare `→`, so `ReifiedGetter`/`ReifiedFold`
  themselves — genuinely new nominal types, not bare `→` — could not reuse
  it even if it were ported at the right shape). Only `Functor`/
  `Applicative`/`Monad` (both types) and the `[Append][Inhabited]`
  monoid-substitute (`ReifiedFold` only, standing in for `Alternative`/
  `Semigroup`/`Monoid`) are ported; every other optic below (`ReifiedLens`,
  `ReifiedIso`, `ReifiedSetter`, `ReifiedTraversal`, `ReifiedPrism`) gets
  none at all, matching upstream's own choice not to give any of *them*
  instances either — only `ReifiedGetter`/`ReifiedFold`/`ReifiedReview`
  (the last out of scope, see above) get any upstream. -/

import Linen.Control.Lens.Fold
import Linen.Control.Lens.Iso
import Linen.Control.Lens.Lens
import Linen.Control.Lens.Prism

open Control.Lens.Internal Data.Functor

namespace Control.Lens

-- ── ReifiedLens ─────────────────────────────────

/-- Reify a `Lens` so it can be stored safely in a container. -/
structure ReifiedLens (S T A B : Type u) where
  /-- Recover the underlying `Lens`. -/
  runLens : Lens S T A B

/-- `ReifiedLens' s a := ReifiedLens s s a a`. -/
abbrev ReifiedLens' (S A : Type u) := ReifiedLens S S A A

-- ── ReifiedIso ──────────────────────────────────

/-- Reify an `Iso` so it can be stored safely in a container. -/
structure ReifiedIso (S T A B : Type u) where
  /-- Recover the underlying `Iso`. -/
  runIso : Iso S T A B

/-- `ReifiedIso' s a := ReifiedIso s s a a`. -/
abbrev ReifiedIso' (S A : Type u) := ReifiedIso S S A A

-- ── ReifiedGetter ───────────────────────────────

/-- Reify a `Getter` so it can be stored safely in a container.

    Also useful for combining getters in novel ways, since (as the
    instances below witness) `ReifiedGetter s` is, like upstream's version,
    isomorphic to a `Reader s` and provides similar `Functor`/`Applicative`/
    `Monad` instances. -/
structure ReifiedGetter (S A : Type u) where
  /-- Recover the underlying `Getter`. -/
  runGetter : Getter S A

/-- `ReifiedGetter S` is a `Monad`: `pure a` is the `Getter` that ignores
    its input and always focuses on `a` (`like a`); `bind` runs the first
    `Getter` to obtain an `a`, feeds it to `f`, and runs the resulting
    `Getter` against the very same input — `ma >>= f = to (\s -> view
    (f (view ma s)).runGetter s)`. Deriving `Functor`/`Applicative` from
    this (rather than porting upstream's separate `Apply`/`Applicative`
    instances) matches how every other `Monad` instance in this codebase's
    ported modules is given. -/
instance {S : Type u} : Monad (ReifiedGetter S) where
  pure a := ⟨like a⟩
  bind ma f := ⟨to (fun s => view (f (view ma.runGetter s)).runGetter s)⟩

-- ── ReifiedFold ─────────────────────────────────

/-- Reify a `Fold` so it can be stored safely in a container.

    Also useful for combining folds in novel ways, since (as the instances
    below witness) `ReifiedFold s` is, like upstream's version, isomorphic
    to `ReaderT s List` and provides similar `Functor`/`Applicative`/
    `Monad` instances, plus an `Append`/`Inhabited` pair standing in for
    upstream's `Alternative`/`Semigroup`/`Monoid` (see the module's scope
    note on instances). -/
structure ReifiedFold (S A : Type u) where
  /-- Recover the underlying `Fold`. -/
  runFold : Fold S A

/-- `ReifiedFold S` is a `Monad`: `pure a` is the `Fold` that ignores its
    input and always yields the single value `a`; `bind` collects every `a`
    the first `Fold` focuses on, runs `f` against each, and concatenates
    every resulting `Fold`'s own targets — `ma >>= f = folding (\s ->
    toListOf ma s >>= \a -> toListOf (f a).runFold s)`. -/
instance {S : Type u} : Monad (ReifiedFold S) where
  pure a := ⟨folding (fun (_ : S) => ([a] : List _))⟩
  bind ma f := ⟨folding (fun s => (toListOf ma.runFold s).flatMap (fun a => toListOf (f a).runFold s))⟩

/-- `ReifiedFold S A` is `Append`: the `Fold` that concatenates both sides'
    targets — `ma ++ mb = folding (\s -> toListOf ma s ++ toListOf mb s)`,
    standing in for upstream's `Alternative`/`Semigroup` instance (`(<>) =
    (<|>)`), per the module's scope note on instances. -/
instance {S A : Type u} : Append (ReifiedFold S A) where
  append ma mb := ⟨folding (fun s => toListOf ma.runFold s ++ toListOf mb.runFold s)⟩

/-- `ReifiedFold S A` is `Inhabited`: the `Fold` that never yields any
    target — `default = folding (\_ -> [])`, standing in for upstream's
    `Alternative`/`Monoid` instance (`empty = mempty = Fold ignored`), per
    the module's scope note on instances. -/
instance {S A : Type u} : Inhabited (ReifiedFold S A) where
  default := ⟨folding (fun (_ : S) => ([] : List A))⟩

-- ── ReifiedSetter ───────────────────────────────

/-- Reify a `Setter` so it can be stored safely in a container. -/
structure ReifiedSetter (S T A B : Type u) where
  /-- Recover the underlying `Setter`. -/
  runSetter : Setter S T A B

/-- `ReifiedSetter' s a := ReifiedSetter s s a a`. -/
abbrev ReifiedSetter' (S A : Type u) := ReifiedSetter S S A A

-- ── ReifiedTraversal ────────────────────────────

/-- Reify a `Traversal` so it can be stored safely in a container. -/
structure ReifiedTraversal (S T A B : Type u) where
  /-- Recover the underlying `Traversal`. -/
  runTraversal : Traversal S T A B

/-- `ReifiedTraversal' s a := ReifiedTraversal s s a a`. -/
abbrev ReifiedTraversal' (S A : Type u) := ReifiedTraversal S S A A

-- ── ReifiedPrism ────────────────────────────────

/-- Reify a `Prism` so it can be stored safely in a container. -/
structure ReifiedPrism (S T A B : Type u) where
  /-- Recover the underlying `Prism`. -/
  runPrism : Prism S T A B

/-- `ReifiedPrism' s a := ReifiedPrism s s a a`. -/
abbrev ReifiedPrism' (S A : Type u) := ReifiedPrism S S A A

end Control.Lens
