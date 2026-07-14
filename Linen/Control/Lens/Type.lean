/-
  Linen.Control.Lens.Type — the optic type-alias family

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Type` (fetched and read via
  Hackage's rendered Haddock). Upstream's own description: "This module
  exports the majority of the types that need to appear in user signatures."
  Every optic in the `lens` library is, underneath, a single higher-order
  function type: a *van Laarhoven encoding* `∀ f, C f => (a -> f b) -> s -> f
  t` for some constraint `C` (`Functor` for `Lens`, `Applicative` for
  `Traversal`, `Settable` for `Setter`, …), optionally generalized over a
  profunctor `p` in place of the bare function arrow for the *indexed*
  variants. This module is nothing but that family of type aliases.

  Lean has no direct counterpart to a rank-2-polymorphic `type` alias with an
  implicit class constraint (`type Lens s t a b = forall f. Functor f => (a
  -> f b) -> s -> f t`): each such alias is translated as an `abbrev` whose
  body is itself a `∀ {F} [C F], …` Pi-type — an `abbrev` (rather than `def`)
  so that, exactly as in Haskell, `Lens' s a` unfolds transparently to `Lens s
  s a a` wherever a caller expects one or the other.

  **Scope note (`Traversal1`/`Traversal1'`/`IndexedTraversal1`).** Upstream's
  real signatures use `Apply f` (from `semigroupoids`) rather than
  `Applicative f`, i.e. an applicative-without-`pure`. `linen` has ported no
  `Apply` class at all (nothing in this batch's scope needs one), so these
  three aliases — whose only difference from `Traversal`/`IndexedTraversal`
  is that weaker constraint — are dropped rather than manufacturing an
  `Apply` class with no other call site.

  **Scope note (`Iso`/`Prism`/`Review`/`AReview`/`Fold`/`Fold1`/
  `IndexPreserving*`).** Upstream's `Control.Lens.Type` also defines these,
  but every one of them is out of scope for this batch: `Iso`/`Prism`/
  `Review`/`AReview` belong to `Control.Lens.{Iso,Prism,Review}` (not yet
  ported at the public-API level), `Fold`/`Fold1` belong to `Control.Lens.
  Fold` (likewise), and the `IndexPreserving*` family exists only to support
  combinators from those same not-yet-ported modules. They are left for
  whichever later batch ports those modules. -/

import Linen.Control.Lens.Internal.Indexed
import Linen.Control.Lens.Internal.Setter
import Linen.Data.Functor

open Control Control.Profunctor Control.Lens.Internal Data.Functor

namespace Control.Lens

-- ── LensLike / Over / Optical / Optic ──────────

/-- `LensLike f s t a b := (a -> f b) -> s -> f t`: the bare van Laarhoven
    shape shared by every un-indexed optic, before any constraint is placed
    on `f`. -/
abbrev LensLike (F : Type u → Type u) (S T A B : Type u) := (A → F B) → S → F T

/-- `LensLike' f s a := LensLike f s s a a`. -/
abbrev LensLike' (F : Type u → Type u) (S A : Type u) := LensLike F S S A A

/-- `Over p f s t a b := p a (f b) -> s -> f t`: `LensLike`, generalized to
    accept a profunctor `p` in place of a bare function on the left. -/
abbrev Over (P : Type u → Type u → Type v) (F : Type u → Type u) (S T A B : Type u) :=
  P A (F B) → S → F T

/-- `Over' p f s a := Over p f s s a a`. -/
abbrev Over' (P : Type u → Type u → Type v) (F : Type u → Type u) (S A : Type u) :=
  Over P F S S A A

/-- `Optical p q f s t a b := p a (f b) -> q s (f t)`: like `Over`, but the
    result is also expressed in a (possibly different) profunctor `q` rather
    than a bare function. -/
abbrev Optical (P Q : Type u → Type u → Type v) (F : Type u → Type u) (S T A B : Type u) :=
  P A (F B) → Q S (F T)

/-- `Optical' p q f s a := Optical p q f s s a a`. -/
abbrev Optical' (P Q : Type u → Type u → Type v) (F : Type u → Type u) (S A : Type u) :=
  Optical P Q F S S A A

/-- `Optic p f s t a b := p a (f b) -> p s (f t)`: `Optical` specialized to a
    single profunctor `p` on both sides. -/
abbrev Optic (P : Type u → Type u → Type v) (F : Type u → Type u) (S T A B : Type u) :=
  P A (F B) → P S (F T)

/-- `Optic' p f s a := Optic p f s s a a`. -/
abbrev Optic' (P : Type u → Type u → Type v) (F : Type u → Type u) (S A : Type u) :=
  Optic P F S S A A

-- ── IndexedLensLike ─────────────────────────────

/-- `IndexedLensLike i f s t a b := ∀ p, Indexable i p => p a (f b) -> s -> f
    t`: the bare shape shared by every *indexed* optic — polymorphic in the
    profunctor `p`, constrained only to be `Indexable` at index `i`. -/
abbrev IndexedLensLike (I : Type u) (F : Type u → Type u) (S T A B : Type u) :=
  ∀ {P : Type u → Type u → Type u} [Indexable I P], P A (F B) → S → F T

/-- `IndexedLensLike' i f s a := IndexedLensLike i f s s a a`. -/
abbrev IndexedLensLike' (I : Type u) (F : Type u → Type u) (S A : Type u) :=
  IndexedLensLike I F S S A A

-- ── Lens ────────────────────────────────────────

/-- `Lens s t a b := ∀ f, Functor f => (a -> f b) -> s -> f t`: a first-class,
    composable getter/setter pair. Given a value of type `s`, a `Lens s t a
    b` can extract an `a` and, given a replacement `b`, rebuild a `t`. -/
abbrev Lens (S T A B : Type u) := ∀ {F : Type u → Type u} [Functor F], LensLike F S T A B

/-- `Lens' s a := Lens s s a a`: the common case where getting and setting
    don't change the container's or the focus's type. -/
abbrev Lens' (S A : Type u) := Lens S S A A

-- ── Traversal ───────────────────────────────────

/-- `Traversal s t a b := ∀ f, Applicative f => (a -> f b) -> s -> f t`: like
    a `Lens`, but may focus on zero, one, or many elements of `s` at once. -/
abbrev Traversal (S T A B : Type u) := ∀ {F : Type u → Type u} [Applicative F], LensLike F S T A B

/-- `Traversal' s a := Traversal s s a a`. -/
abbrev Traversal' (S A : Type u) := Traversal S S A A

-- ── Setter ──────────────────────────────────────

/-- `Setter s t a b := ∀ f, Settable f => (a -> f b) -> s -> f t`: a
    write-only `Traversal` — it can rebuild `t` from a rule for producing
    each `b` from each `a`, but (being restricted to `Settable` functors) it
    can never observe or accumulate the `a`s it visits. -/
abbrev Setter (S T A B : Type u) := ∀ {F : Type u → Type u} [Settable F], LensLike F S T A B

/-- `Setter' s a := Setter s s a a`. -/
abbrev Setter' (S A : Type u) := Setter S S A A

/-- `IndexedSetter i s t a b`: `Setter`, generalized over an `Indexable i p`
    profunctor so that the index is available at every write. -/
abbrev IndexedSetter (I S T A B : Type u) :=
  ∀ {F : Type u → Type u} [Settable F], IndexedLensLike I F S T A B

/-- `IndexedSetter' i s a := IndexedSetter i s s a a`. -/
abbrev IndexedSetter' (I S A : Type u) := IndexedSetter I S S A A

-- ── IndexedGetter / IndexedFold ─────────────────

/-- `IndexedGetter i s a := ∀ p f, (Indexable i p, Contravariant f, Functor
    f) => p a (f a) -> s -> f s`: a read-only indexed optic focused on
    exactly one `a`. -/
abbrev IndexedGetter (I S A : Type u) :=
  ∀ {F : Type u → Type u} [Contravariant F] [Functor F], IndexedLensLike' I F S A

/-- `IndexedFold i s a`: like `IndexedGetter`, but may focus on any number of
    `a`s (requires only `Applicative`, not `Functor`, alongside
    `Contravariant`). -/
abbrev IndexedFold (I S A : Type u) :=
  ∀ {F : Type u → Type u} [Contravariant F] [Applicative F], IndexedLensLike' I F S A

-- ── IndexedLens / IndexedTraversal ──────────────

/-- `IndexedLens i s t a b`: `Lens`, generalized over an `Indexable i p`
    profunctor so the index is available at every read/write. -/
abbrev IndexedLens (I S T A B : Type u) :=
  ∀ {F : Type u → Type u} [Functor F], IndexedLensLike I F S T A B

/-- `IndexedLens' i s a := IndexedLens i s s a a`. -/
abbrev IndexedLens' (I S A : Type u) := IndexedLens I S S A A

/-- `IndexedTraversal i s t a b`: `Traversal`, generalized over an
    `Indexable i p` profunctor so the index is available at every visited
    element. -/
abbrev IndexedTraversal (I S T A B : Type u) :=
  ∀ {F : Type u → Type u} [Applicative F], IndexedLensLike I F S T A B

/-- `IndexedTraversal' i s a := IndexedTraversal i s s a a`. -/
abbrev IndexedTraversal' (I S A : Type u) := IndexedTraversal I S S A A

end Control.Lens
