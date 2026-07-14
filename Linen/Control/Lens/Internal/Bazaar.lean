/-
  Linen.Control.Lens.Internal.Bazaar — `Bazaar`/`BazaarT`, what a `Traversal`
  becomes once applied to a concrete value

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Bazaar` (fetched and
  read via Hackage's rendered source: the type signatures below were pulled
  from the real source, not recalled from memory). Where `Context a b t` is
  what a van-Laarhoven `Lens s t a b` becomes when applied to a concrete `s`
  (one stored position, one "plug it back in" function — see `Linen.Control.
  Lens.Internal.Context`), `Bazaar p a b t` is the analogous thing for a
  `Traversal`: an indexed Cartesian-store comonad / indexed Kleene store
  comonad / indexed `FunList`, in upstream's own words. Concretely:

  ```
  class Profunctor p => Bizarre p w | w -> p where
    bazaar :: Applicative f => p a (f b) -> w a b t -> f t
  newtype Bazaar p a b t =
    Bazaar { runBazaar :: forall f. Applicative f => p a (f b) -> f t }
  type Bazaar' p a = Bazaar p a a
  newtype BazaarT p g a b t =
    BazaarT { runBazaarT :: forall f. Applicative f => p a (f b) -> f t }
  type BazaarT' p g a = BazaarT p g a a
  ```

  `runBazaar`/`runBazaarT` is exactly a reified `Traversal`: hand it a
  profunctor-shaped "visit one element" action `p a (f b)`, get back the
  whole-structure effect `f t`. `BazaarT` differs from `Bazaar` only by
  carrying an extra phantom type `g` that never appears in the body — upstream
  uses it purely to select a (separately-instanced) `Contravariant`
  interpretation for `taking`-style combinators; ported here as an equally
  inert phantom parameter.

  **Scope note (dropped relative to upstream).**
  - **`Bazaar1`/`BazaarT1`/`Bizarre1`** are the `Apply`-only (rather than
    `Applicative`-only) analogues, for traversals that never need an empty
    case. `linen` has no `Apply` class ported anywhere (it sits strictly
    between `Functor` and `Applicative`, and nothing else in this port
    consumes it), and this batch's scope has no caller needing anything
    weaker than `Applicative`. Manufacturing an `Apply` hierarchy with zero
    other call sites just to give `Bazaar` a redundant sibling is not a
    faithful-port concern, so the whole `1`-suffixed family is left unported.
  - **`IndexedFunctor`/`IndexedComonad`** instances: as in `Linen.Control.Lens.
    Internal.Context`'s identical scope note, these two-index classes have no
    other user anywhere in `linen` (their only real consumer,
    `cloneTraversal`-style combinators, lives in a later batch's
    `Control.Lens.Traversal`), so introducing them here would be dead
    machinery. Dropped for the same reason as in `Context.lean`.
  - **`Comonad`/`ComonadApply` instances** (upstream's `a ~ b`-constrained
    `Comonad (Bazaar p a a)`): upstream's real implementation runs the stored
    action at `p a (Identity a)`, built via `Conjoined`'s `conjoined`
    dispatch (`p ~ (->)` case) composed with `Sellable`'s `sell`. `Linen.
    Control.Lens.Internal.Indexed`'s `Conjoined` already drops `conjoined`
    itself (see that module's scope note: no faithful non-contrived Lean
    translation for its rank-2 type-equality dispatch), so there is no
    ingredient left here to build the general instance from. Dropped for the
    same underlying reason, rather than manufacturing one from a weakened
    premise.
  - **`Sellable`** (the class `sell` belongs to, keyed by a `Corepresentable`
    constraint): `linen`'s `Corepresentable` has zero concrete instances (see
    `Linen.Control.Profunctor.Rep`'s scope note), so the class itself would be
    uninhabited. As `Context.lean` already does for `Context`'s own `sell`,
    this port keeps only the concrete operation, specialized to `p =
    Control.Fun` (the one instantiation this batch's scope actually needs),
    rather than the class.
  - **`Contravariant`/`Semigroup`/`Monoid`** instances on `BazaarT`: `linen`
    has none of `Contravariant`/`Semigroup`/`Monoid` ported as classes yet,
    and nothing in this batch's scope calls into them. Dropped.
-/

import Linen.Control.Profunctor.Unsafe

open Control

namespace Control.Lens.Internal

-- ── Bizarre ────────────────────────────────────

/-- A profunctor-indexed family `w` that can be run against any `Applicative`
    action shaped like `p a (f b)`, recovering an effectful whole-structure
    result `f t` — the class `Bazaar`/`BazaarT` below are instances of.
    Upstream ties `w` to a *unique* `p` via a functional dependency
    (`w -> p`); Lean's class resolution instead just unifies `P` from the
    instance actually chosen for `W`, which has the same practical effect. -/
class Bizarre (P : Type u → Type u → Type v) (W : Type u → Type u → Type u → Type (max (u + 1) v))
    [Profunctor P] where
  /-- Run `w a b t` against a profunctor-shaped visitor, producing an
      effectful whole-structure result: $\text{bazaar} : P\,a\,(F\,b) \to
      W\,a\,b\,t \to F\,t$. -/
  bazaar {F : Type u → Type u} [Applicative F] {A B T : Type u} :
    P A (F B) → W A B T → F T

-- ── Bazaar ─────────────────────────────────────

/-- `Bazaar p a b t`: what a `Traversal s t a b` becomes once applied to a
    concrete `s` — a reified "visit every element with `p`" action, isomorphic
    to `∀ f, Applicative f → p a (f b) → f t`. -/
structure Bazaar (P : Type u → Type u → Type v) (A B T : Type u) where
  /-- Run the reified traversal against a profunctor-shaped visitor. -/
  runBazaar : {F : Type u → Type u} → [Applicative F] → P A (F B) → F T

/-- `Bazaar' p a := Bazaar p a a`: shorthand for the common case where the
    "get" and "put" element types coincide. Marked `@[reducible]` so
    typeclass search (e.g. for `Applicative (Bazaar' P A)`) sees straight
    through to the underlying `Bazaar`. -/
@[reducible] def Bazaar' (P : Type u → Type u → Type v) (A T : Type u) := Bazaar P A A T

namespace Bazaar

/-- `Bazaar P A B` is a `Functor` in its result type `T`: post-compose the
    reified effect. -/
instance : Functor (Bazaar P A B) where
  map f b := ⟨fun {_F} _ pafb => f <$> b.runBazaar pafb⟩

/-- `Bazaar P A B` is `Applicative`: `pure` reifies a leafless (no visited
    elements) traversal, and `seq` runs both sides against the same visitor
    and combines their effects. -/
instance : Applicative (Bazaar P A B) where
  pure x := ⟨fun {_F} _ _ => Pure.pure x⟩
  seq bf bx := ⟨fun {_F} _ pafb => bf.runBazaar pafb <*> (bx ()).runBazaar pafb⟩

end Bazaar

/-- `Bazaar P` is `Bizarre`: running it is exactly `runBazaar`. -/
instance [Profunctor P] : Bizarre P (Bazaar P) where
  bazaar pafb b := b.runBazaar pafb

/-- Build the trivial one-element `Bazaar`: visit a single value `a`,
    specialized to `p = Control.Fun` (upstream's `sell`, specialized to the
    one instantiation this batch's scope needs — see the module's scope note
    on why the general `Sellable` class is not ported). -/
@[inline] def Bazaar.sell (a : A) : Bazaar Control.Fun A B B :=
  ⟨fun pafb => pafb.apply a⟩

-- ── BazaarT ────────────────────────────────────

/-- `BazaarT p g a b t`: `Bazaar` with an extra phantom type `g`, unused in
    the representation (upstream keeps it only to select a separate
    `Contravariant`-flavoured interpretation for `taking`-style combinators,
    not ported here — see the module's scope note). -/
structure BazaarT (P : Type u → Type u → Type v) (G : Type u → Type u) (A B T : Type u) where
  /-- Run the reified traversal against a profunctor-shaped visitor. -/
  runBazaarT : {F : Type u → Type u} → [Applicative F] → P A (F B) → F T

/-- `BazaarT' p g a := BazaarT p g a a`: shorthand for the common case where
    the "get" and "put" element types coincide. Marked `@[reducible]` for the
    same reason as `Bazaar'`. -/
@[reducible] def BazaarT' (P : Type u → Type u → Type v) (G : Type u → Type u) (A T : Type u) :=
  BazaarT P G A A T

namespace BazaarT

/-- `BazaarT P G A B` is a `Functor` in its result type `T`. -/
instance : Functor (BazaarT P G A B) where
  map f b := ⟨fun {_F} _ pafb => f <$> b.runBazaarT pafb⟩

/-- `BazaarT P G A B` is `Applicative`, exactly as `Bazaar P A B` is. -/
instance : Applicative (BazaarT P G A B) where
  pure x := ⟨fun {_F} _ _ => Pure.pure x⟩
  seq bf bx := ⟨fun {_F} _ pafb => bf.runBazaarT pafb <*> (bx ()).runBazaarT pafb⟩

end BazaarT

/-- `BazaarT P G` is `Bizarre`: running it is exactly `runBazaarT`. -/
instance [Profunctor P] : Bizarre P (BazaarT P G) where
  bazaar pafb b := b.runBazaarT pafb

/-- Build the trivial one-element `BazaarT`, specialized to `p = Control.Fun`
    (see `Bazaar.sell` and the module's scope note). -/
@[inline] def BazaarT.sell (a : A) : BazaarT Control.Fun G A B B :=
  ⟨fun pafb => pafb.apply a⟩

end Control.Lens.Internal
