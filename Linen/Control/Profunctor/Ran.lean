/-
  Linen.Control.Profunctor.Ran — `Ran` and `Codensity`

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Ran` (module #14
  of `docs/imports/profunctors/dependencies.md`). `Ran p q a b` is the right
  Kan extension of `q` along `p` in the bicategory of profunctors — the
  "opposite" construction to `Linen.Control.Profunctor.Composition`'s
  `Rift`. `Codensity p` is the right Kan extension of `p` along itself, the
  profunctor generalization of the "difference list" trick.

  **Scope note.** Upstream's `precomposeRan` (an `Iso` between `Procompose q
  (Ran p (->))` and `Ran p q`, needed only by `lens`'s own `Iso` machinery)
  is left unported for the same reason as `Linen.Control.Profunctor.Composition`'s
  `idl`/`idr`/`assoc`.
-/

import Linen.Control.Category
import Linen.Control.Profunctor.Composition
import Linen.Control.Profunctor.Monad

open Control

namespace Control.Profunctor

-- ── Ran ────────────────────────────────────────

/-- `Ran p q a b` is the right Kan extension of `q` along `p`:
    $\forall x.\; p\,x\,a \to q\,x\,b$. -/
structure Ran (P Q : Type u → Type u → Type v) (A B : Type u) where
  runRan : ∀ {X : Type u}, P X A → Q X B

instance [Profunctor P] [Profunctor Q] : Profunctor (Ran P Q) where
  dimap ca bd f := ⟨fun p => Profunctor.rmap bd (f.runRan (Profunctor.rmap ca p))⟩
  lmap ca f := ⟨fun p => f.runRan (Profunctor.rmap ca p)⟩
  rmap bd f := ⟨fun p => Profunctor.rmap bd (f.runRan p)⟩

instance [Profunctor Q] : Functor (Ran P Q A) where
  map bd f := ⟨fun p => Profunctor.rmap bd (f.runRan p)⟩

/-- `Ran p p` forms a `Category`, isomorphic to a Haskell `Category`
    instance on `p` itself. -/
instance [Profunctor P] : Category (Ran P P) where
  id := ⟨fun p => p⟩
  comp f g := ⟨fun p => g.runRan (f.runRan p)⟩

instance : ProfunctorFunctor (Ran P) where
  promap f := fun r => ⟨fun p => f (r.runRan p)⟩

-- Note: upstream also gives `Ran p` a `ProfunctorComonad` instance
-- (`proextract f = runRan f id`, `produplicate (Ran f) = Ran (Ran . flip
-- (.) f)`). It is unportable for the same universe-stratification reason as
-- `Procompose p`'s `ProfunctorMonad` instance and `Rift p`'s
-- `ProfunctorComonad` instance in `Linen.Control.Profunctor.Composition`:
-- `Ran`'s field `runRan : ∀ {X : Type u}, ...` puts `Ran P Q a b` one
-- universe above `Type v`, so `Ran P` fits `ProfunctorFunctor` (whose result
-- universe is free) but not the self-composable `ProfunctorComonad`.

/-- The 2-morphism defining the right Kan extension: discharge a
    `Procompose (Ran q p) q` down to a plain `p`. -/
def decomposeRan [Profunctor P] (pr : Procompose (Ran Q P) Q α β) : P α β :=
  pr.outer.runRan pr.inner

/-- Curry a natural transformation out of a `Procompose` into a `Ran`. -/
def curryRan [Profunctor Q] (f : ∀ {α β}, Procompose P Q α β → R α β) (p : P α β) :
    Ran Q R α β :=
  ⟨fun q => f ⟨p, q⟩⟩

/-- Uncurry a natural transformation into a `Ran` back to one out of a
    `Procompose`. -/
def uncurryRan (f : ∀ {α β}, P α β → Ran Q R α β) (pq : Procompose P Q α β) : R α β :=
  (f pq.outer).runRan pq.inner

-- ── Codensity ──────────────────────────────────

/-- `Codensity p a b` is the right Kan extension of `p` along itself:
    $\forall x.\; p\,x\,a \to p\,x\,b$. The profunctor analogue of the
    "difference list"/codensity-monad trick. -/
structure Codensity (P : Type u → Type u → Type v) (A B : Type u) where
  runCodensity : ∀ {X : Type u}, P X A → P X B

instance [Profunctor P] : Profunctor (Codensity P) where
  dimap ca bd f := ⟨fun p => Profunctor.rmap bd (f.runCodensity (Profunctor.rmap ca p))⟩
  lmap ca f := ⟨fun p => f.runCodensity (Profunctor.rmap ca p)⟩
  rmap bd f := ⟨fun p => Profunctor.rmap bd (f.runCodensity p)⟩

instance [Profunctor P] : Functor (Codensity P A) where
  map bd f := ⟨fun p => Profunctor.rmap bd (f.runCodensity p)⟩

instance : Category (Codensity P) where
  id := ⟨fun p => p⟩
  comp f g := ⟨fun p => g.runCodensity (f.runCodensity p)⟩

/-- Discharge a self-composed `Procompose (Codensity p) p` down to `p`. -/
def decomposeCodensity (pp : Procompose (Codensity P) P α β) : P α β :=
  pp.outer.runCodensity pp.inner

end Control.Profunctor
