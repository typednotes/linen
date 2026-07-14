/-
  Linen.Control.Profunctor.Rep — `Representable`/`Corepresentable`, plus
  a folded-in `Distributive`

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Rep` (module #6 of
  `docs/imports/profunctors/dependencies.md`). `P` is `Representable` if
  there is a functor `Rep P` with `P d c ≅ d → Rep P c`; this is the
  `Distributive`-backed factoring of `Star` and backs several of the default
  `Strong` implementations `lens` relies on.

  Per the `comonad`/`distributive` note in `dependencies.md`, the bare
  `Distributive` class (`distribute`/`collect`) is folded in here directly
  rather than imported from a separate `distributive` package: this module
  is the only place in `profunctors` that needs it.
-/

import Linen.Control.Profunctor.Strong
import Linen.Control.Profunctor.Sieve

open Control

namespace Control.Profunctor

-- ── Distributive (folded in) ───────────────────

/-- A **distributive functor** $F$ lets any functor "pull through" it:
    dual to `Traversable`. Laws (definitional for the instances below):

    $$\text{distribute} : G\,(F\,α) \to F\,(G\,α) \quad \text{for any Functor } G$$ -/
class Distributive (F : Type u → Type u) extends Functor F where
  /-- Pull an arbitrary functor `G` through `F`. -/
  distribute {G : Type u → Type u} [Functor G] : G (F α) → F (G α)
  /-- `collect f = distribute ∘ fmap f`. -/
  collect {G : Type u → Type u} [Functor G] (f : α → F β) (g : G α) : F (G β) :=
    distribute (f <$> g)

/-- `Id` is (trivially) `Distributive`: there is nothing to distribute over. -/
instance : Distributive Id where
  distribute g := (id : _ → _) <$> g

-- ── Representable ──────────────────────────────

/-- `P` is **representable** if there is a functor `Rep P` with `P d c ≅ d →
    Rep P c`. Laws: `tabulate ∘ sieve ≡ id` and `sieve ∘ tabulate ≡ id`. -/
class Representable (P : Type u → Type u → Type v) (Rep : outParam (Type u → Type v))
    [Functor Rep] [Strong P] [Sieve P Rep] where
  /-- Build a representable profunctor value from a `d → Rep P c`. -/
  tabulate : (α → Rep β) → P α β

/-- Default `first'` for a `Representable` profunctor. -/
def firstRep [Strong P] [Functor Rep] [Sieve P Rep] [Representable P Rep] (p : P α β) :
    P (α × γ) (β × γ) :=
  Representable.tabulate (fun (a, c) => (fun b => (b, c)) <$> Sieve.sieve p a)

/-- Default `second'` for a `Representable` profunctor. -/
def secondRep [Strong P] [Functor Rep] [Sieve P Rep] [Representable P Rep] (p : P α β) :
    P (γ × α) (γ × β) :=
  Representable.tabulate (fun (c, a) => (fun b => (c, b)) <$> Sieve.sieve p a)

/-- Ordinary functions are `Representable` by `Id`. -/
instance : Representable Control.Fun Id where
  tabulate f := ⟨fun d => (f d : Id _)⟩

/-- `Star F` is `Representable` by `F` itself, by definition. -/
instance [Functor F] : Representable (Star F) F where
  tabulate := Star.mk

-- ── Corepresentable ────────────────────────────

/-- `P` is **corepresentable** if there is a functor `Corep P` with `P d c ≅
    Corep P d → c`. Laws: `cotabulate ∘ cosieve ≡ id` and `cosieve ∘
    cotabulate ≡ id`. -/
class Corepresentable (P : Type u → Type u → Type v) (Corep : outParam (Type u → Type v))
    [Functor Corep] [Costrong P] [Cosieve P Corep] where
  /-- Build a corepresentable profunctor value from a `Corep P d → c`. -/
  cotabulate : (Corep α → β) → P α β

-- Note: upstream also gives `(->)` a `Corepresentable Identity` instance and
-- `Costar f` a `Corepresentable f` instance, but both need a `Costrong`
-- instance (`Costrong Control.Fun` / `Costrong (Costar f)` respectively)
-- whose `unfirst`/`unsecond` (in `Data.Profunctor.Strong`) tie a knot through
-- Haskell laziness (`unfirst f a = b where (b, d) = f (a, d)`, and
-- `Costar`'s version does the same one layer down through `fmap`) that has
-- no total, strict Lean translation — see the identical note in
-- `Linen.Control.Profunctor.Strong`. Since `Corepresentable` requires
-- `[Costrong P]` as one of its own parameters, neither instance is portable
-- here, and `Corepresentable` in this port ends up with no concrete
-- instances at all (matching upstream's own admission that every concrete
-- `Corepresentable` instance relies on laziness).

end Control.Profunctor
