/-
  Linen.Control.Profunctor.Cayley — `Cayley` and the folded-in `Comonad`

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Cayley` (module
  #12 of `docs/imports/profunctors/dependencies.md`). `Cayley f p a b := f
  (p a b)` lifts a profunctor `p` pointwise through an outer `Functor`/
  `Applicative`/`Monad` `f` — "static arrows".

  Per the plan, `Comonad` is folded directly into this module (as
  `Distributive` was folded into `Linen.Control.Profunctor.Rep`) rather than
  imported as a separate Hackage package, since its only use in this port is
  the `ProfunctorComonad (Cayley f)` instance below.

  **Scope note.** Upstream also gives `Category`/`Arrow`/`ArrowChoice`/
  `ArrowLoop`/`ArrowZero`/`ArrowPlus` instances for `Cayley f p` (lifting
  each class pointwise through an `Applicative f`). `lens` itself only
  reaches into `Cayley`'s `Profunctor`/`Strong`/`Choice`/`Closed`/
  `Traversing` structure and the `ProfunctorFunctor`/`ProfunctorMonad`/
  `ProfunctorComonad` instances, so the arrow-class instances are left
  unported, matching the scope trims already made for those same classes
  elsewhere in this package (e.g. `Linen.Control.Profunctor.Types` only
  gives `WrappedArrow` a `Profunctor`, not a full `Arrow`, instance).
  `Mapping`'s instance is likewise omitted, since
  `Linen.Control.Profunctor.Mapping` does not port a general `Distributive`-
  based `Mapping` instance to derive it from (see that module's scope
  note); `Cochoice`'s instance is included since `Cochoice`'s only cost is
  `Functor f`.
-/

import Linen.Control.Profunctor.Choice
import Linen.Control.Profunctor.Closed
import Linen.Control.Profunctor.Monad
import Linen.Control.Profunctor.Traversing

open Control

namespace Control.Profunctor

/-- A **comonad**: the dual of `Monad`, folded into this module since its
    only use here is transporting `Comonad`s on `Hask` to comonads on
    `Prof` via `Cayley`.

    Laws:
    $$\text{extend}\;\text{extract} = \text{id}$$
    $$\text{extract} \circ \text{extend}\;f = f$$
    $$\text{extend}\;f \circ \text{extend}\;g = \text{extend}\;(f \circ \text{extend}\;g)$$ -/
class Comonad (W : Type v → Type v) extends Functor W where
  extract : W α → α
  extend : (W α → β) → W α → W β

/-- `Cayley f p a b := f (p a b)`: `p` lifted pointwise through `f`. Note `f`
    is a functor on `Type v` (the profunctors' own universe), not `Type u`
    (the domain/codomain objects' universe): it wraps whole `P A B` values,
    which live in `Type v`. -/
structure Cayley (F : Type v → Type v) (P : Type u → Type u → Type v) (A B : Type u) where
  runCayley : F (P A B)

instance [Functor F] : ProfunctorFunctor (Cayley F) where
  promap f := fun c => ⟨f <$> c.runCayley⟩

instance [Monad F] : ProfunctorMonad (Cayley F) where
  proreturn p := ⟨pure p⟩
  projoin c := ⟨c.runCayley >>= Cayley.runCayley⟩

instance [Comonad F] : ProfunctorComonad (Cayley F) where
  proextract c := Comonad.extract c.runCayley
  produplicate c := ⟨Comonad.extend (fun w => ⟨w⟩) c.runCayley⟩

instance [Functor F] [Profunctor P] : Profunctor (Cayley F P) where
  dimap l r c := ⟨Profunctor.dimap l r <$> c.runCayley⟩
  lmap l c := ⟨Profunctor.lmap l <$> c.runCayley⟩
  rmap r c := ⟨Profunctor.rmap r <$> c.runCayley⟩

instance [Functor F] [Strong P] : Strong (Cayley F P) where
  first' c := ⟨Strong.first' <$> c.runCayley⟩
  second' c := ⟨Strong.second' <$> c.runCayley⟩

instance [Functor F] [Costrong P] : Costrong (Cayley F P) where
  unfirst c := ⟨Costrong.unfirst <$> c.runCayley⟩
  unsecond c := ⟨Costrong.unsecond <$> c.runCayley⟩

instance [Functor F] [Choice P] : Choice (Cayley F P) where
  left' c := ⟨Choice.left' <$> c.runCayley⟩
  right' c := ⟨Choice.right' <$> c.runCayley⟩

instance [Functor F] [Cochoice P] : Cochoice (Cayley F P) where
  unleft c := ⟨Cochoice.unleft <$> c.runCayley⟩
  unright c := ⟨Cochoice.unright <$> c.runCayley⟩

instance [Functor F] [Closed P] : Closed (Cayley F P) where
  closed c := ⟨Closed.closed <$> c.runCayley⟩

instance [Functor F] [Traversing P] : Traversing (Cayley F P) where
  wander f c := ⟨Traversing.wander f <$> c.runCayley⟩
  traverse' c := ⟨Traversing.traverse' <$> c.runCayley⟩

/-- Change the outer functor of a `Cayley`, given a natural transformation
    between the two outer functors. -/
def mapCayley (f : ∀ {α}, F α → G α) (c : Cayley F P α β) : Cayley G P α β :=
  ⟨f c.runCayley⟩

end Control.Profunctor
