/-
  Linen.Control.Profunctor.Types — `Star`, `Costar`, `WrappedArrow`, `Forget`,
  `Tagged`

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Types` (module #2
  of `docs/imports/profunctors/dependencies.md`). Ports the five concrete
  profunctors upstream builds the rest of the package on top of, each with
  its `Profunctor` instance. `WrappedArrow` needs `Control.Arrow`
  (`Linen.Control.Arrow`) to lift an arrow into a profunctor.

  Per the Hackage-import convention's `tagged` note in `dependencies.md`,
  `Tagged` (upstream's `Data.Tagged`, a one-field phantom-typed wrapper) is
  folded in here rather than ported as its own module: `structure Tagged (s :
  Type u) (β : Type u) where unTagged : β`.

  Upstream also gives `Star`/`Costar`/`Forget` `Functor`/`Applicative`/
  `Monad`/`Distributive`/`Category`/`Contravariant` instances and `Forget` a
  `Semigroup`/`Monoid` instance; those are orthogonal to the `Profunctor`
  instances this module (and the classes built on it) actually need, so only
  the `Profunctor` instances are ported here, matching this file's stated
  scope in the dependency plan ("with their `Profunctor` instances").
-/

import Linen.Control.Profunctor.Unsafe
import Linen.Control.Arrow

open Control

namespace Control.Profunctor

-- ── Star ───────────────────────────────────────

/-- `Star F D C` lifts a functor `F` into a profunctor: $\text{Star}\,F\,d\,c
    \cong d \to F\,c$. -/
structure Star (F : Type u → Type v) (D C : Type u) where
  /-- Run the underlying `D → F C`. -/
  runStar : D → F C

/-- `Star F` is a `Profunctor` whenever `F` is a `Functor`:
    $\text{dimap}\;f\;g\;h = \text{fmap}\;g \circ h \circ f$. -/
instance [Functor F] : Profunctor (Star F) where
  dimap f g h := ⟨fun d => g <$> h.runStar (f d)⟩
  lmap f h := ⟨h.runStar ∘ f⟩
  rmap g h := ⟨fun d => g <$> h.runStar d⟩

-- ── Costar ─────────────────────────────────────

/-- `Costar F D C` lifts a functor `F` into a profunctor "backwards":
    $\text{Costar}\,F\,d\,c \cong F\,d \to c$. -/
structure Costar (F : Type u → Type v) (D C : Type u) where
  /-- Run the underlying `F D → C`. -/
  runCostar : F D → C

/-- `Costar F` is a `Profunctor` whenever `F` is a `Functor`:
    $\text{dimap}\;f\;g\;h = g \circ h \circ \text{fmap}\;f$. -/
instance [Functor F] : Profunctor (Costar F) where
  dimap f g h := ⟨fun fb => g (h.runCostar (f <$> fb))⟩
  lmap f h := ⟨fun fb => h.runCostar (f <$> fb)⟩
  rmap g h := ⟨g ∘ h.runCostar⟩

-- ── WrappedArrow ───────────────────────────────

/-- Wrap an `Arrow` for use as a `Profunctor`. -/
structure WrappedArrow (P : Type u → Type u → Type v) (A B : Type u) where
  /-- Unwrap the underlying arrow. -/
  unwrapArrow : P A B

/-- Every `Arrow` is a `Profunctor` via `WrappedArrow`, using
    $\text{lmap}\;f = \text{arr}\;f \ggg (\cdot)$ and $\text{rmap}\;g = (\cdot) \ggg \text{arr}\;g$. -/
instance [Arrow P] : Profunctor (WrappedArrow P) where
  lmap f k := ⟨Category.comp (Arrow.arr f) k.unwrapArrow⟩
  rmap g k := ⟨Category.comp k.unwrapArrow (Arrow.arr g)⟩
  dimap f g k := ⟨Category.comp (Arrow.arr f) (Category.comp k.unwrapArrow (Arrow.arr g))⟩

-- ── Forget ─────────────────────────────────────

/-- `Forget R A B` discards its second (covariant) argument entirely; it is
    constant in `B`: $\text{Forget}\,r\,a\,b \cong a \to r$. -/
structure Forget (R : Type u) (A B : Type u) where
  /-- Run the underlying `A → R`. -/
  runForget : A → R

/-- `Forget R` is a `Profunctor`: `rmap` is the identity (there is nothing to
    map, `B` is phantom), and `lmap`/`dimap` precompose. -/
instance : Profunctor (Forget R) where
  dimap f _ k := ⟨k.runForget ∘ f⟩
  lmap f k := ⟨k.runForget ∘ f⟩
  rmap _ k := ⟨k.runForget⟩

-- ── Tagged ─────────────────────────────────────

/-- `Tagged S B` pairs a value of type `B` with a phantom "tag" type `S`.
    Ported here (rather than as a separate module) per the `tagged` note in
    `docs/imports/profunctors/dependencies.md`. -/
structure Tagged (S : Type u) (B : Type u) where
  /-- Unwrap the tagged value. -/
  unTagged : B

/-- Change the phantom tag without touching the value:
    $\text{retag} : \text{Tagged}\,s\,b \to \text{Tagged}\,s'\,b$. -/
def Tagged.retag (t : Tagged S B) : Tagged S' B := ⟨t.unTagged⟩

/-- `Tagged` is a `Profunctor`: the phantom first argument carries no data, so
    `lmap` is `retag` and `rmap` maps the underlying value. -/
instance : Profunctor Tagged where
  dimap _ g t := ⟨g t.unTagged⟩
  lmap _ t := t.retag
  rmap g t := ⟨g t.unTagged⟩

end Control.Profunctor
