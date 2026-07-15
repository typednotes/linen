/-
  Linen.Control.Profunctor.Unsafe — the `Profunctor` typeclass

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Unsafe`
  (module #1 of `docs/imports/profunctors/dependencies.md`).

  A `Profunctor` is a bifunctor that is contravariant in its first argument
  and covariant in its second — the categorical shape shared by ordinary
  functions, `Star`/`Costar` (functions "up to" a functor), `Forget`, and
  every other type in this module tree. Upstream's `Data.Profunctor.Unsafe`
  additionally exposes `(#.)`/`(.#)`, GHC-only operators that use
  `unsafeCoerce`-backed casts (`Coercible`) to let the optimizer erase
  `newtype` wrapping at runtime. Lean has no such representational-coercion
  primitive (nor a runtime to optimize), so those two operators are omitted
  here; every derived definition in this port goes through the safe
  `dimap`/`lmap`/`rmap` instead.

  The instance for Haskell's `(->)` is given for `Control.Fun`
  (`Linen.Control.Category`), the nominal wrapper this library already uses
  to view functions as a `Type u → Type u → Type v`-shaped morphism (the same
  device `Control.Arrow`'s `Fun` instances rely on), since Lean's `→` is not
  directly usable as a bare instance head.
-/

import Linen.Control.Category

namespace Control

/-- A **profunctor** $P : \mathsf{Type}^{\mathsf{op}} \times \mathsf{Type} \to \mathsf{Type}$:
    contravariant in its first argument, covariant in its second.

    Upstream's `MINIMAL dimap | lmap, rmap` lets an instance give either
    `dimap` alone or both `lmap`/`rmap`; that mutual pair of defaults (each
    defined via the other) is exactly the shape Lean's class elaborator
    cannot instantiate a default for when a further subclass doesn't
    concretely supply any of the three (it warns "could not be
    instantiated" rather than reject the declaration). Every instance in
    this codebase already supplies `dimap` concretely, so `dimap` is made
    the required primitive here and `lmap`/`rmap` keep their upstream
    defaults, now defined non-circularly in terms of it. Laws (definitional
    when only `dimap` is supplied):

    $$\text{dimap}\;\text{id}\;\text{id} = \text{id}$$
    $$\text{dimap}\;f\;g = \text{lmap}\;f \circ \text{rmap}\;g$$ -/
class Profunctor (P : Type u → Type u → Type v) where
  /-- Map over both arguments at once: $\text{dimap}\;f\;g : P\,b\,c \to P\,a\,d$
      for $f : a \to b$, $g : c \to d$. -/
  dimap : (α → β) → (γ → δ) → P β γ → P α δ
  /-- Map the first argument contravariantly: $\text{lmap}\;f = \text{dimap}\;f\;\text{id}$. -/
  lmap : (α → β) → P β γ → P α γ := fun f => dimap f id
  /-- Map the second argument covariantly: $\text{rmap}\;g = \text{dimap}\;\text{id}\;g$. -/
  rmap : (γ → δ) → P α γ → P α δ := fun g => dimap id g

-- ── Instances ──────────────────────────────────

/-- Ordinary functions (viewed through `Control.Fun`) form a `Profunctor`:
    $\text{dimap}\;f\;g\;h = g \circ h \circ f$. -/
instance : Profunctor Control.Fun where
  dimap f g h := ⟨g ∘ h.apply ∘ f⟩
  lmap f h := ⟨h.apply ∘ f⟩
  rmap g h := ⟨g ∘ h.apply⟩

end Control
