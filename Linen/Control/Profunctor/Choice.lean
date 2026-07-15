/-
  Linen.Control.Profunctor.Choice — the `Choice`/`Cochoice` typeclasses

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Choice` (module #4
  of `docs/imports/profunctors/dependencies.md`). `Choice` is the
  `Sum`-flavoured dual of `Strong`: strength with respect to the cocartesian
  coproduct rather than the cartesian product. This is the class that backs
  `Control.Lens.Prism`.

  **Scope note.** As with `Strong`, upstream's `TambaraSum`/`PastroSum`
  (and their `Cochoice` duals `CotambaraSum`/`CopastroSum`) are
  free/cofree-adjoin-strength scaffolding with no call site in `lens` — see
  the identical scope note in `Linen.Control.Profunctor.Strong`. Left
  unported for the same reason.
-/

import Linen.Control.Profunctor.Unsafe
import Linen.Control.Profunctor.Types

open Control

namespace Control.Profunctor

-- ── Choice ─────────────────────────────────────

/-- Swap the two sides of a `Sum`: $\text{swapE} = \text{either}\;\text{inr}\;\text{inl}$. -/
def swapSum : α ⊕ β → β ⊕ α
  | .inl a => .inr a
  | .inr b => .inl b

/-- A **choice profunctor** lets the coproduct structure of `Type` (`Sum`)
    pass through it — the generalization of `Costar` of a functor that is
    strong with respect to `Sum` rather than `Prod`.

    Same mutual-default note as `Strong` (see `Linen.Control.Profunctor.Strong`):
    `left'` is made the required primitive, `right'` keeps its upstream
    default non-circularly.

    Laws:
    $$\text{left}' = \text{dimap}\;\text{swapE}\;\text{swapE} \circ \text{right}'$$
    $$\text{rmap}\;\text{inl} = \text{lmap}\;\text{inl} \circ \text{left}'$$ -/
class Choice (P : Type u → Type u → Type v) extends Profunctor P where
  /-- Thread an extra alternative `γ` through on the left: $\text{left}' : P\,α\,β \to P\,(α ⊕ γ)\,(β ⊕ γ)$. -/
  left' : P α β → P (α ⊕ γ) (β ⊕ γ)
  /-- Thread an extra alternative `γ` through on the right: $\text{right}' : P\,α\,β \to P\,(γ ⊕ α)\,(γ ⊕ β)$. -/
  right' : P α β → P (γ ⊕ α) (γ ⊕ β) := fun p => dimap swapSum swapSum (left' p)

/-- Ordinary functions are `Choice`:
    $\text{left}'\;f\;(\text{inl}\;a) = \text{inl}\;(f\,a)$, passing `.inr` through untouched. -/
instance : Choice Control.Fun where
  left' f := ⟨fun
    | .inl a => .inl (f.apply a)
    | .inr c => .inr c⟩
  right' f := ⟨fun
    | .inl c => .inl c
    | .inr a => .inr (f.apply a)⟩

/-- `Star F` is `Choice` for any `Applicative F`. -/
instance [Applicative F] : Choice (Star F) where
  left' f := ⟨fun
    | .inl a => .inl <$> f.runStar a
    | .inr c => pure (.inr c)⟩
  right' f := ⟨fun
    | .inl c => pure (.inl c)
    | .inr a => .inr <$> f.runStar a⟩

/-- `Tagged` is `Choice`: inject the phantom-tagged value into either side. -/
instance : Choice Tagged where
  left' t := ⟨.inl t.unTagged⟩
  right' t := ⟨.inr t.unTagged⟩

/-- `WrappedArrow P` is `Choice` for any `ArrowChoice P`. -/
instance [ArrowChoice P] : Choice (WrappedArrow P) where
  left' k := ⟨ArrowChoice.left k.unwrapArrow⟩
  right' k := ⟨ArrowChoice.right k.unwrapArrow⟩

-- ── Cochoice ───────────────────────────────────

/-- The dual of `Choice`: costrength with respect to `Sum`. Same
    mutual-default note as `Choice`: `unleft` is the required primitive,
    `unright` keeps its upstream default non-circularly. -/
class Cochoice (P : Type u → Type u → Type v) extends Profunctor P where
  /-- Discharge an extra alternative `δ` from the left. -/
  unleft : P (α ⊕ δ) (β ⊕ δ) → P α β
  /-- Discharge an extra alternative `δ` from the right. -/
  unright : P (δ ⊕ α) (δ ⊕ β) → P α β := fun p => unleft (dimap swapSum swapSum p)

/-- `Forget R` is `Cochoice`: precompose with the relevant injection. -/
instance : Cochoice (Forget R) where
  unleft k := ⟨fun a => k.runForget (.inl a)⟩
  unright k := ⟨fun a => k.runForget (.inr a)⟩

end Control.Profunctor
