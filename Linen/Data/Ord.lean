/-
  Linen.Data.Ord — Ordering utilities

  `Down` (reversed ordering) and a proof-carrying `clamp`. Haskell's `comparing`
  is already Lean core's `compareOn` (`compareOn f x y = compare (f x) (f y)`),
  so it is not re-ported.
-/

namespace Data

/-- `Down α` reverses the ordering of `α`: if $a \leq b$ then
    $\text{Down}(b) \leq \text{Down}(a)$. Handy for descending sorts.
    $$\text{compare}_{\text{Down}}(x, y) = \text{compare}(y, x)$$ -/
structure Down (α : Type u) where
  /-- Unwrap the reversed-order value. -/
  getDown : α
deriving Repr, Hashable

namespace Down

instance [BEq α] : BEq (Down α) where
  beq a b := a.getDown == b.getDown

/-- Reversed ordering: compares in the opposite direction. -/
instance [Ord α] : Ord (Down α) where
  compare a b := compare b.getDown a.getDown

instance [ToString α] : ToString (Down α) where
  toString d := s!"Down({d.getDown})"

/-- Wrapping then unwrapping `Down` is identity. -/
theorem get_mk (a : α) : (Down.mk a).getDown = a := rfl

/-- `Down` comparison reverses the arguments. -/
theorem compare_reverse [Ord α] (a b : Down α) :
    compare a b = compare b.getDown a.getDown := rfl

end Down

/-- Clamp a value to the interval $[\text{lo}, \text{hi}]$, returning a subtype
    proving the result lies within bounds (`lo ≤ y ∧ y ≤ hi`).

    **Precondition:** `lo ≤ hi`. Reflexivity and totality of `≤` are supplied as
    proofs, keeping the function general over any decidable `LE`. -/
def clamp [LE α] [DecidableRel (α := α) (· ≤ ·)] (x lo hi : α)
    (hle : lo ≤ hi)
    (refl : ∀ a : α, a ≤ a)
    (total : ∀ a b : α, ¬(a ≤ b) → b ≤ a) : { y : α // lo ≤ y ∧ y ≤ hi } :=
  if h₁ : x ≤ lo then
    ⟨lo, refl lo, hle⟩
  else if h₂ : hi ≤ x then
    ⟨hi, hle, refl hi⟩
  else
    ⟨x, total x lo h₁, total hi x h₂⟩

end Data
