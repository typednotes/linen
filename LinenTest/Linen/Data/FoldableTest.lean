/-
  Tests for `Linen.Data.Foldable`.

  `foldr`/`foldl`/`toList` and the derived operations, across the `List`,
  `Option`, `List.NonEmpty`, and `Sum α` instances.
-/
import Linen.Data.Foldable

open Data
open Data.List (NonEmpty)

namespace Tests.Data.Foldable

/-! ### Core folds (List / Option / Sum) -/

#guard Foldable.foldr (· + ·) 0 [1, 2, 3] == 6
#guard Foldable.foldl (· - ·) 10 [1, 2, 3] == 4                 -- ((10-1)-2)-3
#guard Foldable.toList (some 7) == [7]
#guard Foldable.toList (none : Option Nat) == []
#guard Foldable.foldr (· + ·) 100 (Sum.inr 5 : String ⊕ Nat) == 105   -- folds .inr
#guard Foldable.foldr (· + ·) 100 (Sum.inl "x" : String ⊕ Nat) == 100  -- .inl is empty
#guard Foldable.toList (Sum.inr 5 : String ⊕ Nat) == [5]

/-! ### Derived ops -/

#guard Foldable.null ([] : List Nat) == true
#guard Foldable.null [1] == false
#guard Foldable.length [1, 2, 3, 4] == 4
#guard Foldable.any (· > 2) [1, 2, 3] == true
#guard Foldable.all (· > 0) [1, 2, 3] == true
#guard Foldable.find? (· > 1) [1, 2, 3] == some 2
#guard Foldable.elem 2 [1, 2, 3] == true
#guard Foldable.elem 9 [1, 2, 3] == false
#guard Foldable.sum [1, 2, 3, 4] == 10
#guard Foldable.product [1, 2, 3, 4] == 24
#guard Foldable.minimum? [3, 1, 2] == some 1
#guard Foldable.maximum? [3, 1, 2] == some 3
#guard Foldable.maximum? ([] : List Nat) == none

/-! ### foldMap (over a monoid: List append) -/

#guard Foldable.foldMap (fun n => List.replicate n n) [1, 2, 3] == [1, 2, 2, 3, 3, 3]

/-! ### NonEmpty instance + total min/max -/

#guard Foldable.length (⟨1, [2, 3]⟩ : NonEmpty Nat) == 3
#guard Foldable.toList (⟨1, [2, 3]⟩ : NonEmpty Nat) == [1, 2, 3]
#guard Foldable.minimum1 (⟨3, [1, 2]⟩ : NonEmpty Nat) == 1
#guard Foldable.maximum1 (⟨3, [1, 2]⟩ : NonEmpty Nat) == 3

/-! ### Empty-fold laws (compile-time) -/

example {f : α → β → β} {z : β} : Foldable.foldr f z ([] : List α) = z := foldr_nil
example {f : β → α → β} {z : β} : Foldable.foldl f z ([] : List α) = z := foldl_nil

end Tests.Data.Foldable
