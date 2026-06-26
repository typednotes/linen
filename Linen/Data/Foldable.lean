/-
  Linen.Data.Foldable — Foldable typeclass

  Structures that can be folded to a summary value. Lean core has no `Foldable`
  class, so this ports Haskell's `Data.Foldable` with derived operations.

  ## Design

  Provides `foldr`, `foldl`, `toList`, and derived operations: `foldMap`, `null`,
  `length`, `any`, `all`, `find?`, `elem`, `minimum?`, `maximum?`, `sum`,
  `product`, plus total `minimum1`/`maximum1` on a `NonEmpty`.

  Instances for `List`, `Option`, `List.NonEmpty`, and `Sum α` (folding the
  `.inr` case, the Lean spelling of Haskell's `Foldable (Either a)`).
-/

import Linen.Data.List.NonEmpty

namespace Data

/-- `Foldable` captures the pattern of folding a structure into a single value.
    $$\text{foldr}(f, z, [x_1, \ldots, x_n]) = f(x_1, f(x_2, \ldots f(x_n, z)))$$ -/
class Foldable (F : Type u → Type v) where
  /-- Right fold. -/
  foldr : (α → β → β) → β → F α → β
  /-- Left fold. -/
  foldl : (β → α → β) → β → F α → β := fun f z t => foldr (fun a g b => g (f b a)) id t z
  /-- Convert to a list, preserving order. -/
  toList : F α → List α := fun t => foldr (· :: ·) [] t

namespace Foldable

/-- Map each element to a monoid and combine, starting from `default` (`mempty`). -/
@[inline] def foldMap [Foldable F] [Append β] [Inhabited β] (f : α → β) (t : F α) : β :=
  Foldable.foldr (fun a b => f a ++ b) default t

/-- Is the structure empty? -/
@[inline] def null [Foldable F] (t : F α) : Bool :=
  Foldable.foldr (fun _ _ => false) true t

/-- Count of elements. -/
@[inline] def length [Foldable F] (t : F α) : Nat :=
  Foldable.foldl (fun n _ => n + 1) 0 t

/-- Does any element satisfy the predicate? -/
@[inline] def any [Foldable F] (p : α → Bool) (t : F α) : Bool :=
  Foldable.foldr (fun a b => p a || b) false t

/-- Do all elements satisfy the predicate? -/
@[inline] def all [Foldable F] (p : α → Bool) (t : F α) : Bool :=
  Foldable.foldr (fun a b => p a && b) true t

/-- Find the first element satisfying a predicate. -/
@[inline] def find? [Foldable F] (p : α → Bool) (t : F α) : Option α :=
  Foldable.foldr (fun a b => if p a then some a else b) none t

/-- Is the element in the structure? -/
@[inline] def elem [Foldable F] [BEq α] (a : α) (t : F α) : Bool :=
  any (· == a) t

/-- The minimum element, if the structure is non-empty. -/
@[inline] def minimum? [Foldable F] [Min α] (t : F α) : Option α :=
  Foldable.foldl (fun acc a => some (match acc with | none => a | some m => Min.min m a)) none t

/-- The maximum element, if the structure is non-empty. -/
@[inline] def maximum? [Foldable F] [Max α] (t : F α) : Option α :=
  Foldable.foldl (fun acc a => some (match acc with | none => a | some m => Max.max m a)) none t

/-- Sum of all elements. -/
@[inline] def sum [Foldable F] [Add α] [OfNat α 0] (t : F α) : α :=
  Foldable.foldl (· + ·) 0 t

/-- Product of all elements. -/
@[inline] def product [Foldable F] [Mul α] [OfNat α 1] (t : F α) : α :=
  Foldable.foldl (· * ·) 1 t

/-- Total minimum on a non-empty structure. No `Option` needed. -/
@[inline] def minimum1 [Ord α] (ne : List.NonEmpty α) : α :=
  ne.tail.foldl (fun acc a => if compare a acc == .lt then a else acc) ne.head

/-- Total maximum on a non-empty structure. No `Option` needed. -/
@[inline] def maximum1 [Ord α] (ne : List.NonEmpty α) : α :=
  ne.tail.foldl (fun acc a => if compare a acc == .gt then a else acc) ne.head

end Foldable

-- ── Instances ──────────────────────────────────

instance : Foldable List where
  foldr := List.foldr
  foldl := List.foldl
  toList := id

instance : Foldable Option where
  foldr f z
    | some a => f a z
    | none => z
  foldl f z
    | some a => f z a
    | none => z
  toList
    | some a => [a]
    | none => []

instance : Foldable List.NonEmpty where
  foldr f z ne := f ne.head (ne.tail.foldr f z)
  foldl f z ne := ne.tail.foldl f (f z ne.head)
  toList := List.NonEmpty.toList

/-- `Foldable (Sum α)` folds the `.inr` case (the Lean spelling of Haskell's
    `Foldable (Either a)`), passing `.inl` through as empty. -/
instance : Foldable (Sum α) where
  foldr f z
    | .inr b => f b z
    | .inl _ => z
  foldl f z
    | .inr b => f z b
    | .inl _ => z
  toList
    | .inr b => [b]
    | .inl _ => []

-- ── List instance theorems ──────────────────────────────────

/-- `foldr` on an empty list is the initial accumulator. -/
theorem foldr_nil {f : α → β → β} {z : β} : Foldable.foldr f z ([] : List α) = z := by rfl

/-- `foldl` on an empty list is the initial accumulator. -/
theorem foldl_nil {f : β → α → β} {z : β} : Foldable.foldl f z ([] : List α) = z := by rfl

end Data
