/-
  Linen.Data.CaseInsensitive — case-insensitive comparison wrapper

  A `CI α` stores a value's `original` form alongside a pre-computed
  `foldedCase`; equality, ordering, and hashing use only the folded form, while
  `ToString`/`Repr` preserve the original. The structure invariant
  `foldedCase = FoldCase.foldCase original` is carried in the type (and the
  constructor is `protected`), so no inconsistent `CI` can be built — use the
  smart constructor `mk'`. Mirrors Haskell's `Data.CaseInsensitive`.
-/

namespace Data

/-- Case-folding to a canonical form. -/
class FoldCase (α : Type) where
  /-- Fold a value to its canonical case. -/
  foldCase : α → α

/-- A case-insensitive wrapper: the `original` value plus its `foldedCase`,
    with the invariant `foldedCase = FoldCase.foldCase original`. Comparison and
    hashing use only `foldedCase`. -/
structure CI (α : Type) [FoldCase α] where
  protected mk ::
  /-- The original, unmodified value. -/
  original : α
  /-- The case-folded value, used for comparison. -/
  foldedCase : α
  /-- Invariant: `foldedCase` is the case-folded form of `original`. -/
  inv : foldedCase = FoldCase.foldCase original

namespace CI

/-- Smart constructor: computes the folded form automatically. -/
@[inline] def mk' [FoldCase α] (x : α) : CI α :=
  CI.mk x (FoldCase.foldCase x) rfl

/-- Map a function over the wrapped value (recomputing the folded form). -/
@[inline] def map [FoldCase α] [FoldCase β] (f : α → β) (ci : CI α) : CI β :=
  mk' (f ci.original)

instance [FoldCase α] [BEq α] : BEq (CI α) where
  beq a b := a.foldedCase == b.foldedCase

instance [FoldCase α] [Hashable α] : Hashable (CI α) where
  hash ci := hash ci.foldedCase

instance [FoldCase α] [Ord α] : Ord (CI α) where
  compare a b := compare a.foldedCase b.foldedCase

instance [FoldCase α] [ToString α] : ToString (CI α) where
  toString ci := toString ci.original

instance [FoldCase α] [Repr α] : Repr (CI α) where
  reprPrec ci n := reprPrec ci.original n

/-! ── `FoldCase` instances ── -/

instance : FoldCase String where
  foldCase s := s.toLower

instance : FoldCase Char where
  foldCase c := c.toLower

/-! ── Laws ── -/

/-- Two `CI` values are equal iff their folded cases are. -/
theorem ci_eq_iff [FoldCase α] [BEq α] (a b : CI α) :
    (a == b) = (a.foldedCase == b.foldedCase) := rfl

end CI
end Data
