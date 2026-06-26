/-
  Linen.Data.List.NonEmpty — Non-empty list

  A list guaranteed to have at least one element, providing total `head` and
  `last`. Lean core has no non-empty list type (the `Nonempty` *Prop* is
  unrelated), so this is ported from Haskell's `Data.List.NonEmpty`.

  ## Design

  Represented as `head : α` + `tail : List α` rather than `{ l : List α // l ≠ [] }`,
  giving $O(1)$ access to the first element without proof unwrapping.

  ## Guarantees

  - `head` and `last` are total (no `Option` wrapping)
  - `length` returns `{ n : Nat // n ≥ 1 }`
  - `toList` is proven non-nil
-/

namespace Data.List

/-- A non-empty list. Stores `head : α` and `tail : List α`, ensuring at least
    one element. Provides $O(1)$ `head` access without proof elimination:
    $$\text{NonEmpty}(\alpha) \cong \alpha \times \text{List}(\alpha)$$
-/
structure NonEmpty (α : Type u) where
  /-- The first element, accessible in $O(1)$. -/
  head : α
  /-- The remaining elements (possibly empty). -/
  tail : List α
deriving BEq, Ord, Repr, Hashable

namespace NonEmpty

/-- Construct a singleton non-empty list. -/
@[inline] def singleton (x : α) : NonEmpty α := ⟨x, []⟩

/-- Construct from head and tail. -/
@[inline] def cons (x : α) (xs : NonEmpty α) : NonEmpty α := ⟨x, xs.head :: xs.tail⟩

/-- Convert to a standard `List`. Always non-empty. -/
@[inline] def toList (ne : NonEmpty α) : List α := ne.head :: ne.tail

/-- The last element. Total — no `Option` needed. Runs in $O(n)$. -/
def last (ne : NonEmpty α) : α :=
  go ne.head ne.tail
where
  go (x : α) : List α → α
    | [] => x
    | y :: ys => go y ys

/-- The length of a non-empty list, guaranteed $\geq 1$. -/
def length (ne : NonEmpty α) : { n : Nat // n ≥ 1 } :=
  ⟨1 + ne.tail.length, Nat.le_add_right 1 _⟩

/-- Append a non-empty list to another. Result is non-empty. -/
def append (xs : NonEmpty α) (ys : NonEmpty α) : NonEmpty α :=
  ⟨xs.head, xs.tail ++ ys.toList⟩

instance : Append (NonEmpty α) where
  append := NonEmpty.append

/-- Map a function over every element. -/
def map (f : α → β) (ne : NonEmpty α) : NonEmpty β :=
  ⟨f ne.head, ne.tail.map f⟩

/-- Reverse the non-empty list. -/
def reverse (ne : NonEmpty α) : NonEmpty α :=
  match h : ne.toList.reverse with
  | [] => absurd h (by simp [toList])
  | x :: xs => ⟨x, xs⟩

/-- Right fold over the non-empty list. -/
def foldr (f : α → β → β) (init : β) (ne : NonEmpty α) : β :=
  f ne.head (ne.tail.foldr f init)

/-- Right fold without an initial value (uses the last element). -/
def foldr1 (f : α → α → α) (ne : NonEmpty α) : α :=
  go f ne.head ne.tail
where
  go (f : α → α → α) (x : α) : List α → α
    | [] => x
    | y :: ys => f x (go f y ys)

/-- Left fold without an initial value. -/
def foldl1 (f : α → α → α) (ne : NonEmpty α) : α :=
  ne.tail.foldl f ne.head

/-- Construct from a list, if non-empty. Returns `some` iff the input is non-nil. -/
def fromList? : List α → Option (NonEmpty α)
  | [] => none
  | x :: xs => some ⟨x, xs⟩

/-- Construct from a list with proof of non-emptiness. -/
def fromList (l : List α) (h : l ≠ []) : NonEmpty α :=
  match l, h with
  | x :: xs, _ => ⟨x, xs⟩

-- ── Proofs ─────────────────────────────────────

/-- Converting to a list always yields a non-nil list. -/
theorem toList_ne_nil (ne : NonEmpty α) : ne.toList ≠ [] := by
  simp [toList]

/-- The length of a reversed non-empty list equals the original length. -/
theorem reverse_length (ne : NonEmpty α) :
    ne.reverse.toList.length = ne.toList.length := by
  simp [reverse]
  have hrev : ne.toList.reverse ≠ [] := by simp [toList]
  split
  · contradiction
  · next x xs h =>
    simp [toList]
    have := congrArg List.length h
    simp [toList] at this
    omega

/-- `toList` preserves length. -/
theorem toList_length (ne : NonEmpty α) :
    ne.toList.length = 1 + ne.tail.length := by
  unfold toList; simp; omega

/-- `map` preserves length. -/
theorem map_length (f : α → β) (ne : NonEmpty α) :
    (ne.map f).toList.length = ne.toList.length := by
  simp [map, toList, List.length_map]

-- ── Instances ──────────────────────────────────

instance : Functor NonEmpty where
  map := NonEmpty.map

instance : Pure NonEmpty where
  pure x := singleton x

instance : Bind NonEmpty where
  bind ne f :=
    let mapped := ne.map f
    let heads := mapped.head
    let rest := mapped.tail.flatMap NonEmpty.toList
    ⟨heads.head, heads.tail ++ rest⟩

instance : Monad NonEmpty where

instance [ToString α] : ToString (NonEmpty α) where
  toString ne := toString ne.toList

end NonEmpty
end Data.List
