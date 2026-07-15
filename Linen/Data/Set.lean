/-
  Linen.Data.Set — ordered finite sets

  Port of Haskell's `Data.Set` (`containers`). Backed by `Std.TreeSet k` — a
  size-balanced tree, mirroring Haskell's shared balanced-tree representation
  — giving $O(\log n)$ membership, insert, and delete. Named `Set'` to avoid
  clashing with Lean's `Set` (`α → Prop`).

  Reference: https://hackage.haskell.org/package/containers/docs/Data-Set.html
-/

import Std.Data.TreeSet

namespace Data

/-- An ordered finite set, backed by a size-balanced tree.
    $$\text{Set'}(k) \cong \text{TreeSet}(k, \text{compare})$$ -/
abbrev Set' (k : Type u) [Ord k] := Std.TreeSet k

namespace Set'

variable {k : Type u} [Ord k]

/-! ── Construction ── -/

/-- The empty set. -/
@[inline] def empty : Set' k := Std.TreeSet.empty

/-- A single-element set. -/
@[inline] def singleton (x : k) : Set' k :=
  Std.TreeSet.empty.insert x

/-- Build a set from a list of elements. -/
def fromList (l : List k) : Set' k :=
  Std.TreeSet.ofList l

/-! ── Query ── -/

/-- Is the element a member? -/
@[inline] def member (x : k) (s : Set' k) : Bool :=
  Std.TreeSet.contains s x

/-- Is the set empty? -/
@[inline] def null (s : Set' k) : Bool :=
  Std.TreeSet.isEmpty s

/-- The number of elements. -/
@[inline] def size' (s : Set' k) : Nat :=
  Std.TreeSet.size s

/-! ── Insertion / deletion ── -/

/-- Insert an element. -/
@[inline] def insert' (x : k) (s : Set' k) : Set' k :=
  Std.TreeSet.insert s x

/-- Delete an element. -/
@[inline] def delete (x : k) (s : Set' k) : Set' k :=
  Std.TreeSet.erase s x

/-! ── Combine ── -/

/-- Union. -/
def union (s1 s2 : Set' k) : Set' k :=
  Std.TreeSet.union s1 s2

/-- Intersection. -/
def intersection (s1 s2 : Set' k) : Set' k :=
  Std.TreeSet.inter s1 s2

/-- Difference (`s1 \ s2`). -/
def difference (s1 s2 : Set' k) : Set' k :=
  Std.TreeSet.diff s1 s2

/-- Is `s1 ⊆ s2`? -/
def isSubsetOf (s1 s2 : Set' k) : Bool :=
  Std.TreeSet.all s1 (fun key => Std.TreeSet.contains s2 key)

/-! ── Traversal ── -/

/-- Map a function over all elements (result may shrink if `f` collides). -/
def mapSet [Ord k₂] (f : k → k₂) (s : Set' k) : Set' k₂ :=
  Std.TreeSet.foldl (fun acc key => Std.TreeSet.insert acc (f key)) Std.TreeSet.empty s

/-- Filter elements satisfying a predicate. -/
def filter (p : k → Bool) (s : Set' k) : Set' k :=
  Std.TreeSet.filter p s

/-- Left fold over elements, ascending. -/
@[inline] def foldl (f : α → k → α) (init : α) (s : Set' k) : α :=
  Std.TreeSet.foldl f init s

/-- Right fold over elements, ascending. -/
@[inline] def foldr (f : k → α → α) (init : α) (s : Set' k) : α :=
  Std.TreeSet.foldr f init s

/-! ── Conversion ── -/

/-- All elements, ascending. -/
def toList' (s : Set' k) : List k :=
  Std.TreeSet.toList s

/-- Ascending element list (same as `toList'` for an ordered set). -/
@[inline] def toAscList (s : Set' k) : List k :=
  toList' s

/-! ── Min / max ── -/

/-- The smallest element, or `none` if empty. -/
def findMin (s : Set' k) : Option k :=
  Std.TreeSet.min? s

/-- The largest element, or `none` if empty. -/
def findMax (s : Set' k) : Option k :=
  Std.TreeSet.max? s

/-! ── Proofs ── -/

/-- The empty set has no elements. -/
theorem null_empty : null (Set'.empty : Set' k) = true := rfl

/-- Membership in the empty set is false. -/
theorem member_empty (x : k) : member x (Set'.empty : Set' k) = false := rfl

/-- The empty set has size zero. -/
theorem size_empty : size' (Set'.empty : Set' k) = 0 := rfl

end Set'
end Data
