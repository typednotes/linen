/-
  Linen.Data.Set — ordered finite sets

  Port of Haskell's `Data.Set` (`containers`). Backed by
  `Lean.RBMap k Unit compare` — the ordered map with unit values, mirroring
  Haskell's shared balanced-tree representation — giving $O(\log n)$
  membership, insert, and delete. Named `Set'` to avoid clashing with Lean's
  `Set` (`α → Prop`).

  Reference: https://hackage.haskell.org/package/containers/docs/Data-Set.html
-/

import Lean.Data.RBMap

namespace Data

/-- An ordered finite set, backed by a red-black tree.
    $$\text{Set'}(k) \cong \text{RBMap}(k, \text{Unit}, \text{compare})$$ -/
abbrev Set' (k : Type u) [Ord k] := Lean.RBMap k Unit compare

namespace Set'

variable {k : Type u} [Ord k]

/-! ── Construction ── -/

/-- The empty set. -/
@[inline] def empty : Set' k := Lean.RBMap.empty

/-- A single-element set. -/
@[inline] def singleton (x : k) : Set' k :=
  Lean.RBMap.insert Lean.RBMap.empty x ()

/-- Build a set from a list of elements. -/
def fromList (l : List k) : Set' k :=
  l.foldl (fun s x => Lean.RBMap.insert s x ()) Lean.RBMap.empty

/-! ── Query ── -/

/-- Is the element a member? -/
@[inline] def member (x : k) (s : Set' k) : Bool :=
  Lean.RBMap.contains s x

/-- Is the set empty? -/
@[inline] def null (s : Set' k) : Bool :=
  Lean.RBMap.isEmpty s

/-- The number of elements. -/
@[inline] def size' (s : Set' k) : Nat :=
  Lean.RBMap.size s

/-! ── Insertion / deletion ── -/

/-- Insert an element. -/
@[inline] def insert' (x : k) (s : Set' k) : Set' k :=
  Lean.RBMap.insert s x ()

/-- Delete an element. -/
@[inline] def delete (x : k) (s : Set' k) : Set' k :=
  Lean.RBMap.erase s x

/-! ── Combine ── -/

/-- Union. -/
def union (s1 s2 : Set' k) : Set' k :=
  Lean.RBMap.mergeBy (fun _ _ _ => ()) s1 s2

/-- Intersection. -/
def intersection (s1 s2 : Set' k) : Set' k :=
  Lean.RBMap.intersectBy (fun _ _ _ => ()) s1 s2

/-- Difference (`s1 \ s2`). -/
def difference (s1 s2 : Set' k) : Set' k :=
  Lean.RBMap.filter (fun key _ => !(Lean.RBMap.contains s2 key)) s1

/-- Is `s1 ⊆ s2`? -/
def isSubsetOf (s1 s2 : Set' k) : Bool :=
  Lean.RBMap.all s1 (fun key _ => Lean.RBMap.contains s2 key)

/-! ── Traversal ── -/

/-- Map a function over all elements (result may shrink if `f` collides). -/
def mapSet [Ord k₂] (f : k → k₂) (s : Set' k) : Set' k₂ :=
  Lean.RBMap.fold (fun acc key _ => Lean.RBMap.insert acc (f key) ()) Lean.RBMap.empty s

/-- Filter elements satisfying a predicate. -/
def filter (p : k → Bool) (s : Set' k) : Set' k :=
  Lean.RBMap.filter (fun key _ => p key) s

/-- Left fold over elements, ascending. -/
@[inline] def foldl (f : α → k → α) (init : α) (s : Set' k) : α :=
  Lean.RBMap.fold (fun acc key _ => f acc key) init s

/-- Right fold over elements, ascending. -/
@[inline] def foldr (f : k → α → α) (init : α) (s : Set' k) : α :=
  Lean.RBMap.revFold (fun acc key _ => f key acc) init s

/-! ── Conversion ── -/

/-- All elements, ascending. -/
def toList' (s : Set' k) : List k :=
  Lean.RBMap.fold (fun acc key _ => acc ++ [key]) [] s

/-- Ascending element list (same as `toList'` for an ordered set). -/
@[inline] def toAscList (s : Set' k) : List k :=
  toList' s

/-! ── Min / max ── -/

/-- The smallest element, or `none` if empty. -/
def findMin (s : Set' k) : Option k :=
  (Lean.RBMap.min s).map (·.1)

/-- The largest element, or `none` if empty. -/
def findMax (s : Set' k) : Option k :=
  (Lean.RBMap.max s).map (·.1)

/-! ── Instances ── -/

instance : EmptyCollection (Set' k) where
  emptyCollection := Set'.empty

instance : Inhabited (Set' k) where
  default := Set'.empty

instance [Repr k] : Repr (Set' k) where
  reprPrec s _ :=
    let elems := (toList' s).map repr
    "Set'.fromList [" ++ Std.Format.joinSep elems ", " ++ "]"

instance [BEq k] : BEq (Set' k) where
  beq s1 s2 := toList' s1 == toList' s2

/-! ── Proofs ── -/

/-- The empty set has no elements. -/
theorem null_empty : null (Set'.empty : Set' k) = true := rfl

/-- Membership in the empty set is false. -/
theorem member_empty (x : k) : member x (Set'.empty : Set' k) = false := rfl

/-- The empty set has size zero. -/
theorem size_empty : size' (Set'.empty : Set' k) = 0 := rfl

end Set'
end Data
