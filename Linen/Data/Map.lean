/-
  Linen.Data.Map — ordered finite maps

  Port of Haskell's `Data.Map` (`containers`). Backed by Lean's `Lean.RBMap`
  (a red-black tree keyed by `compare`), giving $O(\log n)$ lookup, insert, and
  delete — structurally the same shape as Haskell's size-balanced tree. `Map` is
  a transparent abbreviation, with the Haskell-compatible API on top.

  Reference: https://hackage.haskell.org/package/containers/docs/Data-Map.html
-/

import Lean.Data.RBMap

namespace Data

/-- An ordered finite map from keys `k` to values `v`, backed by a red-black tree.
    $$\text{Map}(k, v) \cong \text{RBMap}(k, v, \text{compare})$$ -/
abbrev Map (k : Type u) (v : Type w) [Ord k] := Lean.RBMap k v compare

namespace Map

variable {k : Type u} {v : Type w} [Ord k]

/-! ── Construction ── -/

/-- The empty map. -/
@[inline] def empty : Map k v := Lean.RBMap.empty

/-- A single key-value pair. -/
@[inline] def singleton (key : k) (val : v) : Map k v :=
  Lean.RBMap.empty.insert key val

/-- Build from an association list (later entries win on duplicate keys). -/
@[inline] def fromList (l : List (k × v)) : Map k v :=
  Lean.RBMap.ofList l

/-! ── Query ── -/

/-- Look up a key. -/
@[inline] def lookup (key : k) (m : Map k v) : Option v :=
  Lean.RBMap.find? m key

/-- Look up a key, returning a default if absent. -/
@[inline] def findWithDefault (dflt : v) (key : k) (m : Map k v) : v :=
  Lean.RBMap.findD m key dflt

/-- Is the key present? -/
@[inline] def member (key : k) (m : Map k v) : Bool :=
  Lean.RBMap.contains m key

/-- Is the map empty? -/
@[inline] def null (m : Map k v) : Bool :=
  Lean.RBMap.isEmpty m

/-- The number of entries. -/
@[inline] def size' (m : Map k v) : Nat :=
  Lean.RBMap.size m

/-! ── Insertion / update / delete ── -/

/-- Insert a key-value pair (replacing any existing value). -/
@[inline] def insert' (key : k) (val : v) (m : Map k v) : Map k v :=
  Lean.RBMap.insert m key val

/-- Delete a key. -/
@[inline] def delete (key : k) (m : Map k v) : Map k v :=
  Lean.RBMap.erase m key

/-- Adjust the value at a key, if present. -/
def adjust (f : v → v) (key : k) (m : Map k v) : Map k v :=
  match Lean.RBMap.find? m key with
  | some val => Lean.RBMap.insert m key (f val)
  | none => m

/-! ── Combine ── -/

/-- Left-biased union (keys in both take the value from the first map). -/
def union (m1 m2 : Map k v) : Map k v :=
  Lean.RBMap.mergeBy (fun _ v1 _ => v1) m1 m2

/-- Union with a combining function. -/
def unionWith (f : v → v → v) (m1 m2 : Map k v) : Map k v :=
  Lean.RBMap.mergeBy (fun _ v1 v2 => f v1 v2) m1 m2

/-- Intersection, keeping values from the first map. -/
def intersection (m1 : Map k v) (m2 : Map k v) : Map k v :=
  Lean.RBMap.intersectBy (fun _ v1 _ => v1) m1 m2

/-- Intersection with a combining function. -/
def intersectionWith (f : v → v → v) (m1 m2 : Map k v) : Map k v :=
  Lean.RBMap.intersectBy (fun _ v1 v2 => f v1 v2) m1 m2

/-- Difference (entries of `m1` whose key is not in `m2`). -/
def difference (m1 : Map k v) (m2 : Map k v) : Map k v :=
  Lean.RBMap.filter (fun key _ => !(Lean.RBMap.contains m2 key)) m1

/-! ── Traversal (ascending key order) ── -/

/-- Left fold over key-value pairs, ascending. -/
@[inline] def foldlWithKey (f : α → k → v → α) (init : α) (m : Map k v) : α :=
  Lean.RBMap.fold f init m

/-- Right fold over key-value pairs, ascending. -/
@[inline] def foldrWithKey (f : k → v → α → α) (init : α) (m : Map k v) : α :=
  Lean.RBMap.revFold (fun acc key val => f key val acc) init m

/-- Map a function over all values. -/
def mapValues (f : v → w) (m : Map k v) : Map k w :=
  Lean.RBMap.fold (fun acc key val => Lean.RBMap.insert acc key (f val)) Lean.RBMap.empty m

/-- Map a function over all key-value pairs. -/
def mapWithKey (f : k → v → w) (m : Map k v) : Map k w :=
  Lean.RBMap.fold (fun acc key val => Lean.RBMap.insert acc key (f key val)) Lean.RBMap.empty m

/-- Map a function over all keys (last value wins if `f` collides keys). -/
def mapKeys [Ord k₂] (f : k → k₂) (m : Map k v) : Map k₂ v :=
  Lean.RBMap.fold (fun acc key val => Lean.RBMap.insert acc (f key) val) Lean.RBMap.empty m

/-- Filter entries by a predicate on keys and values. -/
@[inline] def filterWithKey (p : k → v → Bool) (m : Map k v) : Map k v :=
  Lean.RBMap.filter p m

/-! ── Conversion ── -/

/-- All key-value pairs, ascending key order. -/
@[inline] def toList' (m : Map k v) : List (k × v) :=
  Lean.RBMap.toList m

/-- Ascending association list (same as `toList'` for an ordered map). -/
@[inline] def toAscList (m : Map k v) : List (k × v) :=
  Lean.RBMap.toList m

/-- All keys, ascending. -/
def keys (m : Map k v) : List k :=
  Lean.RBMap.fold (fun acc key _ => acc ++ [key]) [] m

/-- All values, in ascending key order. -/
def elems (m : Map k v) : List v :=
  Lean.RBMap.fold (fun acc _ val => acc ++ [val]) [] m

/-! ── Submap ── -/

/-- Restrict to the keys in `ks`. -/
def restrictKeys (m : Map k v) (ks : List k) : Map k v :=
  let keySet := ks.foldl (fun (s : Lean.RBMap k Unit compare) key =>
    Lean.RBMap.insert s key ()) Lean.RBMap.empty
  Lean.RBMap.filter (fun key _ => Lean.RBMap.contains keySet key) m

/-- Remove the keys in `ks`. -/
def withoutKeys (m : Map k v) (ks : List k) : Map k v :=
  let keySet := ks.foldl (fun (s : Lean.RBMap k Unit compare) key =>
    Lean.RBMap.insert s key ()) Lean.RBMap.empty
  Lean.RBMap.filter (fun key _ => !(Lean.RBMap.contains keySet key)) m

/-- Is `m1` a submap of `m2` (every key of `m1` maps to the same value in `m2`)? -/
def isSubmapOf [BEq v] (m1 m2 : Map k v) : Bool :=
  Lean.RBMap.all m1 (fun key val =>
    match Lean.RBMap.find? m2 key with
    | some v2 => val == v2
    | none => false)

/-! ── Min / max ── -/

/-- The smallest key-value pair, or `none` if empty. -/
def lookupMin (m : Map k v) : Option (k × v) :=
  Lean.RBMap.min m

/-- The largest key-value pair, or `none` if empty. -/
def lookupMax (m : Map k v) : Option (k × v) :=
  Lean.RBMap.max m

/-! ── Instances ── -/

instance : EmptyCollection (Map k v) where
  emptyCollection := Map.empty

instance : Inhabited (Map k v) where
  default := Map.empty

instance [Repr k] [Repr v] : Repr (Map k v) where
  reprPrec m _ :=
    let pairs := (Lean.RBMap.toList m).map (fun (k, v) => repr k ++ " := " ++ repr v)
    "Map.fromList [" ++ Std.Format.joinSep pairs ", " ++ "]"

instance [BEq k] [BEq v] : BEq (Map k v) where
  beq m1 m2 := Lean.RBMap.toList m1 == Lean.RBMap.toList m2

/-! ── Proofs ── -/

/-- The empty map has no entries. -/
theorem null_empty : null (Map.empty : Map k v) = true := rfl

/-- Lookup on the empty map is `none`. -/
theorem lookup_empty (key : k) : lookup key (Map.empty : Map k v) = none := rfl

/-- The empty map has size zero. -/
theorem size_empty : size' (Map.empty : Map k v) = 0 := rfl

/-- Membership in the empty map is false. -/
theorem member_empty (key : k) : member key (Map.empty : Map k v) = false := rfl

end Map
end Data
