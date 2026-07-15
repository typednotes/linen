/-
  Linen.Data.Map — ordered finite maps

  Port of Haskell's `Data.Map` (`containers`). Backed by `Std.TreeMap`
  (a size-balanced tree keyed by `compare`), giving $O(\log n)$ lookup, insert,
  and delete — structurally the same shape as Haskell's size-balanced tree.
  `Map` is a transparent abbreviation, with the Haskell-compatible API on top.

  Reference: https://hackage.haskell.org/package/containers/docs/Data-Map.html
-/

import Std.Data.TreeMap
import Std.Data.TreeSet

namespace Data

/-- An ordered finite map from keys `k` to values `v`, backed by a
    size-balanced tree.
    $$\text{Map}(k, v) \cong \text{TreeMap}(k, v, \text{compare})$$ -/
abbrev Map (k : Type u) (v : Type w) [Ord k] := Std.TreeMap k v

namespace Map

variable {k : Type u} {v : Type w} [Ord k]

/-! ── Construction ── -/

/-- The empty map. -/
@[inline] def empty : Map k v := Std.TreeMap.empty

/-- A single key-value pair. -/
@[inline] def singleton (key : k) (val : v) : Map k v :=
  Std.TreeMap.empty.insert key val

/-- Build from an association list (later entries win on duplicate keys). -/
@[inline] def fromList (l : List (k × v)) : Map k v :=
  Std.TreeMap.ofList l

/-! ── Query ── -/

/-- Look up a key. -/
@[inline] def lookup (key : k) (m : Map k v) : Option v :=
  Std.TreeMap.get? m key

/-- Look up a key, returning a default if absent. -/
@[inline] def findWithDefault (dflt : v) (key : k) (m : Map k v) : v :=
  Std.TreeMap.getD m key dflt

/-- Is the key present? -/
@[inline] def member (key : k) (m : Map k v) : Bool :=
  Std.TreeMap.contains m key

/-- Is the map empty? -/
@[inline] def null (m : Map k v) : Bool :=
  Std.TreeMap.isEmpty m

/-- The number of entries. -/
@[inline] def size' (m : Map k v) : Nat :=
  Std.TreeMap.size m

/-! ── Insertion / update / delete ── -/

/-- Insert a key-value pair (replacing any existing value). -/
@[inline] def insert' (key : k) (val : v) (m : Map k v) : Map k v :=
  Std.TreeMap.insert m key val

/-- Delete a key. -/
@[inline] def delete (key : k) (m : Map k v) : Map k v :=
  Std.TreeMap.erase m key

/-- Adjust the value at a key, if present. -/
@[inline] def adjust (f : v → v) (key : k) (m : Map k v) : Map k v :=
  Std.TreeMap.modify m key f

/-! ── Combine ── -/

/-- Left-biased union (keys in both take the value from the first map). -/
def union (m1 m2 : Map k v) : Map k v :=
  Std.TreeMap.mergeWith (fun _ v1 _ => v1) m1 m2

/-- Union with a combining function. -/
def unionWith (f : v → v → v) (m1 m2 : Map k v) : Map k v :=
  Std.TreeMap.mergeWith (fun _ v1 v2 => f v1 v2) m1 m2

/-- Intersection, keeping values from the first map. -/
def intersection (m1 : Map k v) (m2 : Map k v) : Map k v :=
  Std.TreeMap.inter m1 m2

/-- Intersection with a combining function. -/
def intersectionWith (f : v → v → v) (m1 m2 : Map k v) : Map k v :=
  Std.TreeMap.filterMap (fun key v1 => (Std.TreeMap.get? m2 key).map (f v1)) m1

/-- Difference (entries of `m1` whose key is not in `m2`). -/
def difference (m1 : Map k v) (m2 : Map k v) : Map k v :=
  Std.TreeMap.diff m1 m2

/-! ── Traversal (ascending key order) ── -/

/-- Left fold over key-value pairs, ascending. -/
@[inline] def foldlWithKey (f : α → k → v → α) (init : α) (m : Map k v) : α :=
  Std.TreeMap.foldl f init m

/-- Right fold over key-value pairs, ascending. -/
@[inline] def foldrWithKey (f : k → v → α → α) (init : α) (m : Map k v) : α :=
  Std.TreeMap.foldr f init m

/-- Map a function over all values. -/
def mapValues (f : v → w) (m : Map k v) : Map k w :=
  Std.TreeMap.map (fun _ val => f val) m

/-- Map a function over all key-value pairs. -/
def mapWithKey (f : k → v → w) (m : Map k v) : Map k w :=
  Std.TreeMap.map f m

/-- Map a function over all keys (last value wins if `f` collides keys). -/
def mapKeys [Ord k₂] (f : k → k₂) (m : Map k v) : Map k₂ v :=
  Std.TreeMap.foldl (fun acc key val => Std.TreeMap.insert acc (f key) val) Std.TreeMap.empty m

/-- Filter entries by a predicate on keys and values. -/
@[inline] def filterWithKey (p : k → v → Bool) (m : Map k v) : Map k v :=
  Std.TreeMap.filter p m

/-! ── Conversion ── -/

/-- All key-value pairs, ascending key order. -/
@[inline] def toList' (m : Map k v) : List (k × v) :=
  Std.TreeMap.toList m

/-- Ascending association list (same as `toList'` for an ordered map). -/
@[inline] def toAscList (m : Map k v) : List (k × v) :=
  Std.TreeMap.toList m

/-- All keys, ascending. -/
@[inline] def keys (m : Map k v) : List k :=
  Std.TreeMap.keys m

/-- All values, in ascending key order. -/
@[inline] def elems (m : Map k v) : List v :=
  Std.TreeMap.values m

/-! ── Submap ── -/

/-- Restrict to the keys in `ks`. -/
def restrictKeys (m : Map k v) (ks : List k) : Map k v :=
  let keySet := Std.TreeSet.ofList ks
  Std.TreeMap.filter (fun key _ => Std.TreeSet.contains keySet key) m

/-- Remove the keys in `ks`. -/
def withoutKeys (m : Map k v) (ks : List k) : Map k v :=
  let keySet := Std.TreeSet.ofList ks
  Std.TreeMap.filter (fun key _ => !(Std.TreeSet.contains keySet key)) m

/-- Is `m1` a submap of `m2` (every key of `m1` maps to the same value in `m2`)? -/
def isSubmapOf [BEq v] (m1 m2 : Map k v) : Bool :=
  Std.TreeMap.all m1 (fun key val =>
    match Std.TreeMap.get? m2 key with
    | some v2 => val == v2
    | none => false)

/-! ── Min / max ── -/

/-- The smallest key-value pair, or `none` if empty. -/
def lookupMin (m : Map k v) : Option (k × v) :=
  Std.TreeMap.minEntry? m

/-- The largest key-value pair, or `none` if empty. -/
def lookupMax (m : Map k v) : Option (k × v) :=
  Std.TreeMap.maxEntry? m

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
