/-
  Linen.Data.IntMap — maps with `Nat` keys

  Port of Haskell's `Data.IntMap` (`containers`). Backed by `Std.HashMap Nat v`
  (amortised $O(1)$ lookup/insert) rather than Haskell's Patricia trie. The
  sorted-order operations (`toAscList`, `lookupMin`, `lookupMax`) sort/scan on
  demand, since `HashMap` is unordered.

  Reference: https://hackage.haskell.org/package/containers/docs/Data-IntMap.html
-/

import Std.Data.HashMap

namespace Data

/-- A map from `Nat` keys to values, backed by a hash map.
    $$\text{IntMap}(v) \cong \text{HashMap}(\mathbb{N}, v)$$ -/
abbrev IntMap (v : Type u) := Std.HashMap Nat v

namespace IntMap

variable {v : Type u}

/-! ── Construction ── -/

/-- The empty map. -/
@[inline] def empty : IntMap v := ∅

/-- A single key-value pair. -/
@[inline] def singleton (key : Nat) (val : v) : IntMap v :=
  (∅ : IntMap v).insert key val

/-- Build from an association list (later entries win on duplicate keys). -/
@[inline] def fromList (l : List (Nat × v)) : IntMap v :=
  Std.HashMap.ofList l

/-! ── Query ── -/

/-- Look up a key. -/
@[inline] def lookup (key : Nat) (m : IntMap v) : Option v :=
  Std.HashMap.get? m key

/-- Look up a key, returning a default if absent. -/
@[inline] def findWithDefault (dflt : v) (key : Nat) (m : IntMap v) : v :=
  (Std.HashMap.get? m key).getD dflt

/-- Is the key present? -/
@[inline] def member (key : Nat) (m : IntMap v) : Bool :=
  Std.HashMap.contains m key

/-- Is the map empty? -/
@[inline] def null (m : IntMap v) : Bool :=
  Std.HashMap.isEmpty m

/-- The number of entries. -/
@[inline] def size' (m : IntMap v) : Nat :=
  Std.HashMap.size m

/-! ── Insertion / update / delete ── -/

/-- Insert a key-value pair (replacing any existing value). -/
@[inline] def insert' (key : Nat) (val : v) (m : IntMap v) : IntMap v :=
  Std.HashMap.insert m key val

/-- Delete a key. -/
@[inline] def delete (key : Nat) (m : IntMap v) : IntMap v :=
  Std.HashMap.erase m key

/-- Adjust the value at a key, if present. -/
def adjust (f : v → v) (key : Nat) (m : IntMap v) : IntMap v :=
  match Std.HashMap.get? m key with
  | some val => Std.HashMap.insert m key (f val)
  | none => m

/-! ── Combine ── -/

/-- Left-biased union. -/
def union (m1 m2 : IntMap v) : IntMap v :=
  Std.HashMap.fold (fun acc key val =>
    if Std.HashMap.contains acc key then acc else Std.HashMap.insert acc key val) m1 m2

/-- Union with a combining function (applied to values present in both). -/
def unionWith (f : v → v → v) (m1 m2 : IntMap v) : IntMap v :=
  Std.HashMap.fold (fun acc key val2 =>
    match Std.HashMap.get? acc key with
    | some val1 => Std.HashMap.insert acc key (f val1 val2)
    | none => Std.HashMap.insert acc key val2) m1 m2

/-- Intersection, keeping values from the first map. -/
def intersection (m1 : IntMap v) (m2 : IntMap v) : IntMap v :=
  Std.HashMap.filter (fun key _ => Std.HashMap.contains m2 key) m1

/-- Intersection with a combining function. -/
def intersectionWith (f : v → v → v) (m1 : IntMap v) (m2 : IntMap v) : IntMap v :=
  Std.HashMap.fold (fun acc key val1 =>
    match Std.HashMap.get? m2 key with
    | some val2 => Std.HashMap.insert acc key (f val1 val2)
    | none => acc) (∅ : IntMap v) m1

/-- Difference (entries of `m1` whose key is not in `m2`). -/
def difference (m1 : IntMap v) (m2 : IntMap v) : IntMap v :=
  Std.HashMap.filter (fun key _ => !(Std.HashMap.contains m2 key)) m1

/-! ── Traversal ── -/

/-- Left fold over key-value pairs (unspecified order). -/
@[inline] def foldlWithKey (f : α → Nat → v → α) (init : α) (m : IntMap v) : α :=
  Std.HashMap.fold (fun acc key val => f acc key val) init m

/-- Right fold over key-value pairs (unspecified order). -/
def foldrWithKey (f : Nat → v → α → α) (init : α) (m : IntMap v) : α :=
  (Std.HashMap.toList m).foldr (fun (key, val) acc => f key val acc) init

/-- Map a function over all values. -/
@[inline] def mapValues (f : v → w) (m : IntMap v) : IntMap w :=
  Std.HashMap.map (fun _ val => f val) m

/-- Map a function over all key-value pairs. -/
@[inline] def mapWithKey (f : Nat → v → w) (m : IntMap v) : IntMap w :=
  Std.HashMap.map (fun key val => f key val) m

/-- Filter entries by a predicate on keys and values. -/
@[inline] def filterWithKey (p : Nat → v → Bool) (m : IntMap v) : IntMap v :=
  Std.HashMap.filter p m

/-! ── Conversion ── -/

/-- All key-value pairs (unspecified order). -/
@[inline] def toList' (m : IntMap v) : List (Nat × v) :=
  Std.HashMap.toList m

/-- All key-value pairs sorted by ascending key ($O(n \log n)$). -/
def toAscList (m : IntMap v) : List (Nat × v) :=
  (Std.HashMap.toList m).toArray.qsort (fun a b => a.1 < b.1) |>.toList

/-- All keys (unspecified order). -/
def keys (m : IntMap v) : List Nat :=
  Std.HashMap.fold (fun acc key _ => key :: acc) [] m

/-- All values (unspecified order). -/
def elems (m : IntMap v) : List v :=
  Std.HashMap.fold (fun acc _ val => val :: acc) [] m

/-! ── Submap ── -/

/-- Restrict to the keys in `ks`. -/
def restrictKeys (m : IntMap v) (ks : List Nat) : IntMap v :=
  let keySet := ks.foldl (fun (s : Std.HashMap Nat Unit) key =>
    Std.HashMap.insert s key ()) (∅ : Std.HashMap Nat Unit)
  Std.HashMap.filter (fun key _ => Std.HashMap.contains keySet key) m

/-- Remove the keys in `ks`. -/
def withoutKeys (m : IntMap v) (ks : List Nat) : IntMap v :=
  let keySet := ks.foldl (fun (s : Std.HashMap Nat Unit) key =>
    Std.HashMap.insert s key ()) (∅ : Std.HashMap Nat Unit)
  Std.HashMap.filter (fun key _ => !(Std.HashMap.contains keySet key)) m

/-- Is `m1` a submap of `m2` (every key of `m1` maps to the same value in `m2`)? -/
def isSubmapOf [BEq v] (m1 m2 : IntMap v) : Bool :=
  Std.HashMap.fold (fun acc key val =>
    acc && match Std.HashMap.get? m2 key with
    | some v2 => val == v2
    | none => false) true m1

/-! ── Min / max (O(n); the backing `HashMap` is unordered) ── -/

/-- The smallest key-value pair, or `none` if empty. -/
def lookupMin (m : IntMap v) : Option (Nat × v) :=
  Std.HashMap.fold (fun acc key val =>
    match acc with
    | none => some (key, val)
    | some (k', _) => if key < k' then some (key, val) else acc) none m

/-- The largest key-value pair, or `none` if empty. -/
def lookupMax (m : IntMap v) : Option (Nat × v) :=
  Std.HashMap.fold (fun acc key val =>
    match acc with
    | none => some (key, val)
    | some (k', _) => if key > k' then some (key, val) else acc) none m

/-! ── Instances ── -/

instance [Repr v] : Repr (IntMap v) where
  reprPrec m _ :=
    let pairs := (toAscList m).map (fun (k, v) => repr k ++ " := " ++ repr v)
    "IntMap.fromList [" ++ Std.Format.joinSep pairs ", " ++ "]"

end IntMap
end Data
