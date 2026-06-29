/-
  Tests for `Linen.Data.Map` — Haskell `Data.Map` over `Lean.RBMap`.

  The map is ordered, so `toList'`/`keys`/`elems` are ascending directly.
-/
import Linen.Data.Map

open Data

namespace Tests.Data.Map

/-- A small fixture: `{1 ↦ 10, 2 ↦ 20, 3 ↦ 30}`. -/
private def m : Map Nat Nat := Map.fromList [(1, 10), (2, 20), (3, 30)]

/-! ### query -/

#guard Map.lookup 2 m == some 20
#guard Map.lookup 9 m == none
#guard Map.findWithDefault 0 2 m == 20
#guard Map.findWithDefault 0 9 m == 0
#guard Map.member 3 m == true
#guard Map.member 9 m == false
#guard Map.null m == false
#guard Map.null (Map.empty : Map Nat Nat) == true
#guard Map.size' m == 3
#guard Map.lookup 7 (Map.singleton 7 70) == some 70

/-! ### insert / delete / adjust -/

#guard Map.lookup 4 (Map.insert' 4 40 m) == some 40
#guard Map.lookup 2 (Map.insert' 2 99 m) == some 99
#guard Map.member 2 (Map.delete 2 m) == false
#guard Map.lookup 2 (Map.adjust (· + 5) 2 m) == some 25
#guard Map.toList' (Map.adjust (· + 5) 9 m) == [(1, 10), (2, 20), (3, 30)]

/-! ### combine (results are ascending) -/

#guard Map.toList' (Map.union (Map.fromList [(1, 1), (2, 2)]) (Map.fromList [(2, 99), (3, 3)]))
        == [(1, 1), (2, 2), (3, 3)]
#guard Map.toList' (Map.unionWith (· + ·) (Map.fromList [(1, 1), (2, 2)]) (Map.fromList [(2, 40), (3, 3)]))
        == [(1, 1), (2, 42), (3, 3)]
#guard Map.toList' (Map.intersection (Map.fromList [(1, 1), (2, 2), (3, 3)]) (Map.fromList [(2, 0), (3, 0)]))
        == [(2, 2), (3, 3)]
#guard Map.toList' (Map.intersectionWith (· * ·) (Map.fromList [(1, 2), (2, 3)]) (Map.fromList [(2, 4), (5, 9)]))
        == [(2, 12)]
#guard Map.toList' (Map.difference (Map.fromList [(1, 1), (2, 2), (3, 3)]) (Map.fromList [(2, 0)]))
        == [(1, 1), (3, 3)]

/-! ### traversal -/

#guard Map.foldlWithKey (fun acc k v => acc + k + v) 0 m == 66
#guard Map.foldrWithKey (fun k _ acc => k :: acc) [] m == [1, 2, 3]
#guard Map.toList' (Map.mapValues (· * 2) m) == [(1, 20), (2, 40), (3, 60)]
#guard Map.toList' (Map.mapWithKey (fun k v => k + v) m) == [(1, 11), (2, 22), (3, 33)]
#guard Map.toList' (Map.mapKeys (· + 10) m) == [(11, 10), (12, 20), (13, 30)]
#guard Map.toList' (Map.filterWithKey (fun k _ => k != 2) m) == [(1, 10), (3, 30)]

/-! ### conversion (ascending) -/

#guard Map.toList' m == [(1, 10), (2, 20), (3, 30)]
#guard Map.toAscList m == [(1, 10), (2, 20), (3, 30)]
#guard Map.keys m == [1, 2, 3]
#guard Map.elems m == [10, 20, 30]

/-! ### submap / restrict / without -/

#guard Map.toList' (Map.restrictKeys m [1, 3]) == [(1, 10), (3, 30)]
#guard Map.toList' (Map.withoutKeys m [2]) == [(1, 10), (3, 30)]
#guard Map.isSubmapOf (Map.fromList [(1, 10), (2, 20)]) m == true
#guard Map.isSubmapOf (Map.fromList [(2, 99)]) m == false
#guard Map.isSubmapOf (Map.fromList [(9, 0)]) m == false

/-! ### min / max -/

#guard Map.lookupMin m == some (1, 10)
#guard Map.lookupMax m == some (3, 30)
#guard Map.lookupMin (Map.empty : Map Nat Nat) == none

/-! ### instances -/

#guard (Map.fromList [(1, 10), (2, 20)] : Map Nat Nat) == Map.fromList [(2, 20), (1, 10)]  -- order-independent
#guard ((Map.fromList [(1, 10)] : Map Nat Nat) == Map.fromList [(1, 99)]) == false
#guard Map.null (∅ : Map Nat Nat) == true

/-! ### proofs (compile-time) -/

example : Map.null (Map.empty : Map Nat Nat) = true := Map.null_empty
example (key : Nat) : Map.lookup key (Map.empty : Map Nat Nat) = none := Map.lookup_empty key
example : Map.size' (Map.empty : Map Nat Nat) = 0 := Map.size_empty
example (key : Nat) : Map.member key (Map.empty : Map Nat Nat) = false := Map.member_empty key

end Tests.Data.Map
