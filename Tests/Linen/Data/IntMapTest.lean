/-
  Tests for `Linen.Data.IntMap` — Haskell `Data.IntMap` over `Std.HashMap Nat`.

  Since the backing `HashMap` is unordered, order-sensitive results are checked
  via `toAscList` / `lookupMin` / `lookupMax`; others via `lookup`.
-/
import Linen.Data.IntMap

open Data

namespace Tests.Data.IntMap

/-- A small fixture: `{1 ↦ 10, 2 ↦ 20, 3 ↦ 30}`. -/
private def m : IntMap Nat := IntMap.fromList [(1, 10), (2, 20), (3, 30)]

/-! ### query -/

#guard IntMap.lookup 2 m == some 20
#guard IntMap.lookup 9 m == none
#guard IntMap.findWithDefault 0 2 m == 20
#guard IntMap.findWithDefault 0 9 m == 0
#guard IntMap.member 3 m == true
#guard IntMap.member 9 m == false
#guard IntMap.null m == false
#guard IntMap.null (IntMap.empty : IntMap Nat) == true
#guard IntMap.size' m == 3
#guard IntMap.lookup 7 (IntMap.singleton 7 70) == some 70

/-! ### insert / delete / adjust -/

#guard IntMap.lookup 4 (IntMap.insert' 4 40 m) == some 40
#guard IntMap.lookup 2 (IntMap.insert' 2 99 m) == some 99      -- replace
#guard IntMap.member 2 (IntMap.delete 2 m) == false
#guard IntMap.lookup 2 (IntMap.adjust (· + 5) 2 m) == some 25
#guard IntMap.toAscList (IntMap.adjust (· + 5) 9 m) == [(1, 10), (2, 20), (3, 30)]  -- absent key: no change

/-! ### combine -/

#guard IntMap.toAscList (IntMap.union (IntMap.fromList [(1, 1), (2, 2)]) (IntMap.fromList [(2, 99), (3, 3)]))
        == [(1, 1), (2, 2), (3, 3)]    -- left-biased on key 2
#guard IntMap.toAscList (IntMap.unionWith (· + ·) (IntMap.fromList [(1, 1), (2, 2)]) (IntMap.fromList [(2, 40), (3, 3)]))
        == [(1, 1), (2, 42), (3, 3)]
#guard IntMap.toAscList (IntMap.intersection (IntMap.fromList [(1, 1), (2, 2), (3, 3)]) (IntMap.fromList [(2, 0), (3, 0)]))
        == [(2, 2), (3, 3)]
#guard IntMap.toAscList (IntMap.intersectionWith (· * ·) (IntMap.fromList [(1, 2), (2, 3)]) (IntMap.fromList [(2, 4), (5, 9)]))
        == [(2, 12)]
#guard IntMap.toAscList (IntMap.difference (IntMap.fromList [(1, 1), (2, 2), (3, 3)]) (IntMap.fromList [(2, 0)]))
        == [(1, 1), (3, 3)]

/-! ### traversal -/

#guard IntMap.foldlWithKey (fun acc k v => acc + k + v) 0 m == 66    -- (1+10)+(2+20)+(3+30)
#guard IntMap.toAscList (IntMap.mapValues (· * 2) m) == [(1, 20), (2, 40), (3, 60)]
#guard IntMap.toAscList (IntMap.mapWithKey (fun k v => k + v) m) == [(1, 11), (2, 22), (3, 33)]
#guard IntMap.toAscList (IntMap.filterWithKey (fun k _ => k != 2) m) == [(1, 10), (3, 30)]

/-! ### sorted views / keys / elems -/

#guard IntMap.toAscList m == [(1, 10), (2, 20), (3, 30)]
#guard ((IntMap.keys m).toArray.qsort (· < ·)).toList == [1, 2, 3]
#guard ((IntMap.elems m).toArray.qsort (· < ·)).toList == [10, 20, 30]
#guard IntMap.lookupMin m == some (1, 10)
#guard IntMap.lookupMax m == some (3, 30)
#guard IntMap.lookupMin (IntMap.empty : IntMap Nat) == none

/-! ### submap / restrict / without -/

#guard IntMap.toAscList (IntMap.restrictKeys m [1, 3]) == [(1, 10), (3, 30)]
#guard IntMap.toAscList (IntMap.withoutKeys m [2]) == [(1, 10), (3, 30)]
#guard IntMap.isSubmapOf (IntMap.fromList [(1, 10), (2, 20)]) m == true
#guard IntMap.isSubmapOf (IntMap.fromList [(2, 99)]) m == false      -- wrong value
#guard IntMap.isSubmapOf (IntMap.fromList [(9, 0)]) m == false       -- missing key

end Tests.Data.IntMap
