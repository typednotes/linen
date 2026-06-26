/-
  Tests for `Linen.Data.Ix`.

  `range` / `rangeSize` / `index` / `inRange` across the `Nat`, `Int`, `Char`,
  `Bool`, and product instances, plus the `inRange`/`index` consistency law.
-/
import Linen.Data.Ix

open Data

namespace Tests.Data.Ix

/-! ### Nat -/

#guard Ix.range ((2 : Nat), 5) == [2, 3, 4, 5]
#guard Ix.range ((5 : Nat), 2) == []                       -- empty when lo > hi
#guard Ix.rangeSize ((2 : Nat), 5) == 4
#guard (Ix.index ((2 : Nat), 5) 4).map (·.val) == some 2   -- 4 is at position 2
#guard (Ix.index ((2 : Nat), 5) 6).isNone                  -- out of range
#guard Ix.inRange ((2 : Nat), 5) 3 == true
#guard Ix.inRange ((2 : Nat), 5) 6 == false

/-! ### Int -/

#guard Ix.range ((-1 : Int), 2) == [-1, 0, 1, 2]
#guard Ix.rangeSize ((-1 : Int), 2) == 4
#guard (Ix.index ((-1 : Int), 2) 1).map (·.val) == some 2

/-! ### Char -/

#guard Ix.range ('a', 'd') == ['a', 'b', 'c', 'd']
#guard (Ix.index ('a', 'd') 'c').map (·.val) == some 2

/-! ### Bool -/

#guard Ix.range (false, true) == [false, true]
#guard Ix.rangeSize (true, false) == 0

/-! ### Product (row-major) -/

#guard Ix.range (((0 : Nat), (0 : Nat)), ((1 : Nat), (1 : Nat))) == [(0, 0), (0, 1), (1, 0), (1, 1)]
#guard Ix.rangeSize (((0 : Nat), (0 : Nat)), ((1 : Nat), (2 : Nat))) == 6        -- 2 × 3
#guard (Ix.index (((0 : Nat), (0 : Nat)), ((1 : Nat), (1 : Nat))) (1, 0)).map (·.val) == some 2  -- 1·2 + 0

/-! ### Consistency law (compile-time) -/

example (b : Nat × Nat) (x : Nat) : Ix.inRange b x = (Ix.index b x).isSome :=
  Ix.inRange_iff_index_isSome_nat b x

end Tests.Data.Ix
