/-
  Tests for `Linen.Data.Vector` — the `Array` extensions Haskell's
  `Data.Vector` needs beyond what `Array` already provides.
-/
import Linen.Data.Vector

namespace Tests.Data.Vector

/-! ### Construction -/

#guard Array.generate 4 (· * 2) == #[0, 2, 4, 6]
#guard Array.generate 0 (· * 2) == (#[] : Array Nat)

/-! ### Filtering -/

#guard Array.ifilter (fun i _ => i % 2 == 0) (#[10, 20, 30, 40] : Array Nat) == #[10, 30]

/-! ### Folding -/

#guard Array.foldl1' (· + ·) (#[1, 2, 3] : Array Nat) == some 6
#guard Array.foldl1' (· + ·) (#[] : Array Nat) == none
#guard Array.foldr1 (· - ·) (#[1, 2, 3] : Array Int) == some (1 - (2 - 3))
#guard Array.foldr1 (· + ·) (#[] : Array Nat) == none
#guard Array.foldr1 (· + ·) (#[7] : Array Nat) == some 7
#guard Array.ifoldl' (fun acc i x => acc + i + x) 0 (#[10, 10, 10] : Array Nat) == 33
#guard Array.ifoldr (fun i x acc => i + x + acc) 0 (#[10, 10, 10] : Array Nat) == 33

/-! ### Boolean / numeric reductions -/

#guard Array.and #[true, true, true] == true
#guard Array.and #[true, false] == false
#guard Array.or #[false, false, true] == true
#guard Array.or #[false, false] == false
#guard Array.product (#[1, 2, 3, 4] : Array Nat) == 24
#guard Array.product (#[] : Array Nat) == 1

/-! ### Search -/

#guard Array.notElem (2 : Nat) #[1, 2, 3] == false
#guard Array.notElem (9 : Nat) #[1, 2, 3] == true

/-! ### Reordering / slicing -/

#guard Array.backpermute (α := Nat) #[10, 20, 30] #[2, 0, 1] == #[30, 10, 20]
#guard Array.slice 1 2 (#[10, 20, 30, 40] : Array Nat) == #[20, 30]

/-! ### Laws -/

example (n : Nat) (f : Nat → Nat) : (Array.generate n f).size = n := Array.size_generate n f
example (v : Array Nat) (is : Array Nat) :
    (Array.backpermute v is).size = is.size := Array.size_backpermute v is

end Tests.Data.Vector
