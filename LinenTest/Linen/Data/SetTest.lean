/-
  Tests for `Linen.Data.Set` — Haskell `Data.Set` (`Set'`) over `Lean.RBMap _ Unit`.

  The set is ordered, so `toList'` is ascending and deduplicated.
-/
import Linen.Data.Set

open Data

namespace Tests.Data.Set

/-- A small fixture: `{1, 2, 3}`. -/
private def s : Set' Nat := Set'.fromList [3, 1, 2, 1]   -- order/dups irrelevant

/-! ### query / construction -/

#guard Set'.member 2 s == true
#guard Set'.member 9 s == false
#guard Set'.null s == false
#guard Set'.null (Set'.empty : Set' Nat) == true
#guard Set'.size' s == 3                                   -- duplicate 1 collapsed
#guard Set'.toList' (Set'.singleton 7) == [7]
#guard Set'.toList' s == [1, 2, 3]                         -- ascending, deduped

/-! ### insert / delete -/

#guard Set'.toList' (Set'.insert' 4 s) == [1, 2, 3, 4]
#guard Set'.toList' (Set'.insert' 2 s) == [1, 2, 3]        -- already present
#guard Set'.member 2 (Set'.delete 2 s) == false
#guard Set'.toList' (Set'.delete 2 s) == [1, 3]

/-! ### combine -/

#guard Set'.toList' (Set'.union (Set'.fromList [1, 2]) (Set'.fromList [2, 3])) == [1, 2, 3]
#guard Set'.toList' (Set'.intersection (Set'.fromList [1, 2, 3]) (Set'.fromList [2, 3, 4])) == [2, 3]
#guard Set'.toList' (Set'.difference (Set'.fromList [1, 2, 3]) (Set'.fromList [2])) == [1, 3]
#guard Set'.isSubsetOf (Set'.fromList [1, 2]) s == true
#guard Set'.isSubsetOf (Set'.fromList [1, 9]) s == false

/-! ### traversal -/

#guard Set'.toList' (Set'.mapSet (· + 10) s) == [11, 12, 13]
#guard Set'.toList' (Set'.mapSet (fun _ => 0) s) == [0]     -- collisions collapse
#guard Set'.toList' (Set'.filter (· != 2) s) == [1, 3]
#guard Set'.foldl (· + ·) 0 s == 6
#guard Set'.foldr (fun x acc => x :: acc) [] s == [1, 2, 3]

/-! ### min / max -/

#guard Set'.findMin s == some 1
#guard Set'.findMax s == some 3
#guard Set'.findMin (Set'.empty : Set' Nat) == none

/-! ### instances -/

#guard (Set'.fromList [1, 2, 3] : Set' Nat) == Set'.fromList [3, 2, 1]   -- order-independent
#guard ((Set'.fromList [1, 2] : Set' Nat) == Set'.fromList [1, 2, 3]) == false
#guard Set'.null (∅ : Set' Nat) == true

/-! ### proofs (compile-time) -/

example : Set'.null (Set'.empty : Set' Nat) = true := Set'.null_empty
example (x : Nat) : Set'.member x (Set'.empty : Set' Nat) = false := Set'.member_empty x
example : Set'.size' (Set'.empty : Set' Nat) = 0 := Set'.size_empty

end Tests.Data.Set
