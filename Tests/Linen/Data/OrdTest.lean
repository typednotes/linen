/-
  Tests for `Linen.Data.Ord`.

  `Down` (reversed ordering) and the proof-carrying `clamp`. (`comparing` is core's
  `compareOn`, exercised at the bottom to document it isn't re-ported.)
-/
import Linen.Data.Ord

open Data

namespace Tests.Data.Ord

/-! ### Down: reversed ordering -/

#guard compare (Down.mk 1) (Down.mk 2) == Ordering.gt          -- reversed
#guard compare (Down.mk 2) (Down.mk 1) == Ordering.lt
#guard compare (Down.mk 5) (Down.mk 5) == Ordering.eq
#guard (Down.mk 5 == Down.mk 5) == true
#guard (Down.mk 5 == Down.mk 6) == false
#guard toString (Down.mk 7) == "Down(7)"

/-! ### clamp (proof-carrying) -/

#guard (clamp (5 : Nat) 0 10 (by omega) (fun a => Nat.le_refl a) (fun _ _ h => by omega)).val == 5
#guard (clamp (15 : Nat) 0 10 (by omega) (fun a => Nat.le_refl a) (fun _ _ h => by omega)).val == 10
#guard (clamp (0 : Nat) 3 10 (by omega) (fun a => Nat.le_refl a) (fun _ _ h => by omega)).val == 3

/-! ### Proofs (compile-time) -/

example (a : Nat) : (Down.mk a).getDown = a := Down.get_mk a
example (a b : Down Nat) : compare a b = compare b.getDown a.getDown := Down.compare_reverse a b

/-! ### Haskell `comparing` is core's `compareOn` -/

#guard compareOn (·.length) "ab" "ccc" == Ordering.lt          -- by length: 2 < 3

end Tests.Data.Ord
