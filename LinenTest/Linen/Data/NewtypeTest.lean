/-
  Tests for `Linen.Data.Newtype`.

  The semigroup/monoid newtype wrappers and their `Append` instances.
-/
import Linen.Data.Newtype

open Data

namespace Tests.Data.Newtype

/-! ### Dual (reverses) / Endo (composition) -/

#guard (Dual.mk "a" ++ Dual.mk "b").getDual == "ba"
#guard ((Endo.mk (· + 1) ++ Endo.mk (· * 2)).appEndo 5) == 11      -- (·+1) ∘ (·*2): 5→10→11

/-! ### First / Last -/

#guard (First.mk (some 1) ++ First.mk (some 2)).getFirst == some 1
#guard (First.mk (none : Option Nat) ++ First.mk (some 2)).getFirst == some 2
#guard (Last.mk (some 1) ++ Last.mk (some 2)).getLast == some 2
#guard (Last.mk (some 1) ++ Last.mk (none : Option Nat)).getLast == some 1

/-! ### Sum / Product (numeric monoids) -/

#guard (Sum.mk 3 ++ Sum.mk 4).getSum == 7
#guard (Product.mk 3 ++ Product.mk 4).getProduct == 12

/-! ### All / Any (boolean monoids) -/

#guard (All.mk true ++ All.mk false).getAll == false
#guard (All.mk true ++ All.mk true).getAll == true
#guard (Any.mk false ++ Any.mk true).getAny == true
#guard (Any.mk false ++ Any.mk false).getAny == false

/-! ### ToString -/

#guard toString (Sum.mk 5) == "Sum(5)"
#guard toString (All.mk true) == "All(true)"

/-! ### Associativity (compile-time) -/

example (a b c : All) : a ++ b ++ c = a ++ (b ++ c) := All.append_assoc a b c
example (a b c : Sum Nat) : a ++ b ++ c = a ++ (b ++ c) := Sum.append_assoc a b c

end Tests.Data.Newtype
