/-
  Tests for `Linen.Data.Fixed` — type-level-precision fixed-point decimals.
-/
import Linen.Data.Fixed

open Data

namespace Tests.Data.Fixed

/-! ### scale & construction -/

#guard Fixed.scale 0 == 1
#guard Fixed.scale 2 == 100
#guard Fixed.scale 4 == 10000
#guard (Fixed.fromInt (p := 2) 3).raw == 300        -- 3 stored as 300
#guard (1 : Fixed 2).raw == 100                      -- one = 10^p
#guard (0 : Fixed 2).raw == 0

/-! ### exact addition / subtraction / negation -/

#guard (((⟨314⟩ : Fixed 2)) + ⟨86⟩).raw == 400       -- 3.14 + 0.86 = 4.00
#guard (((⟨314⟩ : Fixed 2)) - ⟨14⟩).raw == 300       -- 3.14 - 0.14 = 3.00
#guard (-(⟨314⟩ : Fixed 2)).raw == -314

/-! ### multiplication rescales (and truncates) -/

#guard (((⟨200⟩ : Fixed 2)) * ⟨300⟩).raw == 600      -- 2.00 * 3.00 = 6.00
#guard (((⟨314⟩ : Fixed 2)) * ⟨314⟩).raw == 985      -- 3.14² = 9.8596 ↦ 9.85

/-! ### display -/

#guard toString (⟨314⟩ : Fixed 2) == "3.14"
#guard toString (⟨-314⟩ : Fixed 2) == "-3.14"
#guard toString (⟨5⟩ : Fixed 2) == "0.05"            -- fractional zero-padding
#guard toString (⟨100⟩ : Fixed 2) == "1.00"
#guard toString (⟨42⟩ : Fixed 0) == "42"             -- no fractional part
#guard toString (⟨31400⟩ : Fixed 4) == "3.1400"

/-! ### BEq -/

#guard (⟨314⟩ : Fixed 2) == ⟨314⟩
#guard !((⟨314⟩ : Fixed 2) == ⟨315⟩)

/-! ### exact conversion to core `Rat` -/

#guard (Fixed.toRat (⟨314⟩ : Fixed 2)).num == 157     -- 314/100 = 157/50
#guard (Fixed.toRat (⟨314⟩ : Fixed 2)).den == 50
#guard (Fixed.toRat (⟨300⟩ : Fixed 2)).num == 3       -- 300/100 = 3/1
#guard (Fixed.toRat (⟨-50⟩ : Fixed 2)).num == -1      -- -50/100 = -1/2
#guard (Fixed.toRat (⟨-50⟩ : Fixed 2)).den == 2

/-! ### laws (compile-time) -/

example (a b : Fixed 2) : (a + b).raw = a.raw + b.raw := Fixed.add_exact a b
example (a b : Fixed 2) : (a - b).raw = a.raw - b.raw := Fixed.sub_exact a b
example (a : Fixed 2) : -(-a) = a := Fixed.neg_neg a
example : (Fixed.fromInt (p := 2) 0).raw = 0 := Fixed.fromInt_zero

end Tests.Data.Fixed
