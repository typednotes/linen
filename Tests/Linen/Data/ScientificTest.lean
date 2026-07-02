/-
  Tests for `Linen.Data.Scientific`.
-/
import Linen.Data.Scientific

open Data

namespace Tests.Data.Scientific

-- Construction and normalization.
#guard Scientific.scientific 123 (-2) == Scientific.scientific 123 (-2)
#guard (Scientific.scientific 1200 0).normalize == Scientific.scientific 12 2
#guard (Scientific.scientific 0 5).normalize == Scientific.scientific 0 0
#guard Scientific.fromInt 42 == Scientific.scientific 42 0

-- Predicates.
#guard Scientific.isZero (Scientific.scientific 0 3) == true
#guard Scientific.isZero (Scientific.scientific 1 0) == false
#guard Scientific.isInteger (Scientific.scientific 12 2) == true
#guard Scientific.isInteger (Scientific.scientific 12 (-1)) == false

-- Float round trip (best-effort; tolerate the last-bit error from repeated
-- float division in `toRealFloat`'s digit-by-digit scaling).
#guard Float.abs (Scientific.toRealFloat (Scientific.scientific 314 (-2)) - 3.14) < 1e-9
#guard Scientific.fromFloatDigits 3.14 == Scientific.scientific 314 (-2)
#guard Scientific.fromFloatDigits 0.0 == Scientific.scientific 0 0

-- Bounded integer conversion.
#guard Scientific.toBoundedInteger (Scientific.scientific 42 0) == some 42
#guard Scientific.toBoundedInteger (Scientific.scientific 42 (-1)) == none

-- Decimal digit decomposition: 123.45 -> ([1,2,3,4,5], 3).
#guard Scientific.toDecimalDigits (Scientific.scientific 12345 (-2)) == ([1, 2, 3, 4, 5], 3)
#guard Scientific.toDecimalDigits (Scientific.scientific 0 0) == ([0], 1)

-- Arithmetic.
#guard Scientific.scientific 12 (-1) + Scientific.scientific 5 0 == Scientific.scientific 62 (-1)
#guard Scientific.scientific 5 0 - Scientific.scientific 2 0 == Scientific.scientific 3 0
#guard Scientific.scientific 3 1 * Scientific.scientific 2 (-1) == Scientific.scientific 6 0
#guard -Scientific.scientific 5 0 == Scientific.scientific (-5) 0

-- Comparison.
#guard compare (Scientific.scientific 1 0) (Scientific.scientific 2 0) == Ordering.lt
#guard compare (Scientific.scientific (-1) 0) (Scientific.scientific 1 0) == Ordering.lt
#guard compare (Scientific.scientific 20 (-1)) (Scientific.scientific 2 0) == Ordering.eq

-- OfNat / OfScientific literals.
#guard (3 : Scientific) == Scientific.scientific 3 0
#guard (3.14 : Scientific) == Scientific.scientific 314 (-2)

-- ToString formatting.
#guard toString (Scientific.scientific 123 3) == "123000.0"
#guard toString (Scientific.scientific 123 (-2)) == "1.23"
#guard toString (Scientific.scientific (-42) 0) == "-42.0"
#guard toString (Scientific.scientific 5 (-3)) == "0.005"
#guard toString (Scientific.scientific 0 0) == "0.0"

end Tests.Data.Scientific
