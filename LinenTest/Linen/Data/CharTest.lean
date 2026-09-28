/-
  Tests for `Linen.Data.Char`.

  The predicates and conversions that core lacks. (Haskell names already in core
  — `isAlphanum`, `isHexDigit`, `toNat`/`ord`, … — are exercised at the bottom
  to document that they aren't re-ported.)
-/
import Linen.Data.Char

open Data.Char'

namespace Tests.Data.Char

/-! ### Range predicates -/

#guard isAscii 'A' == true
#guard isAscii (Char.ofNat 200) == false
#guard isLatin1 (Char.ofNat 255) == true
#guard isLatin1 (Char.ofNat 256) == false
#guard isControl '\n' == true
#guard isControl 'A' == false
#guard isControl (Char.ofNat 127) == true
#guard isPrint 'A' == true
#guard isPrint '\n' == false

/-! ### Digit / case / punctuation predicates -/

#guard isOctDigit '7' == true
#guard isOctDigit '8' == false
#guard isAsciiUpper 'A' == true
#guard isAsciiUpper 'a' == false
#guard isAsciiLower 'a' == true
#guard isAsciiLower 'A' == false
#guard isPunctuation '!' == true
#guard isPunctuation 'a' == false
#guard isPunctuation '0' == false

/-! ### Conversions -/

#guard (digitToInt '0').map (·.val) == some 0
#guard (digitToInt '7').map (·.val) == some 7
#guard (digitToInt 'a').map (·.val) == some 10
#guard (digitToInt 'F').map (·.val) == some 15
#guard (digitToInt 'g').isNone
#guard intToDigit 5 == '5'
#guard intToDigit 10 == 'a'
#guard intToDigit 15 == 'f'

/-! ### Proofs (compile-time) -/

example (c : Char) (h : isAscii c = true) : c.toNat < 128 := isAscii_bound c h
example : digitToInt (intToDigit 12) = some ⟨12, by omega⟩ := digitToInt_intToDigit 12 (by omega)

/-! ### Already in core — not re-ported -/

#guard 'A'.isAlphanum == true      -- Haskell `isAlphaNum`
#guard 'f'.isHexDigit == true      -- Haskell `isHexDigit`
#guard 'A'.toNat == 65             -- Haskell `ord`
#guard (Char.ofNat 65) == 'A'      -- Haskell `chr`

end Tests.Data.Char
