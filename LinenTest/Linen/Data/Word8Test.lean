/-
  Tests for `Linen.Data.Word8`.
-/
import Linen.Data.Word8

open Data.Word8

namespace Tests.Data.Word8

/-! ### Classification predicates -/

#guard isUpper _A == true
#guard isUpper _a == false
#guard isLower _a == true
#guard isLower _A == false
#guard isAlpha _A == true
#guard isAlpha _0 == false
#guard isDigit _0 == true
#guard isDigit _A == false
#guard isAlphaNum _0 == true
#guard isAlphaNum _a == true
#guard isAlphaNum _space == false
#guard isSpace _space == true
#guard isSpace _tab == true
#guard isSpace _A == false
#guard isControl _nul == true
#guard isControl _del == true
#guard isControl _space == false
#guard isPrint _space == true
#guard isPrint _tilde == true
#guard isPrint _del == false
#guard isHexDigit _9 == true
#guard isHexDigit _F == true
#guard isHexDigit _f == true
#guard isHexDigit _G == false
#guard isOctDigit _7 == true
#guard isOctDigit _8 == false
#guard isAscii _del == true
#guard isAscii (128 : UInt8) == false

/-! ### Case conversion -/

#guard toLower _A == _a
#guard toLower _a == _a
#guard toUpper _a == _A
#guard toUpper _A == _A

example : toLower (toLower _A) = toLower _A := toLower_idempotent _A
example : toUpper (toUpper _a) = toUpper _a := toUpper_idempotent _a
example (h : isUpper _A = true) : isLower (toLower _A) = true := isUpper_toLower _A h
example (h : isLower _a = true) : isUpper (toUpper _a) = true := isLower_toUpper _a h

/-! ### Byte constants -/

#guard _nul == 0
#guard _space == 32
#guard _0 == 48
#guard _9 == 57
#guard _A == 65
#guard _Z == 90
#guard _a == 97
#guard _z == 122
#guard _del == 127
#guard _colon == 58
#guard _slash == 47
#guard _underscore == 95

end Tests.Data.Word8
