/-
  Tests for `Linen.Data.Base64` — RFC 4648 codec over core `ByteArray`.

  Decoded `ByteArray`s are compared via `.toList` (so no `BEq ByteArray` is
  assumed).
-/
import Linen.Data.Base64

open Data.Base64

namespace Tests.Data.Base64

/-! ### encode — canonical vectors + padding -/

#guard encode "".toUTF8 == ""
#guard encode "M".toUTF8 == "TQ=="            -- 1-byte tail → two pads
#guard encode "Ma".toUTF8 == "TWE="           -- 2-byte tail → one pad
#guard encode "Man".toUTF8 == "TWFu"          -- exact triple → no pad
#guard encode "hello".toUTF8 == "aGVsbG8="
#guard encode "foobar".toUTF8 == "Zm9vYmFy"

/-! ### encode emits only alphabet characters -/

#guard (encode "hello".toUTF8).toList.all fun c =>
  decide (('A'.toNat ≤ c.toNat ∧ c.toNat ≤ 'Z'.toNat)
        ∨ ('a'.toNat ≤ c.toNat ∧ c.toNat ≤ 'z'.toNat)
        ∨ ('0'.toNat ≤ c.toNat ∧ c.toNat ≤ '9'.toNat)
        ∨ c = '+' ∨ c = '/' ∨ c = '=')

/-! ### decode — roundtrips (compare byte lists) -/

#guard (decode "TWFu").map (·.toList) == some "Man".toUTF8.toList
#guard (decode "TWE=").map (·.toList) == some "Ma".toUTF8.toList
#guard (decode "TQ==").map (·.toList) == some "M".toUTF8.toList
#guard (decode "").map (·.toList) == some ([] : List UInt8)
#guard (decode (encode "hello world".toUTF8)).map (·.toList) == some "hello world".toUTF8.toList
#guard (decode (encode "any carnal pleasure.".toUTF8)).map (·.toList)
        == some "any carnal pleasure.".toUTF8.toList

/-! ### decode ignores whitespace -/

#guard (decode "TW\nFu").map (·.toList) == some "Man".toUTF8.toList
#guard (decode "TW Fu").map (·.toList) == some "Man".toUTF8.toList

/-! ### decode rejects invalid input -/

#guard (decode "TQ=").isNone        -- length not a multiple of 4
#guard (decode "****").isNone       -- characters outside the alphabet
#guard (decode "AB!=").isNone       -- '!' is invalid
#guard (decode "TQ==TQ==").isNone   -- '=' padding only allowed in the final group

end Tests.Data.Base64
