/-
  Tests for `Linen.Data.Json.Decode`.

  Covers `decode` (all value kinds, whitespace, escapes, numbers, nesting,
  and error cases) and `decodeAs`, plus encode/decode roundtrips.
-/
import Linen.Data.Json

open Data.Json

namespace Tests.Json.Decode

-- ── Literals ──────────────────────────────────────────────────────────

#guard Decode.decode "null" = .ok .null
#guard Decode.decode "true" = .ok (.bool true)
#guard Decode.decode "false" = .ok (.bool false)
#guard Decode.decode "\"hello\"" = .ok (.string "hello")
#guard Decode.decode "\"\"" = .ok (.string "")

-- ── Numbers ───────────────────────────────────────────────────────────

#guard Decode.decode "0" = .ok (.number 0)
#guard Decode.decode "42" = .ok (.number 42)
#guard Decode.decode "-7" = .ok (.number (-7))
#guard Decode.decode "3.5" = .ok (.number 3.5)
#guard Decode.decode "-0.25" = .ok (.number (-0.25))
#guard Decode.decode "1e3" = .ok (.number 1000)
#guard Decode.decode "2.5E2" = .ok (.number 250)
#guard Decode.decode "1e-2" = .ok (.number 0.01)

-- ── Whitespace handling ───────────────────────────────────────────────

#guard Decode.decode "  \n\t 42 \r\n " = .ok (.number 42)
#guard Decode.decode " [ 1 , 2 ] " = .ok (.array #[.number 1, .number 2])

-- ── String escapes ────────────────────────────────────────────────────

#guard Decode.decode "\"a\\nb\"" = .ok (.string "a\nb")       -- \n
#guard Decode.decode "\"a\\tb\"" = .ok (.string "a\tb")       -- \t
#guard Decode.decode "\"a\\\"b\"" = .ok (.string "a\"b")      -- \"
#guard Decode.decode "\"a\\\\b\"" = .ok (.string "a\\b")      -- \\
#guard Decode.decode "\"a\\/b\"" = .ok (.string "a/b")        -- \/
#guard Decode.decode "\"\\u0041\"" = .ok (.string "A")        -- \uXXXX (BMP)
#guard Decode.decode "\"\\uD83D\\uDE00\"" = .ok (.string (String.singleton (Char.ofNat 0x1F600)))  -- surrogate pair → 😀

-- ── Arrays and objects ────────────────────────────────────────────────

#guard Decode.decode "[]" = .ok (.array #[])
#guard Decode.decode "{}" = .ok (.object [])
#guard Decode.decode "[1,2,3]" = .ok (.array #[.number 1, .number 2, .number 3])
#guard Decode.decode "{\"a\":1,\"b\":true}" = .ok (.object [("a", .number 1), ("b", .bool true)])
#guard Decode.decode "{\"nested\":{\"xs\":[1,null]}}"
  = .ok (.object [("nested", .object [("xs", .array #[.number 1, .null])])])

-- ── Error cases ───────────────────────────────────────────────────────

#guard (Decode.decode "\"unterminated").toOption = none      -- missing closing quote
#guard (Decode.decode "42 trailing").toOption = none         -- trailing content
#guard (Decode.decode "tru").toOption = none                 -- truncated keyword
#guard (Decode.decode "").toOption = none                    -- empty input
#guard (Decode.decode "[1,2").toOption = none                -- unterminated array
#guard (Decode.decode "{\"a\":}").toOption = none            -- missing value
#guard (Decode.decode "-").toOption = none                   -- no digits after sign
#guard (Decode.decode "@").toOption = none                   -- unexpected character
#guard (Decode.decode "01").toOption = none                  -- leading zero
#guard (Decode.decode "-01").toOption = none                 -- leading zero, negative

-- ── decodeAs ──────────────────────────────────────────────────────────

#guard (Decode.decodeAs "42" : Except String Int) = .ok 42
#guard (Decode.decodeAs "9" : Except String Nat) = .ok 9
#guard (Decode.decodeAs "3.5" : Except String Float) = .ok 3.5
#guard (Decode.decodeAs "\"hi\"" : Except String String) = .ok "hi"
#guard (Decode.decodeAs "true" : Except String Bool) = .ok true
#guard (Decode.decodeAs "[1,2,3]" : Except String (List Int)) = .ok [1, 2, 3]
#guard (Decode.decodeAs "[1,2,3]" : Except String (Array Int)) = .ok #[1, 2, 3]
#guard (Decode.decodeAs "null" : Except String (Option Int)) = .ok none
#guard (Decode.decodeAs "5" : Except String (Option Int)) = .ok (some 5)
#guard ((Decode.decodeAs "\"x\"" : Except String Int).toOption = none)  -- wrong target type

-- ── Encode/decode roundtrips ──────────────────────────────────────────

private def roundtrips (v : Value) : Bool := Decode.decode (Encode.encode v) = .ok v

#guard roundtrips .null
#guard roundtrips (.bool true)
#guard roundtrips (.string "with \"quotes\" and \n newline")
#guard roundtrips (.number 123)
#guard roundtrips (.number (-4.5))
#guard roundtrips (.array #[.null, .bool false, .string "test", .number 1])
#guard roundtrips (.object [("k", .string "v"), ("nested", .array #[.number 1, .object [("x", .null)]])])

end Tests.Json.Decode
