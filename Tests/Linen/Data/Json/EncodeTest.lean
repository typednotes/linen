/-
  Tests for `Linen.Data.Json.Encode`.

  Covers string escaping, number rendering, `encode`, and `encodePretty`.
-/
import Linen.Data.Json

open Data.Json

namespace Tests.Json.Encode

-- ── escapeString ──────────────────────────────────────────────────────

#guard Encode.escapeString "hi" = "\"hi\""
#guard Encode.escapeString "" = "\"\""
#guard Encode.escapeString "a\"b" = "\"a\\\"b\""        -- double quote → \"
#guard Encode.escapeString "a\\b" = "\"a\\\\b\""        -- backslash → \\
#guard Encode.escapeString "a/b" = "\"a\\/b\""          -- forward slash → \/
#guard Encode.escapeString "a\nb" = "\"a\\nb\""          -- newline → \n
#guard Encode.escapeString "a\tb" = "\"a\\tb\""          -- tab → \t
#guard Encode.escapeString "a\rb" = "\"a\\rb\""          -- carriage return → \r
#guard Encode.escapeString (String.singleton (Char.ofNat 1)) = "\"\\u0001\""  -- control → \uXXXX

-- ── renderNumber ──────────────────────────────────────────────────────

#guard Encode.renderNumber 42 = "42"          -- integer-valued floats render without a point
#guard Encode.renderNumber (-7) = "-7"
#guard Encode.renderNumber 0 = "0"
#guard Encode.renderNumber 3.5 = "3.500000"   -- non-integer keeps Float's textual form
#guard Encode.renderNumber (1.0 / 0.0) = "null"   -- +∞ has no JSON form → null
#guard Encode.renderNumber (0.0 / 0.0) = "null"   -- NaN has no JSON form → null

-- ── encode ────────────────────────────────────────────────────────────

#guard Encode.encode .null = "null"
#guard Encode.encode (.bool true) = "true"
#guard Encode.encode (.bool false) = "false"
#guard Encode.encode (.string "hi") = "\"hi\""
#guard Encode.encode (.number 42) = "42"
#guard Encode.encode (.array #[]) = "[]"
#guard Encode.encode (.object []) = "{}"
#guard Encode.encode (.array #[.number 1, .bool true, .null]) = "[1,true,null]"
#guard Encode.encode (.object [("a", .number 1), ("b", .string "x")]) = "{\"a\":1,\"b\":\"x\"}"
#guard Encode.encode (.array #[.array #[.number 1], .object [("k", .null)]]) = "[[1],{\"k\":null}]"

-- ── encodePretty ──────────────────────────────────────────────────────
-- Correctness: pretty output is still valid JSON that decodes to the original.

#guard Decode.decode (Encode.encodePretty (.object [("a", .number 1), ("b", .array #[.number 2, .bool true])]))
  = .ok (.object [("a", .number 1), ("b", .array #[.number 2, .bool true])])
#guard Encode.encodePretty (.array #[]) = "[]"
#guard Encode.encodePretty (.object []) = "{}"

-- Illustration: the exact rendered layout (pinned with `#guard_msgs`).
/-- info: {
  "a": 1,
  "b": [
    2,
    true
  ]
} -/
#guard_msgs in
#eval IO.println <|
  Encode.encodePretty (.object [("a", .number 1), ("b", .array #[.number 2, .bool true])])

end Tests.Json.Encode
