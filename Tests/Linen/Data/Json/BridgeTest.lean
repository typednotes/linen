/-
  Tests for `Linen.Data.Json.Bridge`: converting between linen's and Lean
  core's JSON, including exactly where they differ.
-/
import Linen.Data.Json.Bridge

open Data.Json

namespace Tests.Data.Json.Bridge

deriving instance BEq for Except

/-- A linen value survives a trip through Lean core's JSON. -/
def roundTrips (v : Value) : Bool :=
  match v.toLeanJson >>= Value.ofLeanJson with
  | .ok w => w == v
  | .error _ => false

def sample : Value := .object [
  ("name", .string "a \"quoted\" / slashed\nline"), ("ok", .bool true), ("none", .null),
  ("n", .number 42), ("half", .number 0.5), ("neg", .number (-3)),
  ("list", .array #[.number 1, .string "x", .array #[]]), ("obj", .object [("k", .number 2)])]

-- Everything linen holds survives (keys sorted, since Lean core sorts them).
#guard roundTrips (.string "é ✓") && roundTrips .null && roundTrips (.bool false)
#guard roundTrips (.number 9007199254740992) && roundTrips (.number 0.1)
#guard roundTrips (.object [("a", .number 1), ("b", .array #[.null])])
#guard match sample.toLeanJson with
  | .ok j => j.getObjValAs? String "name" == .ok "a \"quoted\" / slashed\nline" &&
      j.getObjValAs? Nat "n" == .ok 42
  | .error _ => false
-- Lean core's values arrive in linen — numbers beyond a double rounded, as documented.
#guard Value.ofLeanJson (Lean.Json.num 12345) == .ok (.number 12345)
#guard Value.ofLeanJson (Lean.Json.str "x") == .ok (.string "x")
#guard Value.ofLeanJson (Lean.Json.num (9007199254740993 : Nat)) == .ok (.number 9007199254740992)
-- Duplicate keys: Lean core keeps the last one.
#guard match (Value.object [("k", .number 1), ("k", .number 2)]).toLeanJson with
  | .ok j => j.getObjValAs? Nat "k" == .ok 2
  | .error _ => false
-- A non-finite number becomes `null`, as linen's encoder writes it.
#guard (Value.number (0.0 / 0.0)).toLeanJson == .ok Lean.Json.null
-- Non-integer numbers keep 6 significant digits (linen's encoder), as documented.
#guard (Value.number 0.123456789012345).toLeanJson == Lean.Json.parse "0.123457"

end Tests.Data.Json.Bridge
