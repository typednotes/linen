/-
  Tests for `Linen.System.GitFn.Worker`: the wire protocol. Spawning and
  calling workers over stdio and HTTP is exercised end to end by
  `lake exe gitfn-integration`.
-/
import Linen.System.GitFn.Worker

open System.GitFn Lean

namespace Tests.System.GitFn.Worker

deriving instance BEq for Except

#guard request [(1 : Nat), "a"] == "{\"args\":[1,\"a\"]}"
#guard request [] == "{\"args\":[]}"
#guard reply "{\"ok\":{\"x\":11}}" == .ok (Json.mkObj [("x", (11 : Nat))])
#guard reply "{\"error\":\"too big\"}" == .error "too big"
#guard reply "{\"what\":1}" == .error "malformed reply: {\"what\":1}"
#guard (reply "nope") matches .error _
-- Numbers stay exact on the wire (Lean core's JSON numbers are decimals).
#guard reply "{\"ok\":123456789012345678901234567890}" ==
  .ok (Json.num (123456789012345678901234567890 : Nat))
#guard (decodeResult (.ok (Json.num 7)) : Except String Nat) == .ok 7
#guard (decodeResult (.ok (Json.str "x")) : Except String Nat) matches .error _
#guard (decodeResult (.error "boom") : Except String Nat) == .error "boom"

end Tests.System.GitFn.Worker
