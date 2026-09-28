/-
  Tests for `Linen.System.GitFn.Worker`: the wire protocol. Spawning and
  calling workers over stdio and HTTP is exercised end to end by
  `lake exe gitfn-integration`.
-/
import Linen.System.GitFn.Worker

open System.GitFn Lean

namespace Tests.System.GitFn.Worker

deriving instance BEq for Except

-- A call is a JSON-RPC 2.0 request `call`, arguments by position.
#guard request 7 [(1 : Nat), "a"] ==
  "{\"id\":7,\"jsonrpc\":\"2.0\",\"method\":\"call\",\"params\":[1,\"a\"]}"
#guard (Json.parse (request 0 []) >>= (·.getObjValAs? (List Json) "params")) == .ok []
#guard exitNotification == "{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}"
-- A reply is a response to that request, or an error.
#guard reply 3 "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"x\":11}}" == .ok (Json.mkObj [("x", (11 : Nat))])
#guard reply 3 "{\"jsonrpc\":\"2.0\",\"id\":3,\"error\":{\"code\":-32602,\"message\":\"too big\"}}" == .error "too big"
#guard (reply 3 "{\"jsonrpc\":\"2.0\",\"id\":4,\"result\":1}") matches .error _
#guard (reply 3 "{\"what\":1}") matches .error _
#guard (reply 3 "nope") matches .error _
-- Numbers stay exact on the wire (Lean core's JSON numbers are decimals).
#guard reply 0 "{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":123456789012345678901234567890}" ==
  .ok (Json.num (123456789012345678901234567890 : Nat))
#guard (decodeResult (.ok (Json.num 7)) : Except String Nat) == .ok 7
#guard (decodeResult (.ok (Json.str "x")) : Except String Nat) matches .error _
#guard (decodeResult (.error "boom") : Except String Nat) == .error "boom"

end Tests.System.GitFn.Worker
