/-
  Tests for `Linen.CDP.Internal.Utils`.
-/
import Linen.CDP.Internal.Utils

open CDP.Internal.Utils
open Data.Json (Value FromJSON)

namespace Tests.CDP.Internal.Utils

/-! ### ProtocolError.ofCode (JSON-RPC 2.0 §5.1 error codes) -/

#guard ProtocolError.ofCode (-32700) "bad json" == .parseError "bad json"
#guard ProtocolError.ofCode (-32600) "bad req" == .invalidRequest "bad req"
#guard ProtocolError.ofCode (-32601) "no method" == .methodNotFound "no method"
#guard ProtocolError.ofCode (-32602) "bad params" == .invalidParams "bad params"
#guard ProtocolError.ofCode (-32603) "internal" == .internalError "internal"
#guard ProtocolError.ofCode (-32050) "server" == .serverError "server"
#guard ProtocolError.ofCode (-1) "?" == .other "?"

#guard toString (ProtocolError.methodNotFound "Foo.bar") ==
  "Method not found protocol error:\nFoo.bar"

/-! ### ProtocolError FromJSON -/

#guard FromJSON.parseJSON (α := ProtocolError)
    (Value.object [("code", .number (-32601)), ("message", .string "no such method")])
  = .ok (.methodNotFound "no such method")

/-! ### Error rendering -/

#guard toString (Error.protocol (.invalidParams "x")) ==
  "error encountered by the browser:\nInvalid params protocol error:\nx"
#guard toString Error.noResponse == "no response received from the browser"

/-! ### Config defaults -/

#guard (Data.Default.default : Config).hostPort == ("http://127.0.0.1", 9222)
#guard (Data.Default.default : Config).connectToBrowser == false
#guard (Data.Default.default : Config).commandTimeout == none

end Tests.CDP.Internal.Utils
