/-
  Tests for `Linen.Network.HTTP.Client.Contrib`.
-/
import Linen.Network.HTTP.Client.Contrib

open Network.HTTP.Client
open Network.HTTP.Client.Contrib
open Network.HTTP.Types

namespace Tests.Network.HTTP.Client.Contrib

private structure Greeting where
  message : String
deriving Repr, BEq

private instance : Data.Json.FromJSON Greeting where
  parseJSON
    | .object fields =>
      match fields.find? (·.1 == "message") with
      | some (_, .string s) => .ok { message := s }
      | _ => .error "missing message field"
    | _ => .error "expected object"

private instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .error a, .error b => a == b
    | .ok a, .ok b => a == b
    | _, _ => false

private def mkResponse (status : Status) (body : String) : Response :=
  { statusCode := status, headers := [], body := body.toUTF8 }

-- A 2xx response yields its body unchanged.
#guard handleResponse (mkResponse status200 "hello") == .ok "hello".toUTF8

-- A non-2xx response with a body surfaces that body as the error.
#guard handleResponse (mkResponse status404 "not found") == .error "not found".toUTF8

-- A non-2xx response with an empty body surfaces the status line instead.
#guard handleResponse (mkResponse status500 "") == .error "500 Internal Server Error".toUTF8

-- `handleResponseJSON` decodes a successful JSON body.
#guard handleResponseJSON (mkResponse status200 "{\"message\":\"hi\"}")
  == (Except.ok { message := "hi" } : Except ByteArray Greeting)

-- `handleResponseJSON` propagates the `handleResponse` failure for a non-2xx status.
#guard handleResponseJSON (mkResponse status404 "nope")
  == (Except.error "nope".toUTF8 : Except ByteArray Greeting)

end Tests.Network.HTTP.Client.Contrib
