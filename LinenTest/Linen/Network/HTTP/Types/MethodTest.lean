/-
  Tests for `Linen.Network.HTTP.Types.Method`.

  HTTP methods, parsing, rendering, and the RFC 9110 §9.2 safe/idempotent
  predicates are pure, so behaviour is checked with `#guard` and the laws with
  `rfl` examples.
-/
import Linen.Network.HTTP.Types.Method

open Network.HTTP.Types

namespace Tests.Network.HTTP.Types.Method

/-! ### StdMethod / Method rendering -/

#guard toString StdMethod.GET == "GET"
#guard toString StdMethod.DELETE == "DELETE"
#guard toString (Method.standard .POST) == "POST"
#guard toString (Method.custom "BREW") == "BREW"

/-! ### parseMethod / renderMethod -/

#guard parseMethod "GET" == Method.standard .GET
#guard parseMethod "PATCH" == Method.standard .PATCH
#guard parseMethod "BREW" == Method.custom "BREW"     -- unknown ⇒ custom
#guard parseMethod "get" == Method.custom "get"       -- case-sensitive
#guard renderMethod (parseMethod "OPTIONS") == "OPTIONS"
#guard renderMethod (parseMethod "BREW") == "BREW"

/-! ### isSafe (RFC 9110 §9.2.1) -/

#guard (Method.standard .GET).isSafe == true
#guard (Method.standard .HEAD).isSafe == true
#guard (Method.standard .OPTIONS).isSafe == true
#guard (Method.standard .TRACE).isSafe == true
#guard (Method.standard .POST).isSafe == false
#guard (Method.standard .PUT).isSafe == false
#guard (Method.custom "BREW").isSafe == false

/-! ### isIdempotent (RFC 9110 §9.2.2) -/

#guard (Method.standard .PUT).isIdempotent == true
#guard (Method.standard .DELETE).isIdempotent == true
#guard (Method.standard .GET).isIdempotent == true        -- safe ⇒ idempotent
#guard (Method.standard .POST).isIdempotent == false
#guard (Method.standard .PATCH).isIdempotent == false
#guard (Method.custom "BREW").isIdempotent == false

/-! ### Laws (compile-time) -/

example : parseMethod "GET" = .standard .GET := parseMethod_GET
example (m : Method) (h : m.isSafe = true) : m.isIdempotent = true :=
  Method.safe_implies_idempotent m h

end Tests.Network.HTTP.Types.Method
