/-
  Tests for `Linen.Network.HTTP.Types.Status`.

  Status codes (proof-carrying, 100–999), their class predicates, and the
  RFC 9110 body rules are pure, so behaviour is checked with `#guard`.
-/
import Linen.Network.HTTP.Types.Status

open Network.HTTP.Types

namespace Tests.Network.HTTP.Types.Status

/-! ### Constants, fields, BEq (by code), Ord, ToString -/

#guard status200.statusCode == 200
#guard status404.statusMessage == "Not Found"
#guard status418.statusMessage == "I'm a teapot"
#guard toString status200 == "200 OK"
#guard status200 == ok200                       -- aliases are equal (same code)
#guard (status200 == status404) == false
#guard compare status200 status404 == Ordering.lt
#guard compare status404 status200 == Ordering.gt
#guard (mkStatus 418 "teapot").statusCode == 418

-- The erased proof field really constrains the code.
example : 100 ≤ status200.statusCode ∧ status200.statusCode ≤ 999 := status200.statusValid

/-! ### Class predicates (n / 100) -/

#guard status100.isInformational == true
#guard status200.isSuccessful == true
#guard status301.isRedirection == true
#guard status404.isClientError == true
#guard status500.isServerError == true
#guard status200.isInformational == false
#guard status404.isSuccessful == false
#guard status500.isClientError == false

/-! ### RFC 9110 §6.4.1: responses that MUST NOT carry a body -/

#guard status100.mustNotHaveBody == true        -- 1xx
#guard status101.mustNotHaveBody == true
#guard status204.mustNotHaveBody == true        -- No Content
#guard status304.mustNotHaveBody == true        -- Not Modified
#guard status200.mustNotHaveBody == false
#guard status404.mustNotHaveBody == false
#guard status500.mustNotHaveBody == false

/-! ### Laws (compile-time) -/

example : status204.mustNotHaveBody = true := status204_no_body
example : status200.mustNotHaveBody = false := status200_may_have_body

end Tests.Network.HTTP.Types.Status
