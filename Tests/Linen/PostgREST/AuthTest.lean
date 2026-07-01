/-
  Tests for `Linen.PostgREST.Auth`.
-/
import Linen.PostgREST.Auth

open PostgREST.Auth

namespace Tests.PostgREST.Auth

/-! ### `extractBearerToken` -/

#guard extractBearerToken "Bearer abc123" == some "abc123"
#guard extractBearerToken "bearer abc123" == some "abc123"
#guard extractBearerToken "Bearer   abc123  " == some "abc123"
#guard extractBearerToken "Basic abc123" == none
#guard extractBearerToken "" == none

/-! ### `findAuthHeader` -/

#guard findAuthHeader [("Authorization", "Bearer abc123")] == some "Bearer abc123"
#guard findAuthHeader [("authorization", "Bearer abc123")] == some "Bearer abc123"
#guard findAuthHeader [("Content-Type", "text/plain")] == none
#guard findAuthHeader [] == none

/-! ### `extractRole` -/

#guard extractRole ".role" [("role", "editor")] "anon" == "editor"
#guard extractRole "role" [("role", "editor")] "anon" == "editor"
#guard extractRole ".role" [] "anon" == "anon"

/-! ### `authenticate` -/

#guard (match authenticate [] none "role" "anon" (by decide) with
  | .ok r => r.authRole == "anon" && r.authClaims.isEmpty | _ => false)

#guard (match authenticate [("Content-Type", "text/plain")] none "role" "anon" (by decide) with
  | .ok r => r.authRole == "anon" | _ => false)

#guard (match authenticate [("Authorization", "Basic abc123")] none "role" "anon" (by decide) with
  | .error _ => true | _ => false)

#guard (match authenticate [("Authorization", "Bearer tok")] none "role" "anon" (by decide) with
  | .error "JWT secret not configured" => true | _ => false)

#guard (match authenticate [("Authorization", "Bearer tok")] (some "secret") "role" "anon" (by decide) with
  | .ok r => r.authRole == "anon" | _ => false)

end Tests.PostgREST.Auth
