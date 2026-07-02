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

#eval show IO Unit from do
  match ← authenticate [] none "role" "anon" (by decide) 0 with
  | .ok r => unless r.authRole == "anon" && r.authClaims.isEmpty do
      throw (IO.userError "no auth header should yield the anonymous role")
  | .error e => throw (IO.userError s!"unexpected error: {e}")

#eval show IO Unit from do
  match ← authenticate [("Content-Type", "text/plain")] none "role" "anon" (by decide) 0 with
  | .ok r => unless r.authRole == "anon" do
      throw (IO.userError "non-auth headers should yield the anonymous role")
  | .error e => throw (IO.userError s!"unexpected error: {e}")

#eval show IO Unit from do
  match ← authenticate [("Authorization", "Basic abc123")] none "role" "anon" (by decide) 0 with
  | .error _ => pure ()
  | .ok _ => throw (IO.userError "a non-Bearer Authorization header should be rejected")

#eval show IO Unit from do
  match ← authenticate [("Authorization", "Bearer tok")] none "role" "anon" (by decide) 0 with
  | .error "JWT secret not configured" => pure ()
  | r => throw (IO.userError s!"expected 'JWT secret not configured', got {repr r}")

#eval show IO Unit from do
  -- Not a well-formed JWT (not 3 dot-separated base64url parts), so verification fails.
  match ← authenticate [("Authorization", "Bearer tok")] (some "secret") "role" "anon" (by decide) 0 with
  | .error _ => pure ()
  | .ok _ => throw (IO.userError "a malformed token should fail verification")

#eval show IO Unit from do
  -- A genuine HS256 JWT signed with "secret", claiming {"role":"editor"} and no exp/nbf.
  -- header  = {"alg":"HS256","typ":"JWT"}      → base64url eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9
  -- payload = {"role":"editor"}                → base64url eyJyb2xlIjoiZWRpdG9yIn0
  -- signature = HMAC-SHA256(key="secret", header_b64 + "." + payload_b64), base64url
  let token := "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiZWRpdG9yIn0" ++
    ".E2aU_-4o6p9Rf-vAGKzLeNvx4kOD8Ch6eLRNtD5Jy7s"
  match ← authenticate [("Authorization", s!"Bearer {token}")] (some "secret") "role" "anon" (by decide) 0 with
  | .error e => throw (IO.userError s!"expected the correctly-signed JWT to verify, got error: {e}")
  | .ok _ => pure ()  -- signature genuinely verified (a tampered/garbage token above was rejected)

#eval show IO Unit from do
  -- Same token, tamper with one signature character: verification must fail.
  let token := "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiZWRpdG9yIn0" ++
    ".F2aU_-4o6p9Rf-vAGKzLeNvx4kOD8Ch6eLRNtD5Jy7s"
  match ← authenticate [("Authorization", s!"Bearer {token}")] (some "secret") "role" "anon" (by decide) 0 with
  | .error _ => pure ()
  | .ok _ => throw (IO.userError "a tampered signature should not verify")

#eval show IO Unit from do
  -- Wrong secret: verification must fail even though the token is well-formed.
  let token := "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiZWRpdG9yIn0" ++
    ".E2aU_-4o6p9Rf-vAGKzLeNvx4kOD8Ch6eLRNtD5Jy7s"
  match ← authenticate [("Authorization", s!"Bearer {token}")] (some "wrong-secret") "role" "anon" (by decide) 0 with
  | .error _ => pure ()
  | .ok _ => throw (IO.userError "verification against the wrong secret should fail")

end Tests.PostgREST.Auth
