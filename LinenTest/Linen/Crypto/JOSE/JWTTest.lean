/-
  Tests for `Linen.Crypto.JOSE.JWT`.

  `validateClaims` is pure (`#guard`, with a local `BEq (Except …)`); `verifyJWT`
  runs in `IO`, so it is exercised end-to-end with `#eval` — building a real
  HS256 token with OpenSSL and verifying it.
-/
import Linen.Crypto.JOSE.JWT

open Crypto.JOSE Crypto.JOSE.JWT

namespace Tests.Crypto.JOSE.JWT

-- Core has no `BEq (Except ε α)`; local instance for comparing validation results.
local instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .ok a, .ok b => a == b
    | .error a, .error b => a == b
    | _, _ => false

/-! ### validateClaims (RFC 7519 §4.1) -/

-- No constraints ⇒ ok.
#guard validateClaims {} 1000 {} == Except.ok ()
-- Expiry.
#guard validateClaims { exp := some 500 } 1000 {} == .error (.tokenExpired 500 1000)
#guard validateClaims { exp := some 2000 } 1000 {} == Except.ok ()
-- Clock skew tolerates a slightly-expired token (exp 500 + skew 600 ≥ now 1000).
#guard validateClaims { exp := some 500 } 1000 { allowedSkew := 600 } == Except.ok ()
-- Not-before.
#guard validateClaims { nbf := some 2000 } 1000 {} == .error (.tokenNotYetValid 2000 1000)
#guard validateClaims { nbf := some 500 } 1000 {} == Except.ok ()
-- Audience.
#guard validateClaims { aud := some ["a", "b"] } 1000 { audienceMatches := some ["b"] } == Except.ok ()
#guard validateClaims { aud := some ["a"] } 1000 { audienceMatches := some ["z"] } == .error .audienceMismatch
#guard validateClaims {} 1000 { audienceMatches := some ["a"] } == .error .audienceMismatch
-- Issuer.
#guard validateClaims { iss := some "me" } 1000 { issuerMatches := some ["me", "you"] } == Except.ok ()
#guard validateClaims { iss := some "me" } 1000 { issuerMatches := some ["other"] } == .error .issuerMismatch

/-! ### verifyJWT — full HS256 round-trip through OpenSSL -/

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

-- Build an HS256 JWT signed with "secret", then verify it.
#eval show IO Unit from do
  let key := "secret".toUTF8
  let headerB64 ← FFI.base64urlEncode "{\"alg\":\"HS256\"}".toUTF8
  let payloadB64 ← FFI.base64urlEncode "{\"sub\":\"1234\",\"exp\":9999999999}".toUTF8
  let sig ← FFI.hmac key (headerB64 ++ "." ++ payloadB64).toUTF8 0
  let sigB64 ← FFI.base64urlEncode sig
  let token := headerB64 ++ "." ++ payloadB64 ++ "." ++ sigB64
  let jwk ← JWK.parseOctKey "c2VjcmV0"      -- base64url("secret")
  let keySet : JWKSet := { keys := #[jwk] }
  match ← verifyJWT token keySet 1000 with
  | .ok claims =>
    check (claims.sub == some "1234") s!"sub should be 1234, got {claims.sub}"
    check (claims.exp == some 9999999999) s!"exp mismatch: {claims.exp}"
  | .error e => throw (IO.userError s!"expected valid token, got error: {e}")

-- A tampered signature is rejected.
#eval show IO Unit from do
  let headerB64 ← FFI.base64urlEncode "{\"alg\":\"HS256\"}".toUTF8
  let payloadB64 ← FFI.base64urlEncode "{\"sub\":\"1234\"}".toUTF8
  let badSig ← FFI.base64urlEncode "tampered".toUTF8
  let token := headerB64 ++ "." ++ payloadB64 ++ "." ++ badSig
  let jwk ← JWK.parseOctKey "c2VjcmV0"
  match ← verifyJWT token { keys := #[jwk] } 1000 with
  | .error .signatureVerificationFailed => pure ()
  | .error e => throw (IO.userError s!"expected signatureVerificationFailed, got {e}")
  | .ok _ => throw (IO.userError "tampered token must not verify")

-- A malformed token (not 3 parts) is rejected before any crypto.
#eval show IO Unit from do
  let jwk ← JWK.parseOctKey "c2VjcmV0"
  match ← verifyJWT "not.a.valid.jwt" { keys := #[jwk] } 1000 with
  | .error (.malformedToken _) => pure ()
  | _ => throw (IO.userError "malformed token should be rejected")

/-! ### Signatures -/

example : ClaimsSet → Nat → JWTValidationSettings → Except JwtError Unit := validateClaims
example : String → JWKSet → Nat → JWTValidationSettings → IO (Except JwtError ClaimsSet) :=
  fun t k n s => verifyJWT t k n s

end Tests.Crypto.JOSE.JWT
