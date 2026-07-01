/-
  Tests for `Linen.Crypto.JOSE.JWS`.

  `splitCompact` is pure (`#guard`); `verifySignature` runs in `IO` via the
  OpenSSL FFI, so the HMAC path is exercised end-to-end with `#eval`.
-/
import Linen.Crypto.JOSE.JWS

open Crypto.JOSE Crypto.JOSE.JWS

namespace Tests.Crypto.JOSE.JWS

/-! ### splitCompact (RFC 7515 compact serialization) -/

#guard splitCompact "aaa.bbb.ccc" == some ("aaa", "bbb", "ccc")
#guard splitCompact "header.payload.sig" == some ("header", "payload", "sig")
#guard splitCompact "onlytwo.parts" == none
#guard splitCompact "a.b.c.d" == none          -- too many parts
#guard splitCompact "nodots" == none

/-! ### verifySignature — HMAC path (real OpenSSL round-trip) -/

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

-- A correct HS256 HMAC verifies; a wrong signature does not.
#eval show IO Unit from do
  let jwk ← JWK.parseOctKey "c2VjcmV0"          -- oct key "secret"
  let input := "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0In0".toUTF8
  let sig ← FFI.hmac "secret".toUTF8 input 0     -- the HS256 signature over `input`
  check (← verifySignature .HS256 jwk input sig) "valid HS256 signature should verify"
  let bad ← verifySignature .HS256 jwk input "not-the-signature".toUTF8
  check (bad == false) "a wrong signature must not verify"

-- HS384 / HS512 round-trip too (algCodes 1 and 2).
#eval show IO Unit from do
  let jwk ← JWK.parseOctKey "c2VjcmV0"
  let input := "payload".toUTF8
  let sig384 ← FFI.hmac "secret".toUTF8 input 1
  check (← verifySignature .HS384 jwk input sig384) "HS384 should verify"
  let sig512 ← FFI.hmac "secret".toUTF8 input 2
  check (← verifySignature .HS512 jwk input sig512) "HS512 should verify"

-- EdDSA is not supported ⇒ always false.
#eval show IO Unit from do
  let jwk ← JWK.parseOctKey "c2VjcmV0"
  let r ← verifySignature .EdDSA jwk "x".toUTF8 "y".toUTF8
  check (r == false) "EdDSA is unsupported (false)"

/-! ### Signature -/

example : JWSAlgorithm → JWK → ByteArray → ByteArray → IO Bool := verifySignature

end Tests.Crypto.JOSE.JWS
