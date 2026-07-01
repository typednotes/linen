/-
  Tests for `Linen.Crypto.JOSE.JWK`.

  The operations run in `IO` (they call the OpenSSL FFI), so behaviour is
  checked with `#eval` (a thrown error fails the build), which also confirms the
  base64url FFI actually works end-to-end.
-/
import Linen.Crypto.JOSE.JWK

open Crypto.JOSE Crypto.JOSE.JWK

namespace Tests.Crypto.JOSE.JWK

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

-- The base64url FFI round-trips (real OpenSSL).
#eval show IO Unit from do
  let enc ← FFI.base64urlEncode "hello world".toUTF8
  let dec ← FFI.base64urlDecode enc
  check (String.fromUTF8! dec == "hello world") s!"base64url round-trip: {String.fromUTF8! dec}"

-- parseOctKey base64url-decodes and builds a coherent oct JWK.
#eval show IO Unit from do
  let jwk ← parseOctKey "c2VjcmV0"   -- base64url("secret")
  check (jwk.kty == .oct) "kty should be oct"
  match jwk.material with
  | .oct k => check (String.fromUTF8! k == "secret") s!"key material: {String.fromUTF8! k}"
  | _ => throw (IO.userError "material should be oct")

-- toDerPublicKey on a symmetric key has no public key (none).
#eval show IO Unit from do
  let jwk ← parseOctKey "c2VjcmV0"
  let der ← toDerPublicKey jwk
  check der.isNone "oct key has no DER public key"

/-! ### Signatures -/

example : String → IO JWK := parseOctKey
example : JWK → IO (Option ByteArray) := toDerPublicKey

end Tests.Crypto.JOSE.JWK
