/-
  Linen.Crypto.JOSE.JWS — JSON Web Signature verification

  Implements JWS Compact Serialization verification (RFC 7515).
  The compact format is: `BASE64URL(header).BASE64URL(payload).BASE64URL(signature)`

  ## Haskell source
  - `Crypto.JOSE.JWS` (jose package)
-/

import Linen.Crypto.JOSE.Types
import Linen.Crypto.JOSE.FFI
import Linen.Crypto.JOSE.JWK

namespace Crypto.JOSE.JWS

open Crypto.JOSE

/-- Split a JWS compact serialization into its three parts. -/
def splitCompact (token : String) : Option (String × String × String) :=
  match token.splitOn "." with
  | [header, payload, signature] => some (header, payload, signature)
  | _ => none

/-- Verify a JWS signature using the given key.
    Returns `true` if the signature is valid. -/
def verifySignature (alg : JWSAlgorithm) (jwk : JWK)
    (signingInput : ByteArray) (signature : ByteArray) : IO Bool := do
  match alg with
  | .HS256 | .HS384 | .HS512 =>
    match jwk.material with
    | .oct key =>
      let algCode : UInt8 := match alg with
        | .HS256 => 0 | .HS384 => 1 | .HS512 => 2 | _ => 0
      let expected ← FFI.hmac key signingInput algCode
      return expected == signature
    | _ => return false
  | .RS256 | .RS384 | .RS512 =>
    match ← JWK.toDerPublicKey jwk with
    | some der =>
      let algCode : UInt8 := match alg with
        | .RS256 => 0 | .RS384 => 1 | .RS512 => 2 | _ => 0
      let result ← FFI.rsaVerify der signingInput signature algCode 0
      return result == 1
    | none => return false
  | .PS256 | .PS384 | .PS512 =>
    match ← JWK.toDerPublicKey jwk with
    | some der =>
      let algCode : UInt8 := match alg with
        | .PS256 => 0 | .PS384 => 1 | .PS512 => 2 | _ => 0
      let result ← FFI.rsaVerify der signingInput signature algCode 1
      return result == 1
    | none => return false
  | .ES256 | .ES384 | .ES512 =>
    match ← JWK.toDerPublicKey jwk with
    | some der =>
      let algCode : UInt8 := match alg with
        | .ES256 => 0 | .ES384 => 1 | .ES512 => 2 | _ => 0
      let result ← FFI.ecVerify der signingInput signature algCode
      return result == 1
    | none => return false
  | .EdDSA =>
    -- EdDSA not yet supported in this implementation
    return false

end Crypto.JOSE.JWS
