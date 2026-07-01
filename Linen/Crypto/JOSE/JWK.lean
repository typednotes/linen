/-
  Linen.Crypto.JOSE.JWK — JSON Web Key parsing

  Parses JWK and JWK Set JSON representations into `JWK` structures.

  ## Haskell source
  - `Crypto.JOSE.JWK` (jose package)

  ## RFC
  - RFC 7517 (JSON Web Key)
  - RFC 7518 (JSON Web Algorithms)
-/

import Linen.Crypto.JOSE.Types
import Linen.Crypto.JOSE.FFI

namespace Crypto.JOSE.JWK

open Crypto.JOSE

/-- Parse a symmetric (oct) key from a base64url-encoded string. -/
def parseOctKey (kBase64 : String) : IO JWK := do
  let keyBytes ← FFI.base64urlDecode kBase64
  return {
    kty := .oct
    material := .oct keyBytes
    kty_material_coherent := ⟨fun h => absurd h (by decide), fun h => absurd h (by decide), fun _ => ⟨keyBytes, rfl⟩⟩
  }

/-- Determine the DER-encoded public key bytes from a JWK's key material.
    Used for RSA and EC signature verification. -/
def toDerPublicKey (jwk : JWK) : IO (Option ByteArray) := do
  match jwk.material with
  | .rsa n e _ =>
    let der ← FFI.rsaPubkeyFromComponents n e
    return some der
  | .ec crv x y _ =>
    let crvCode : UInt8 := match crv with
      | .P256 => 0 | .P384 => 1 | .P521 => 2
    let der ← FFI.ecPubkeyFromComponents crvCode x y
    return some der
  | .oct _ =>
    return none

end Crypto.JOSE.JWK
