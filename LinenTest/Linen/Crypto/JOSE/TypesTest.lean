/-
  Tests for `Linen.Crypto.JOSE.Types`.

  The JOSE/JWT/JWK data types are pure, so their enums, parsers, and accessors
  are checked with `#guard` and the round-trip laws with `rfl` examples.
-/
import Linen.Crypto.JOSE.Types

open Crypto.JOSE

namespace Tests.Crypto.JOSE.Types

/-! ### JWSAlgorithm -/

#guard JWSAlgorithm.fromString "HS256" == some .HS256
#guard JWSAlgorithm.fromString "ES512" == some .ES512
#guard JWSAlgorithm.fromString "EdDSA" == some .EdDSA
#guard JWSAlgorithm.fromString "none" == none
#guard toString JWSAlgorithm.RS384 == "RS384"
#guard JWSAlgorithm.HS256.isSymmetric == true
#guard JWSAlgorithm.HS512.isSymmetric == true
#guard JWSAlgorithm.RS256.isSymmetric == false
#guard JWSAlgorithm.EdDSA.isSymmetric == false

/-! ### ECCurve / JWKKeyType / JWKUse -/

#guard ECCurve.fromString "P-256" == some .P256
#guard ECCurve.fromString "P-999" == none
#guard toString ECCurve.P521 == "P-521"
#guard JWKKeyType.fromString "RSA" == some .RSA
#guard JWKKeyType.fromString "bad" == none
#guard toString JWKKeyType.oct == "oct"
#guard toString JWKUse.sig == "sig"

/-! ### JWK — key type coherent with material -/

def octKey : JWK :=
  { kty := .oct, kid := some "k1", material := .oct "secret".toUTF8,
    kty_material_coherent :=
      ⟨fun h => by simp at h, fun h => by simp at h, fun _ => ⟨"secret".toUTF8, rfl⟩⟩ }

#guard octKey.kty == JWKKeyType.oct
#guard octKey.kid == some "k1"
#guard octKey.alg == none
#guard octKey.use == none

/-! ### ClaimsSet -/

def claims : ClaimsSet :=
  { iss := some "issuer", sub := some "user", exp := some 9999,
    unregisteredClaims := [("role", "admin"), ("dept", "eng")] }

#guard claims.iss == some "issuer"
#guard claims.exp == some 9999
#guard claims.aud == none                       -- default
#guard claims.lookupClaim "role" == some "admin"
#guard claims.lookupClaim "dept" == some "eng"
#guard claims.lookupClaim "absent" == none
#guard (default : ClaimsSet).iss == none

/-! ### JWSHeader / JWTValidationSettings -/

#guard ({ alg := .HS256 } : JWSHeader) == { alg := .HS256 }
#guard (({ alg := .HS256 } : JWSHeader) == { alg := .RS256 }) == false
#guard (default : JWTValidationSettings).allowedSkew == 0
#guard (default : JWTValidationSettings).checkExpiry == true
#guard ({ allowedSkew := 300, allowedSkew_bounded := by omega : JWTValidationSettings }).allowedSkew == 300

/-! ### JwtError -/

#guard JwtError.audienceMismatch == JwtError.audienceMismatch
#guard (JwtError.tokenExpired 100 200 == JwtError.tokenExpired 100 201) == false
#guard toString (JwtError.tokenExpired 100 200) == "JWT expired: exp=100, now=200"
#guard toString (JwtError.unsupportedAlgorithm "FOO") == "Unsupported JWT algorithm: FOO"
#guard toString JwtError.noneAlgorithmNotAllowed == "The 'none' algorithm is not allowed"

/-! ### Round-trip laws (compile-time) -/

example (a : JWSAlgorithm) : JWSAlgorithm.fromString (toString a) = some a :=
  JWSAlgorithm.fromString_toString_roundtrip a
example (k : JWKKeyType) : JWKKeyType.fromString (toString k) = some k :=
  JWKKeyType.fromString_toString_roundtrip k

end Tests.Crypto.JOSE.Types
