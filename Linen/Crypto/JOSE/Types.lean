/-
  Linen.Crypto.JOSE.Types — JWT/JWK/JWS core types

  Core types for JSON Web Tokens (RFC 7519), JSON Web Keys (RFC 7517),
  and JSON Web Signatures (RFC 7515).

  ## Haskell source
  - `Crypto.JOSE.Types`, `Crypto.JWT` (jose package)

  ## Design
  - `JWSAlgorithm` enumerates all supported signing algorithms
  - `JWKKeyMaterial` represents the key material (RSA, EC, symmetric)
  - `ClaimsSet` captures registered and custom JWT claims
  - `JWTValidationSettings` configures validation rules
-/

namespace Crypto.JOSE

-- ────────────────────────────────────────────────────────────────────
-- Algorithms
-- ────────────────────────────────────────────────────────────────────

/-- JWS signing algorithms (RFC 7518 §3.1).
    $$\text{JWSAlgorithm} \in \{\text{HS256}, \text{RS256}, \text{ES256}, \ldots\}$$ -/
inductive JWSAlgorithm where
  | HS256 | HS384 | HS512
  | RS256 | RS384 | RS512
  | ES256 | ES384 | ES512
  | PS256 | PS384 | PS512
  | EdDSA
  deriving BEq, Repr, Inhabited

/-- Convert algorithm name to JWSAlgorithm. -/
def JWSAlgorithm.fromString : String → Option JWSAlgorithm
  | "HS256" => some .HS256
  | "HS384" => some .HS384
  | "HS512" => some .HS512
  | "RS256" => some .RS256
  | "RS384" => some .RS384
  | "RS512" => some .RS512
  | "ES256" => some .ES256
  | "ES384" => some .ES384
  | "ES512" => some .ES512
  | "PS256" => some .PS256
  | "PS384" => some .PS384
  | "PS512" => some .PS512
  | "EdDSA" => some .EdDSA
  | _ => none

instance : ToString JWSAlgorithm where
  toString
    | .HS256 => "HS256" | .HS384 => "HS384" | .HS512 => "HS512"
    | .RS256 => "RS256" | .RS384 => "RS384" | .RS512 => "RS512"
    | .ES256 => "ES256" | .ES384 => "ES384" | .ES512 => "ES512"
    | .PS256 => "PS256" | .PS384 => "PS384" | .PS512 => "PS512"
    | .EdDSA => "EdDSA"

/-- Is this an HMAC (symmetric) algorithm? -/
def JWSAlgorithm.isSymmetric : JWSAlgorithm → Bool
  | .HS256 | .HS384 | .HS512 => true
  | _ => false

/-- Roundtrip: `fromString (toString a) = some a` for all algorithms. -/
theorem JWSAlgorithm.fromString_toString_roundtrip (a : JWSAlgorithm) :
    JWSAlgorithm.fromString (toString a) = some a := by
  cases a <;> rfl

/-- `isSymmetric` returns true if and only if the algorithm is HS256, HS384, or HS512. -/
theorem JWSAlgorithm.isSymmetric_iff (a : JWSAlgorithm) :
    a.isSymmetric = true ↔ (a = .HS256 ∨ a = .HS384 ∨ a = .HS512) := by
  cases a <;> simp [isSymmetric]

-- ────────────────────────────────────────────────────────────────────
-- EC Curves
-- ────────────────────────────────────────────────────────────────────

/-- Elliptic curves for EC keys (RFC 7518 §6.2.1.1). -/
inductive ECCurve where
  | P256
  | P384
  | P521
  deriving BEq, Repr, Inhabited

def ECCurve.fromString : String → Option ECCurve
  | "P-256" => some .P256
  | "P-384" => some .P384
  | "P-521" => some .P521
  | _ => none

instance : ToString ECCurve where
  toString
    | .P256 => "P-256"
    | .P384 => "P-384"
    | .P521 => "P-521"

/-- Roundtrip: `fromString (toString c) = some c` for all EC curves. -/
theorem ECCurve.fromString_toString_roundtrip (c : ECCurve) :
    ECCurve.fromString (toString c) = some c := by
  cases c <;> rfl

-- ────────────────────────────────────────────────────────────────────
-- JWK Key Material
-- ────────────────────────────────────────────────────────────────────

/-- `Repr` instance for `ByteArray` (not provided by Lean 4.30+ stdlib). -/
instance : Repr ByteArray where
  reprPrec ba _ := Std.Format.text s!"ByteArray.mk {ba.data.toList}"

/-- Key material for a JSON Web Key (RFC 7517).
    $$\text{JWKKeyMaterial} = \text{RSA}\ n\ e\ |\ \text{EC}\ \text{crv}\ x\ y\ |\ \text{Oct}\ k$$ -/
inductive JWKKeyMaterial where
  /-- RSA public key: modulus `n`, exponent `e`, optional private exponent `d`. -/
  | rsa (n : ByteArray) (e : ByteArray) (d : Option ByteArray)
  /-- EC public key: curve, x-coordinate, y-coordinate, optional private key `d`. -/
  | ec (crv : ECCurve) (x : ByteArray) (y : ByteArray) (d : Option ByteArray)
  /-- Symmetric (octet) key. -/
  | oct (k : ByteArray)
  deriving Repr

-- ────────────────────────────────────────────────────────────────────
-- JWK
-- ────────────────────────────────────────────────────────────────────

/-- Valid JWK key types per RFC 7518 §6.1. -/
inductive JWKKeyType where
  | RSA
  | EC
  | oct
  deriving BEq, DecidableEq, Repr, Inhabited

instance : ToString JWKKeyType where
  toString
    | .RSA => "RSA"
    | .EC  => "EC"
    | .oct => "oct"

/-- Parse a key type string. -/
def JWKKeyType.fromString : String → Option JWKKeyType
  | "RSA" => some .RSA
  | "EC"  => some .EC
  | "oct" => some .oct
  | _     => none

/-- Roundtrip: `fromString (toString k) = some k` for all key types. -/
theorem JWKKeyType.fromString_toString_roundtrip (k : JWKKeyType) :
    JWKKeyType.fromString (toString k) = some k := by
  cases k <;> rfl

/-- Valid JWK public key usage values per RFC 7517 §4.2. -/
inductive JWKUse where
  | sig
  | enc
  deriving BEq, Repr

instance : ToString JWKUse where
  toString
    | .sig => "sig"
    | .enc => "enc"

/-- A JSON Web Key (RFC 7517).
    $$\text{JWK} = \{ \text{kty}, \text{kid}?, \text{alg}?, \text{use}?, \text{material} \}$$

    The `kty` field is a `JWKKeyType` (not a raw `String`), constraining it
    to the valid values "RSA", "EC", "oct" per RFC 7518 §6.1.
    The `kty_material_coherent` proof ensures the key type matches the key material. -/
structure JWK where
  /-- Key type: constrained to valid values per RFC 7518 §6.1. -/
  kty : JWKKeyType
  /-- Key ID (optional). -/
  kid : Option String := none
  /-- Algorithm (optional). -/
  alg : Option JWSAlgorithm := none
  /-- Public key use: "sig" or "enc" (optional), now typed. -/
  use : Option JWKUse := none
  /-- The key material. -/
  material : JWKKeyMaterial
  /-- The key type must be coherent with the key material. -/
  kty_material_coherent :
    (kty = .RSA → ∃ n e d, material = .rsa n e d) ∧
    (kty = .EC → ∃ crv x y d, material = .ec crv x y d) ∧
    (kty = .oct → ∃ k, material = .oct k)

instance : Repr JWK where
  reprPrec jwk _ :=
    s!"JWK(kty={repr jwk.kty}, kid={repr jwk.kid}, alg={repr jwk.alg}, material={repr jwk.material})"

/-- A set of JWKs (RFC 7517 §5). -/
structure JWKSet where
  keys : Array JWK

instance : Repr JWKSet where
  reprPrec s _ := s!"JWKSet(keys={repr s.keys})"

-- ────────────────────────────────────────────────────────────────────
-- JWT Claims
-- ────────────────────────────────────────────────────────────────────

/-- Registered claim names for JWTs (RFC 7519 §4.1). -/
structure ClaimsSet where
  /-- Issuer claim. -/
  iss : Option String := none
  /-- Subject claim. -/
  sub : Option String := none
  /-- Audience claim (can be a single string or array). -/
  aud : Option (List String) := none
  /-- Expiration time (Unix timestamp). -/
  exp : Option Nat := none
  /-- Not before (Unix timestamp). -/
  nbf : Option Nat := none
  /-- Issued at (Unix timestamp). -/
  iat : Option Nat := none
  /-- JWT ID. -/
  jti : Option String := none
  /-- Unregistered (custom) claims as key-value pairs.
      Values are stored as their JSON string representation. -/
  unregisteredClaims : List (String × String) := []
  deriving Repr, Inhabited

/-- Look up a custom claim value by name. -/
def ClaimsSet.lookupClaim (claims : ClaimsSet) (key : String) : Option String :=
  claims.unregisteredClaims.lookup key

-- ────────────────────────────────────────────────────────────────────
-- JWS Header
-- ────────────────────────────────────────────────────────────────────

/-- JWS JOSE Header (RFC 7515 §4). -/
structure JWSHeader where
  alg : JWSAlgorithm
  kid : Option String := none
  typ : Option String := none
  cty : Option String := none
  deriving BEq, Repr

-- ────────────────────────────────────────────────────────────────────
-- Validation settings
-- ────────────────────────────────────────────────────────────────────

/-- Configuration for JWT validation.
    `allowedSkew` is bounded to at most 600 seconds (10 minutes) to prevent
    dangerously permissive clock skew that could accept expired/future tokens. -/
structure JWTValidationSettings where
  /-- Allowed clock skew in seconds (max 600 = 10 minutes). -/
  allowedSkew : Nat := 0
  /-- The clock skew must not exceed 10 minutes (600 seconds). -/
  allowedSkew_bounded : allowedSkew ≤ 600 := by omega
  /-- Whether to check the `exp` claim. -/
  checkExpiry : Bool := true
  /-- Whether to check the `nbf` claim. -/
  checkNotBefore : Bool := true
  /-- If set, the `aud` claim must contain one of these values. -/
  audienceMatches : Option (List String) := none
  /-- If set, the `iss` claim must be one of these values. -/
  issuerMatches : Option (List String) := none
  deriving Repr

instance : Inhabited JWTValidationSettings where
  default := {
    allowedSkew := 0
    allowedSkew_bounded := by omega
    checkExpiry := true
    checkNotBefore := true
    audienceMatches := none
    issuerMatches := none
  }

-- ────────────────────────────────────────────────────────────────────
-- JWT Errors
-- ────────────────────────────────────────────────────────────────────

/-- Errors that can occur during JWT validation. -/
inductive JwtError where
  /-- The JWT string has an invalid format (not 3 dot-separated parts). -/
  | malformedToken (msg : String)
  /-- The header JSON cannot be parsed. -/
  | headerParseError (msg : String)
  /-- The claims JSON cannot be parsed. -/
  | claimsParseError (msg : String)
  /-- The algorithm in the header is not supported. -/
  | unsupportedAlgorithm (alg : String)
  /-- Signature verification failed. -/
  | signatureVerificationFailed
  /-- The token has expired. -/
  | tokenExpired (exp : Nat) (now : Nat)
  /-- The token is not yet valid. -/
  | tokenNotYetValid (nbf : Nat) (now : Nat)
  /-- The audience claim does not match. -/
  | audienceMismatch
  /-- The issuer claim does not match. -/
  | issuerMismatch
  /-- No matching key found in the key set. -/
  | noMatchingKey
  /-- The "none" algorithm is not allowed. -/
  | noneAlgorithmNotAllowed
  deriving BEq, Repr

instance : ToString JwtError where
  toString
    | .malformedToken msg => s!"Malformed JWT: {msg}"
    | .headerParseError msg => s!"JWT header parse error: {msg}"
    | .claimsParseError msg => s!"JWT claims parse error: {msg}"
    | .unsupportedAlgorithm alg => s!"Unsupported JWT algorithm: {alg}"
    | .signatureVerificationFailed => "JWT signature verification failed"
    | .tokenExpired exp now => s!"JWT expired: exp={exp}, now={now}"
    | .tokenNotYetValid nbf now => s!"JWT not yet valid: nbf={nbf}, now={now}"
    | .audienceMismatch => "JWT audience mismatch"
    | .issuerMismatch => "JWT issuer mismatch"
    | .noMatchingKey => "No matching JWK found"
    | .noneAlgorithmNotAllowed => "The 'none' algorithm is not allowed"

end Crypto.JOSE
