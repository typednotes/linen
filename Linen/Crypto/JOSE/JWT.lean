/-
  Linen.Crypto.JOSE.JWT — JSON Web Token validation

  Validates JWT tokens: parses the compact serialization, verifies
  the signature, and checks registered claims (exp, nbf, aud, iss).

  ## Haskell source
  - `Crypto.JWT` (jose package)

  ## RFC
  - RFC 7519 (JSON Web Token)
-/

import Linen.Crypto.JOSE.Types
import Linen.Crypto.JOSE.FFI
import Linen.Crypto.JOSE.JWS

namespace Crypto.JOSE.JWT

open Crypto.JOSE

/-- Validate registered JWT claims against the current time and settings. -/
def validateClaims (claims : ClaimsSet) (now : Nat)
    (settings : JWTValidationSettings) : Except JwtError Unit := do
  -- Check expiration
  if settings.checkExpiry then
    match claims.exp with
    | some exp =>
      if now > exp + settings.allowedSkew then
        .error (.tokenExpired exp now)
    | none => pure ()
  -- Check not-before
  if settings.checkNotBefore then
    match claims.nbf with
    | some nbf =>
      if now + settings.allowedSkew < nbf then
        .error (.tokenNotYetValid nbf now)
    | none => pure ()
  -- Check audience
  match settings.audienceMatches with
  | some allowedAuds =>
    match claims.aud with
    | some tokenAuds =>
      let anyMatch := allowedAuds.any fun allowed =>
        tokenAuds.any (· == allowed)
      if !anyMatch then
        .error .audienceMismatch
    | none =>
      .error .audienceMismatch
  | none => pure ()
  -- Check issuer
  match settings.issuerMatches with
  | some allowedIssuers =>
    match claims.iss with
    | some iss =>
      if !allowedIssuers.elem iss then
        .error .issuerMismatch
    | none =>
      .error .issuerMismatch
  | none => pure ()

/-- Verify and decode a JWT token.
    Returns the validated claims set on success. -/
def verifyJWT (token : String) (keySet : JWKSet) (now : Nat)
    (settings : JWTValidationSettings := {}) : IO (Except JwtError ClaimsSet) := do
  -- 1. Split the compact serialization
  match JWS.splitCompact token with
  | none => return .error (.malformedToken "expected 3 dot-separated parts")
  | some (headerB64, payloadB64, signatureB64) =>
    -- 2. Decode header (simplified: just extract "alg")
    let headerBytes ← FFI.base64urlDecode headerB64
    let headerStr := String.fromUTF8! headerBytes
    -- Simple JSON parsing for header (extract "alg" field)
    let algStr := extractJsonField headerStr "alg"
    match algStr >>= JWSAlgorithm.fromString with
    | none => return .error (.unsupportedAlgorithm (algStr.getD "unknown"))
    | some alg =>
      -- 3. Decode signature
      let signatureBytes ← FFI.base64urlDecode signatureB64
      -- 4. Build signing input: header.payload (ASCII bytes)
      let signingInput := (headerB64 ++ "." ++ payloadB64).toUTF8
      -- 5. Try each key in the set
      let kidHint := extractJsonField headerStr "kid"
      let candidateKeys := keySet.keys.filter fun jwk =>
        match kidHint, jwk.kid with
        | some kid, some jwkKid => kid == jwkKid
        | _, _ => true  -- If no kid hint or no kid on key, try it
      let mut verified := false
      for jwk in candidateKeys do
        let valid ← JWS.verifySignature alg jwk signingInput signatureBytes
        if valid then
          verified := true
          break
      if !verified then
        return .error .signatureVerificationFailed
      -- 6. Decode payload (claims)
      let payloadBytes ← FFI.base64urlDecode payloadB64
      let payloadStr := String.fromUTF8! payloadBytes
      -- Simple JSON parsing for claims
      let claims : ClaimsSet := {
        iss := extractJsonField payloadStr "iss"
        sub := extractJsonField payloadStr "sub"
        aud := (extractJsonField payloadStr "aud").map (·  :: [])
        exp := (extractJsonField payloadStr "exp") >>= String.toNat?
        nbf := (extractJsonField payloadStr "nbf") >>= String.toNat?
        iat := (extractJsonField payloadStr "iat") >>= String.toNat?
        jti := extractJsonField payloadStr "jti"
        unregisteredClaims := extractAllFields payloadStr
      }
      -- 7. Validate claims
      match validateClaims claims now settings with
      | .ok () => return .ok claims
      | .error e => return .error e
where
  /-- Extract a JSON string field value (very simplified parser).
      Uses `String.splitOn` to locate patterns, avoiding raw `String.Pos` manipulation. -/
  extractJsonField (json : String) (key : String) : Option String :=
    let pattern := "\"" ++ key ++ "\""
    -- Check that the pattern exists somewhere in the JSON
    let patternParts := json.splitOn pattern
    if patternParts.length <= 1 then none
    else
      -- Find the value after "key":
      let afterKeyParts := json.splitOn (pattern ++ ":")
      match afterKeyParts.getD 1 "" with
      | "" => none
      | afterKey =>
        let trimmed := afterKey.trimAsciiStart.toString
        if trimmed.startsWith "\"" then
          -- String value: drop the opening quote, split on closing quote
          let rest := trimmed.drop 1 |>.toString
          let quoteParts := rest.splitOn "\""
          match quoteParts.head? with
          | some val => if val.isEmpty && rest.isEmpty then none else some val
          | none => none
        else
          -- Number or other: take until comma or }
          let value := (trimmed.takeWhile (fun c => c != ',' && c != '}' && c != ' ')).toString
          if value.isEmpty then none else some value
  /-- Extract all key-value pairs from a simple JSON object. -/
  extractAllFields (_json : String) : List (String × String) :=
    -- Very simplified: just return empty for now
    -- Full implementation would properly parse JSON
    []

end Crypto.JOSE.JWT
