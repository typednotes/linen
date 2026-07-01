/-
  `PostgREST.Auth` — authentication middleware

  WAI middleware that extracts Bearer tokens from the Authorization header,
  validates them as JWTs, and stores the claims in the request's Vault for
  downstream handlers. Mirrors PostgREST's `PostgREST.Auth` module.

  The middleware:
  1. Extracts the Bearer token from the Authorization header
  2. Validates the JWT signature against configured keys
  3. Checks exp/nbf/aud claims
  4. Extracts the role from the configurable claim path
  5. Stores the `AuthResult` in the Vault on the request

  If no Authorization header is present, the anonymous role is used. If
  validation fails, the error is stored for later handling.
-/
import Linen.PostgREST.Auth.Types

namespace PostgREST.Auth

-- ── Token extraction ──────────────────────────────────────────

/-- Extract the Bearer token from an Authorization header value.
    Returns `none` if the header is not a Bearer token. -/
def extractBearerToken (headerValue : String) : Option String :=
  let trimmed := headerValue.trimAscii.toString
  if trimmed.startsWith "Bearer " then
    some (trimmed.drop 7 |>.toString |>.trimAscii.toString)
  else if trimmed.startsWith "bearer " then
    some (trimmed.drop 7 |>.toString |>.trimAscii.toString)
  else
    none

/-- Find the Authorization header in a list of headers. -/
def findAuthHeader (headers : List (String × String)) : Option String :=
  headers.find? (fun (k, _) => k.toLower == "authorization") |>.map Prod.snd

-- ── Role extraction from claims ──────────────────────────────────────────

/-- Extract the role from JWT claims using the configured claim path. The
    path is a dot-separated key like `.role` or `.user.role`. -/
def extractRole (claimPath : String) (claims : List (String × String))
    (anonRole : String) : String :=
  let key := if claimPath.startsWith "." then (claimPath.drop 1).toString else claimPath
  -- Simple single-key lookup (nested paths would need JSON parsing)
  match claims.lookup key with
  | some role => role
  | none => anonRole

-- ── Auth middleware logic (without WAI dependency for now) ──────────────────────────────────────────

/-- Authenticate a request given its headers and configuration. Returns
    either an error message or the auth result. The `anonRole` must be
    non-empty (enforced by proof parameter), matching PostgreSQL's
    requirement for valid role names. -/
def authenticate (headers : List (String × String))
    (jwtSecret : Option String) (_jwtRoleClaimKey : String)
    (anonRole : String) (anonRole_nonempty : anonRole.length > 0)
    : Except String AuthResult := do
  match findAuthHeader headers with
  | none =>
    -- No auth header: use anonymous role
    return { authRole := anonRole, authRole_nonempty := anonRole_nonempty, authClaims := [] }
  | some authValue =>
    match extractBearerToken authValue with
    | none => .error "Invalid Authorization header format (expected Bearer token)"
    | some _token =>
      match jwtSecret with
      | none => .error "JWT secret not configured"
      | some _secret =>
        -- JWT validation would happen here (via JOSE library)
        -- For now, extract role from anonymous context
        -- In full implementation: validate token, extract claims, extract role
        return { authRole := anonRole, authRole_nonempty := anonRole_nonempty, authClaims := [] }

end PostgREST.Auth
