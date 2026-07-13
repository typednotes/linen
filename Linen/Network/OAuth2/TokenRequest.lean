/-
  Linen.Network.OAuth2.TokenRequest — exchanging codes/refresh tokens

  Port of `hoauth2`'s `Network.OAuth2.TokenRequest` (see
  `docs/imports/hoauth2/dependencies.md`), module #8: RFC 6749 §4.1.3/§4.1.4
  (exchange an authorization code for an access token) and §6 (refresh an
  access token), plus the token endpoint's error shape (§5.2).

  ## Substitutions
  - **`instance Binary TokenResponse` is dropped entirely** (per
    `dependencies.md`'s `binary`/`binary-instances` note): its `put`/`get`
    were only ever a thin proxy over the already-derived `ToJSON`/
    `FromJSON` (`put rawResponse` / `fromJSON (Aeson.Object rawt)`), a
    GHC-`Data.Binary` disk-serialization convenience with no OAuth2
    semantics of its own. Anything needing to persist a `TokenResponse`
    uses `ToJSON`/`FromJSON` directly.
  - `Control.Monad.Trans.Except.ExceptT TokenResponseError m` is a plain
    `IO (Except TokenResponseError _)` — no transformer-stack equivalent
    elsewhere in this port's `hoauth2` call sites either (see
    `Linen.Network.OAuth2.Internal`'s `MonadThrow` note).
  - `http-client`'s `applyBasicAuth`/`urlEncodedBody`: `applyBasicAuth` is
    `Linen.Network.HTTP.Client.Conduit.applyBasicAuth` (added in this same
    import batch); `urlEncodedBody` has exactly one call site in the whole
    package (`doSimplePostRequest` below) and is ported as a private
    helper here rather than a new general `Linen.Network.HTTP.Client`
    export.
  - `Network.HTTP.Types.URI.parseQuery` is `Linen.Network.HTTP.Types.
    parseQuery`/`urlEncode`/`urlDecode` (already ported).
-/

import Linen.Network.OAuth2.Internal
import Linen.Network.HTTP.Client.Conduit
import Linen.Network.HTTP.Types.URI
import Linen.Data.Json.Types
import Linen.Data.Json.Decode

namespace Network.OAuth2.TokenRequest

open Network.OAuth2.Internal
open Network.HTTP.Client (Request Response)
open Network.HTTP.Types
open Data.Json (Value ToJSON FromJSON Object)
open Data.Json.Decode (decodeAs)

-- ────────────────────────────────────────────────────────────────────
-- Token Request Errors
-- ────────────────────────────────────────────────────────────────────

/-- Token endpoint error codes, RFC 6749 §5.2. -/
inductive TokenResponseErrorCode where
  | invalidRequest
  | invalidClient
  | invalidGrant
  | unauthorizedClient
  | unsupportedGrantType
  | invalidScope
  /-- A code this port doesn't recognise, carrying the raw wire value. -/
  | unknownErrorCode (code : String)
deriving Repr, BEq

instance : FromJSON TokenResponseErrorCode where
  parseJSON
    | .string s =>
      .ok <| match s with
        | "invalid_request" => .invalidRequest
        | "invalid_client" => .invalidClient
        | "invalid_grant" => .invalidGrant
        | "unauthorized_client" => .unauthorizedClient
        | "unsupported_grant_type" => .unsupportedGrantType
        | "invalid_scope" => .invalidScope
        | other => .unknownErrorCode other
    | v => .error s!"expected a string, got {repr v}"

/-- What the token endpoint returns on failure, RFC 6749 §5.2. -/
structure TokenResponseError where
  tokenResponseError : TokenResponseErrorCode
  tokenResponseErrorDescription : Option String
  tokenResponseErrorUri : Option Network.URI.URI
deriving Repr, BEq

instance : FromJSON TokenResponseError where
  parseJSON v := do
    pure
      { tokenResponseError := ← Value.getField v "error" >>= FromJSON.parseJSON
        tokenResponseErrorDescription :=
          ← (← Value.getFieldOpt v "error_description").mapM FromJSON.parseJSON
        tokenResponseErrorUri := ← (← Value.getFieldOpt v "error_uri").mapM FromJSON.parseJSON }

/-- Parse a raw token-endpoint response body as a `TokenResponseError`,
    falling back to an `unknownErrorCode` carrying the decode failure and
    the original response if it isn't valid JSON at all (`hoauth2`'s
    `parseTokeResponseError`). -/
def parseTokeResponseError (raw : String) : TokenResponseError :=
  match decodeAs (α := TokenResponseError) raw with
  | .ok e => e
  | .error err =>
    { tokenResponseError := .unknownErrorCode ""
      tokenResponseErrorDescription :=
        some s!"Decode TokenResponseError failed: {err}\n Original Response:\n{raw}"
      tokenResponseErrorUri := none }

-- ────────────────────────────────────────────────────────────────────
-- Tokens
-- ────────────────────────────────────────────────────────────────────

/-- A successful token-endpoint response, RFC 6749 §4.1.4. `rawResponse`
    keeps the full decoded JSON object, so a provider's non-standard extra
    fields (or a future field this port hasn't named) are never lost. -/
structure TokenResponse where
  accessToken : AccessToken
  /-- Present when `offline_access` was requested and the provider
      supports refreshing. -/
  refreshToken : Option RefreshToken
  expiresIn : Option Int
  /-- RFC 6749 §5.1 says this is required, but providers vary in
      practice — kept optional, matching upstream. -/
  tokenType : Option String
  /-- Present when `openid` was requested and the provider supports
      OpenID Connect. -/
  idToken : Option IdToken
  scope : Option String
  rawResponse : Object
deriving Repr

/-- Parse a numeric field that may be wire-encoded as either a JSON number
    or a JSON string (some providers send `"expires_in": "3600"`). -/
private def parseIntFlexible : Value → Except String Int
  | .string s =>
    match s.toInt? with
    | some n => .ok n
    | none => .error s!"expected an integer string, got {s}"
  | v => FromJSON.parseJSON v

instance : FromJSON TokenResponse where
  parseJSON v := do
    pure
      { accessToken := ← Value.getField v "access_token" >>= FromJSON.parseJSON
        refreshToken := ← (← Value.getFieldOpt v "refresh_token").mapM FromJSON.parseJSON
        expiresIn := ← (← Value.getFieldOpt v "expires_in").mapM parseIntFlexible
        tokenType := ← (← Value.getFieldOpt v "token_type").mapM FromJSON.parseJSON
        idToken := ← (← Value.getFieldOpt v "id_token").mapM FromJSON.parseJSON
        scope := ← (← Value.getFieldOpt v "scope").mapM FromJSON.parseJSON
        rawResponse := ← match v with
          | .object o => .ok o
          | _ => .error s!"expected a JSON object, got {repr v}" }

instance : ToJSON TokenResponse where
  toJSON t := .object t.rawResponse

-- ────────────────────────────────────────────────────────────────────
-- URL
-- ────────────────────────────────────────────────────────────────────

/-- The access-token request URL and body for exchanging an authorization
    code, RFC 6749 §4.1.3. -/
def accessTokenUrl (oa : OAuth2) (code : ExchangeToken) : Network.URI.URI × PostBody :=
  ( oa.oauth2TokenEndpoint
  , [ ("code", code.extoken)
    , ("redirect_uri", Network.URI.uriToString Network.URI.defaultUserInfoMap oa.oauth2RedirectUri)
    , ("grant_type", "authorization_code") ] )

/-- The access-token request URL and body for a Refresh Token grant,
    RFC 6749 §6. -/
def refreshAccessTokenUrl (oa : OAuth2) (token : RefreshToken) : Network.URI.URI × PostBody :=
  (oa.oauth2TokenEndpoint, [("grant_type", "refresh_token"), ("refresh_token", token.rtoken)])

-- ────────────────────────────────────────────────────────────────────
-- Utilities
-- ────────────────────────────────────────────────────────────────────

/-- Add `client_id`/`client_secret` to a request's POST body
    (`ClientSecretPost`). -/
def clientSecretPost (oa : OAuth2) : PostBody :=
  [("client_id", oa.oauth2ClientId), ("client_secret", oa.oauth2ClientSecret)]

/-- Add an `Authorization: Basic` header built from `client_id`/
    `client_secret` (`ClientSecretBasic`). -/
def addBasicAuth (oa : OAuth2) (req : Request) : Request :=
  Network.HTTP.Client.Conduit.applyBasicAuth oa.oauth2ClientId oa.oauth2ClientSecret req

/-- Percent-encode `body` as an `application/x-www-form-urlencoded`
    request body, replacing the request's method with `POST` (`hoauth2`'s
    one call site for `http-client`'s `urlEncodedBody`; see the module
    doc-comment for why it isn't ported as a general export). -/
private def urlEncodedBody (body : PostBody) (req : Request) : Request :=
  let encoded :=
    "&".intercalate (body.map fun (k, v) => s!"{Network.HTTP.Types.urlEncode k}={Network.HTTP.Types.urlEncode v}")
  { req with
      method := .standard .POST
      body := some encoded.toUTF8
      headers := (hContentType, "application/x-www-form-urlencoded") :: req.headers.filter (fun h => h.1 != hContentType) }

/-- Gets a response's body if the request succeeded, otherwise a parsed
    `TokenResponseError` (`hoauth2`'s `handleOAuth2TokenResponse`). -/
def handleOAuth2TokenResponse (resp : Response) : Except TokenResponseError String :=
  let bodyText := String.fromUTF8! resp.body
  if resp.isSuccess then .ok bodyText else .error (parseTokeResponseError bodyText)

/-- Conduct a POST request against the token endpoint (`hoauth2`'s
    `doSimplePostRequest`). -/
def doSimplePostRequest (oa : OAuth2) (url : Network.URI.URI) (body : PostBody) :
    IO (Except TokenResponseError String) := do
  match uriToRequest url with
  | .error e => pure (.error (parseTokeResponseError e))
  | .ok req =>
    let req' := (addBasicAuth oa ∘ addDefaultRequestHeaders) req
    let resp ← Network.HTTP.Client.Conduit.withResponse (urlEncodedBody body req') pure
    pure (handleOAuth2TokenResponse resp)

/-- Parse a response body that is a query string (some providers reply
    `application/x-www-form-urlencoded` instead of JSON) into `a`
    (`hoauth2`'s `parseResponseString`). -/
def parseResponseString [FromJSON a] (raw : String) : Except TokenResponseError a :=
  match Network.HTTP.Types.parseQuery raw with
  | [] => .error (parseTokeResponseError raw)
  | items =>
    let obj : Value :=
      .object (items.map fun (k, v) =>
        (Network.HTTP.Types.urlDecode k, match v with
          | some s => .string (Network.HTTP.Types.urlDecode s)
          | none => .null))
    match FromJSON.parseJSON (α := a) obj with
    | .ok x => .ok x
    | .error _ => .error (parseTokeResponseError raw)

/-- Try to parse a response body as JSON; if that fails, try it as a query
    string (`hoauth2`'s `parseResponseFlexible`). -/
def parseResponseFlexible [FromJSON a] (raw : String) : Except TokenResponseError a :=
  match decodeAs (α := a) raw with
  | .ok x => .ok x
  | .error _ => parseResponseString raw

/-- Conduct a POST request against the token endpoint, and decode a
    successful response as `a` (`hoauth2`'s `doJSONPostRequest`). -/
def doJSONPostRequest [FromJSON a] (oa : OAuth2) (uri : Network.URI.URI) (body : PostBody) :
    IO (Except TokenResponseError a) := do
  match ← doSimplePostRequest oa uri body with
  | .error e => pure (.error e)
  | .ok resp => pure (parseResponseFlexible resp)

-- ────────────────────────────────────────────────────────────────────
-- Token management
-- ────────────────────────────────────────────────────────────────────

/-- Exchange an authorization code for an access token, letting the
    caller pick how client credentials are authenticated (RFC 6749 §2.3;
    `hoauth2`'s `fetchAccessTokenWithAuthMethod`). -/
def fetchAccessTokenWithAuthMethod (authMethod : ClientAuthenticationMethod) (oa : OAuth2)
    (code : ExchangeToken) : IO (Except TokenResponseError TokenResponse) := do
  let (uri, body) := accessTokenUrl oa code
  let extraBody := if authMethod == .ClientSecretPost then clientSecretPost oa else []
  doJSONPostRequest oa uri (body ++ extraBody)

/-- Exchange an authorization code for an access token, authenticating
    with `ClientSecretBasic` (`hoauth2`'s `fetchAccessToken`). -/
def fetchAccessToken (oa : OAuth2) (code : ExchangeToken) : IO (Except TokenResponseError TokenResponse) :=
  fetchAccessTokenWithAuthMethod .ClientSecretBasic oa code

/-- Fetch a new access token using a refresh token, letting the caller
    pick how client credentials are authenticated (`hoauth2`'s
    `refreshAccessTokenWithAuthMethod`). -/
def refreshAccessTokenWithAuthMethod (authMethod : ClientAuthenticationMethod) (oa : OAuth2)
    (token : RefreshToken) : IO (Except TokenResponseError TokenResponse) := do
  let (uri, body) := refreshAccessTokenUrl oa token
  let extraBody := if authMethod == .ClientSecretPost then clientSecretPost oa else []
  doJSONPostRequest oa uri (body ++ extraBody)

/-- Fetch a new access token using a refresh token, authenticating with
    `ClientSecretBasic` (`hoauth2`'s `refreshAccessToken`). -/
def refreshAccessToken (oa : OAuth2) (token : RefreshToken) :
    IO (Except TokenResponseError TokenResponse) :=
  refreshAccessTokenWithAuthMethod .ClientSecretBasic oa token

end Network.OAuth2.TokenRequest
