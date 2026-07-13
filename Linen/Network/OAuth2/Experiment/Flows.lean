/-
  Linen.Network.OAuth2.Experiment.Flows — the typed request-builder layer's
  HTTP-performing entry points

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Flows` (see
  `docs/imports/hoauth2/dependencies.md`), module #21.

  ## Not a pure facade
  `dependencies.md` labels this module a "facade" (it introduces no new
  request/response *types* of its own — everything it touches is a type
  already ported by modules #1–#19), but upstream's `Flows.hs` is not a
  re-export-only module: it is where the package's actual HTTP-performing
  entry points live — `mkAuthorizationRequest`, `mkPkceAuthorizeRequest`,
  `conduitDeviceAuthorizationRequest`, `pollDeviceTokenRequest`,
  `conduitTokenRequest`/`conduitPkceTokenRequest`,
  `conduitRefreshTokenRequest`, and `conduitUserInfoRequest`/
  `conduitUserInfoRequestWithCustomMethod` — built by composing the typed
  request parameters from modules #10–#19 with module #9's low-level HTTP
  plumbing. This port faithfully carries over that logic rather than
  reducing the module to a bare `export`, per AGENTS.md's ban on
  simplifying away real behaviour to dodge work.

  ## Substitutions
  - **`Manager`**: `http-conduit`'s connection-pool handle has no analogue
    in this port's `Linen.Network.HTTP.Client` (one connection per request,
    see `Linen.Network.OAuth2.HttpClient`'s doc-comment for the same
    substitution already made at module #7); every function below drops
    the `Manager` parameter accordingly.
  - **`ExceptT ε m α` / `MonadIO m`**: no transformer-stack equivalent
    elsewhere in this port's `hoauth2` call sites either (see
    `Linen.Network.OAuth2.Internal`'s `MonadThrow` note); every function
    below is a plain `IO (Except ε α)`, pinned at `IO`, matching modules #7
    and #8.
  - **`pollDeviceTokenRequestInternal`'s unbounded retry loop** (RFC 8628
    §3.5's poll-until-ready): this is a genuinely unbounded loop by design
    (there is no spec-mandated bound on how long an end user takes to
    approve a device), so it cannot be structurally recursive. Ported as a
    `while` loop over a mutable flag, which desugars to the standard
    library's `Loop.forIn` (see `Linen.Network.WebApp.Server.Run`'s
    doc-comment for the same idiom already established in this codebase)
    — not a `partial def`, and not a fuel parameter that would falsify the
    spec's actual (unbounded) behaviour.
  - `Data.Bifunctor.first`/`Data.Map.Strict.toList` are folded directly into
    each call site; `Data.Aeson`'s `FromJSON` is
    `Linen.Data.Json.Types.FromJSON`.
  - `http-client`'s `urlEncodedBody` has one call site in each of this
    module and `Linen.Network.OAuth2.TokenRequest`; both keep their own
    small private copy rather than introducing a shared export, matching
    that module's own doc-comment rationale (too few call sites in either
    module alone to warrant a general `Linen.Network.HTTP.Client.Conduit`
    export).
-/

import Linen.Network.OAuth2
import Linen.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
import Linen.Network.OAuth2.Experiment.Flows.RefreshTokenRequest
import Linen.Network.OAuth2.Experiment.Flows.TokenRequest
import Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest
import Linen.Network.OAuth2.Experiment.Grants.AuthorizationCode
import Linen.Network.OAuth2.Experiment.Grants.DeviceAuthorization
import Linen.Network.OAuth2.Experiment.Pkce
import Linen.Network.OAuth2.Experiment.Types
import Linen.Network.OAuth2.Experiment.Utils
import Linen.Network.HTTP.Client.Contrib
import Linen.Network.HTTP.Client.Conduit
import Linen.Network.HTTP.Types.URI
import Linen.Data.Json.Types

namespace Network.OAuth2.Experiment.Flows

open Network.OAuth2
open Network.HTTP.Client (Request Response)
open Network.HTTP.Types
open Data.Json (FromJSON)
open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Utils (unionMapsToQueryParams)
open Network.OAuth2.Experiment.Pkce (CodeVerifier)
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.RefreshTokenRequest
open Network.OAuth2.Experiment.Flows.UserInfoRequest (HasUserInfoRequest)
open Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
open Network.OAuth2.Experiment.Grants.AuthorizationCode
open Network.OAuth2.Experiment.Grants.DeviceAuthorization

-- ────────────────────────────────────────────────────────────────────
-- Internal helpers
-- ────────────────────────────────────────────────────────────────────

/-- Percent-encode `body` as an `application/x-www-form-urlencoded` request
    body, replacing the request's method with `POST` — a private copy of
    `Linen.Network.OAuth2.TokenRequest`'s own private helper for
    `http-client`'s `urlEncodedBody` (see the module doc-comment for why
    each module keeps its own). -/
private def urlEncodedBody (body : Network.OAuth2.Experiment.Utils.QueryParams) (req : Request) :
    Request :=
  let encoded :=
    "&".intercalate (body.map fun (k, v) => s!"{Network.HTTP.Types.urlEncode k}={Network.HTTP.Types.urlEncode v}")
  { req with
      method := .standard .POST
      body := some encoded.toUTF8
      headers := (hContentType, "application/x-www-form-urlencoded") :: req.headers.filter (fun h => h.1 != hContentType) }

/-- Conduct a request against a grant's `/token` endpoint and decode a
    successful response as a `TokenResponse` (`hoauth2`'s
    `conduitTokenRequestInternal`). -/
private def conduitTokenRequestInternal {i a : Type} [HasClientAuthenticationMethod a]
    (idpApp : IdpApplication i a) (body : Network.OAuth2.Experiment.Utils.QueryParams) :
    IO (Except TokenResponseError TokenResponse) := do
  let updateAuthHeader : Request → Request :=
    match getClientAuthenticationMethod idpApp.application with
    | .ClientSecretBasic => addClientAuthToHeader idpApp.application
    | .ClientSecretPost => id
    | .ClientAssertionJwt => id
  match uriToRequest idpApp.idp.idpTokenEndpoint with
  | .error e => pure (.error (parseTokeResponseError e))
  | .ok req0 =>
    let req' := (updateAuthHeader ∘ addDefaultRequestHeaders) req0
    let resp ← Network.HTTP.Client.Conduit.withResponse (urlEncodedBody body req') pure
    match handleOAuth2TokenResponse resp with
    | .error e => pure (.error e)
    | .ok raw => pure (parseResponseFlexible (a := TokenResponse) raw)

-- ────────────────────────────────────────────────────────────────────
-- Authorization Requests
-- ────────────────────────────────────────────────────────────────────

/-- Constructs an Authorization Code request URI according to
    RFC 6749 §4.1.1. -/
def mkAuthorizationRequest {i : Type} (idpApp : IdpApplication i AuthorizationCodeApplication) :
    Network.URI.URI :=
  let req := mkAuthorizationRequestParam idpApp.application
  let allParams := unionMapsToQueryParams [toQueryParam req]
  appendQueryParams allParams idpApp.idp.idpAuthorizeEndpoint

/-- Constructs an Authorization Code request URI with PKCE support
    according to RFC 7636. Returns both the authorization URI and the
    generated code verifier, which must be stored securely for the later
    token request. -/
def mkPkceAuthorizeRequest {i : Type} (idpApp : IdpApplication i AuthorizationCodeApplication) :
    IO (Network.URI.URI × CodeVerifier) := do
  let (req, codeVerifier) ← mkPkceAuthorizeRequestParam idpApp.application
  let allParams := unionMapsToQueryParams [toQueryParam req]
  pure (appendQueryParams allParams idpApp.idp.idpAuthorizeEndpoint, codeVerifier)

-- ────────────────────────────────────────────────────────────────────
-- Token Request
-- ────────────────────────────────────────────────────────────────────

/-- Sends a token request according to RFC 6749 §4.1.3: exchanges an
    authorization code, device code, or other grant-specific token for an
    access token. -/
def conduitTokenRequest {i a tokenRequest exchangeTokenInfo : Type}
    [HasTokenRequest a tokenRequest exchangeTokenInfo] [ToQueryParam tokenRequest]
    (idpApp : IdpApplication i a) (exchangeToken : exchangeTokenInfo) :
    IO (Except TokenResponseError TokenResponse) :=
  let req := mkTokenRequestParam idpApp.application exchangeToken
  conduitTokenRequestInternal idpApp (unionMapsToQueryParams [toQueryParam req])

-- ────────────────────────────────────────────────────────────────────
-- Device Auth
-- ────────────────────────────────────────────────────────────────────

/-- Makes a Device Authorization Request, RFC 8628 §3.1. -/
def conduitDeviceAuthorizationRequest {i : Type}
    (idpApp : IdpApplication i DeviceAuthorizationApplication) :
    IO (Except ByteArray DeviceAuthorizationResponse) := do
  match idpApp.idp.idpDeviceAuthorizationEndpoint with
  | none =>
    pure <| .error
      "[conduitDeviceAuthorizationRequest] Device Authorization Flow is not supported: missing device_authorization_endpoint.".toUTF8
  | some deviceAuthEndpoint =>
    let deviceAuthReq := mkDeviceAuthorizationRequestParam idpApp.application
    let body := unionMapsToQueryParams [toQueryParam deviceAuthReq]
    match uriToRequest deviceAuthEndpoint with
    | .error e => pure (.error (s!"[conduitDeviceAuthorizationRequest] {e}").toUTF8)
    | .ok req0 =>
      let req := addDefaultRequestHeaders req0
      let req' :=
        if idpApp.application.daAuthorizationRequestAuthenticationMethod == .ClientSecretBasic then
          addSecretToHeader idpApp.application.daClientId idpApp.application.daClientSecret req
        else req
      let resp ← Network.HTTP.Client.Conduit.withResponse (urlEncodedBody body req') pure
      pure <|
        Except.mapError (fun e => (s!"[conduitDeviceAuthorizationRequest] {String.fromUTF8! e}").toUTF8)
          (Network.HTTP.Client.Contrib.handleResponseJSON resp)

/-- See the module doc-comment for why this is a `while` loop (desugaring
    to `Loop.forIn`) rather than a `partial def` or a fuel parameter: the
    spec places no bound on how long polling may continue
    (`hoauth2`'s `pollDeviceTokenRequestInternal`). -/
private def pollDeviceTokenRequestInternal {i : Type}
    (idpApp : IdpApplication i DeviceAuthorizationApplication)
    (deviceCode : DeviceCode) (intervalSeconds : Int) :
    IO (Except TokenResponseError TokenResponse) := do
  let mut interval := intervalSeconds
  let mut done := false
  let mut result : Except TokenResponseError TokenResponse :=
    .error
      { tokenResponseError := .unknownErrorCode "unreachable"
        tokenResponseErrorDescription := none
        tokenResponseErrorUri := none }
  while !done do
    match ← conduitTokenRequest idpApp deviceCode with
    | .ok v =>
      result := .ok v
      done := true
    | .error trRespError =>
      match trRespError.tokenResponseError with
      -- Device Token Response additional error codes, RFC 8628 §3.5.
      | .unknownErrorCode "authorization_pending" =>
        IO.sleep (interval * 1000).toNat.toUInt32
      | .unknownErrorCode "slow_down" =>
        interval := interval + 5
        IO.sleep (interval * 1000).toNat.toUInt32
      | _ =>
        result := .error trRespError
        done := true
  pure result

/-- Polls for a token using the device authorization flow, RFC 8628 §3.5:
    handles the mandated retries and interval adjustments based on the
    IdP's `authorization_pending`/`slow_down` responses. -/
def pollDeviceTokenRequest {i : Type} (idpApp : IdpApplication i DeviceAuthorizationApplication)
    (deviceAuthResp : DeviceAuthorizationResponse) :
    IO (Except TokenResponseError TokenResponse) :=
  pollDeviceTokenRequestInternal idpApp deviceAuthResp.deviceCode (deviceAuthResp.interval.getD 5)

-- ────────────────────────────────────────────────────────────────────
-- PKCE Token Request
-- ────────────────────────────────────────────────────────────────────

/-- RFC 7636 §4.5: sends a token request augmented with the PKCE
    `code_verifier`. -/
def conduitPkceTokenRequest {i a tokenRequest exchangeTokenInfo : Type}
    [HasTokenRequest a tokenRequest exchangeTokenInfo] [ToQueryParam tokenRequest]
    (idpApp : IdpApplication i a) (exchangeToken : exchangeTokenInfo) (codeVerifier : CodeVerifier) :
    IO (Except TokenResponseError TokenResponse) :=
  let req := mkTokenRequestParam idpApp.application exchangeToken
  conduitTokenRequestInternal idpApp
    (unionMapsToQueryParams [toQueryParam req, toQueryParam codeVerifier])

-- ────────────────────────────────────────────────────────────────────
-- Refresh Token
-- ────────────────────────────────────────────────────────────────────

/-- Makes a Refresh Token Request according to RFC 6749 §6: obtains a new
    access token using a refresh token. -/
def conduitRefreshTokenRequest {i a : Type} [HasRefreshTokenRequest a]
    (idpApp : IdpApplication i a) (rt : Network.OAuth2.RefreshToken) :
    IO (Except TokenResponseError TokenResponse) :=
  let tokenReq := mkRefreshTokenRequestParam idpApp.application rt
  conduitTokenRequestInternal idpApp (unionMapsToQueryParams [toQueryParam tokenReq])

-- ────────────────────────────────────────────────────────────────────
-- User Info
-- ────────────────────────────────────────────────────────────────────

/-- Makes a request to the userinfo endpoint using a custom HTTP method.
    Some IdPs require a method other than `GET`, or custom headers, to
    fetch user information; this gives that flexibility. -/
def conduitUserInfoRequestWithCustomMethod {i a b : Type} [HasUserInfoRequest a] [FromJSON b]
    (fetchMethod : AccessToken → Network.URI.URI → IO (Except ByteArray b))
    (idpApp : IdpApplication i a) (token : AccessToken) : IO (Except ByteArray b) :=
  fetchMethod token idpApp.idp.idpUserInfoEndpoint

/-- Makes a standard `GET` request to the userinfo endpoint using an
    access token — commonly used with OpenID Connect providers to fetch a
    user's profile. -/
def conduitUserInfoRequest {i a b : Type} [HasUserInfoRequest a] [FromJSON b]
    (idpApp : IdpApplication i a) (token : AccessToken) : IO (Except ByteArray b) :=
  conduitUserInfoRequestWithCustomMethod Network.OAuth2.authGetJSON idpApp token

end Network.OAuth2.Experiment.Flows
