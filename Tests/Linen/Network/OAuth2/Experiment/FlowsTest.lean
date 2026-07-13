/-
  Tests for `Linen.Network.OAuth2.Experiment.Flows`.

  `mkAuthorizationRequest` is pure, so it gets a real `#guard`.
  `mkPkceAuthorizeRequest` touches the same OpenSSL-backed random-generator
  FFI as `Network.OAuth2.Experiment.Pkce.mkPkceParam`, so — like
  `Grants/AuthorizationCodeTest.lean`'s own PKCE test — it is exercised via
  `#eval` with a small local `check` helper rather than `#guard`. Every
  other function here performs real network IO (`conduitTokenRequest`,
  `conduitDeviceAuthorizationRequest`, `pollDeviceTokenRequest`,
  `conduitPkceTokenRequest`, `conduitRefreshTokenRequest`,
  `conduitUserInfoRequest`/`conduitUserInfoRequestWithCustomMethod`), so —
  matching `HttpClientTest.lean`'s precedent — they are pinned at the type
  level with `example` instead.
-/
import Linen.Network.OAuth2.Experiment.Flows

open Network.OAuth2
open Network.URI
open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Pkce (CodeVerifier)
open Network.OAuth2.Experiment.Flows
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.RefreshTokenRequest
open Network.OAuth2.Experiment.Flows.UserInfoRequest (HasUserInfoRequest)
open Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
open Network.OAuth2.Experiment.Grants.AuthorizationCode
open Network.OAuth2.Experiment.Grants.DeviceAuthorization
open Data.Json (FromJSON)

namespace Tests.Network.OAuth2.Experiment.Flows

private def sampleIdp : Idp Unit :=
  { idpUserInfoEndpoint := (parseURI "https://idp.example.com/userinfo").get!
    idpAuthorizeEndpoint := (parseURI "https://idp.example.com/authorize").get!
    idpTokenEndpoint := (parseURI "https://idp.example.com/token").get!
    idpDeviceAuthorizationEndpoint := some (parseURI "https://idp.example.com/device").get! }

private def sampleAcApp : AuthorizationCodeApplication :=
  { acName := "sample"
    acClientId := ⟨"cid"⟩
    acClientSecret := ⟨"csecret"⟩
    acScope := Data.Set'.fromList [⟨"openid"⟩]
    acRedirectUri := (parseURI "https://client.example.com/cb").get!
    acAuthorizeState := ⟨"st8"⟩
    acAuthorizeRequestExtraParams := Data.Map.empty
    acClientAuthenticationMethod := .ClientSecretBasic }

private def sampleIdpApp : IdpApplication Unit AuthorizationCodeApplication :=
  { idp := sampleIdp, application := sampleAcApp }

-- `mkAuthorizationRequest` builds a `code`-response-type request URI
-- against the IdP's authorize endpoint, carrying the app's scope/state.
#guard
  let uri := mkAuthorizationRequest sampleIdpApp
  uri.uriPath == "/authorize" && uri.uriQuery.startsWith "?"

private def check (b : Bool) (msg : String) : IO Unit := unless b do throw (IO.userError msg)

-- `mkPkceAuthorizeRequest` generates a fresh PKCE pair and folds its
-- `code_challenge`/`code_challenge_method` into the authorization URI.
#eval show IO Unit from do
  let (uri, _codeVerifier) ← mkPkceAuthorizeRequest sampleIdpApp
  check ((uri.uriQuery.splitOn "code_challenge=").length > 1) "expected a code_challenge parameter"

/-! ### Signatures — real-network-IO functions -/

example {i a tokenRequest exchangeTokenInfo : Type}
    [HasTokenRequest a tokenRequest exchangeTokenInfo] [ToQueryParam tokenRequest] :
    IdpApplication i a → exchangeTokenInfo → IO (Except TokenResponseError TokenResponse) :=
  conduitTokenRequest

example {i : Type} :
    IdpApplication i DeviceAuthorizationApplication →
      IO (Except ByteArray DeviceAuthorizationResponse) :=
  conduitDeviceAuthorizationRequest

example {i : Type} :
    IdpApplication i DeviceAuthorizationApplication → DeviceAuthorizationResponse →
      IO (Except TokenResponseError TokenResponse) :=
  pollDeviceTokenRequest

example {i a tokenRequest exchangeTokenInfo : Type}
    [HasTokenRequest a tokenRequest exchangeTokenInfo] [ToQueryParam tokenRequest] :
    IdpApplication i a → exchangeTokenInfo → CodeVerifier →
      IO (Except TokenResponseError TokenResponse) :=
  conduitPkceTokenRequest

example {i a : Type} [HasRefreshTokenRequest a] :
    IdpApplication i a → Network.OAuth2.RefreshToken → IO (Except TokenResponseError TokenResponse) :=
  conduitRefreshTokenRequest

example {i a b : Type} [HasUserInfoRequest a] [FromJSON b] :
    (AccessToken → Network.URI.URI → IO (Except ByteArray b)) →
      IdpApplication i a → AccessToken → IO (Except ByteArray b) :=
  conduitUserInfoRequestWithCustomMethod

example {i a b : Type} [HasUserInfoRequest a] [FromJSON b] :
    IdpApplication i a → AccessToken → IO (Except ByteArray b) :=
  conduitUserInfoRequest

end Tests.Network.OAuth2.Experiment.Flows
