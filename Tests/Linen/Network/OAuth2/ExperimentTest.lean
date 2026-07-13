/-
  Tests for `Linen.Network.OAuth2.Experiment` — the top-level facade over
  the typed OAuth2 request-builder API (`Types`, `Grants`, `Flows`, `Pkce`,
  `Utils`, and `Network.OAuth2.ClientAuthenticationMethod`). This
  smoke-tests that every re-exported name resolves through the facade
  namespace; each underlying submodule's own test file exercises the real
  behaviour in depth (see `Tests/Linen/Network/OAuth2/Experiment/*`).
-/
import Linen.Network.OAuth2.Experiment
import Linen.Network.URI

open Network.URI
open Network.OAuth2.Experiment

namespace Tests.Network.OAuth2.Experiment

-- `Network.OAuth2.ClientAuthenticationMethod` resolves through the facade.
#guard ((.ClientSecretBasic : ClientAuthenticationMethod) == .ClientSecretBasic)

-- `Types` names (`Idp`/`IdpApplication`, request-parameter newtypes,
-- `ToQueryParam`) resolve through the facade.
#guard (⟨"openid"⟩ : Scope).unScope == "openid"
#guard toQueryParam (⟨"cid"⟩ : ClientId) == Data.Map.fromList [("client_id", "cid")]
#guard toQueryParam (.GTPassword : GrantTypeValue) == Data.Map.fromList [("grant_type", "password")]

private def sampleIdp : Idp Unit :=
  { idpUserInfoEndpoint := (parseURI "https://idp.example.com/userinfo").get!
    idpAuthorizeEndpoint := (parseURI "https://idp.example.com/authorize").get!
    idpTokenEndpoint := (parseURI "https://idp.example.com/token").get!
    idpDeviceAuthorizationEndpoint := none }

-- `Pkce` names resolve through the facade.
#guard cvMaxLen == 128

-- `Utils` names resolve through the facade.
#guard unionMapsToQueryParams [Data.Map.fromList [("a", "1")], Data.Map.fromList [("b", "2")]]
    == [("a", "1"), ("b", "2")]
#guard uriToText sampleIdp.idpTokenEndpoint == "https://idp.example.com/token"

-- `Grants` application configurations resolve through the facade.
private def ccApp : ClientCredentialsApplication :=
  { ccClientId := ⟨"cid"⟩
    ccClientSecret := ⟨"csecret"⟩
    ccName := "sample"
    ccScope := Data.Set'.empty
    ccTokenRequestExtraParams := Data.Map.empty
    ccClientAuthenticationMethod := .ClientSecretBasic }

#guard (mkTokenRequestParam ccApp {} : ClientCredentialsTokenRequest).trGrantType == .GTClientCredentials

-- `Flows` entry points resolve through the facade (pure ones checked for
-- real behaviour; network-performing ones pinned at the type level, see
-- `Tests/Linen/Network/OAuth2/Experiment/FlowsTest.lean`).
private def acApp : AuthorizationCodeApplication :=
  { acName := "sample"
    acClientId := ⟨"cid"⟩
    acClientSecret := ⟨"csecret"⟩
    acScope := Data.Set'.empty
    acRedirectUri := (parseURI "https://client.example.com/cb").get!
    acAuthorizeState := ⟨"st8"⟩
    acAuthorizeRequestExtraParams := Data.Map.empty
    acClientAuthenticationMethod := .ClientSecretBasic }

private def sampleIdpApp : IdpApplication Unit AuthorizationCodeApplication :=
  { idp := sampleIdp, application := acApp }

#guard (mkAuthorizationRequest sampleIdpApp).uriPath == "/authorize"

example {i a b : Type} [HasUserInfoRequest a] [Data.Json.FromJSON b] :
    IdpApplication i a → Network.OAuth2.AccessToken → IO (Except ByteArray b) :=
  conduitUserInfoRequest

example {i : Type} :
    IdpApplication i DeviceAuthorizationApplication →
      IO (Except ByteArray DeviceAuthorizationResponse) :=
  conduitDeviceAuthorizationRequest

example (dc : DeviceCode) : DeviceCode := dc

end Tests.Network.OAuth2.Experiment
