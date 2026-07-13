/-
  Tests for `Linen.Network.OAuth2.Experiment.Types`.
-/
import Linen.Network.OAuth2.Experiment.Types

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Pkce (CodeVerifier CodeChallenge CodeChallengeMethod)

namespace Tests.Network.OAuth2.Experiment.Types

-- `ToQueryParam (Option a)` is `Data.Map.empty` for `none`, `toQueryParam a` for `some a`.
#guard (Data.Map.toList' (toQueryParam (none : Option ClientId))) == []
#guard (Data.Map.toList' (toQueryParam (some (ClientId.mk "abc")))) == [("client_id", "abc")]

-- Each credential newtype contributes its one query parameter.
#guard Data.Map.toList' (toQueryParam (ClientId.mk "cid")) == [("client_id", "cid")]
#guard Data.Map.toList' (toQueryParam (ClientSecret.mk "sec")) == [("client_secret", "sec")]
#guard Data.Map.toList' (toQueryParam (Username.mk "bob")) == [("username", "bob")]
#guard Data.Map.toList' (toQueryParam (Password.mk "pw")) == [("password", "pw")]
#guard Data.Map.toList' (toQueryParam (AuthorizeState.mk "st8")) == [("state", "st8")]

#guard
  Data.Map.toList'
    (toQueryParam (RedirectUri.mk (Network.URI.parseURI "https://client.example.com/cb").get!))
    == [("redirect_uri", "https://client.example.com/cb")]

-- Every `GrantTypeValue` renders its RFC 6749/RFC 7523/RFC 8628 `grant_type` string.
#guard Data.Map.toList' (toQueryParam GrantTypeValue.GTAuthorizationCode) == [("grant_type", "authorization_code")]
#guard Data.Map.toList' (toQueryParam GrantTypeValue.GTPassword) == [("grant_type", "password")]
#guard Data.Map.toList' (toQueryParam GrantTypeValue.GTClientCredentials) == [("grant_type", "client_credentials")]
#guard Data.Map.toList' (toQueryParam GrantTypeValue.GTRefreshToken) == [("grant_type", "refresh_token")]
#guard
  Data.Map.toList' (toQueryParam GrantTypeValue.GTJwtBearer)
    == [("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer")]
#guard
  Data.Map.toList' (toQueryParam GrantTypeValue.GTDeviceCode)
    == [("grant_type", "urn:ietf:params:oauth:grant-type:device_code")]

#guard Data.Map.toList' (toQueryParam ResponseType.Code) == [("response_type", "code")]

-- A `Set' Scope` renders as a single space-joined `scope` parameter, empty maps to nothing.
#guard Data.Map.toList' (toQueryParam (Data.Set'.empty : Data.Set' Scope)) == []
#guard
  Data.Map.toList' (toQueryParam (Data.Set'.fromList [Scope.mk "openid", Scope.mk "email"] : Data.Set' Scope))
    == [("scope", "email openid")]

-- PKCE and OAuth2 core tokens each contribute their own parameter.
#guard Data.Map.toList' (toQueryParam (CodeVerifier.mk "verifier123")) == [("code_verifier", "verifier123")]
#guard Data.Map.toList' (toQueryParam (CodeChallenge.mk "challenge123")) == [("code_challenge", "challenge123")]
#guard Data.Map.toList' (toQueryParam CodeChallengeMethod.S256) == [("code_challenge_method", "S256")]
#guard
  Data.Map.toList' (toQueryParam (⟨"authcode"⟩ : Network.OAuth2.ExchangeToken)) == [("code", "authcode")]
#guard
  Data.Map.toList' (toQueryParam (⟨"rtok"⟩ : Network.OAuth2.RefreshToken)) == [("refresh_token", "rtok")]

/-! ### `Idp`/`IdpApplication` — signature-only, as they carry no logic of their own -/

private inductive GoogleMarker : Type

private def googleIdp : Idp GoogleMarker :=
  { idpUserInfoEndpoint := Network.URI.nullURI
    idpAuthorizeEndpoint := Network.URI.nullURI
    idpTokenEndpoint := Network.URI.nullURI
    idpDeviceAuthorizationEndpoint := none }

private def googleApp : IdpApplication GoogleMarker Unit :=
  { idp := googleIdp, application := () }

#guard googleApp.idp.idpDeviceAuthorizationEndpoint == none

end Tests.Network.OAuth2.Experiment.Types
