/-
  Tests for `Linen.Network.OAuth2.Experiment.Grants.AuthorizationCode`.
-/
import Linen.Network.OAuth2.Experiment.Grants.AuthorizationCode

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.RefreshTokenRequest
open Network.OAuth2.Experiment.Grants.AuthorizationCode

namespace Tests.Network.OAuth2.Experiment.Grants.AuthorizationCode

private def redirectUri : Network.URI.URI :=
  (Network.URI.parseURI "https://client.example.com/cb").get!

private def sampleApp : AuthorizationCodeApplication :=
  { acName := "sample"
    acClientId := ⟨"cid"⟩
    acClientSecret := ⟨"csecret"⟩
    acScope := Data.Set'.fromList [Scope.mk "openid", Scope.mk "email"]
    acRedirectUri := redirectUri
    acAuthorizeState := ⟨"st8"⟩
    acAuthorizeRequestExtraParams := Data.Map.empty
    acClientAuthenticationMethod := .ClientSecretBasic }

#guard getClientAuthenticationMethod sampleApp == .ClientSecretBasic

#guard
  Data.Map.toList' (toQueryParam (mkAuthorizationRequestParam sampleApp)) ==
    [ ("client_id", "cid"), ("redirect_uri", "https://client.example.com/cb")
    , ("response_type", "code"), ("scope", "email openid"), ("state", "st8") ]

-- `ClientSecretBasic` puts nothing extra in the `/token` body.
#guard
  Data.Map.toList' (toQueryParam (mkTokenRequestParam sampleApp ⟨"authcode123"⟩)) ==
    [ ("code", "authcode123"), ("grant_type", "authorization_code")
    , ("redirect_uri", "https://client.example.com/cb")]

-- `ClientSecretPost` adds `client_id`/`client_secret` to the `/token` body.
#guard
  Data.Map.toList'
    (toQueryParam
      (mkTokenRequestParam { sampleApp with acClientAuthenticationMethod := .ClientSecretPost }
        ⟨"authcode123"⟩)) ==
    [ ("client_id", "cid"), ("client_secret", "csecret")
    , ("code", "authcode123"), ("grant_type", "authorization_code")
    , ("redirect_uri", "https://client.example.com/cb") ]

-- Refreshing omits `client_id`/`client_secret` unless the auth method is `ClientSecretPost`.
#guard (mkRefreshTokenRequestParam sampleApp ⟨"rtok"⟩).rrClientId == none

private def postApp : AuthorizationCodeApplication :=
  { sampleApp with acClientAuthenticationMethod := .ClientSecretPost }

#guard (mkRefreshTokenRequestParam postApp ⟨"rtok"⟩).rrClientId == some ⟨"cid"⟩
#guard (mkRefreshTokenRequestParam postApp ⟨"rtok"⟩).rrClientSecret == some ⟨"csecret"⟩

-- `mkPkceAuthorizeRequestParam` runs in `IO` (real OpenSSL FFI calls via
-- `Pkce.mkPkceParam`), so it is checked with `#eval` (a thrown error fails
-- the build) — see `Tests.Network.OAuth2.Experiment.Pkce`'s own doc-comment
-- for the same pattern.
private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

-- PKCE-augmented authorize request adds `code_challenge`/`code_challenge_method` and
-- hands back the matching `code_verifier`.
#eval show IO Unit from do
  let (param, verifier) ← mkPkceAuthorizeRequestParam sampleApp
  let params := Data.Map.toList' param.arExtraParams
  check (params.length == 2) s!"extra params: {params}"
  check (verifier.unCodeVerifier.length == 128) "code verifier length"

end Tests.Network.OAuth2.Experiment.Grants.AuthorizationCode
