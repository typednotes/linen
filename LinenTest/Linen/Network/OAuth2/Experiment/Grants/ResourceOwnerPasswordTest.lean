/-
  Tests for `Linen.Network.OAuth2.Experiment.Grants.ResourceOwnerPassword`.
-/
import Linen.Network.OAuth2.Experiment.Grants.ResourceOwnerPassword

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.RefreshTokenRequest
open Network.OAuth2.Experiment.Grants.ResourceOwnerPassword

namespace Tests.Network.OAuth2.Experiment.Grants.ResourceOwnerPassword

private def sampleApp : ResourceOwnerPasswordApplication :=
  { ropClientId := ⟨"cid"⟩
    ropClientSecret := ⟨"csecret"⟩
    ropName := "sample"
    ropScope := Data.Set'.fromList [Scope.mk "offline_access"]
    ropUserName := ⟨"alice"⟩
    ropPassword := ⟨"s3cr3t"⟩
    ropTokenRequestExtraParams := Data.Map.empty
    ropClientAuthenticationMethod := .ClientSecretBasic }

#guard getClientAuthenticationMethod sampleApp == .ClientSecretBasic

#guard
  Data.Map.toList' (toQueryParam (mkTokenRequestParam sampleApp ⟨⟩)) ==
    [ ("grant_type", "password"), ("password", "s3cr3t")
    , ("scope", "offline_access"), ("username", "alice") ]

-- Refreshing omits `client_id`/`client_secret` unless the auth method is `ClientSecretPost`.
#guard (mkRefreshTokenRequestParam sampleApp ⟨"rtok"⟩).rrClientId == none

private def postApp : ResourceOwnerPasswordApplication :=
  { sampleApp with ropClientAuthenticationMethod := .ClientSecretPost }

#guard (mkRefreshTokenRequestParam postApp ⟨"rtok"⟩).rrClientId == some ⟨"cid"⟩
#guard (mkRefreshTokenRequestParam postApp ⟨"rtok"⟩).rrClientSecret == some ⟨"csecret"⟩
#guard (mkRefreshTokenRequestParam postApp ⟨"rtok"⟩).rrGrantType == .GTRefreshToken

end Tests.Network.OAuth2.Experiment.Grants.ResourceOwnerPassword
