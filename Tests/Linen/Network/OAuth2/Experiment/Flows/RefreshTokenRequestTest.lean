/-
  Tests for `Linen.Network.OAuth2.Experiment.Flows.RefreshTokenRequest`.
-/
import Linen.Network.OAuth2.Experiment.Flows.RefreshTokenRequest

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.RefreshTokenRequest
open Network.OAuth2.Experiment.Flows.TokenRequest (HasClientAuthenticationMethod
  getClientAuthenticationMethod)

namespace Tests.Network.OAuth2.Experiment.Flows.RefreshTokenRequest

private def sampleParam : RefreshTokenRequest :=
  { rrRefreshToken := ⟨"rtok123"⟩
    rrGrantType := .GTRefreshToken
    rrScope := Data.Set'.fromList [Scope.mk "offline_access"]
    rrClientId := some ⟨"client1"⟩
    rrClientSecret := some ⟨"secret1"⟩ }

#guard Data.Map.toList' (toQueryParam sampleParam) ==
  [ ("client_id", "client1")
  , ("client_secret", "secret1")
  , ("grant_type", "refresh_token")
  , ("refresh_token", "rtok123")
  , ("scope", "offline_access") ]

-- Optional fields simply drop their keys when absent.
#guard
  Data.Map.toList' (toQueryParam { sampleParam with rrClientId := none, rrClientSecret := none }) ==
    [("grant_type", "refresh_token"), ("refresh_token", "rtok123"), ("scope", "offline_access")]

-- `HasRefreshTokenRequest` extends `HasClientAuthenticationMethod`; a dummy
-- instance pins down the signature.
private structure DummyApp where

private instance : HasClientAuthenticationMethod DummyApp where
  getClientAuthenticationMethod _ := .ClientSecretBasic

private instance : HasRefreshTokenRequest DummyApp where
  mkRefreshTokenRequestParam _ token :=
    { rrRefreshToken := token
      rrGrantType := .GTRefreshToken
      rrScope := Data.Set'.empty
      rrClientId := none
      rrClientSecret := none }

#guard
  (mkRefreshTokenRequestParam (DummyApp.mk) (⟨"newtok"⟩ : Network.OAuth2.RefreshToken)).rrRefreshToken
    == ⟨"newtok"⟩
#guard getClientAuthenticationMethod (DummyApp.mk) == .ClientSecretBasic

end Tests.Network.OAuth2.Experiment.Flows.RefreshTokenRequest
