/-
  Tests for `Linen.Network.OAuth2.Experiment.Grants.DeviceAuthorization`.
-/
import Linen.Network.OAuth2.Experiment.Grants.DeviceAuthorization

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
open Network.OAuth2.Experiment.Grants.DeviceAuthorization

namespace Tests.Network.OAuth2.Experiment.Grants.DeviceAuthorization

private def sampleApp : DeviceAuthorizationApplication :=
  { daName := "sample"
    daClientId := ⟨"cid"⟩
    daClientSecret := ⟨"csecret"⟩
    daScope := Data.Set'.fromList [Scope.mk "offline_access"]
    daAuthorizationRequestExtraParam := Data.Map.empty
    daAuthorizationRequestAuthenticationMethod := .ClientSecretBasic }

#guard getClientAuthenticationMethod sampleApp == .ClientSecretBasic

-- With `ClientSecretBasic`, `client_id` is omitted from both requests.
#guard (mkDeviceAuthorizationRequestParam sampleApp).darClientId == none
#guard (mkTokenRequestParam sampleApp ⟨"devcode123"⟩).trClientId == none

-- With `ClientSecretPost`, `client_id` is included in both requests.
private def postApp : DeviceAuthorizationApplication :=
  { sampleApp with daAuthorizationRequestAuthenticationMethod := .ClientSecretPost }

#guard (mkDeviceAuthorizationRequestParam postApp).darClientId == some ⟨"cid"⟩
#guard (mkTokenRequestParam postApp ⟨"devcode123"⟩).trClientId == some ⟨"cid"⟩

#guard
  Data.Map.toList' (toQueryParam (mkTokenRequestParam postApp ⟨"devcode123"⟩)) ==
    [ ("client_id", "cid")
    , ("device_code", "devcode123")
    , ("grant_type", "urn:ietf:params:oauth:grant-type:device_code") ]

end Tests.Network.OAuth2.Experiment.Grants.DeviceAuthorization
