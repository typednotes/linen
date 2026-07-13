/-
  Tests for `Linen.Network.OAuth2.Experiment.Grants.ClientCredentials`.
-/
import Linen.Network.OAuth2.Experiment.Grants.ClientCredentials

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Grants.ClientCredentials

namespace Tests.Network.OAuth2.Experiment.Grants.ClientCredentials

private def sampleApp : ClientCredentialsApplication :=
  { ccClientId := ⟨"cid"⟩
    ccClientSecret := ⟨"csecret"⟩
    ccName := "sample"
    ccScope := Data.Set'.fromList [Scope.mk "api.read"]
    ccTokenRequestExtraParams := Data.Map.empty
    ccClientAuthenticationMethod := .ClientSecretBasic }

#guard getClientAuthenticationMethod sampleApp == .ClientSecretBasic

-- `ClientSecretBasic` puts nothing extra in the body.
#guard
  Data.Map.toList' (toQueryParam (mkTokenRequestParam sampleApp ⟨⟩)) ==
    [("grant_type", "client_credentials"), ("scope", "api.read")]

-- `ClientSecretPost` adds `client_id`/`client_secret` to the body.
#guard
  Data.Map.toList'
    (toQueryParam (mkTokenRequestParam { sampleApp with ccClientAuthenticationMethod := .ClientSecretPost } ⟨⟩)) ==
    [ ("client_id", "cid"), ("client_secret", "csecret")
    , ("grant_type", "client_credentials"), ("scope", "api.read") ]

-- `ClientAssertionJwt` adds `client_assertion`/`client_assertion_type`.
#guard
  Data.Map.toList'
    (toQueryParam (mkTokenRequestParam { sampleApp with ccClientAuthenticationMethod := .ClientAssertionJwt } ⟨⟩)) ==
    [ ("client_assertion", "csecret")
    , ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
    , ("grant_type", "client_credentials"), ("scope", "api.read") ]

end Tests.Network.OAuth2.Experiment.Grants.ClientCredentials
