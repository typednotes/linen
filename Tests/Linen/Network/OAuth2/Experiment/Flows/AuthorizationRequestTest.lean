/-
  Tests for `Linen.Network.OAuth2.Experiment.Flows.AuthorizationRequest`.
-/
import Linen.Network.OAuth2.Experiment.Flows.AuthorizationRequest

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.AuthorizationRequest

namespace Tests.Network.OAuth2.Experiment.Flows.AuthorizationRequest

private def sampleParam : AuthorizationRequestParam :=
  { arScope := Data.Set'.fromList [Scope.mk "openid", Scope.mk "email"]
    arState := ⟨"xyz"⟩
    arClientId := ⟨"client1"⟩
    arRedirectUri := some ⟨(Network.URI.parseURI "https://client.example.com/cb").get!⟩
    arResponseType := .Code
    arExtraParams := Data.Map.singleton "audience" "api" }

-- Every field contributes its own query parameter; `arExtraParams` is
-- merged in too, and no earlier field's key is clobbered by it.
#guard Data.Map.toList' (toQueryParam sampleParam) ==
  [ ("audience", "api")
  , ("client_id", "client1")
  , ("redirect_uri", "https://client.example.com/cb")
  , ("response_type", "code")
  , ("scope", "email openid")
  , ("state", "xyz") ]

-- Omitting `arRedirectUri` simply drops that key.
#guard
  (Data.Map.toList' (toQueryParam { sampleParam with arRedirectUri := none })).contains
      ("redirect_uri", "https://client.example.com/cb") == false

end Tests.Network.OAuth2.Experiment.Flows.AuthorizationRequest
