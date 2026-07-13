/-
  Tests for `Linen.Network.OAuth2.Experiment.Grants.JwtBearer`.
-/
import Linen.Network.OAuth2.Experiment.Grants.JwtBearer
import Linen.Network.HTTP.Client.Types

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Grants.JwtBearer

namespace Tests.Network.OAuth2.Experiment.Grants.JwtBearer

private def sampleApp : JwtBearerApplication :=
  { jbName := "sample", jbJwtAssertion := "signed.jwt.assertion".toUTF8 }

-- The grant always authenticates via a signed JWT assertion.
#guard getClientAuthenticationMethod sampleApp == .ClientAssertionJwt

-- The default `addClientAuthToHeader` leaves the request unchanged (no
-- override for JWT Bearer, unlike the client-secret grants).
private def sampleRequest : Network.HTTP.Client.Request :=
  { method := .standard .GET, host := "example.com", port := 443 }

#guard (addClientAuthToHeader sampleApp sampleRequest).headers == sampleRequest.headers

#guard
  Data.Map.toList' (toQueryParam (mkTokenRequestParam sampleApp ⟨⟩)) ==
    [ ("assertion", "signed.jwt.assertion")
    , ("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer") ]

end Tests.Network.OAuth2.Experiment.Grants.JwtBearer
