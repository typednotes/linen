/-
  Tests for `Linen.Network.OAuth2.Experiment.Flows.TokenRequest`.

  Most of this module is class *signatures* (`HasClientAuthenticationMethod`,
  `HasTokenRequest`) that grant-type modules from a later batch instantiate;
  these tests pin down the signatures and exercise the one concrete
  function, `addSecretToHeader`.
-/
import Linen.Network.OAuth2.Experiment.Flows.TokenRequest

open Network.OAuth2.Internal (ClientAuthenticationMethod)
open Network.OAuth2.Experiment.Types (ClientId ClientSecret)
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.HTTP.Client (Request)
open Network.HTTP.Types (hAuthorization)

namespace Tests.Network.OAuth2.Experiment.Flows.TokenRequest

-- `addSecretToHeader` adds a `Basic` auth header built from client id/secret.
private def sampleRequest : Request :=
  { method := .standard .POST, host := "idp.example.com", port := 443
    path := "/token", queryString := "", headers := [], body := none, isSecure := true }

#guard
  (addSecretToHeader ⟨"cid"⟩ ⟨"secret"⟩ sampleRequest).headers.any
    (fun h => h.1 == hAuthorization && h.2.startsWith "Basic ")

-- A dummy grant-type application configuration, exercising
-- `HasClientAuthenticationMethod`'s default `addClientAuthToHeader`.
private structure DummyApp where

private instance : HasClientAuthenticationMethod DummyApp where
  getClientAuthenticationMethod _ := .ClientSecretPost

#guard getClientAuthenticationMethod (DummyApp.mk) == .ClientSecretPost
#guard (addClientAuthToHeader (DummyApp.mk) sampleRequest).headers == sampleRequest.headers

-- `HasTokenRequest`: a dummy instance fixing its two `outParam` associated
-- types (the substitute for upstream's `TokenRequest a`/`ExchangeTokenInfo
-- a` data/type families — see the module doc-comment).
private structure DummyTokenRequest where
  grantType : String

private instance : HasTokenRequest DummyApp DummyTokenRequest NoNeedExchangeToken where
  mkTokenRequestParam _ _ := { grantType := "dummy" }

#guard (mkTokenRequestParam (DummyApp.mk) NoNeedExchangeToken.mk).grantType == "dummy"

end Tests.Network.OAuth2.Experiment.Flows.TokenRequest
