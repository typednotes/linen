/-
  Linen.Network.OAuth2.Experiment.Grants.ClientCredentials — Client
  Credentials Grant

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Grants.ClientCredentials`
  (see `docs/imports/hoauth2/dependencies.md`), module #15: the
  `ClientCredentialsApplication` configuration and its `/token` request
  shape, RFC 6749 §4.4.

  ## Substitutions
  - **`TypeFamilies`**: upstream's `instance HasTokenRequest
    ClientCredentialsApplication` declares its associated `TokenRequest`/
    `ExchangeTokenInfo` types inline via `data`/`type` family syntax; this
    port instead declares `ClientCredentialsTokenRequest` as an ordinary
    top-level structure and instantiates
    `Network.OAuth2.Experiment.Flows.TokenRequest.HasTokenRequest`'s
    `outParam` parameters with it — see that module's own doc-comment for
    why `outParam` class parameters stand in for Lean's lack of type
    families.
  - `Data.Set`/`Data.Map.Strict` are `Linen.Data.Set`/`Linen.Data.Map`;
    `Data.Text.Lazy` is `linen`'s single `String` type (see
    `Network.OAuth2.Experiment.Types`'s doc-comment for the same
    substitutions).
-/

import Linen.Network.OAuth2.Experiment.Flows.TokenRequest
import Linen.Network.OAuth2.Experiment.Types
import Linen.Network.OAuth2.Experiment.Utils
import Linen.Data.Map
import Linen.Data.Set

namespace Network.OAuth2.Experiment.Grants.ClientCredentials

open Network.OAuth2.Internal (ClientAuthenticationMethod)
open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest

-- ────────────────────────────────────────────────────────────────────
-- Application
-- ────────────────────────────────────────────────────────────────────

/-- An application that supports the Client Credentials Grant,
    RFC 6749 §4.4. -/
structure ClientCredentialsApplication where
  ccClientId : ClientId
  ccClientSecret : ClientSecret
  ccName : String
  ccScope : Data.Set' Scope
  ccTokenRequestExtraParams : Data.Map String String
  ccClientAuthenticationMethod : ClientAuthenticationMethod

instance : HasClientAuthenticationMethod ClientCredentialsApplication where
  getClientAuthenticationMethod app := app.ccClientAuthenticationMethod
  addClientAuthToHeader app := addSecretToHeader app.ccClientId app.ccClientSecret

-- ────────────────────────────────────────────────────────────────────
-- Token request
-- ────────────────────────────────────────────────────────────────────

/-- The `/token` request parameters for the Client Credentials Grant,
    RFC 6749 §4.4.2. -/
structure ClientCredentialsTokenRequest where
  trScope : Data.Set' Scope
  trGrantType : GrantTypeValue
  trClientSecret : ClientSecret
  trClientId : ClientId
  trExtraParams : Data.Map String String
  trClientAuthenticationMethod : ClientAuthenticationMethod

instance :
    HasTokenRequest ClientCredentialsApplication ClientCredentialsTokenRequest
      NoNeedExchangeToken where
  mkTokenRequestParam app _ :=
    { trScope := app.ccScope
      trGrantType := .GTClientCredentials
      trClientSecret := app.ccClientSecret
      trClientAuthenticationMethod := app.ccClientAuthenticationMethod
      trExtraParams := app.ccTokenRequestExtraParams
      trClientId := app.ccClientId }

instance : ToQueryParam ClientCredentialsTokenRequest where
  toQueryParam r :=
    let extraBodyBasedOnClientAuthMethod :=
      match r.trClientAuthenticationMethod with
      | .ClientAssertionJwt =>
        [ Data.Map.fromList
            [ ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
            , ("client_assertion", r.trClientSecret.unClientSecret) ] ]
      | .ClientSecretPost => [toQueryParam r.trClientId, toQueryParam r.trClientSecret]
      | .ClientSecretBasic => []
    ([toQueryParam r.trGrantType, toQueryParam r.trScope, r.trExtraParams]
      ++ extraBodyBasedOnClientAuthMethod).foldl Data.Map.union Data.Map.empty

end Network.OAuth2.Experiment.Grants.ClientCredentials
