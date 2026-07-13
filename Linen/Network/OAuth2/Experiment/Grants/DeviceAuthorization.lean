/-
  Linen.Network.OAuth2.Experiment.Grants.DeviceAuthorization — Device
  Authorization Grant

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Grants.DeviceAuthorization`
  (see `docs/imports/hoauth2/dependencies.md`), module #16: the
  `DeviceAuthorizationApplication` configuration, its device-authorization
  request builder, and its `/token` request shape, RFC 8628 §3.1/§3.4.

  ## Substitutions
  - **`TypeFamilies`**: as in `.ClientCredentials`, the associated
    `TokenRequest`/`ExchangeTokenInfo` types are ported as an ordinary
    top-level `DeviceAuthorizationTokenRequest` structure plus `outParam`
    instantiation — see
    `Network.OAuth2.Experiment.Flows.TokenRequest`'s doc-comment.
  - `Data.Set`/`Data.Map.Strict` are `Linen.Data.Set`/`Linen.Data.Map`;
    `Data.Text.Lazy` is `linen`'s single `String` type (see
    `Network.OAuth2.Experiment.Types`'s doc-comment for the same
    substitutions).
-/

import Linen.Network.OAuth2
import Linen.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
import Linen.Network.OAuth2.Experiment.Flows.TokenRequest
import Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest
import Linen.Network.OAuth2.Experiment.Types
import Linen.Data.Map
import Linen.Data.Set

namespace Network.OAuth2.Experiment.Grants.DeviceAuthorization

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
open Network.OAuth2.Experiment.Flows.UserInfoRequest (HasUserInfoRequest)

-- ────────────────────────────────────────────────────────────────────
-- Application
-- ────────────────────────────────────────────────────────────────────

/-- An application that supports the Device Authorization Grant,
    RFC 8628 §3.1. -/
structure DeviceAuthorizationApplication where
  daName : String
  daClientId : ClientId
  daClientSecret : ClientSecret
  daScope : Data.Set' Scope
  /-- Additional parameters to the device authorization request. Most
      identity providers follow the spec strictly but AzureAD requires a
      `tenant` parameter. -/
  daAuthorizationRequestExtraParam : Data.Map String String
  /-- The spec requires a similar authentication method as the `/token`
      request. Most identity providers don't require it, but some do
      (e.g. Okta). -/
  daAuthorizationRequestAuthenticationMethod : Network.OAuth2.ClientAuthenticationMethod

instance : HasClientAuthenticationMethod DeviceAuthorizationApplication where
  getClientAuthenticationMethod app := app.daAuthorizationRequestAuthenticationMethod
  addClientAuthToHeader app := addSecretToHeader app.daClientId app.daClientSecret

/-- Build the device-authorization request parameters, RFC 8628 §3.1. -/
def mkDeviceAuthorizationRequestParam (app : DeviceAuthorizationApplication) :
    DeviceAuthorizationRequestParam :=
  { darScope := app.daScope
    darClientId :=
      if app.daAuthorizationRequestAuthenticationMethod == .ClientSecretPost
        then some app.daClientId
        else none
    darExtraParams := app.daAuthorizationRequestExtraParam }

-- ────────────────────────────────────────────────────────────────────
-- Token request
-- ────────────────────────────────────────────────────────────────────

/-- The `/token` request parameters for the Device Authorization Grant,
    RFC 8628 §3.4. -/
structure DeviceAuthorizationTokenRequest where
  trCode : DeviceCode
  trGrantType : GrantTypeValue
  trClientId : Option ClientId

instance :
    HasTokenRequest DeviceAuthorizationApplication DeviceAuthorizationTokenRequest
      DeviceCode where
  mkTokenRequestParam app deviceCode :=
    -- This is a bit hacky! The token request uses `ClientSecretBasic` by
    -- default (has to pick one client-authentication method). `client_id`
    -- should also be in the request body per spec. However, for some IdPs
    -- (e.g. Okta), when using `ClientSecretBasic` to authenticate the
    -- client, they don't allow `client_id` in the request body.
    -- `daAuthorizationRequestAuthenticationMethod` sets the tone for the
    -- authorization request, hence we just follow it here in the token
    -- request too.
    { trCode := deviceCode
      trGrantType := .GTDeviceCode
      trClientId :=
        if app.daAuthorizationRequestAuthenticationMethod == .ClientSecretPost
          then some app.daClientId
          else none }

instance : ToQueryParam DeviceAuthorizationTokenRequest where
  toQueryParam r :=
    [toQueryParam r.trCode, toQueryParam r.trGrantType, toQueryParam r.trClientId].foldl
      Data.Map.union Data.Map.empty

instance : HasUserInfoRequest DeviceAuthorizationApplication where

end Network.OAuth2.Experiment.Grants.DeviceAuthorization
