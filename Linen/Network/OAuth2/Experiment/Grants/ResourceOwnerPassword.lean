/-
  Linen.Network.OAuth2.Experiment.Grants.ResourceOwnerPassword — Resource
  Owner Password Credentials Grant

  Port of `hoauth2`'s
  `Network.OAuth2.Experiment.Grants.ResourceOwnerPassword` (see
  `docs/imports/hoauth2/dependencies.md`), module #18: the
  `ResourceOwnerPasswordApplication` configuration, its `/token` request
  shape, and its refresh-token request, RFC 6749 §4.3.

  ## Substitutions
  - **`TypeFamilies`**: as in `.ClientCredentials`, the associated
    `TokenRequest`/`ExchangeTokenInfo` types are ported as an ordinary
    top-level `PasswordTokenRequest` structure plus `outParam`
    instantiation — see
    `Network.OAuth2.Experiment.Flows.TokenRequest`'s doc-comment.
  - `Data.Set`/`Data.Map.Strict` are `Linen.Data.Set`/`Linen.Data.Map`;
    `Data.Text.Lazy` is `linen`'s single `String` type; `Network.OAuth2
    qualified as OAuth2`'s `RefreshToken` is
    `Linen.Network.OAuth2.Internal.RefreshToken` (see
    `Network.OAuth2.Experiment.Types`'s doc-comment for the same
    substitutions).
-/

import Linen.Network.OAuth2.Experiment.Flows.RefreshTokenRequest
import Linen.Network.OAuth2.Experiment.Flows.TokenRequest
import Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest
import Linen.Network.OAuth2.Experiment.Types
import Linen.Data.Map
import Linen.Data.Set

namespace Network.OAuth2.Experiment.Grants.ResourceOwnerPassword

open Network.OAuth2.Internal (ClientAuthenticationMethod)
open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.RefreshTokenRequest
open Network.OAuth2.Experiment.Flows.UserInfoRequest (HasUserInfoRequest)

-- ────────────────────────────────────────────────────────────────────
-- Application
-- ────────────────────────────────────────────────────────────────────

/-- An application that supports the Resource Owner Password Credentials
    Grant, RFC 6749 §4.3. -/
structure ResourceOwnerPasswordApplication where
  ropClientId : ClientId
  ropClientSecret : ClientSecret
  ropName : String
  ropScope : Data.Set' Scope
  ropUserName : Username
  ropPassword : Password
  ropTokenRequestExtraParams : Data.Map String String
  ropClientAuthenticationMethod : ClientAuthenticationMethod

instance : HasClientAuthenticationMethod ResourceOwnerPasswordApplication where
  getClientAuthenticationMethod app := app.ropClientAuthenticationMethod
  addClientAuthToHeader app := addSecretToHeader app.ropClientId app.ropClientSecret

-- ────────────────────────────────────────────────────────────────────
-- Token request
-- ────────────────────────────────────────────────────────────────────

/-- The `/token` request parameters for the Resource Owner Password
    Credentials Grant, RFC 6749 §4.3.2. -/
structure PasswordTokenRequest where
  trScope : Data.Set' Scope
  trUsername : Username
  trPassword : Password
  trGrantType : GrantTypeValue
  trExtraParams : Data.Map String String

instance :
    HasTokenRequest ResourceOwnerPasswordApplication PasswordTokenRequest
      NoNeedExchangeToken where
  mkTokenRequestParam app _ :=
    { trUsername := app.ropUserName
      trPassword := app.ropPassword
      trGrantType := .GTPassword
      trScope := app.ropScope
      trExtraParams := app.ropTokenRequestExtraParams }

instance : ToQueryParam PasswordTokenRequest where
  toQueryParam r :=
    [ toQueryParam r.trGrantType, toQueryParam r.trScope, toQueryParam r.trUsername
    , toQueryParam r.trPassword, r.trExtraParams ].foldl Data.Map.union Data.Map.empty

instance : HasUserInfoRequest ResourceOwnerPasswordApplication where

-- ────────────────────────────────────────────────────────────────────
-- Refresh token request
-- ────────────────────────────────────────────────────────────────────

instance : HasRefreshTokenRequest ResourceOwnerPasswordApplication where
  mkRefreshTokenRequestParam app rt :=
    { rrScope := app.ropScope
      rrGrantType := .GTRefreshToken
      rrRefreshToken := rt
      rrClientId := if app.ropClientAuthenticationMethod == .ClientSecretPost
        then some app.ropClientId else none
      rrClientSecret := if app.ropClientAuthenticationMethod == .ClientSecretPost
        then some app.ropClientSecret else none }

end Network.OAuth2.Experiment.Grants.ResourceOwnerPassword
