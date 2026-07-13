/-
  Linen.Network.OAuth2.Experiment.Grants.AuthorizationCode — Authorization
  Code Grant

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Grants.AuthorizationCode`
  (see `docs/imports/hoauth2/dependencies.md`), module #19: the
  `AuthorizationCodeApplication` configuration, its `/authorize` request
  builder (plain and PKCE-augmented, RFC 7636), its `/token` request
  shape, and its refresh-token request, RFC 6749 §4.1.

  ## Substitutions
  - **`TypeFamilies`**: as in `.ClientCredentials`, the associated
    `TokenRequest`/`ExchangeTokenInfo` types are ported as an ordinary
    top-level `AuthorizationCodeTokenRequest` structure plus `outParam`
    instantiation — see
    `Network.OAuth2.Experiment.Flows.TokenRequest`'s doc-comment.
  - **`MonadIO m => m ...`**: upstream's `mkPkceAuthorizeRequestParam` is
    polymorphic in any `MonadIO` (so it can run inside whatever
    transformer stack the caller has); this port has no generic
    `MonadIO` typeclass and pins it to plain `IO` (the same treatment
    `Network.OAuth2.Experiment.Pkce.mkPkceParam` itself already receives),
    which is the only monad any existing call site instantiates it at.
  - `Data.Set`/`Data.Map.Strict` are `Linen.Data.Set`/`Linen.Data.Map`;
    `Data.Text.Lazy` is `linen`'s single `String` type; `URI.ByteString.URI`
    is `Linen.Network.URI.URI`; `Network.OAuth2 qualified as OAuth2`'s
    `RefreshToken` is `Linen.Network.OAuth2.Internal.RefreshToken` (see
    `Network.OAuth2.Experiment.Types`'s doc-comment for the same
    substitutions).
-/

import Linen.Network.OAuth2.Experiment.Flows.AuthorizationRequest
import Linen.Network.OAuth2.Experiment.Flows.RefreshTokenRequest
import Linen.Network.OAuth2.Experiment.Flows.TokenRequest
import Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest
import Linen.Network.OAuth2.Experiment.Pkce
import Linen.Network.OAuth2.Experiment.Types
import Linen.Network.OAuth2.Experiment.Utils
import Linen.Network.OAuth2.Internal
import Linen.Network.URI
import Linen.Data.Map
import Linen.Data.Set

namespace Network.OAuth2.Experiment.Grants.AuthorizationCode

open Network.OAuth2.Internal (ClientAuthenticationMethod ExchangeToken)
open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.AuthorizationRequest
open Network.OAuth2.Experiment.Flows.RefreshTokenRequest
open Network.OAuth2.Experiment.Flows.UserInfoRequest (HasUserInfoRequest)
open Network.OAuth2.Experiment.Pkce (PkceRequestParam CodeVerifier mkPkceParam)

-- ────────────────────────────────────────────────────────────────────
-- Application
-- ────────────────────────────────────────────────────────────────────

/-- An application that supports the Authorization Code Grant,
    RFC 6749 §4.1. -/
structure AuthorizationCodeApplication where
  acName : String
  acClientId : ClientId
  acClientSecret : ClientSecret
  acScope : Data.Set' Scope
  acRedirectUri : Network.URI.URI
  acAuthorizeState : AuthorizeState
  acAuthorizeRequestExtraParams : Data.Map String String
  acClientAuthenticationMethod : ClientAuthenticationMethod

instance : HasClientAuthenticationMethod AuthorizationCodeApplication where
  getClientAuthenticationMethod app := app.acClientAuthenticationMethod
  addClientAuthToHeader app := addSecretToHeader app.acClientId app.acClientSecret

-- ────────────────────────────────────────────────────────────────────
-- Authorization request
-- ────────────────────────────────────────────────────────────────────

/-- Build the plain (non-PKCE) authorization request parameters,
    RFC 6749 §4.1.1. -/
def mkAuthorizationRequestParam (app : AuthorizationCodeApplication) :
    AuthorizationRequestParam :=
  { arScope := app.acScope
    arState := app.acAuthorizeState
    arClientId := app.acClientId
    arRedirectUri := some ⟨app.acRedirectUri⟩
    arResponseType := .Code
    arExtraParams := app.acAuthorizeRequestExtraParams }

/-- Build the PKCE-augmented authorization request parameters (RFC 7636):
    a fresh `code_verifier`/`code_challenge` pair is generated, and the
    challenge plus its method are folded into the request's extra
    parameters. The returned `CodeVerifier` must be kept by the caller to
    send with the later token request. -/
def mkPkceAuthorizeRequestParam (app : AuthorizationCodeApplication) :
    IO (AuthorizationRequestParam × CodeVerifier) := do
  let pkce ← mkPkceParam
  let authReqParam := mkAuthorizationRequestParam app
  let combinedExtraParams :=
    Data.Map.union authReqParam.arExtraParams <|
    Data.Map.union (toQueryParam pkce.codeChallenge) (toQueryParam pkce.codeChallengeMethod)
  pure ({ authReqParam with arExtraParams := combinedExtraParams }, pkce.codeVerifier)

-- ────────────────────────────────────────────────────────────────────
-- Token request
-- ────────────────────────────────────────────────────────────────────

/-- The `/token` request parameters for the Authorization Code Grant,
    RFC 6749 §4.1.3. -/
structure AuthorizationCodeTokenRequest where
  trCode : ExchangeToken
  trGrantType : GrantTypeValue
  trRedirectUri : RedirectUri
  trClientId : ClientId
  trClientSecret : ClientSecret
  trClientAuthenticationMethod : ClientAuthenticationMethod

instance :
    HasTokenRequest AuthorizationCodeApplication AuthorizationCodeTokenRequest
      ExchangeToken where
  mkTokenRequestParam app authCode :=
    { trCode := authCode
      trGrantType := .GTAuthorizationCode
      trRedirectUri := ⟨app.acRedirectUri⟩
      trClientId := app.acClientId
      trClientSecret := app.acClientSecret
      trClientAuthenticationMethod := app.acClientAuthenticationMethod }

instance : ToQueryParam AuthorizationCodeTokenRequest where
  toQueryParam r :=
    let extraBodyBasedOnClientAuthMethod :=
      match r.trClientAuthenticationMethod with
      | .ClientAssertionJwt =>
        [ Data.Map.fromList
            [ ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
            , ("client_assertion", r.trClientSecret.unClientSecret) ] ]
      | .ClientSecretPost => [toQueryParam r.trClientId, toQueryParam r.trClientSecret]
      | .ClientSecretBasic => []
    ([toQueryParam r.trCode, toQueryParam r.trGrantType, toQueryParam r.trRedirectUri]
      ++ extraBodyBasedOnClientAuthMethod).foldl Data.Map.union Data.Map.empty

instance : HasUserInfoRequest AuthorizationCodeApplication where

-- ────────────────────────────────────────────────────────────────────
-- Refresh token request
-- ────────────────────────────────────────────────────────────────────

instance : HasRefreshTokenRequest AuthorizationCodeApplication where
  mkRefreshTokenRequestParam app rt :=
    { rrScope := app.acScope
      rrGrantType := .GTRefreshToken
      rrRefreshToken := rt
      rrClientId := if app.acClientAuthenticationMethod == .ClientSecretPost
        then some app.acClientId else none
      rrClientSecret := if app.acClientAuthenticationMethod == .ClientSecretPost
        then some app.acClientSecret else none }

end Network.OAuth2.Experiment.Grants.AuthorizationCode
