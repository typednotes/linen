/-
  Linen.Network.OAuth2.Experiment.Grants.JwtBearer — JWT Bearer Grant

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Grants.JwtBearer` (see
  `docs/imports/hoauth2/dependencies.md`), module #17: the
  `JwtBearerApplication` configuration and its `/token` request shape,
  RFC 7523.

  ## Substitutions
  - **`TypeFamilies`**: as in `.ClientCredentials`, the associated
    `TokenRequest`/`ExchangeTokenInfo` types are ported as an ordinary
    top-level `JwtBearerTokenRequest` structure plus `outParam`
    instantiation — see
    `Network.OAuth2.Experiment.Flows.TokenRequest`'s doc-comment.
  - `Data.ByteString.ByteString` is Lean's native `ByteArray`.
  - `Data.Map.Strict` is `Linen.Data.Map`; `Data.Text.Lazy` is `linen`'s
    single `String` type (see `Network.OAuth2.Experiment.Types`'s
    doc-comment for the same substitutions).
-/

import Linen.Network.OAuth2.Experiment.Flows.TokenRequest
import Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest
import Linen.Network.OAuth2.Experiment.Types
import Linen.Network.OAuth2.Experiment.Utils
import Linen.Data.Map

namespace Network.OAuth2.Experiment.Grants.JwtBearer

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Flows.UserInfoRequest (HasUserInfoRequest)

-- ────────────────────────────────────────────────────────────────────
-- Application
-- ────────────────────────────────────────────────────────────────────

/-- An application that supports the JWT Bearer Grant, RFC 7523. -/
structure JwtBearerApplication where
  jbName : String
  /-- The signed JWT assertion. -/
  jbJwtAssertion : ByteArray

instance : HasClientAuthenticationMethod JwtBearerApplication where
  getClientAuthenticationMethod _ := .ClientAssertionJwt

-- ────────────────────────────────────────────────────────────────────
-- Token request
-- ────────────────────────────────────────────────────────────────────

/-- The `/token` request parameters for the JWT Bearer Grant. -/
structure JwtBearerTokenRequest where
  /-- Always `GTJwtBearer`. -/
  trGrantType : GrantTypeValue
  /-- The signed JWT token. -/
  trAssertion : ByteArray

instance : HasTokenRequest JwtBearerApplication JwtBearerTokenRequest NoNeedExchangeToken where
  mkTokenRequestParam app _ :=
    { trGrantType := .GTJwtBearer
      trAssertion := app.jbJwtAssertion }

instance : ToQueryParam JwtBearerTokenRequest where
  toQueryParam r :=
    Data.Map.union (toQueryParam r.trGrantType)
      (Data.Map.singleton "assertion" (String.fromUTF8! r.trAssertion))

instance : HasUserInfoRequest JwtBearerApplication where

end Network.OAuth2.Experiment.Grants.JwtBearer
