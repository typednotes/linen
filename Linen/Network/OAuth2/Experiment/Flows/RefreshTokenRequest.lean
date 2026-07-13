/-
  Linen.Network.OAuth2.Experiment.Flows.RefreshTokenRequest — RFC 6749 §6
  refresh-token request

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Flows.RefreshTokenRequest`
  (see `docs/imports/hoauth2/dependencies.md`), module #14: the typed
  request-parameter record for refreshing an access token, and the
  `HasRefreshTokenRequest` class each grant-type application configuration
  that supports it implements.

  ## Substitutions
  `Data.Set`/`Data.Map.Strict` are `Linen.Data.Set`/`Linen.Data.Map`;
  `Data.Text.Lazy` is `linen`'s single `String` type; `Network.OAuth2
  qualified as OAuth2`'s `RefreshToken` is
  `Linen.Network.OAuth2.Internal.RefreshToken` (re-exported by the facade,
  module #9) — see `Network.OAuth2.Experiment.Types`'s doc-comment for the
  same substitutions.
-/

import Linen.Network.OAuth2
import Linen.Network.OAuth2.Experiment.Types
import Linen.Network.OAuth2.Experiment.Flows.TokenRequest
import Linen.Data.Map
import Linen.Data.Set

namespace Network.OAuth2.Experiment.Flows.RefreshTokenRequest

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest (HasClientAuthenticationMethod)

-- ────────────────────────────────────────────────────────────────────
-- Refresh Token Request
-- ────────────────────────────────────────────────────────────────────

/-- The parameters of a Refresh Token request, RFC 6749 §6. -/
structure RefreshTokenRequest where
  rrRefreshToken : Network.OAuth2.RefreshToken
  rrGrantType : GrantTypeValue
  rrScope : Data.Set' Scope
  rrClientId : Option ClientId
  rrClientSecret : Option ClientSecret

instance : ToQueryParam RefreshTokenRequest where
  toQueryParam p :=
    Data.Map.union (toQueryParam p.rrGrantType) <|
    Data.Map.union (toQueryParam p.rrScope) <|
    Data.Map.union (toQueryParam p.rrRefreshToken) <|
    Data.Map.union (toQueryParam p.rrClientId) (toQueryParam p.rrClientSecret)

/-- Grant types that support refreshing an access token (RFC 6749 §6). -/
class HasRefreshTokenRequest (a : Type) extends HasClientAuthenticationMethod a where
  mkRefreshTokenRequestParam : a → Network.OAuth2.RefreshToken → RefreshTokenRequest

export HasRefreshTokenRequest (mkRefreshTokenRequestParam)

end Network.OAuth2.Experiment.Flows.RefreshTokenRequest
