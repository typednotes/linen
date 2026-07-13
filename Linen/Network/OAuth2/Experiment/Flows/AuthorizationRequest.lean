/-
  Linen.Network.OAuth2.Experiment.Flows.AuthorizationRequest — Authorization
  Code Grant `/authorize` request parameters

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Flows.AuthorizationRequest`
  (see `docs/imports/hoauth2/dependencies.md`), module #11: the typed
  request-parameter record for RFC 6749 §4.1.1's authorization request,
  built on `Network.OAuth2.Experiment.Types`'s (module #10) `ToQueryParam`
  machinery.

  ## Substitutions
  `Data.Set`/`Data.Map.Strict` are `Linen.Data.Set`/`Linen.Data.Map`
  (`Data.Set'`/`Data.Map`); `Data.Text.Lazy` is `linen`'s single `String`
  type — see `Network.OAuth2.Experiment.Types`'s own doc-comment for the
  same substitutions.
-/

import Linen.Network.OAuth2.Experiment.Types
import Linen.Data.Map
import Linen.Data.Set

namespace Network.OAuth2.Experiment.Flows.AuthorizationRequest

open Network.OAuth2.Experiment.Types

-- ────────────────────────────────────────────────────────────────────
-- Authorization Request
-- ────────────────────────────────────────────────────────────────────

/-- The parameters of an Authorization Code Grant authorization request,
    RFC 6749 §4.1.1. -/
structure AuthorizationRequestParam where
  arScope : Data.Set' Scope
  arState : AuthorizeState
  arClientId : ClientId
  /-- Optional per RFC 6749 §3.1.2.3: may be omitted when only one
      `redirect_uri` is registered with the IdP. -/
  arRedirectUri : Option RedirectUri
  arResponseType : ResponseType
  arExtraParams : Data.Map String String

instance : ToQueryParam AuthorizationRequestParam where
  toQueryParam p :=
    Data.Map.union (toQueryParam p.arResponseType) <|
    Data.Map.union (toQueryParam p.arScope) <|
    Data.Map.union (toQueryParam p.arClientId) <|
    Data.Map.union (toQueryParam p.arState) <|
    Data.Map.union (toQueryParam p.arRedirectUri) p.arExtraParams

end Network.OAuth2.Experiment.Flows.AuthorizationRequest
