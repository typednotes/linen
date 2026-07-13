/-
  Linen.Network.OAuth2.Experiment.Flows.TokenRequest — client authentication
  and the per-grant token-request shape

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Flows.TokenRequest` (see
  `docs/imports/hoauth2/dependencies.md`), module #13: the
  `HasClientAuthenticationMethod` class every grant-type application
  configuration implements, and the `HasTokenRequest` class describing how
  each grant type builds its own `/token` request shape.

  This is a *different* module from the already-ported
  `Linen.Network.OAuth2.TokenRequest` (batch 2, upstream's flat
  `Network.OAuth2.TokenRequest`): that one is the low-level RFC 6749 §4.1.3–
  §6 HTTP mechanics (parsing `TokenResponse`, POSTing to the token
  endpoint); this one is the `Experiment` typed-request-builder layer's
  per-grant `TokenRequest` shape declaration, built on module #9's facade
  and module #10's `Types`.

  ## Substitutions
  - **`TypeFamilies`** (`data TokenRequest a` / `type ExchangeTokenInfo a`):
    upstream's `HasTokenRequest` associates, per grant-type instance `a`, an
    *associated data family* `TokenRequest a` (the shape of that grant's
    `/token` request parameters) and an *associated type family*
    `ExchangeTokenInfo a` (whatever extra info `mkTokenRequestParam` needs
    to build it — e.g. an `ExchangeToken` for Authorization Code, `Unit`
    for grants that need nothing extra). Lean 4 classes have no type-family
    mechanism, but the same "each instance picks its own associated type"
    behaviour is exactly what `outParam` class parameters give
    (`Membership`/`GetElem` in the Lean stdlib are ported the same way):
    `HasTokenRequest a tokenRequest exchangeTokenInfo` takes the associated
    types as `outParam` parameters instead of a `class ... where data ...`
    declaration, and every instance still fixes them to concrete types the
    same way a `data`/`type` instance would.
  - `http-client`'s `applyBasicAuth` is
    `Linen.Network.HTTP.Client.Conduit.applyBasicAuth` (already ported).
-/

import Linen.Network.OAuth2.Internal
import Linen.Network.OAuth2.Experiment.Types
import Linen.Network.HTTP.Client.Conduit

namespace Network.OAuth2.Experiment.Flows.TokenRequest

open Network.HTTP.Client (Request)
open Network.OAuth2.Internal (ClientAuthenticationMethod)
open Network.OAuth2.Experiment.Types (ClientId ClientSecret)

-- ────────────────────────────────────────────────────────────────────
-- Client authentication
-- ────────────────────────────────────────────────────────────────────

/-- Add an `Authorization: Basic` header built from a client id/secret pair
    (RFC 6749 §2.3.1's `client_secret_basic`). -/
def addSecretToHeader (cid : ClientId) (csecret : ClientSecret) (req : Request) : Request :=
  Network.HTTP.Client.Conduit.applyBasicAuth cid.unClientId csecret.unClientSecret req

/-- How a grant-type application configuration authenticates itself against
    the token endpoint. -/
class HasClientAuthenticationMethod (a : Type) where
  getClientAuthenticationMethod : a → ClientAuthenticationMethod
  /-- Add whatever header the chosen authentication method needs; the
      default does nothing (matching upstream's `addClientAuthToHeader _ =
      id`), for grants that authenticate purely via POST-body parameters. -/
  addClientAuthToHeader : a → Request → Request := fun _ req => req

export HasClientAuthenticationMethod (getClientAuthenticationMethod addClientAuthToHeader)

-- ────────────────────────────────────────────────────────────────────
-- Token request
-- ────────────────────────────────────────────────────────────────────

/-- Marker for grant types whose `/token` request needs no exchange token
    at all (Resource Owner Password and Client Credentials make the
    request directly; only Authorization Code involves one). -/
structure NoNeedExchangeToken where
deriving Repr, BEq

/-- Each grant-type application configuration `a` has slightly different
    `/token` request parameters. `tokenRequest` is the shape of that
    request (upstream's associated data family `TokenRequest a`);
    `exchangeTokenInfo` is whatever extra info is needed to build it
    (upstream's associated type family `ExchangeTokenInfo a` — e.g. an
    `ExchangeToken` for Authorization Code, `NoNeedExchangeToken` for
    grants that need nothing extra). See the module doc-comment for why
    these are `outParam` class parameters rather than a `data`/`type`
    family declaration. -/
class HasTokenRequest (a : Type) (tokenRequest : outParam Type)
    (exchangeTokenInfo : outParam Type) extends HasClientAuthenticationMethod a where
  mkTokenRequestParam : a → exchangeTokenInfo → tokenRequest

export HasTokenRequest (mkTokenRequestParam)

end Network.OAuth2.Experiment.Flows.TokenRequest
