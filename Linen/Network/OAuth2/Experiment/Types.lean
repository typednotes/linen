/-
  Linen.Network.OAuth2.Experiment.Types — typed OAuth2 request builders

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Types` (see
  `docs/imports/hoauth2/dependencies.md`), module #10: the `Idp`/
  `IdpApplication` phantom-typed application-configuration pair, the
  wrapper newtypes for OAuth2 request parameters (`Scope`, `ClientId`,
  `ClientSecret`, `RedirectUri`, `AuthorizeState`, `Username`, `Password`),
  `GrantTypeValue`/`ResponseType`, and the `ToQueryParam` class that turns
  each of those into a flat `client_id=...&scope=...` parameter map.

  ## Substitutions
  - **`PolyKinds`**: upstream's `Idp (i :: k)`/`IdpApplication (i :: k) a`
    let the phantom identifying type `i` be of *any* kind (so an `Idp`
    can be indexed by, say, a promoted symbol via `DataKinds`, not just an
    ordinary type). Lean has no kind polymorphism to match: `i` is ported
    as an ordinary `Type` parameter (`Idp (i : Type)`), which still lets
    every existing use — a distinct marker type per identity provider,
    used purely so `IdpApplication Google.type SomeApp` and
    `IdpApplication GitHub.type SomeApp` can't be mixed up — go through
    unchanged; only kinds other than `Type` (which `hoauth2` itself never
    actually uses for `i`) are unreachable.
  - **`IsString`/`OverloadedStrings`**: `Scope "openid"`-style literal
    overloading for `Scope`/`AuthorizeState`/`Username`/`Password`/
    `ClientId`/`ClientSecret` has no Lean analogue (Lean string literals
    are just `String`); callers wrap explicitly with the newtype's
    constructor (`⟨"openid"⟩`) instead.
  - `Data.Text.Lazy` is `linen`'s single `String` type throughout (see
    `Network.OAuth2.Experiment.Utils`'s own doc-comment for the same
    substitution); `tlToBS`/`bs8ToLazyText` conversions accordingly drop
    out.
  - `Data.Map.Strict`/`Data.Set` are `Linen.Data.Map`/`Linen.Data.Set`
    (`Data.Map`/`Data.Set'`).
  - `URI.ByteString`'s `URI`/`serializeURIRef'` is `Linen.Network.URI.URI`/
    `Network.OAuth2.Experiment.Utils.uriToText`.
  - The `Network.OAuth2 hiding (RefreshToken)` / `Network.OAuth2 qualified
    as OAuth2` dance (disambiguating `hoauth2`'s own `RefreshToken` from a
    same-named type in scope) has no counterpart here: this port just
    refers to `Network.OAuth2.RefreshToken`/`Network.OAuth2.ExchangeToken`
    by their full names where needed.
-/

import Linen.Network.OAuth2
import Linen.Network.OAuth2.Experiment.Pkce
import Linen.Network.OAuth2.Experiment.Utils
import Linen.Network.URI
import Linen.Data.Map
import Linen.Data.Set

namespace Network.OAuth2.Experiment.Types

open Network.OAuth2.Experiment.Pkce (CodeVerifier CodeChallenge CodeChallengeMethod)

-- ────────────────────────────────────────────────────────────────────
-- Idp App
-- ────────────────────────────────────────────────────────────────────

/-- An identity provider's endpoints. `i` is a phantom marker type
    identifying which provider this is (e.g. a distinct empty type per
    provider), carrying no data of its own — see the module doc-comment
    for the `PolyKinds` → `Type`-only simplification. -/
structure Idp (i : Type) where
  /-- Userinfo endpoint. -/
  idpUserInfoEndpoint : Network.URI.URI
  /-- Authorization endpoint. -/
  idpAuthorizeEndpoint : Network.URI.URI
  /-- Token endpoint. -/
  idpTokenEndpoint : Network.URI.URI
  /-- Not every IdP supports the Device Authorization flow. -/
  idpDeviceAuthorizationEndpoint : Option Network.URI.URI
deriving Repr

/-- An OAuth2 application `a` of IdP `i`. `a` is typically one of this
    port's grant-type-specific application configurations (Authorization
    Code, Device Authorization, Client Credentials, Resource Owner
    Password, or JWT Bearer). -/
structure IdpApplication (i : Type) (a : Type) where
  idp : Idp i
  application : a

-- ────────────────────────────────────────────────────────────────────
-- Scope
-- ────────────────────────────────────────────────────────────────────

/-- An OAuth2 scope value, e.g. `openid`/`profile`/`email`, or a
    provider-specific custom scope. -/
structure Scope where
  unScope : String
deriving Repr, BEq

instance : Ord Scope where
  compare a b := compare a.unScope b.unScope

-- ────────────────────────────────────────────────────────────────────
-- Grant Type value
-- ────────────────────────────────────────────────────────────────────

/-- The `grant_type` request parameter's value. Not a strict 1:1 mapping
    to a single flow — e.g. both Authorization Code and Resource Owner
    Password can also drive a `GTRefreshToken` request. -/
inductive GrantTypeValue where
  | GTAuthorizationCode
  | GTPassword
  | GTClientCredentials
  | GTRefreshToken
  | GTJwtBearer
  | GTDeviceCode
deriving Repr, BEq

/-- The wire value of a `GrantTypeValue`. -/
private def grantTypeValueToString : GrantTypeValue → String
  | .GTAuthorizationCode => "authorization_code"
  | .GTPassword => "password"
  | .GTClientCredentials => "client_credentials"
  | .GTRefreshToken => "refresh_token"
  | .GTJwtBearer => "urn:ietf:params:oauth:grant-type:jwt-bearer"
  | .GTDeviceCode => "urn:ietf:params:oauth:grant-type:device_code"

-- ────────────────────────────────────────────────────────────────────
-- Response Type
-- ────────────────────────────────────────────────────────────────────

/-- The `response_type` request parameter. `hoauth2` only ever produces
    the Authorization Code Grant's `code`, so that is the only
    constructor ported. -/
inductive ResponseType where
  | Code
deriving Repr, BEq

-- ────────────────────────────────────────────────────────────────────
-- Credentials
-- ────────────────────────────────────────────────────────────────────

/-- A client (relying party) identifier. -/
structure ClientId where
  unClientId : String
deriving Repr, BEq

/-- Either a plain client secret or a JWT, depending on
    `ClientAuthenticationMethod`. -/
structure ClientSecret where
  unClientSecret : String
deriving Repr, BEq

/-- The redirect URI registered with the IdP. -/
structure RedirectUri where
  unRedirectUri : Network.URI.URI
deriving Repr, BEq

/-- The `state` request parameter, an opaque anti-CSRF value. -/
structure AuthorizeState where
  unAuthorizeState : String
deriving Repr, BEq

/-- A Resource Owner Password Credentials username. -/
structure Username where
  unUsername : String
deriving Repr, BEq

/-- A Resource Owner Password Credentials password. -/
structure Password where
  unPassword : String
deriving Repr, BEq

-- ────────────────────────────────────────────────────────────────────
-- Query parameters
-- ────────────────────────────────────────────────────────────────────

/-- Render a value as the request parameters it contributes. -/
class ToQueryParam (a : Type) where
  toQueryParam : a → Data.Map String String

export ToQueryParam (toQueryParam)

instance [ToQueryParam a] : ToQueryParam (Option a) where
  toQueryParam
    | none => Data.Map.empty
    | some a => ToQueryParam.toQueryParam a

instance : ToQueryParam GrantTypeValue where
  toQueryParam x := Data.Map.singleton "grant_type" (grantTypeValueToString x)

instance : ToQueryParam ClientId where
  toQueryParam c := Data.Map.singleton "client_id" c.unClientId

instance : ToQueryParam ClientSecret where
  toQueryParam c := Data.Map.singleton "client_secret" c.unClientSecret

instance : ToQueryParam Username where
  toQueryParam u := Data.Map.singleton "username" u.unUsername

instance : ToQueryParam Password where
  toQueryParam p := Data.Map.singleton "password" p.unPassword

instance : ToQueryParam AuthorizeState where
  toQueryParam s := Data.Map.singleton "state" s.unAuthorizeState

instance : ToQueryParam RedirectUri where
  toQueryParam r := Data.Map.singleton "redirect_uri" (Network.OAuth2.Experiment.Utils.uriToText r.unRedirectUri)

instance : ToQueryParam (Data.Set' Scope) where
  toQueryParam scopes :=
    let names := (Data.Set'.toList' scopes).map Scope.unScope
    if names.isEmpty then Data.Map.empty
    else Data.Map.singleton "scope" (" ".intercalate names)

instance : ToQueryParam CodeVerifier where
  toQueryParam c := Data.Map.singleton "code_verifier" c.unCodeVerifier

instance : ToQueryParam CodeChallenge where
  toQueryParam c := Data.Map.singleton "code_challenge" c.unCodeChallenge

instance : ToQueryParam CodeChallengeMethod where
  toQueryParam m := Data.Map.singleton "code_challenge_method" (toString m)

instance : ToQueryParam Network.OAuth2.ExchangeToken where
  toQueryParam t := Data.Map.singleton "code" t.extoken

instance : ToQueryParam Network.OAuth2.RefreshToken where
  toQueryParam t := Data.Map.singleton "refresh_token" t.rtoken

instance : ToQueryParam ResponseType where
  toQueryParam
    | .Code => Data.Map.singleton "response_type" "code"

end Network.OAuth2.Experiment.Types
