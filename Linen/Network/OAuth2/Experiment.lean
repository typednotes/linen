/-
  Linen.Network.OAuth2.Experiment — the typed OAuth2 request-builder API

  Port of `hoauth2`'s `Network.OAuth2.Experiment` (see
  `docs/imports/hoauth2/dependencies.md`), module #22: the package's
  top-level facade over the typed-request-builder layer, tying together
  `Types` (the `Idp`/`IdpApplication` pair and request-parameter newtypes),
  `Grants` (the five supported grant types), `Flows` (the HTTP-performing
  entry points), `Pkce` (RFC 7636), and `Utils`.

  Upstream builds this facade's export list out of *deliberately restricted*
  re-imports of some of its dependencies (e.g. importing only
  `HasRefreshTokenRequest` from `Flows.RefreshTokenRequest`, not the
  `RefreshTokenRequest` record itself or `mkRefreshTokenRequestParam`) —
  Haskell's per-module `import (...)` / `export (module X)` re-export
  mechanics let a later re-export item silently inherit whatever an earlier,
  narrower import already restricted it to. Mechanically replaying those
  restrictions here would produce a surface that is *less* usable than the
  already-ported submodules underneath it, purely as an artifact of
  Haskell's scoping rules rather than a deliberate design choice. This port
  instead re-exports a complete, usable surface — everything a caller needs
  to drive the whole typed-request-builder API through this one module,
  matching how `Linen.Network.OAuth2` (module #9) already curates its own
  export list by hand rather than mechanically transcribing scope rules.
-/

import Linen.Network.OAuth2
import Linen.Network.OAuth2.Experiment.Flows
import Linen.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
import Linen.Network.OAuth2.Experiment.Flows.RefreshTokenRequest
import Linen.Network.OAuth2.Experiment.Flows.TokenRequest
import Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest
import Linen.Network.OAuth2.Experiment.Grants
import Linen.Network.OAuth2.Experiment.Pkce
import Linen.Network.OAuth2.Experiment.Types
import Linen.Network.OAuth2.Experiment.Utils

namespace Network.OAuth2.Experiment

export Network.OAuth2 (ClientAuthenticationMethod)

export Network.OAuth2.Experiment.Grants
  (AuthorizationCodeApplication AuthorizationCodeTokenRequest
   mkAuthorizationRequestParam mkPkceAuthorizeRequestParam
   ClientCredentialsApplication ClientCredentialsTokenRequest
   DeviceAuthorizationApplication DeviceAuthorizationTokenRequest
   mkDeviceAuthorizationRequestParam
   JwtBearerApplication JwtBearerTokenRequest
   ResourceOwnerPasswordApplication PasswordTokenRequest)

export Network.OAuth2.Experiment.Flows
  (mkAuthorizationRequest mkPkceAuthorizeRequest
   conduitDeviceAuthorizationRequest pollDeviceTokenRequest
   conduitTokenRequest conduitPkceTokenRequest
   conduitRefreshTokenRequest
   conduitUserInfoRequest conduitUserInfoRequestWithCustomMethod)

export Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest (DeviceCode DeviceAuthorizationResponse)

export Network.OAuth2.Experiment.Flows.TokenRequest
  (HasClientAuthenticationMethod getClientAuthenticationMethod addClientAuthToHeader addSecretToHeader
   NoNeedExchangeToken HasTokenRequest mkTokenRequestParam)

export Network.OAuth2.Experiment.Flows.RefreshTokenRequest
  (RefreshTokenRequest HasRefreshTokenRequest mkRefreshTokenRequestParam)

export Network.OAuth2.Experiment.Flows.UserInfoRequest (HasUserInfoRequest)

export Network.OAuth2.Experiment.Types
  (Idp IdpApplication Scope GrantTypeValue ResponseType ClientId ClientSecret RedirectUri
   AuthorizeState Username Password ToQueryParam toQueryParam)

export Network.OAuth2.Experiment.Pkce
  (CodeChallenge CodeVerifier CodeChallengeMethod PkceRequestParam
   cvMaxLen genCodeVerifier encodeCodeVerifier mkPkceParam)

export Network.OAuth2.Experiment.Utils (QueryParams unionMapsToQueryParams uriToText)

end Network.OAuth2.Experiment
