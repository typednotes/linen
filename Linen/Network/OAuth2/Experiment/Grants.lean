/-
  Linen.Network.OAuth2.Experiment.Grants — the five supported grant types

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Grants` (see
  `docs/imports/hoauth2/dependencies.md`), module #20: the package's own
  facade over its five grant-type modules, re-exporting each one's
  application configuration and `/token`-request shape (RFC 6749's
  Authorization Code, Client Credentials, Resource Owner Password
  Credentials, RFC 7523's JWT Bearer, and RFC 8628's Device Authorization).

  This module is a genuine pure re-export: unlike
  `Linen.Network.OAuth2.Experiment.Flows` (module #21), upstream's
  `Grants.hs` defines no logic of its own — it only imports and re-exports
  `module Network.OAuth2.Experiment.Grants.*` for each of the five grants.
-/

import Linen.Network.OAuth2.Experiment.Grants.AuthorizationCode
import Linen.Network.OAuth2.Experiment.Grants.ClientCredentials
import Linen.Network.OAuth2.Experiment.Grants.DeviceAuthorization
import Linen.Network.OAuth2.Experiment.Grants.JwtBearer
import Linen.Network.OAuth2.Experiment.Grants.ResourceOwnerPassword

namespace Network.OAuth2.Experiment.Grants

export Network.OAuth2.Experiment.Grants.AuthorizationCode
  (AuthorizationCodeApplication AuthorizationCodeTokenRequest
   mkAuthorizationRequestParam mkPkceAuthorizeRequestParam)

export Network.OAuth2.Experiment.Grants.ClientCredentials
  (ClientCredentialsApplication ClientCredentialsTokenRequest)

export Network.OAuth2.Experiment.Grants.DeviceAuthorization
  (DeviceAuthorizationApplication DeviceAuthorizationTokenRequest
   mkDeviceAuthorizationRequestParam)

export Network.OAuth2.Experiment.Grants.JwtBearer
  (JwtBearerApplication JwtBearerTokenRequest)

export Network.OAuth2.Experiment.Grants.ResourceOwnerPassword
  (ResourceOwnerPasswordApplication PasswordTokenRequest)

end Network.OAuth2.Experiment.Grants
