/-
  Linen.Network.OAuth2 — a lightweight OAuth2 client

  Port of `hoauth2`'s `Network.OAuth2` (see
  `docs/imports/hoauth2/dependencies.md`), module #9: the package's facade,
  re-exporting `.Internal`, `.AuthorizationRequest`, `.HttpClient`, and
  `.TokenRequest`.

  Upstream hides `AuthorizationResponseError`/`AuthorizationResponseErrorCode`
  from this facade (there being a naming collision with `TokenRequest`'s
  `TokenResponseError`/`TokenResponseErrorCode` otherwise) — the same
  exclusion is kept here; reach those two by importing
  `Linen.Network.OAuth2.AuthorizationRequest` directly.
-/

import Linen.Network.OAuth2.Internal
import Linen.Network.OAuth2.AuthorizationRequest
import Linen.Network.OAuth2.HttpClient
import Linen.Network.OAuth2.TokenRequest

namespace Network.OAuth2

export Network.OAuth2.Internal
  (OAuth2 AccessToken RefreshToken IdToken ExchangeToken ClientAuthenticationMethod
   PostBody QueryParams linenVersion defaultRequestHeaders addDefaultRequestHeaders
   appendQueryParams uriToRequest)

export Network.OAuth2.AuthorizationRequest (authorizationUrlWithParams authorizationUrl)

export Network.OAuth2.HttpClient
  (APIAuthenticationMethod accessTokenToParam appendAccessToken updateRequestHeaders
   setMethod jsonBody authRequest
   authGetBSWithAuthMethod authGetBS authGetJSONWithAuthMethod authGetJSON
   authPostBSWithAuthMethod authPostBS authPostJSONWithAuthMethod authPostJSON)

export Network.OAuth2.TokenRequest
  (TokenResponseErrorCode TokenResponseError parseTokeResponseError TokenResponse
   accessTokenUrl refreshAccessTokenUrl clientSecretPost addBasicAuth
   handleOAuth2TokenResponse doSimplePostRequest parseResponseString parseResponseFlexible
   doJSONPostRequest
   fetchAccessTokenWithAuthMethod fetchAccessToken
   refreshAccessTokenWithAuthMethod refreshAccessToken)

end Network.OAuth2
