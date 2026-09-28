/-
  Tests for `Linen.Network.OAuth2.TokenRequest`.

  `doSimplePostRequest`/`doJSONPostRequest`/`fetchAccessToken*`/
  `refreshAccessToken*` all perform real network IO, so — like
  `Network.OAuth2.HttpClient`'s own test module — they're pinned at the
  type level rather than exercised against a live server. Everything else
  here is pure and gets a real `#guard` check.
-/
import Linen.Network.OAuth2.TokenRequest
import Linen.Data.Json.Encode

open Network.OAuth2.Internal
open Network.OAuth2.TokenRequest
open Network.HTTP.Client (Request Response)
open Network.HTTP.Types
open Data.Json (Value FromJSON ToJSON)

namespace Tests.Network.OAuth2.TokenRequest

private instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .error a, .error b => a == b
    | .ok a, .ok b => a == b
    | _, _ => false

private def sampleOAuth : OAuth2 :=
  { oauth2ClientId := "client1"
    oauth2ClientSecret := "secret1"
    oauth2AuthorizeEndpoint := (Network.URI.parseURI "https://idp.example.com/authorize").get!
    oauth2TokenEndpoint := (Network.URI.parseURI "https://idp.example.com/token").get!
    oauth2RedirectUri := (Network.URI.parseURI "https://client.example.com/cb").get! }

-- `FromJSON TokenResponseErrorCode` recognises RFC 6749 §5.2 codes.
#guard (FromJSON.parseJSON (Value.string "invalid_grant") : Except String TokenResponseErrorCode)
    == .ok .invalidGrant
#guard (FromJSON.parseJSON (Value.string "made_up") : Except String TokenResponseErrorCode)
    == .ok (.unknownErrorCode "made_up")

-- `parseTokeResponseError` decodes a well-formed error body.
#guard (parseTokeResponseError "{\"error\": \"invalid_client\"}").tokenResponseError == .invalidClient

-- `parseTokeResponseError` falls back to `unknownErrorCode` on unparseable input,
-- keeping the raw response in the description.
#guard (parseTokeResponseError "not json").tokenResponseError == .unknownErrorCode ""

-- `FromJSON TokenResponse` decodes the standard fields and keeps the raw object.
#guard
  (match (FromJSON.parseJSON
      (Value.object
        [ ("access_token", .string "atok")
        , ("refresh_token", .string "rtok")
        , ("expires_in", .number 3600)
        , ("token_type", .string "Bearer") ])
      : Except String TokenResponse) with
   | .ok t => (t.accessToken, t.refreshToken, t.expiresIn, t.tokenType)
   | .error _ => (⟨""⟩, none, none, none))
    == (⟨"atok"⟩, some ⟨"rtok"⟩, some 3600, some "Bearer")

-- `expires_in` may be wire-encoded as a string.
#guard
  (match (FromJSON.parseJSON
      (Value.object [("access_token", .string "atok"), ("expires_in", .string "3600")])
      : Except String TokenResponse) with
   | .ok t => t.expiresIn
   | .error _ => none)
    == some 3600

-- `ToJSON TokenResponse` re-emits the full raw object (not just the named fields).
#guard
  (Data.Json.Encode.encode (ToJSON.toJSON
    ({ accessToken := ⟨"atok"⟩, refreshToken := none, expiresIn := none, tokenType := none
       idToken := none, scope := none
       rawResponse := [("access_token", .string "atok"), ("extra", .string "field")] } : TokenResponse)))
    == Data.Json.Encode.encode (Value.object [("access_token", .string "atok"), ("extra", .string "field")])

-- `accessTokenUrl` builds the exchange-code request.
#guard
  (accessTokenUrl sampleOAuth ⟨"authcode"⟩).snd
    == [ ("code", "authcode")
       , ("redirect_uri", "https://client.example.com/cb")
       , ("grant_type", "authorization_code") ]

-- `refreshAccessTokenUrl` builds the refresh-token request.
#guard
  (refreshAccessTokenUrl sampleOAuth ⟨"rtok"⟩).snd
    == [("grant_type", "refresh_token"), ("refresh_token", "rtok")]

-- `clientSecretPost` carries the client credentials in the body.
#guard clientSecretPost sampleOAuth == [("client_id", "client1"), ("client_secret", "secret1")]

-- `addBasicAuth` adds an `Authorization: Basic` header.
#guard
  (addBasicAuth sampleOAuth { method := .standard .GET, host := "idp.example.com", port := 443 }).headers.head?
    == some (hAuthorization, s!"Basic {Data.Base64.encode "client1:secret1".toUTF8}")

-- `handleOAuth2TokenResponse` returns the body on a 2xx status.
#guard handleOAuth2TokenResponse { statusCode := status200, headers := [], body := "ok body".toUTF8 }
    == .ok "ok body"

-- `handleOAuth2TokenResponse` parses the error body on a non-2xx status.
#guard
  (match handleOAuth2TokenResponse
      { statusCode := status400, headers := [], body := "{\"error\": \"invalid_grant\"}".toUTF8 } with
   | .error e => e.tokenResponseError
   | .ok _ => .unknownErrorCode "unexpected ok")
    == .invalidGrant

-- `parseResponseString` decodes a query-string-shaped response body.
#guard
  (match (parseResponseString "access_token=atok&token_type=Bearer" : Except TokenResponseError TokenResponse) with
   | .ok t => some t.accessToken
   | .error _ => none)
    == some ⟨"atok"⟩

-- `parseResponseFlexible` prefers JSON, falling back to a query string.
#guard
  (match (parseResponseFlexible "{\"access_token\": \"atok\"}" : Except TokenResponseError TokenResponse) with
   | .ok t => some t.accessToken
   | .error _ => none)
    == some ⟨"atok"⟩
#guard
  (match (parseResponseFlexible "access_token=atok" : Except TokenResponseError TokenResponse) with
   | .ok t => some t.accessToken
   | .error _ => none)
    == some ⟨"atok"⟩

/-! ### Signatures -/

example : OAuth2 → Network.URI.URI → PostBody → IO (Except TokenResponseError String) :=
  doSimplePostRequest
example [FromJSON a] : OAuth2 → Network.URI.URI → PostBody → IO (Except TokenResponseError a) :=
  doJSONPostRequest
example : ClientAuthenticationMethod → OAuth2 → ExchangeToken → IO (Except TokenResponseError TokenResponse) :=
  fetchAccessTokenWithAuthMethod
example : OAuth2 → ExchangeToken → IO (Except TokenResponseError TokenResponse) := fetchAccessToken
example : ClientAuthenticationMethod → OAuth2 → RefreshToken → IO (Except TokenResponseError TokenResponse) :=
  refreshAccessTokenWithAuthMethod
example : OAuth2 → RefreshToken → IO (Except TokenResponseError TokenResponse) := refreshAccessToken

end Tests.Network.OAuth2.TokenRequest
