/-
  Tests for `Linen.Network.OAuth2.AuthorizationRequest`.
-/
import Linen.Network.OAuth2.AuthorizationRequest

open Network.OAuth2.Internal
open Network.OAuth2.AuthorizationRequest
open Data.Json (Value FromJSON)

namespace Tests.Network.OAuth2.AuthorizationRequest

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

-- `authorizationUrl` sends `client_id`/`response_type`/`redirect_uri`.
#guard (authorizationUrl sampleOAuth).uriQuery
    == "?client_id=client1&response_type=code&redirect_uri=https%3A%2F%2Fclient.example.com%2Fcb"

-- `authorizationUrlWithParams` prepends caller-supplied params.
#guard (authorizationUrlWithParams [("state", "xyz")] sampleOAuth).uriQuery
    == "?state=xyz&client_id=client1&response_type=code&redirect_uri=https%3A%2F%2Fclient.example.com%2Fcb"

-- A caller-supplied key wins over this function's own default for the same key
-- (`dedupByKey` keeps the first occurrence).
#guard (authorizationUrlWithParams [("client_id", "override")] sampleOAuth).uriQuery
    == "?client_id=override&response_type=code&redirect_uri=https%3A%2F%2Fclient.example.com%2Fcb"

-- `FromJSON AuthorizationResponseErrorCode` recognises RFC 6749 §4.1.2.1 codes.
#guard (FromJSON.parseJSON (Value.string "access_denied") : Except String AuthorizationResponseErrorCode)
    == .ok .accessDenied
#guard (FromJSON.parseJSON (Value.string "totally_made_up") : Except String AuthorizationResponseErrorCode)
    == .ok (.unknownErrorCode "totally_made_up")

-- `FromJSON AuthorizationResponseError` decodes the full error shape.
#guard
  (match FromJSON.parseJSON
      (Value.object [("error", .string "invalid_scope"), ("error_description", .string "bad scope")])
      with
   | Except.ok (e : AuthorizationResponseError) =>
     (e.authorizationResponseError, e.authorizationResponseErrorDescription)
   | Except.error _ => (.unknownErrorCode "parse failed", none))
    == (.invalidScope, some "bad scope")

/-! ### Signatures -/

example : QueryParams → OAuth2 → Network.URI.URI := authorizationUrlWithParams
example : OAuth2 → Network.URI.URI := authorizationUrl

end Tests.Network.OAuth2.AuthorizationRequest
