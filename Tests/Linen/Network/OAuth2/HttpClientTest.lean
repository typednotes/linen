/-
  Tests for `Linen.Network.OAuth2.HttpClient`.

  `authRequest`/`authGetBS*`/`authPostBS*`/`authGetJSON*`/`authPostJSON*` all
  perform real network IO, so — like `Network.HTTP.Client.Conduit`'s own
  test module — they're pinned at the type level rather than exercised
  against a live server. The pure request-mutating helpers get real
  `#guard` checks.
-/
import Linen.Network.OAuth2.HttpClient

open Network.OAuth2.Internal
open Network.OAuth2.HttpClient
open Network.HTTP.Client (Request Response)
open Network.HTTP.Types
open Data.Json (FromJSON)

namespace Tests.Network.OAuth2.HttpClient

private def sampleRequest : Request :=
  { method := .standard .GET, host := "example.com", port := 443 }

-- `accessTokenToParam` renders an `access_token` body/query pair.
#guard accessTokenToParam ⟨"tok"⟩ == [("access_token", "tok")]

-- `appendAccessToken` adds `access_token` to a URI's query string.
#guard (appendAccessToken Network.URI.nullURI ⟨"tok"⟩).uriQuery == "?access_token=tok"

-- `updateRequestHeaders none` only adds the default headers.
#guard (updateRequestHeaders none sampleRequest).headers == defaultRequestHeaders

-- `updateRequestHeaders (some t)` also adds a Bearer `Authorization` header.
#guard (updateRequestHeaders (some ⟨"tok"⟩) sampleRequest).headers.head?
    == some (hAuthorization, "Bearer tok")

-- `setMethod` overrides the request method.
#guard (setMethod .POST sampleRequest).method == .standard .POST

-- `jsonBody` renders the body as a JSON object and sets `Content-Type`.
#guard (jsonBody [("a", "1")] sampleRequest).body
    == some (Data.Json.Encode.encode (.object [("a", .string "1")])).toUTF8
#guard (jsonBody [("a", "1")] sampleRequest).headers.head? == some (hContentType, "application/json")

/-! ### Signatures -/

example : APIAuthenticationMethod → AccessToken → Network.URI.URI → IO (Except ByteArray ByteArray) :=
  authGetBSWithAuthMethod
example : AccessToken → Network.URI.URI → IO (Except ByteArray ByteArray) := authGetBS
example [FromJSON a] :
    APIAuthenticationMethod → AccessToken → Network.URI.URI → IO (Except ByteArray a) :=
  authGetJSONWithAuthMethod
example [FromJSON a] : AccessToken → Network.URI.URI → IO (Except ByteArray a) := authGetJSON
example :
    APIAuthenticationMethod → AccessToken → Network.URI.URI → PostBody →
      IO (Except ByteArray ByteArray) :=
  authPostBSWithAuthMethod
example : AccessToken → Network.URI.URI → PostBody → IO (Except ByteArray ByteArray) := authPostBS
example [FromJSON a] :
    APIAuthenticationMethod → AccessToken → Network.URI.URI → PostBody → IO (Except ByteArray a) :=
  authPostJSONWithAuthMethod
example [FromJSON a] : AccessToken → Network.URI.URI → PostBody → IO (Except ByteArray a) := authPostJSON
example : Request → IO (Except ByteArray ByteArray) := authRequest

end Tests.Network.OAuth2.HttpClient
