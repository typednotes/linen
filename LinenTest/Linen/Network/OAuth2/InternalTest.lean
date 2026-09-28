/-
  Tests for `Linen.Network.OAuth2.Internal`.
-/
import Linen.Network.OAuth2.Internal

open Network.OAuth2.Internal
open Network.HTTP.Client
open Network.HTTP.Types
open Data.Json (ToJSON FromJSON)

namespace Tests.Network.OAuth2.Internal

private instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .error a, .error b => a == b
    | .ok a, .ok b => a == b
    | _, _ => false

-- `OAuth2` has a `Default` instance with an empty (`nullURI`) configuration.
#guard (Data.Default.default : OAuth2).oauth2ClientId == ""

-- Token newtypes round-trip through `ToJSON`/`FromJSON`.
#guard (FromJSON.parseJSON (ToJSON.toJSON (AccessToken.mk "tok"))
  : Except String AccessToken) == .ok ⟨"tok"⟩
#guard (FromJSON.parseJSON (ToJSON.toJSON (RefreshToken.mk "rtok"))
  : Except String RefreshToken) == .ok ⟨"rtok"⟩
#guard (FromJSON.parseJSON (ToJSON.toJSON (IdToken.mk "itok"))
  : Except String IdToken) == .ok ⟨"itok"⟩
#guard (FromJSON.parseJSON (ToJSON.toJSON (ExchangeToken.mk "code"))
  : Except String ExchangeToken) == .ok ⟨"code"⟩

-- `addDefaultRequestHeaders` prepends the User-Agent/Accept headers.
#guard
  (addDefaultRequestHeaders { method := .standard .GET, host := "example.com", port := 443 }).headers
    == defaultRequestHeaders

-- `appendQueryParams` adds params to a query-less URI, prefixed with `?`.
#guard (appendQueryParams [("a", "1")] Network.URI.nullURI).uriQuery == "?a=1"

-- `appendQueryParams` appends to an existing query with `&`.
#guard (appendQueryParams [("b", "2")] { Network.URI.nullURI with uriQuery := "?a=1" }).uriQuery
    == "?a=1&b=2"

-- `appendQueryParams []` is the identity.
#guard appendQueryParams [] Network.URI.nullURI == Network.URI.nullURI

-- `uriToRequest` builds a GET request from an absolute http(s) URI.
#guard
  (match Network.URI.parseURI "https://example.com/token?x=1" with
   | some uri =>
     match uriToRequest uri with
     | .ok req => some (req.method == .standard .GET, req.host, req.port, req.path, req.queryString, req.isSecure)
     | .error _ => none
   | none => none)
    == some (true, "example.com", 443, "/token", "?x=1", true)

-- `uriToRequest` rejects a relative reference (no authority).
#guard
  (match Network.URI.parseURIReference "/relative/path" with
   | some uri => (uriToRequest uri).isOk
   | none => true)
    == false

/-! ### Signatures -/

example : Request → Request := addDefaultRequestHeaders
example : QueryParams → Network.URI.URI → Network.URI.URI := appendQueryParams
example : Network.URI.URI → Except String Request := uriToRequest

end Tests.Network.OAuth2.Internal
