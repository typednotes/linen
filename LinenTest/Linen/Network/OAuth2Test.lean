/-
  Tests for `Linen.Network.OAuth2` (the package facade).

  This only checks that the re-exported names actually resolve unqualified
  under the `Network.OAuth2` namespace and keep the signature each
  submodule gives them — the submodules' own test files already exercise
  behaviour.
-/
import Linen.Network.OAuth2

open Network.OAuth2

namespace Tests.Network.OAuth2

private instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .error a, .error b => a == b
    | .ok a, .ok b => a == b
    | _, _ => false

-- `OAuth2` (the config record) and the token newtypes are re-exported from `.Internal`.
#guard (Data.Default.default : OAuth2).oauth2ClientId == ""
#guard (⟨"tok"⟩ : AccessToken).atoken == "tok"

-- `authorizationUrl` is re-exported from `.AuthorizationRequest` (its error types are not).
#guard
  (authorizationUrl
      { oauth2ClientId := "c"
        oauth2AuthorizeEndpoint := (Network.URI.parseURI "https://idp.example.com/authorize").get!
        oauth2RedirectUri := (Network.URI.parseURI "https://client.example.com/cb").get! }).uriQuery
    == "?client_id=c&response_type=code&redirect_uri=https%3A%2F%2Fclient.example.com%2Fcb"

-- `accessTokenToParam` is re-exported from `.HttpClient`.
#guard accessTokenToParam ⟨"tok"⟩ == [("access_token", "tok")]

-- `clientSecretPost`/`TokenResponseErrorCode` are re-exported from `.TokenRequest`.
#guard
  clientSecretPost { oauth2ClientId := "c", oauth2ClientSecret := "s" }
    == [("client_id", "c"), ("client_secret", "s")]
#guard
  (Data.Json.FromJSON.parseJSON (Data.Json.Value.string "invalid_grant")
    : Except String TokenResponseErrorCode)
    == .ok .invalidGrant

end Tests.Network.OAuth2
