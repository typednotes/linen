/-
  Linen.Network.OAuth2.AuthorizationRequest — the `/authorize` URL

  Port of `hoauth2`'s `Network.OAuth2.AuthorizationRequest` (see
  `docs/imports/hoauth2/dependencies.md`), module #6: building the
  authorization-request URL a client redirects the user's browser to
  (RFC 6749 §4.1.1), plus the error shape a provider's `/authorize`
  redirect can carry back (RFC 6749 §4.1.2.1).

  ## Substitutions
  - `Lens.Micro`'s `over (queryL . queryPairsL) (++ queryParts) uri` is
    `Linen.Network.OAuth2.Internal.appendQueryParams` (already ported in
    an earlier batch — see that module's own doc-comment; this is one of
    the three call sites `dependencies.md` notes for that helper).
  - `URI.ByteString`'s `URI`/`URIRef Absolute`/`serializeURIRef'` are
    `Linen.Network.URI.URI`/`Network.URI.uriToString`.
  - `uri-bytestring-aeson`'s `FromJSON URI` instance is
    `Linen.Network.OAuth2.Internal`'s `FromJSON Network.URI.URI` (added in
    this same import batch — see that module's doc-comment).
  - `Data.List.nubBy ((==) \`on\` fst)` (drop later query-parameter entries
    that repeat an earlier key, RFC 6749 doesn't otherwise specify what to
    do with a caller-supplied `client_id`/`response_type`/`redirect_uri`
    override) is `dedupByKey` below: a plain structural pass with an
    explicit "seen keys" accumulator, since a general `nubBy` has no
    existing home in this port and this is its only call site.
-/

import Linen.Network.OAuth2.Internal
import Linen.Data.Json.Types

namespace Network.OAuth2.AuthorizationRequest

open Network.OAuth2.Internal (OAuth2 QueryParams appendQueryParams)
open Data.Json (Value ToJSON FromJSON)

-- ────────────────────────────────────────────────────────────────────
-- Authorization Request Errors
-- ────────────────────────────────────────────────────────────────────

/-- Authorization Code Grant error codes, RFC 6749 §4.1.2.1. -/
inductive AuthorizationResponseErrorCode where
  | invalidRequest
  | unauthorizedClient
  | accessDenied
  | unsupportedResponseType
  | invalidScope
  | serverError
  | temporarilyUnavailable
  /-- A code this port doesn't recognise, carrying the raw wire value. -/
  | unknownErrorCode (code : String)
deriving Repr, BEq

instance : FromJSON AuthorizationResponseErrorCode where
  parseJSON
    | .string s =>
      .ok <| match s with
        | "invalid_request" => .invalidRequest
        | "unauthorized_client" => .unauthorizedClient
        | "access_denied" => .accessDenied
        | "unsupported_response_type" => .unsupportedResponseType
        | "invalid_scope" => .invalidScope
        | "server_error" => .serverError
        | "temporarily_unavailable" => .temporarilyUnavailable
        | other => .unknownErrorCode other
    | v => .error s!"expected a string, got {repr v}"

/-- What an `/authorize` redirect carries back on failure, RFC 6749 §4.1.2.1.

    See the upstream doc-comment's own caveat, kept verbatim: it is hard to
    exercise this in practice, since a misconfigured `/authorize` request
    usually just gets stuck rendering the provider's own error page rather
    than redirecting back to the relying party at all. -/
structure AuthorizationResponseError where
  authorizationResponseError : AuthorizationResponseErrorCode
  authorizationResponseErrorDescription : Option String
  authorizationResponseErrorUri : Option Network.URI.URI
deriving Repr

instance : FromJSON AuthorizationResponseError where
  parseJSON v := do
    pure
      { authorizationResponseError := ← Value.getField v "error" >>= FromJSON.parseJSON
        authorizationResponseErrorDescription :=
          ← (← Value.getFieldOpt v "error_description").mapM FromJSON.parseJSON
        authorizationResponseErrorUri := ← (← Value.getFieldOpt v "error_uri").mapM FromJSON.parseJSON }

-- ────────────────────────────────────────────────────────────────────
-- URLs
-- ────────────────────────────────────────────────────────────────────

/-- Drop every later occurrence of a query-parameter key, keeping the
    first (Haskell's `nubBy ((==) \`on\` fst)`; see the module doc-comment
    for why a general `nubBy` isn't ported for this one call site). -/
private def dedupByKeyGo (seen : List String) : QueryParams → QueryParams
  | [] => []
  | (k, v) :: rest =>
    if seen.contains k then dedupByKeyGo seen rest
    else (k, v) :: dedupByKeyGo (k :: seen) rest

private def dedupByKey (qs : QueryParams) : QueryParams := dedupByKeyGo [] qs

/-- Prepare the authorization URL. Redirect the user's browser here to ask
    for interactive authentication (RFC 6749 §4.1.1). `qs` may add extra
    query parameters (e.g. `state`, `scope`); any of `qs` sharing a key
    with the three parameters this always sends (`client_id`,
    `response_type`, `redirect_uri`) wins over this function's own default.

    $$\text{authorizationUrlWithParams} : \text{QueryParams} \to \text{OAuth2} \to \text{URI}$$ -/
def authorizationUrlWithParams (qs : QueryParams) (oa : OAuth2) : Network.URI.URI :=
  let queryParts :=
    dedupByKey <|
      qs ++
        [ ("client_id", oa.oauth2ClientId)
        , ("response_type", "code")
        , ("redirect_uri", Network.URI.uriToString Network.URI.defaultUserInfoMap oa.oauth2RedirectUri) ]
  appendQueryParams queryParts oa.oauth2AuthorizeEndpoint

/-- See `authorizationUrlWithParams`. -/
def authorizationUrl (oa : OAuth2) : Network.URI.URI := authorizationUrlWithParams [] oa

end Network.OAuth2.AuthorizationRequest
