/-
  Linen.Network.OAuth2.HttpClient — bearer-token-authenticated requests

  Port of `hoauth2`'s `Network.OAuth2.HttpClient` (see
  `docs/imports/hoauth2/dependencies.md`), module #7: RFC 6750 "Bearer
  Token Usage" — GET/POST helpers that attach an `AccessToken` to a
  request, either in the `Authorization` header, the request body, or the
  URL's query string.

  ## Substitutions
  - `http-conduit`'s `Manager` (an HTTP connection pool handle) has no
    analogue in this port's `Linen.Network.HTTP.Client`, which opens and
    closes one connection per request (`Network.HTTP.Client.Conduit.
    withResponse`) — every function below drops the `Manager` parameter
    accordingly, the same "no connection-pooling abstraction to thread
    through" substitution the rest of this port's HTTP call sites already
    make.
  - `Control.Monad.Trans.Except.ExceptT BSL.ByteString m` is a plain
    `IO (Except ByteArray _)` — this port has no transformer-stack
    equivalent elsewhere in `hoauth2`'s own call sites either (see
    `Linen.Network.OAuth2.Internal`'s `MonadThrow` substitution note).
  - `Network.HTTP.Client.Conduit.applyBearerAuth` and `Data.Aeson.encode`
    are `Linen.Network.HTTP.Client.Conduit.applyBearerAuth` (added in this
    same import batch) and `Linen.Data.Json.Encode.encode`.
  - `Lens.Micro`'s `over (queryL . queryPairsL) (++ ...)` — the second of
    the three call sites `dependencies.md` notes for this pattern — is
    `Network.OAuth2.Internal.appendQueryParams`.
-/

import Linen.Network.OAuth2.Internal
import Linen.Network.HTTP.Client.Contrib
import Linen.Network.HTTP.Client.Conduit
import Linen.Data.Json.Types
import Linen.Data.Json.Decode
import Linen.Data.Json.Encode

namespace Network.OAuth2.HttpClient

open Network.OAuth2.Internal
open Network.HTTP.Client (Request Response)
open Network.HTTP.Types
open Data.Json (Value ToJSON FromJSON)
open Data.Json.Decode (decodeAs)

-- ────────────────────────────────────────────────────────────────────
-- Types
-- ────────────────────────────────────────────────────────────────────

/-- How an `AccessToken` is attached to an authenticated request,
    RFC 6750 §2. -/
inductive APIAuthenticationMethod where
  /-- `Authorization: Bearer <token>` header (RFC 6750 §2.1). -/
  | authInRequestHeader
  /-- In the POST request body (RFC 6750 §2.2). -/
  | authInRequestBody
  /-- `access_token` query parameter (RFC 6750 §2.3). -/
  | authInRequestQuery
deriving Repr, BEq

-- ────────────────────────────────────────────────────────────────────
-- Utilities
-- ────────────────────────────────────────────────────────────────────

/-- `access_token` as a query/body parameter. -/
def accessTokenToParam (t : AccessToken) : PostBody := [("access_token", t.atoken)]

/-- Append `access_token` to a URI's query string (for
    `authInRequestQuery`). -/
def appendAccessToken (uri : Network.URI.URI) (t : AccessToken) : Network.URI.URI :=
  appendQueryParams (accessTokenToParam t) uri

/-- Add the default headers, and — if a token is given — a
    `Authorization: Bearer` header, to a request. -/
def updateRequestHeaders (mt : Option AccessToken) (req : Request) : Request :=
  let req' := addDefaultRequestHeaders req
  match mt with
  | none => req'
  | some t => Network.HTTP.Client.Conduit.applyBearerAuth t.atoken req'

/-- Override a request's HTTP method. -/
def setMethod (m : Network.HTTP.Types.StdMethod) (req : Request) : Request :=
  { req with method := .standard m }

/-- Render a `PostBody` as a JSON object request body, setting the
    `Content-Type` header (`hoauth2`'s `jsonBody`). -/
def jsonBody (body : PostBody) (req : Request) : Request :=
  let json : Value := .object (body.map fun (k, v) => (k, .string v))
  { req with
      body := some (Data.Json.Encode.encode json).toUTF8
      headers := (hContentType, "application/json") :: req.headers.filter (fun h => h.1 != hContentType) }

/-- Send `req` and turn a non-2xx response into `Except.error` (`hoauth2`'s
    `authRequest`). -/
def authRequest (req : Request) : IO (Except ByteArray ByteArray) := do
  let resp ← Network.HTTP.Client.Conduit.withResponse req pure
  pure (Network.HTTP.Client.Contrib.handleResponse resp)

-- ────────────────────────────────────────────────────────────────────
-- AUTH requests
-- ────────────────────────────────────────────────────────────────────

/-- Conduct an authorized GET request, returning the response body as
    bytes. Allows specifying how the `AccessToken` is attached. -/
def authGetBSWithAuthMethod (authTypes : APIAuthenticationMethod) (token : AccessToken)
    (url : Network.URI.URI) : IO (Except ByteArray ByteArray) := do
  let appendToUrl := authTypes == .authInRequestQuery
  let appendToHeader := authTypes == .authInRequestHeader
  let uri := if appendToUrl then appendAccessToken url token else url
  match uriToRequest uri with
  | .error e => pure (.error e.toUTF8)
  | .ok req =>
    let req' := updateRequestHeaders (if appendToHeader then some token else none) (setMethod .GET req)
    authRequest req'

/-- Conduct an authorized GET request, injecting the `AccessToken` into the
    `Authorization` header (`hoauth2`'s `authGetBS`). -/
def authGetBS : AccessToken → Network.URI.URI → IO (Except ByteArray ByteArray) :=
  authGetBSWithAuthMethod .authInRequestHeader

/-- Like `authGetBSWithAuthMethod`, but JSON-decodes a successful body. -/
def authGetJSONWithAuthMethod [FromJSON a] (authTypes : APIAuthenticationMethod)
    (token : AccessToken) (uri : Network.URI.URI) : IO (Except ByteArray a) := do
  match ← authGetBSWithAuthMethod authTypes token uri with
  | .error e => pure (.error e)
  | .ok body =>
    match decodeAs (α := a) (String.fromUTF8! body) with
    | .ok v => pure (.ok v)
    | .error msg => pure (.error msg.toUTF8)

/-- Conduct an authorized GET request, injecting the `AccessToken` into the
    `Authorization` header, and decode the response as JSON (`hoauth2`'s
    `authGetJSON`). -/
def authGetJSON [FromJSON a] : AccessToken → Network.URI.URI → IO (Except ByteArray a) :=
  authGetJSONWithAuthMethod .authInRequestHeader

/-- Conduct an authorized POST request, returning the response body as
    bytes. Allows specifying how the `AccessToken` is attached. -/
def authPostBSWithAuthMethod (authTypes : APIAuthenticationMethod) (token : AccessToken)
    (url : Network.URI.URI) (body : PostBody) : IO (Except ByteArray ByteArray) := do
  let appendToBody := authTypes == .authInRequestBody
  let appendToHeader := authTypes == .authInRequestHeader
  let reqBody := if appendToBody then body ++ accessTokenToParam token else body
  match uriToRequest url with
  | .error e => pure (.error e.toUTF8)
  | .ok req =>
    let req' := setMethod .POST req
    let req'' := updateRequestHeaders (if appendToHeader then some token else none) req'
    let req''' := if reqBody.isEmpty then req'' else jsonBody reqBody req''
    authRequest req'''

/-- Conduct an authorized POST request, injecting the `AccessToken` into
    the `Authorization` header (`hoauth2`'s `authPostBS`). -/
def authPostBS : AccessToken → Network.URI.URI → PostBody → IO (Except ByteArray ByteArray) :=
  authPostBSWithAuthMethod .authInRequestHeader

/-- Like `authPostBSWithAuthMethod`, but JSON-decodes a successful body. -/
def authPostJSONWithAuthMethod [FromJSON a] (authTypes : APIAuthenticationMethod)
    (token : AccessToken) (url : Network.URI.URI) (body : PostBody) : IO (Except ByteArray a) := do
  match ← authPostBSWithAuthMethod authTypes token url body with
  | .error e => pure (.error e)
  | .ok resp =>
    match decodeAs (α := a) (String.fromUTF8! resp) with
    | .ok v => pure (.ok v)
    | .error msg => pure (.error msg.toUTF8)

/-- Conduct an authorized POST request, injecting the `AccessToken` into
    the `Authorization` header, and decode the response as JSON
    (`hoauth2`'s `authPostJSON`). -/
def authPostJSON [FromJSON a] :
    AccessToken → Network.URI.URI → PostBody → IO (Except ByteArray a) :=
  authPostJSONWithAuthMethod .authInRequestHeader

end Network.OAuth2.HttpClient
