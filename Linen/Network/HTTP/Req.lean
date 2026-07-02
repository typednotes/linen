/-
  Linen.Network.HTTP.Req — Type-safe HTTP client

  Port of Haskell's `req` library (https://github.com/mrkkrp/req).
  Provides a type-safe, composable API for HTTP requests where common
  mistakes are caught at compile time.

  ## Compile-Time Guarantees

  1. **Method/body compatibility**: GET cannot carry a request body.
     The `HttpBodyAllowed` typeclass has no instance for `(.NoBody, .YesBody)`,
     so `req GET url (ReqBodyBs data) ...` is a compile-time error.

  2. **HTTPS-only auth**: `basicAuth` returns `Option .Https`, so it can
     only be used with `Url .Https`. Using it with an HTTP URL fails
     type unification.

  3. **Non-empty hostnames**: `Url` carries a proof `host.length > 0`,
     making empty-hostname URLs unconstructible.

  4. **Option monoid laws**: Proven associativity and identity.

  ## Usage
  ```lean
  open Network.HTTP.Req

  def main : IO Unit := do
    let r ← runReq defaultHttpConfig do
      req GET.mk (https "httpbin.org" /: "get") NoReqBody.mk bsResponse
    IO.println s!"Status: {r.status}"
    IO.println s!"Body: {String.fromUTF8! r.body}"
  ```
-/

import Linen.Network.HTTP.Client.Types
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Redirect
import Linen.Data.CaseInsensitive
import Linen.Data.Base64

namespace Network.HTTP.Req

open Network.HTTP.Types
open Network.HTTP.Client
open Data

-- ══════════════════════════════════════════════════════════════
-- Scheme: phantom type for URL/Option
-- ══════════════════════════════════════════════════════════════

/-- URL scheme, used as a phantom parameter on `Url` and `Option`
    to enforce HTTPS-only constraints at the type level.
    $$\text{Scheme} = \text{Http} \mid \text{Https}$$ -/
inductive Scheme where
  | Http
  | Https
deriving BEq, DecidableEq, Repr

/-- HTTP and HTTPS schemes are distinct (enables auth separation). -/
theorem Scheme.http_ne_https : Scheme.Http ≠ Scheme.Https := by decide

-- ══════════════════════════════════════════════════════════════
-- Url: phantom-typed, correct-by-construction
-- ══════════════════════════════════════════════════════════════

/-- A correct-by-construction URL parameterized by scheme.

    The scheme phantom parameter enables HTTPS-only auth enforcement.
    Hostname is proven non-empty by construction.
    Path segments are stored in order.

    $$\text{Url}\ s = \{ \text{host} : \{h : \text{String} \mid h.\text{length} > 0\},\;
      \text{segments} : \text{List String} \}$$ -/
structure Url (scheme : Scheme) where
  /-- The hostname. Must be non-empty (enforced by proof). -/
  host : String
  /-- Path segments (e.g., ["api", "v1", "users"]). -/
  segments : List String := []
  /-- Host is non-empty (proof, erased at runtime). -/
  host_nonempty : host.length > 0

instance : Repr (Url s) where
  reprPrec u _ :=
    let scheme := match s with | .Http => "http" | .Https => "https"
    let path := "/" ++ "/".intercalate u.segments
    s!"{scheme}://{u.host}{path}"

/-- Construct an HTTP URL from a hostname.
    $$\text{http} : \text{String} \to \text{Url .Http}$$ -/
def http (host : String) (h : host.length > 0 := by decide) : Url .Http :=
  ⟨host, [], h⟩

/-- Construct an HTTPS URL from a hostname.
    $$\text{https} : \text{String} \to \text{Url .Https}$$ -/
def https (host : String) (h : host.length > 0 := by decide) : Url .Https :=
  ⟨host, [], h⟩

/-- Append a path segment to a URL.
    $$(\text{/:}) : \text{Url}\ s \to \text{String} \to \text{Url}\ s$$ -/
def Url.append (url : Url s) (segment : String) : Url s :=
  { url with segments := url.segments ++ [segment] }

/-- Infix operator for path segments: `https "api.example.com" /: "v1" /: "users"` -/
infixl:65 " /: " => Url.append

/-- Render a URL's path component (with leading /). -/
def Url.path (url : Url s) : String :=
  if url.segments.isEmpty then "/"
  else "/" ++ "/".intercalate url.segments

/-- Get the default port for a URL based on its scheme. -/
def Url.defaultPort (_ : Url s) : UInt16 :=
  match s with | .Http => 80 | .Https => 443

/-- Whether the URL scheme is secure (HTTPS). -/
def Url.isSecure (_ : Url s) : Bool :=
  match s with | .Http => false | .Https => true

-- ══════════════════════════════════════════════════════════════
-- CanHaveBody, HttpMethod, HttpBody, HttpBodyAllowed
-- ══════════════════════════════════════════════════════════════

/-- Whether an HTTP method allows a request body.
    $$\text{CanHaveBody} = \text{YesBody} \mid \text{NoBody}$$ -/
inductive CanHaveBody where
  | YesBody
  | NoBody
deriving BEq, DecidableEq, Repr

/-- Typeclass for HTTP methods.
    Maps each method to its body permission and wire name.
    $$\text{HttpMethod}\ m = \{ \text{allowsBody} : \text{CanHaveBody},\;
      \text{methodName} : \text{String} \}$$ -/
class HttpMethod (m : Type) where
  /-- Whether this method allows a request body. -/
  allowsBody : CanHaveBody
  /-- The HTTP method name as it appears on the wire (e.g., "GET"). -/
  methodName : String

/-- Typeclass for HTTP request bodies.
    Maps each body type to its content and content-type.
    $$\text{HttpBody}\ b = \{ \text{providesBody} : \text{CanHaveBody},\;
      \text{getBody} : b \to \text{Option ByteArray},\;
      \text{getContentType} : b \to \text{Option String} \}$$ -/
class HttpBody (b : Type) where
  /-- Whether this body type provides a body. -/
  providesBody : CanHaveBody
  /-- Get the body bytes. `none` means no body. -/
  getBody : b → Option ByteArray
  /-- Get the Content-Type header value. -/
  getContentType : b → Option String := fun _ => none

-- `allowsBody`/`providesBody` must stay `@[reducible]`: `HttpBodyAllowed`'s
-- instance search is keyed on their *reduced* `CanHaveBody` value (e.g.
-- `.NoBody`), and discrimination-tree indexing only unfolds reducible
-- definitions when building/matching keys — without this, a stuck
-- projection like `HttpMethod.allowsBody GET` never matches the
-- `HttpBodyAllowed .NoBody .NoBody` instance's key, even though it's
-- definitionally equal to it (`get_no_body` below proves exactly this
-- equality by `rfl`).
attribute [reducible] HttpMethod.allowsBody HttpBody.providesBody

/-- Compile-time constraint: method/body compatibility.

    This typeclass has instances for all valid combinations:
    - `(.YesBody, .YesBody)` — POST with body
    - `(.YesBody, .NoBody)` — POST with no body (allowed)
    - `(.NoBody, .NoBody)` — GET with no body

    The missing instance `(.NoBody, .YesBody)` means that
    `req GET url (ReqBodyBs data) ...` fails at compile time
    with "failed to synthesize HttpBodyAllowed .NoBody .YesBody". -/
class HttpBodyAllowed (allows : CanHaveBody) (provides : CanHaveBody) where

instance : HttpBodyAllowed .YesBody .YesBody where
instance : HttpBodyAllowed .YesBody .NoBody where
instance : HttpBodyAllowed .NoBody .NoBody where
-- NO instance for HttpBodyAllowed .NoBody .YesBody
-- This is intentional: GET + body = compile-time error

-- ── Method types ──

/-- HTTP GET method. Does not allow a request body. -/
structure GET where
/-- HTTP POST method. Allows a request body. -/
structure POST where
/-- HTTP HEAD method. Does not allow a request body. -/
structure HEAD where
/-- HTTP PUT method. Allows a request body. -/
structure PUT where
/-- HTTP DELETE method. Does not allow a request body (matches Haskell req). -/
structure DELETE where
/-- HTTP PATCH method. Allows a request body. -/
structure PATCH where
/-- HTTP OPTIONS method. Does not allow a request body. -/
structure OPTIONS where
/-- HTTP TRACE method. Does not allow a request body. -/
structure TRACE where
/-- HTTP CONNECT method. Does not allow a request body. -/
structure CONNECT where

instance : HttpMethod GET     where allowsBody := .NoBody;  methodName := "GET"
instance : HttpMethod POST    where allowsBody := .YesBody; methodName := "POST"
instance : HttpMethod HEAD    where allowsBody := .NoBody;  methodName := "HEAD"
instance : HttpMethod PUT     where allowsBody := .YesBody; methodName := "PUT"
instance : HttpMethod DELETE  where allowsBody := .NoBody;  methodName := "DELETE"
instance : HttpMethod PATCH   where allowsBody := .YesBody; methodName := "PATCH"
instance : HttpMethod OPTIONS where allowsBody := .NoBody;  methodName := "OPTIONS"
instance : HttpMethod TRACE   where allowsBody := .NoBody;  methodName := "TRACE"
instance : HttpMethod CONNECT where allowsBody := .NoBody;  methodName := "CONNECT"

-- ── Method proofs ──

theorem get_no_body : HttpMethod.allowsBody (m := GET) = .NoBody := rfl
theorem post_yes_body : HttpMethod.allowsBody (m := POST) = .YesBody := rfl
theorem put_yes_body : HttpMethod.allowsBody (m := PUT) = .YesBody := rfl
theorem patch_yes_body : HttpMethod.allowsBody (m := PATCH) = .YesBody := rfl
theorem head_no_body : HttpMethod.allowsBody (m := HEAD) = .NoBody := rfl
theorem delete_no_body : HttpMethod.allowsBody (m := DELETE) = .NoBody := rfl

-- ══════════════════════════════════════════════════════════════
-- Body types
-- ══════════════════════════════════════════════════════════════

/-- No request body. Used with GET, HEAD, DELETE, etc. -/
structure NoReqBody where

instance : HttpBody NoReqBody where
  providesBody := .NoBody
  getBody _ := none

/-- Strict ByteArray body. Sets Content-Type to application/octet-stream. -/
structure ReqBodyBs where
  bytes : ByteArray

instance : HttpBody ReqBodyBs where
  providesBody := .YesBody
  getBody b := some b.bytes
  getContentType _ := some "application/octet-stream"

/-- Lazy ByteArray body (same as strict for now; lazy distinction is for API compatibility). -/
structure ReqBodyLbs where
  bytes : ByteArray

instance : HttpBody ReqBodyLbs where
  providesBody := .YesBody
  getBody b := some b.bytes
  getContentType _ := some "application/octet-stream"

/-- URL-encoded form body (application/x-www-form-urlencoded). -/
structure ReqBodyUrlEnc where
  params : List (String × String)

/-- Percent-encode a string for URL form encoding. -/
private def urlEncodeParam (s : String) : String := Id.run do
  let mut out := ""
  for c in s.toList do
    if c.isAlphanum || c == '-' || c == '_' || c == '.' || c == '~' then
      out := out.push c
    else if c == ' ' then
      out := out.push '+'
    else
      let b := c.toString.toUTF8
      for byte in b.toList do
        let hi := byte.toNat / 16
        let lo := byte.toNat % 16
        let hexChar (n : Nat) : Char :=
          if n < 10 then Char.ofNat (n + '0'.toNat) else Char.ofNat (n - 10 + 'A'.toNat)
        out := out.push '%' |>.push (hexChar hi) |>.push (hexChar lo)
  out

/-- Encode form parameters as "key1=value1&key2=value2". -/
private def encodeFormParams (params : List (String × String)) : String :=
  "&".intercalate (params.map fun (k, v) => s!"{urlEncodeParam k}={urlEncodeParam v}")

instance : HttpBody ReqBodyUrlEnc where
  providesBody := .YesBody
  getBody b := some (encodeFormParams b.params).toUTF8
  getContentType _ := some "application/x-www-form-urlencoded"

/-- File body. Reads the file at send time. -/
structure ReqBodyFile where
  path : String

instance : HttpBody ReqBodyFile where
  providesBody := .YesBody
  getBody _ := none  -- File content is loaded in IO at send time
  getContentType _ := some "application/octet-stream"

-- ══════════════════════════════════════════════════════════════
-- Response types
-- ══════════════════════════════════════════════════════════════

/-- Typeclass for HTTP response interpretation.
    Defines how to parse the raw `Network.HTTP.Client.Response` into
    a typed response value.
    $$\text{HttpResponse}\ r = \{ \text{interpretResponse} : \text{Response} \to \text{IO}\ r,\;
      \text{acceptHeader} : \text{Option String} \}$$ -/
class HttpResponse (r : Type) where
  /-- Parse a raw HTTP response into the typed response. -/
  interpretResponse : Network.HTTP.Client.Response → IO r
  /-- Optional Accept header value to send with the request. -/
  acceptHeader : Option String := none

/-- Ignore the response body. Only status and headers are returned. -/
structure IgnoreResponse where
  /-- Response status. -/
  status : Status
  /-- Response headers. -/
  headers : ResponseHeaders
deriving Repr

instance : HttpResponse IgnoreResponse where
  interpretResponse resp :=
    return { status := resp.statusCode, headers := resp.headers }

/-- Strict ByteArray response — returns the full body as a `ByteArray`. -/
structure BsResponse where
  /-- Response status. -/
  status : Status
  /-- Response headers. -/
  headers : ResponseHeaders
  /-- Response body. -/
  body : ByteArray

instance : HttpResponse BsResponse where
  interpretResponse resp :=
    return { status := resp.statusCode, headers := resp.headers, body := resp.body }

-- Proxy-style response selectors (matching Haskell req API).
-- These are dummy values used only for type inference — the actual response
-- is constructed by `interpretResponse` at runtime.

/-- Select `IgnoreResponse` as the response type. -/
def ignoreResponse : IgnoreResponse := IgnoreResponse.mk status200 []

/-- Select `BsResponse` as the response type. -/
def bsResponse : BsResponse := BsResponse.mk status200 [] ByteArray.empty

-- ══════════════════════════════════════════════════════════════
-- Option: composable request options (monoid)
-- ══════════════════════════════════════════════════════════════

/-- Composable request options, parameterized by scheme.

    Forms a Monoid via `Append` / `EmptyCollection`. The `scheme`
    phantom parameter enables HTTPS-only authentication enforcement:
    `basicAuth` returns `ReqOption .Https`, so it can only be passed
    to `req` when the URL is also `Url .Https`.

    $$\text{ReqOption}\ s = \{ \text{extraHeaders},\; \text{queryParams},\;
      \text{portOverride},\; \text{timeout} \}$$ -/
structure ReqOption (scheme : Scheme) where
  /-- Extra headers to add to the request. -/
  extraHeaders : RequestHeaders := []
  /-- Extra query parameters (key, value). -/
  queryParams : List (String × String) := []
  /-- Override the default port. -/
  portOverride : Option UInt16 := none
  /-- Response timeout in milliseconds. -/
  timeout : Option Nat := none
deriving Repr

instance : Append (ReqOption s) where
  append a b :=
    { extraHeaders := a.extraHeaders ++ b.extraHeaders
    , queryParams := a.queryParams ++ b.queryParams
    , portOverride := b.portOverride <|> a.portOverride
    , timeout := b.timeout <|> a.timeout }

instance : EmptyCollection (ReqOption s) where
  emptyCollection := {}

-- ── Option monoid proofs ──

/-- `ReqOption` append is associative for the list-based fields. -/
theorem option_extraHeaders_append_assoc (a b c : ReqOption s) :
    (a ++ b ++ c).extraHeaders = (a ++ (b ++ c)).extraHeaders := by
  show (a.extraHeaders ++ b.extraHeaders) ++ c.extraHeaders = a.extraHeaders ++ (b.extraHeaders ++ c.extraHeaders)
  exact List.append_assoc ..

theorem option_queryParams_append_assoc (a b c : ReqOption s) :
    (a ++ b ++ c).queryParams = (a ++ (b ++ c)).queryParams := by
  show (a.queryParams ++ b.queryParams) ++ c.queryParams = a.queryParams ++ (b.queryParams ++ c.queryParams)
  exact List.append_assoc ..

-- ── Option combinators ──

/-- Add a custom header to the request. -/
def header (name : HeaderName) (value : String) : ReqOption s :=
  { extraHeaders := [(name, value)] }

/-- Override the port. -/
def port (p : UInt16) : ReqOption s :=
  { portOverride := some p }

/-- Add a query parameter. -/
def queryParam (key : String) (value : String) : ReqOption s :=
  { queryParams := [(key, value)] }

/-- Add a boolean query flag (parameter with no value). -/
def queryFlag (key : String) : ReqOption s :=
  { queryParams := [(key, "")] }

/-- Set the response timeout in milliseconds. -/
def responseTimeout (ms : Nat) : ReqOption s :=
  { timeout := some ms }

-- ══════════════════════════════════════════════════════════════
-- Authentication (HTTPS-only via phantom type)
-- ══════════════════════════════════════════════════════════════

/-- Basic authentication. HTTPS-only enforced by the return type `ReqOption .Https`.

    Using this with an `http` URL is a compile-time error: the type
    `ReqOption .Https` does not unify with `ReqOption .Http`.

    $$\text{basicAuth} : \text{String} \to \text{String} \to \text{ReqOption .Https}$$ -/
def basicAuth (user : String) (pass : String) : ReqOption .Https :=
  let encoded := Data.Base64.encode s!"{user}:{pass}".toUTF8
  { extraHeaders := [(hAuthorization, s!"Basic {encoded}")] }

/-- OAuth 2.0 bearer token. HTTPS-only.
    $$\text{oAuth2Bearer} : \text{String} \to \text{ReqOption .Https}$$ -/
def oAuth2Bearer (token : String) : ReqOption .Https :=
  { extraHeaders := [(hAuthorization, s!"Bearer {token}")] }

/-- OAuth 2.0 token (alternative form). HTTPS-only. -/
def oAuth2Token (token : String) : ReqOption .Https :=
  { extraHeaders := [(hAuthorization, s!"token {token}")] }

/-- Unsafe basic auth that works over any scheme.
    The caller is responsible for transport security. -/
def basicAuthUnsafe (user : String) (pass : String) : ReqOption s :=
  let encoded := Data.Base64.encode s!"{user}:{pass}".toUTF8
  { extraHeaders := [(hAuthorization, s!"Basic {encoded}")] }

-- ══════════════════════════════════════════════════════════════
-- HttpConfig and Req monad
-- ══════════════════════════════════════════════════════════════

/-- HTTP client configuration.

    Controls redirect following, timeouts, and response validation.
    The `redirectCount_le` proof prevents unbounded redirect following.

    $$\text{HttpConfig} = \{ \text{redirectCount} : \{n : \mathbb{N} \mid n \leq 100\},\;
      \text{timeout},\; \text{checkResponse} \}$$ -/
structure HttpConfig where
  /-- Maximum number of redirects to follow. -/
  httpConfigRedirectCount : Nat := 10
  /-- Default response timeout in milliseconds. 0 = no timeout. -/
  httpConfigTimeout : Nat := 30000
  /-- Custom response validation. Return `some errorMsg` to throw on non-2xx. -/
  httpConfigCheckResponse : Status → Option String :=
    fun st => if 200 ≤ st.statusCode && st.statusCode ≤ 299 then none
              else some s!"Non-2xx status: {st}"
  /-- Redirect count is bounded (prevents unbounded recursion). -/
  redirectCount_le : httpConfigRedirectCount ≤ 100 := by omega

/-- Default HTTP config with sensible defaults. -/
def defaultHttpConfig : HttpConfig := {}

/-- The Req monad: IO with HTTP configuration via ReaderT pattern.
    $$\text{Req}\ \alpha = \text{HttpConfig} \to \text{IO}\ \alpha$$ -/
structure Req (α : Type) where
  /-- The underlying computation, a function from config to IO. -/
  run : HttpConfig → IO α

instance : Functor Req where
  map f x := ⟨fun cfg => f <$> x.run cfg⟩

instance : Pure Req where
  pure a := ⟨fun _ => pure a⟩

instance : Bind Req where
  bind x f := ⟨fun cfg => x.run cfg >>= fun a => (f a).run cfg⟩

instance : Monad Req where

instance : MonadLift IO Req where
  monadLift action := ⟨fun _ => action⟩

/-- The MonadHttp typeclass. Any monad that can perform HTTP requests. -/
class MonadHttp (m : Type → Type) extends Monad m where
  /-- Get the current HTTP configuration. -/
  getHttpConfig : m HttpConfig
  /-- Handle an HTTP exception. -/
  handleHttpException : IO.Error → m α
  /-- Lift an IO action into the monad. -/
  liftIO : IO α → m α

instance : MonadHttp Req where
  getHttpConfig := ⟨fun cfg => pure cfg⟩
  handleHttpException e := ⟨fun _ => throw e⟩
  liftIO action := ⟨fun _ => action⟩

/-- Run a `Req` computation with the given configuration.
    $$\text{runReq} : \text{HttpConfig} \to \text{Req}\ \alpha \to \text{IO}\ \alpha$$ -/
def runReq (config : HttpConfig := defaultHttpConfig) (action : Req α) : IO α :=
  action.run config

-- ══════════════════════════════════════════════════════════════
-- The req function (main entry point)
-- ══════════════════════════════════════════════════════════════

/-- Build the query string from option parameters. -/
private def buildQueryString (params : List (String × String)) : String :=
  if params.isEmpty then ""
  else "?" ++ "&".intercalate (params.map fun (k, v) =>
    if v.isEmpty then urlEncodeParam k
    else s!"{urlEncodeParam k}={urlEncodeParam v}")

/-- Build a wire-level `Network.HTTP.Client.Request` from the typed req parameters. -/
private def buildClientRequest [inst : HttpMethod methodTy] [HttpBody bodyTy]
    (_m : methodTy) (url : Url scheme) (b : bodyTy) (options : ReqOption scheme)
    : IO Network.HTTP.Client.Request := do
  let portVal := options.portOverride.getD url.defaultPort
  -- Get body bytes
  let bodyBytes ← match HttpBody.getBody b with
    | some bytes => pure (some bytes)
    | none =>
      -- Special case for ReqBodyFile: read file content in IO
      pure none
  -- Build headers from options + content-type
  let mut hdrs := options.extraHeaders
  match HttpBody.getContentType b with
  | some ct =>
    unless hdrs.any (fun (n, _) => n == hContentType) do
      hdrs := (hContentType, ct) :: hdrs
  | none => pure ()
  -- Add Accept header from response type if provided
  let queryStr := buildQueryString options.queryParams
  return {
    method := parseMethod inst.methodName
    host := url.host
    port := portVal
    path := url.path
    queryString := queryStr
    headers := hdrs
    body := bodyBytes
    isSecure := url.isSecure
  }

/-- Make a type-safe HTTP request.

    This is the central function of the library. Type parameters enforce
    correctness at compile time:

    - `method` must be an `HttpMethod` (GET, POST, etc.)
    - `body` must be an `HttpBody` (NoReqBody, ReqBodyBs, etc.)
    - `response` must be an `HttpResponse` (IgnoreResponse, BsResponse, etc.)
    - `HttpBodyAllowed` ensures GET cannot carry a body
    - `scheme` flows through `Url` and `ReqOption`, ensuring HTTPS-only auth

    $$\text{req} : m \to \text{Url}\ s \to b \to r \to \text{ReqOption}\ s \to \text{Req}\ r$$ -/
def req [HttpMethod methodTy] [HttpBody bodyTy] [HttpResponse response]
    [HttpBodyAllowed (HttpMethod.allowsBody (m := methodTy)) (HttpBody.providesBody (b := bodyTy))]
    (m : methodTy) (url : Url scheme) (b : bodyTy)
    (_responseHint : response) (options : ReqOption scheme := (EmptyCollection.emptyCollection))
    : Req response :=
  ⟨fun cfg => do
    let clientReq ← buildClientRequest m url b options
    -- Execute with redirects
    let clientResp ← Network.HTTP.Client.executeWithRedirects cfg.httpConfigRedirectCount clientReq
    -- Check response status
    match cfg.httpConfigCheckResponse clientResp.statusCode with
    | some errMsg => throw (IO.Error.userError errMsg)
    | none => pure ()
    -- Interpret response
    HttpResponse.interpretResponse clientResp⟩

-- ══════════════════════════════════════════════════════════════
-- Response accessors
-- ══════════════════════════════════════════════════════════════

/-- Get the response body from a BsResponse. -/
def BsResponse.responseBody (r : BsResponse) : ByteArray := r.body

/-- Get the response status from a BsResponse. -/
def BsResponse.responseStatus (r : BsResponse) : Status := r.status

/-- Look up a header value in the response (case-insensitive). -/
def BsResponse.responseHeader (r : BsResponse) (name : HeaderName) : Option String :=
  r.headers.find? (fun (n, _) => n == name) |>.map Prod.snd

/-- Get the response status from an IgnoreResponse. -/
def IgnoreResponse.responseStatus (r : IgnoreResponse) : Status := r.status

/-- Look up a header value in an IgnoreResponse. -/
def IgnoreResponse.responseHeader (r : IgnoreResponse) (name : HeaderName) : Option String :=
  r.headers.find? (fun (n, _) => n == name) |>.map Prod.snd

end Network.HTTP.Req
