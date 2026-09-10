/-
  `Cloud.Transport` — the one place cloud requests go out

  ## A record, so the network is swappable

  `Transport` is a record with a `send` field rather than a direct call to the
  HTTP client. That single indirection buys three things:

  - **Tests with no network.** A stub transport answers from a table, so every
    protocol dialect and every service client is exercised end to end in
    `lake build Tests`, with no credentials and no sockets. This is the same
    seam `Control.Monad.Effect.HTTP`'s `runHTTPWith` opens for the same reason.
  - **A place for policy.** Retries, timeouts, logging and request recording
    are decorators on a `Transport`, not conditionals inside every client.
  - **Local backends.** A transport pointed at a container — MinIO, ElasticMQ,
    LocalStack — is just a different `Endpoint` and the same code path.

  ## Signing and sending must agree

  A signature covers the request as sent, so anything that rewrites a request
  after signing invalidates it. Two consequences, both learned the hard way:

  - The query string is rendered **once**, by `canonicalQuery`, and the same
    rendering is both signed and sent.
  - `Host` comes back from `Auth.headersAt` rather than being added by the HTTP
    client, because SigV4 signs it.

  A presigned URL inverts this: its signature covers a query string *somebody
  else* encoded and ordered, so re-rendering it is fatal. `performPresigned`
  exists for that one case and passes the query through untouched.

  ## Failure is a value

  `perform` answers `Except Error Response` and never raises. A non-2xx becomes
  a classified `Error` carrying the provider's own code; a socket failure
  becomes `Class.transport`. Callers therefore handle "the object is not there"
  the same way they handle everything else, which is the point of
  `Cloud.Error`.

  ## Provenance

  Moved down from the sibling `typednotes/infra`
  (`Infra/Providers/Http.lean`), which now uses this instead. The change on the
  way in is that the transport is a parameter rather than a fixed call, and
  that errors are returned rather than thrown.
-/
import Linen.Cloud.Auth
import Linen.Cloud.Error
import Linen.Network.HTTP.Client.Retry
import Linen.Network.HTTP.Types.URI
import Linen.Data.CaseInsensitive

namespace Cloud

open Network.HTTP.Client (Request Response RetryPolicy executeWithRetry)
open Network.HTTP.Types (Query canonicalQuery parseMethod)

-- ── Policy ──────────────────────────────────────────────────────────────────

/-- How cloud calls behave under failure.

    More patient than `linen`'s default, because a cloud service throttling a
    burst is normal rather than exceptional, and because `Retry-After` is
    honoured when one is sent. -/
def retryPolicy : RetryPolicy :=
  { maxAttempts := 5, baseDelayMillis := 200, maxDelayMillis := 20000 }

/-- How long any single socket read or write may block. -/
def timeoutMillis : Nat := 30000

-- ── The transport ───────────────────────────────────────────────────────────

/-- How a request actually reaches the network.

    A record so it can be replaced — by a stub in tests, by a recorder while
    debugging, by a transport pointed at a local container. See the module
    header. -/
structure Transport where
  /-- Send one request and return its response. May raise; `perform` catches. -/
  send : Request → IO Response

/-- The real network, with the retry policy above. -/
def Transport.network : Transport :=
  { send := fun req => executeWithRetry retryPolicy req }

/-- The network with no retries. For a caller doing its own scheduling, and
    for keeping a test's failure fast. -/
def Transport.networkNoRetry : Transport :=
  { send := fun req => executeWithRetry Network.HTTP.Client.noRetry req }

/-- A transport that answers every request from `respond`, without a socket.

    The debugging and testing seam. `respond` sees the fully-built request —
    signed, with its query rendered — so a stub can assert on exactly what
    would have gone out. -/
def Transport.stub (respond : Request → IO Response) : Transport := { send := respond }

/-- Wrap a transport so that every request is recorded before being sent.

    For debugging a client against a real provider: the log holds
    `METHOD host path?query` for each call, in order. -/
def Transport.recording (log : IO.Ref (List String)) (inner : Transport) : Transport :=
  { send := fun req => do
      log.modify (fun l => l ++ [s!"{req.method} {req.host}{req.path}{req.queryString}"])
      inner.send req }

-- ── A call ──────────────────────────────────────────────────────────────────

/-- One cloud API call, before it is signed.

    Everything a signature covers, plus where to send it. Protocol dialects
    build these; `perform` signs and sends them. -/
structure Call where
  /-- The HTTP method, uppercase. -/
  method   : String
  /-- Where it goes, and what to sign it as. -/
  endpoint : Endpoint
  /-- The path, **unencoded**, beginning with `/`.

      Raw rather than pre-encoded because the signature and the wire want
      different encodings of the same path — see `Call.wirePath`. Pre-encoding
      here would make the signer encode the `%` signs a second time. -/
  path     : String := "/"
  /-- Query parameters, unencoded. Rendered once by `canonicalQuery` and both
      signed and sent in that form. -/
  query    : Query := []
  /-- Headers the caller wants sent. The signature covers them. -/
  headers  : List (String × String) := []
  /-- The request body. -/
  body     : ByteArray := ByteArray.empty
  /-- How to authenticate. -/
  auth     : Auth
  /-- S3 signs the path as sent; every other AWS service double-encodes it. -/
  doubleEncodePath : Bool := false
  /-- Send `UNSIGNED-PAYLOAD` instead of hashing the body. S3 only, and for
      large uploads where hashing twice is the dominant cost. -/
  unsignedBody : Bool := false
  /-- Send `path` exactly as given, without percent-encoding it.

      For the one case the structural encoding cannot express: a path segment
      that itself contains a `/`. Google Cloud Storage addresses an object by
      putting its whole name in a single segment, so `logs/a.json` must reach
      the wire as `logs%2Fa.json` — and `wirePath`, which treats `/` as a
      separator, would leave it as two segments naming a different object.

      **Only valid with a non-signing scheme.** A SigV4 signature is computed
      over a canonical URI derived from the unencoded path, so a pre-encoded
      one would sign `%2F` as `%252F` and be rejected. `toRequestAt` refuses
      the combination rather than producing a request that fails obscurely. -/
  pathPreEncoded : Bool := false

/-- The rendered query string, without the leading `?`. Rendered once here so
    that what is signed and what is sent cannot diverge. -/
def Call.queryString (c : Call) : String := canonicalQuery c.query

/-- The percent-encoded path to put in the request line.

    **Always single-encoded**, even when the signature double-encodes it. That
    is not an inconsistency but AWS's actual rule: for every service except S3
    the *canonical request* encodes the path twice, while the wire carries it
    once. Signing a double-encoded canonical URI and sending a single-encoded
    path is what the service expects; sending what was signed would 404.

    Because `Call.path` is unencoded, both encodings are derived from one value
    and cannot disagree — the same "the wire is derived from what was checked"
    property `Effect.PostgreSQL`'s rendered SQL has. A key containing a space
    or a `#` therefore signs and sends correctly without the caller doing
    anything. -/
def Call.wirePath (c : Call) : String :=
  if c.pathPreEncoded then c.path else Crypto.SigV4.canonicalUri c.path false

/-- Whether this call combines a pre-encoded path with a signing scheme, which
    cannot work — see `Call.pathPreEncoded`. -/
def Call.pathEncodingConflicts (c : Call) : Bool :=
  c.pathPreEncoded && (match c.auth with | .sigV4 _ _ _ => true | _ => false)

/-- Sign a call and build the HTTP request, at a given time.

    The time is a parameter so a test can pin a signature; `toRequest` reads
    the clock. -/
def Call.toRequestAt (c : Call) (now : Data.Time.UTCTime) : IO Request := do
  if c.pathEncodingConflicts then
    throw (IO.userError
      "Cloud.Call: pathPreEncoded cannot be combined with SigV4 signing; the \
signature covers the unencoded path (see Call.pathPreEncoded)")
  let authHeaders ← c.auth.headersAt now c.endpoint.host c.method c.path c.query
    c.headers c.body c.doubleEncodePath c.unsignedBody
  let rendered := c.queryString
  return {
      method := parseMethod c.method
    , host := c.endpoint.host
    , port := 443
    , path := c.wirePath
    , queryString := if rendered.isEmpty then "" else "?" ++ rendered
    , headers := (c.headers ++ authHeaders).map fun (n, v) => (Data.CI.mk' n, v)
    , body := if c.body.isEmpty then none else some c.body
    , isSecure := true
    , timeoutMillis := timeoutMillis }

/-- Sign a call against the current wall clock and build the HTTP request. -/
def Call.toRequest (c : Call) : IO Request := do
  c.toRequestAt (← Data.Time.getCurrentTime)

-- ── Reading responses ───────────────────────────────────────────────────────

/-- A response body as text, or a `protocol` error if it is not UTF-8.

    `String.fromUTF8?` rather than the panicking `!`: a provider sending
    something unexpected is a value the caller handles, not a crash. -/
def bodyText (resp : Response) : Except Error String :=
  match String.fromUTF8? resp.body with
  | some s => .ok s
  | none   => .error (Error.protocol "response body is not valid UTF-8")

/-- The body as text, or the empty string if it is not UTF-8.

    For building an *error* message, where refusing to decode would discard the
    only diagnostic available. Never for reading a result. -/
def bodyTextLossy (resp : Response) : String :=
  (String.fromUTF8? resp.body).getD ""

/-- The HTTP status as a number. -/
def statusOf (resp : Response) : Nat := resp.statusCode.statusCode

/-- Whether the status is 2xx. -/
def isSuccess (resp : Response) : Bool :=
  let s := statusOf resp
  200 ≤ s && s ≤ 299

-- ── Performing a call ───────────────────────────────────────────────────────

/-- Send a call and require a 2xx, at a given time.

    Never raises. A non-2xx becomes a classified `Error` carrying the
    provider's own code and message; a socket failure becomes
    `Class.transport` with the exception's text. -/
def perform (t : Transport) (c : Call) (now : Data.Time.UTCTime) :
    IO (Except Error Response) := do
  if !c.auth.usable then
    return .error {
        klass := .denied
      , message := s!"credentials for {c.auth.scheme} are incomplete" }
  if c.pathEncodingConflicts then
    return .error {
        klass := .invalid
      , message := "a pre-encoded path cannot be SigV4-signed (see Call.pathPreEncoded)" }
  let resp ← try
      let req ← c.toRequestAt now
      pure (Except.ok (← t.send req))
    catch e => pure (Except.error (Error.transport (toString e)))
  match resp with
  | .error e => return .error e
  | .ok resp =>
    if isSuccess resp then return .ok resp
    else return .error (describeError (statusOf resp) (bodyTextLossy resp))

/-- `perform` against the current wall clock. -/
def performNow (t : Transport) (c : Call) : IO (Except Error Response) := do
  perform t c (← Data.Time.getCurrentTime)

/-- Send a call and return the response whatever its status.

    For the handful of operations where a specific non-2xx is the expected
    answer and the caller wants to see it rather than a classified error. -/
def performRaw (t : Transport) (c : Call) (now : Data.Time.UTCTime) :
    IO (Except Error Response) := do
  try
    let req ← c.toRequestAt now
    return .ok (← t.send req)
  catch e => return .error (Error.transport (toString e))

/-- Send a request to a URL somebody else signed.

    For a **presigned** URL, and only for that. `Call.queryString` renders the
    query through `canonicalQuery`, which percent-encodes each component and
    sorts the parameters — correct when this library is the signer, and fatal
    when it is not. A presigned URL's signature covers the exact encoded string
    in the exact order it arrived, so re-encoding or reordering it produces a
    request the issuer refuses, with an error about the signature that says
    nothing about why.

    `queryString` is passed without the leading `?`. -/
def performPresigned (t : Transport) (method host path queryString : String)
    (headers : List (String × String) := []) (body : ByteArray := ByteArray.empty) :
    IO (Except Error Response) := do
  let req : Request :=
    { method := parseMethod method
    , host, path
    , port := 443
    , queryString := if queryString.isEmpty then "" else "?" ++ queryString
    , headers := headers.map fun (n, v) => (Data.CI.mk' n, v)
    , body := if body.isEmpty then none else some body
    , isSecure := true
    , timeoutMillis := timeoutMillis }
  try
    let resp ← t.send req
    if isSuccess resp then return .ok resp
    else return .error (describeError (statusOf resp) (bodyTextLossy resp))
  catch e => return .error (Error.transport (toString e))

/-- Treat a `notFound` as an absence rather than a failure.

    The idiom for "read it if it is there", which is the most common thing a
    caller wants and the reason `Class.notFound` exists as a class. Every other
    error still propagates, so a permissions problem is never silently a
    `none`.

    Not called `optional`: that name is taken by the `Alternative` combinator in
    the root namespace, and the two would be ambiguous at every call site. -/
def absentAsNone {α : Type} (r : Except Error α) : Except Error (Option α) :=
  match r with
  | .ok a    => .ok (some a)
  | .error e => if e.klass == .notFound then .ok none else .error e

end Cloud
