/-
  Linen.Network.WebApp.Internal — Web Application Interface internal types

  Core WAI-style types: Request, Response, Application, Middleware, AppM.

  ## Design

  Ports `Network.Wai.Internal` (itself mirroring Haskell's
  `Network.Wai.Internal`), renamed from the Haskell-specific acronym `WAI`
  to `WebApp` per this project's naming convention of adapting identifiers
  to read as idiomatic Lean rather than mirroring the source project's
  naming. The `Request` type contains all parsed HTTP request information.
  `Response` is an inductive type covering file, builder, and streaming
  response modes.

  ## Response lifecycle

  `AppM` uses the shared indexed resource program over `Green`. The
  response action moves from pending to sent. Servers interpret the program,
  and middleware that implements its own synchronization has an explicit
  escape hatch.
-/

import Linen.Network.HTTP.Types.Header
import Linen.Network.HTTP.Types.Method
import Linen.Network.HTTP.Types.Status
import Linen.Network.HTTP.Types.Version
import Linen.Network.HTTP.Types.URI
import Linen.Data.Vault
import Linen.Network.Socket.Types
import Linen.Network.Sendfile
import Linen.Control.Concurrent.Green
import Linen.Control.Resource

namespace Network.WebApp

open Network.HTTP.Types
open Network.Socket (SockAddr)
open Network.Sendfile (FilePart)
open Control.Concurrent.Green (Green)

/-- Completion token returned by a server's response callback. The
`AppM` state tracks the order of the indexed actions. -/
structure ResponseReceived where
  private mk ::

/-- Construct a `ResponseReceived` token. Intended for use by server
    implementations that provide the response callback. Application code
    should not call this directly. -/
def ResponseReceived.done : ResponseReceived := ⟨⟩

/-- Body streaming callback type.
    $$\text{StreamingBody} = (\text{ByteArray} \to \text{IO}()) \to \text{IO}() \to \text{IO}()$$ -/
abbrev StreamingBody := (ByteArray → IO Unit) → IO Unit → IO Unit

/-- The size of the request body.
    In the case of chunked transfer encoding, the size is unknown.
    $$\text{RequestBodyLength} = \text{ChunkedBody} \mid \text{KnownLength}\ \mathbb{N}$$ -/
inductive RequestBodyLength where
  /-- Chunked transfer encoding — size unknown. -/
  | chunkedBody
  /-- Content-Length header present — size known.
      $$\text{KnownLength}\ n,\; n : \mathbb{N}$$ -/
  | knownLength (bytes : Nat)
deriving BEq, Repr

/-- An HTTP request with all parsed information.
    $$\text{Request} = \{ \text{method}, \text{version}, \text{path}, \text{query}, \text{headers}, \ldots \}$$ -/
structure Request where
  /-- The HTTP method (GET, POST, etc.). -/
  requestMethod : Method
  /-- The HTTP version. -/
  httpVersion : HttpVersion
  /-- The raw path info (e.g., "/foo/bar").
      Middlewares should not modify this — modify `pathInfo` instead. -/
  rawPathInfo : String
  /-- The raw query string (e.g., "?key=val"), including leading '?'.
      Do not modify this raw value — modify `queryString` instead. -/
  rawQueryString : String
  /-- The request headers. -/
  requestHeaders : RequestHeaders
  /-- Whether the current connection is secure (HTTPS/TLS).
      Note: does not reflect whether the original client connection was secure
      (e.g., behind a reverse proxy). -/
  isSecure : Bool
  /-- The remote client address. -/
  remoteHost : SockAddr
  /-- Parsed path segments (e.g., ["foo", "bar"]). -/
  pathInfo : List String
  /-- Parsed query string. -/
  queryString : Query
  /-- IO action to read the next chunk of the request body.
      Returns empty ByteArray when body is exhausted.
      Each call consumes a chunk — this is not idempotent. -/
  requestBody : IO ByteArray
  /-- Per-request extensible storage. -/
  vault : Data.Vault
  /-- The size of the request body — chunked or known length. -/
  requestBodyLength : RequestBodyLength
  /-- The Host header value. -/
  requestHeaderHost : Option String
  /-- The Range header value. -/
  requestHeaderRange : Option String
  /-- The Referer header value. -/
  requestHeaderReferer : Option String
  /-- The User-Agent header value. -/
  requestHeaderUserAgent : Option String

/-- An HTTP response. -/
inductive Response where
  /-- Respond with a file. -/
  | responseFile (status : Status) (headers : ResponseHeaders)
      (path : String) (part : Option FilePart)
  /-- Respond with a ByteArray body (built in memory). -/
  | responseBuilder (status : Status) (headers : ResponseHeaders)
      (body : ByteArray)
  /-- Respond with a streaming body. -/
  | responseStream (status : Status) (headers : ResponseHeaders)
      (body : StreamingBody)
  /-- Respond with raw data sent directly to the socket. -/
  | responseRaw (rawAction : (IO ByteArray) → (ByteArray → IO Unit) → IO Unit)
      (fallback : Response)

namespace Response

/-- Get the status from a response. -/
def status : Response → Status
  | .responseFile s _ _ _ => s
  | .responseBuilder s _ _ => s
  | .responseStream s _ _ => s
  | .responseRaw _ fb => fb.status

/-- Get the headers from a response. -/
def headers : Response → ResponseHeaders
  | .responseFile _ h _ _ => h
  | .responseBuilder _ h _ => h
  | .responseStream _ h _ => h
  | .responseRaw _ fb => fb.headers

/-- Map over the response headers. -/
def mapResponseHeaders (f : ResponseHeaders → ResponseHeaders) : Response → Response
  | .responseFile s h p fp => .responseFile s (f h) p fp
  | .responseBuilder s h b => .responseBuilder s (f h) b
  | .responseStream s h b => .responseStream s (f h) b
  | .responseRaw a fb => .responseRaw a (fb.mapResponseHeaders f)

/-- Map over the response status. -/
def mapResponseStatus (f : Status → Status) : Response → Response
  | .responseFile s h p fp => .responseFile (f s) h p fp
  | .responseBuilder s h b => .responseBuilder (f s) h b
  | .responseStream s h b => .responseStream (f s) h b
  | .responseRaw a fb => .responseRaw a (fb.mapResponseStatus f)

/-- Whether a response has an empty body.
    Files and streams are conservatively assumed non-empty;
    raw responses are opaque. -/
def bodyIsEmpty : Response → Bool
  | .responseBuilder _ _ body => body.isEmpty
  | .responseFile _ _ _ _     => false
  | .responseStream _ _ _     => false
  | .responseRaw _ _          => false

-- ── Response accessor laws ──

/-- Status accessor returns the status of a builder response. -/
theorem status_responseBuilder (s : Status) (h : ResponseHeaders) (b : ByteArray) :
    (Response.responseBuilder s h b).status = s := rfl

/-- Status accessor returns the status of a file response. -/
theorem status_responseFile (s : Status) (h : ResponseHeaders) (p : String) (fp : Option FilePart) :
    (Response.responseFile s h p fp).status = s := rfl

/-- Status accessor returns the status of a stream response. -/
theorem status_responseStream (s : Status) (h : ResponseHeaders) (b : StreamingBody) :
    (Response.responseStream s h b).status = s := rfl

/-- Headers accessor returns the headers of a builder response. -/
theorem headers_responseBuilder (s : Status) (h : ResponseHeaders) (b : ByteArray) :
    (Response.responseBuilder s h b).headers = h := rfl

/-- Headers accessor returns the headers of a file response. -/
theorem headers_responseFile (s : Status) (h : ResponseHeaders) (p : String) (fp : Option FilePart) :
    (Response.responseFile s h p fp).headers = h := rfl

/-- `mapResponseHeaders id` is identity for builder responses. -/
theorem mapResponseHeaders_id_responseBuilder (s : Status) (h : ResponseHeaders) (b : ByteArray) :
    (Response.responseBuilder s h b).mapResponseHeaders id = .responseBuilder s h b := rfl

/-- `mapResponseHeaders id` is identity for file responses. -/
theorem mapResponseHeaders_id_responseFile (s : Status) (h : ResponseHeaders) (p : String) (fp : Option FilePart) :
    (Response.responseFile s h p fp).mapResponseHeaders id = .responseFile s h p fp := rfl

/-- `mapResponseHeaders id` is identity for stream responses. -/
theorem mapResponseHeaders_id_responseStream (s : Status) (h : ResponseHeaders) (b : StreamingBody) :
    (Response.responseStream s h b).mapResponseHeaders id = .responseStream s h b := rfl

/-- `mapResponseStatus id` is identity for builder responses. -/
theorem mapResponseStatus_id_responseBuilder (s : Status) (h : ResponseHeaders) (b : ByteArray) :
    (Response.responseBuilder s h b).mapResponseStatus id = .responseBuilder s h b := rfl

/-- `mapResponseStatus id` is identity for file responses. -/
theorem mapResponseStatus_id_responseFile (s : Status) (h : ResponseHeaders) (p : String) (fp : Option FilePart) :
    (Response.responseFile s h p fp).mapResponseStatus id = .responseFile s h p fp := rfl

/-- `mapResponseStatus id` is identity for stream responses. -/
theorem mapResponseStatus_id_responseStream (s : Status) (h : ResponseHeaders) (b : StreamingBody) :
    (Response.responseStream s h b).mapResponseStatus id = .responseStream s h b := rfl

end Response

/-- State of the response as tracked by the indexed application program. -/
inductive ResponseState where
  /-- No response has been sent yet. -/
  | pending
  /-- A response has been sent. -/
  | sent
deriving BEq, DecidableEq, Repr

/-- Response states are distinct. -/
theorem ResponseState.pending_ne_sent : ResponseState.pending ≠ ResponseState.sent := by decide

private opaque responseAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- A Green computation indexed by the response lifecycle. A successful
application starts pending and ends sent. -/
abbrev AppM := Control.Resource.ActionT Green responseAccess.val (fun _ : ResponseState => Unit)

namespace AppM

open Control.Resource (ProgramT)

/-- Interpret an application action. Intended for servers and middleware. -/
@[inline] def run (action : AppM pre post α) : Green α :=
  (·.1) <$> ProgramT.run responseAccess.property action ()

/-- Lift a value without changing response state. -/
@[inline] def ipure (a : α) : AppM s s α := ProgramT.pure a

/-- Lift an IO action without changing response state. -/
@[inline] def liftIO (action : IO α) : AppM s s α := ProgramT.lift (action : Green α)

/-- Sequence actions whose response states agree at the boundary. -/
@[inline] def ibind (first : AppM pre mid α) (next : α → AppM mid post β) : AppM pre post β :=
  ProgramT.ibind first next

/-- Send through the supplied callback and mark the response sent on success. -/
@[inline] def respond (callback : Response → Green ResponseReceived) (resp : Response) :
    AppM .pending .sent ResponseReceived :=
  ProgramT.step responseAccess.property fun () => do return ⟨← callback resp, ()⟩

/-- Compute a response in IO, then send it through the supplied callback. -/
@[inline] def respondIO (callback : Response → Green ResponseReceived) (action : IO Response) :
    AppM .pending .sent ResponseReceived :=
  ibind (liftIO action) (respond callback)

/-- Perform IO, then choose a continuation. -/
@[inline] def ioThen (action : IO α) (next : α → AppM pre post β) : AppM pre post β :=
  ibind (liftIO action) next

/-- Escape hatch for middleware that implements its own response synchronization.
The caller must enforce the response protocol. -/
protected def unsafeLift (action : IO α) : AppM pre post α :=
  ProgramT.step responseAccess.property fun () => do return ⟨← (action : Green α), ()⟩

end AppM

/-- IO actions lift into response state preserving code automatically. -/
instance : MonadLiftT IO (AppM s s) where
  monadLift := AppM.liftIO

/-- A web application returns a program that consumes a pending response.
The supplied callback and the framework escape hatch are the trust boundary. -/
abbrev Application := Request → (Response → Green ResponseReceived) → AppM .pending .sent ResponseReceived

/-- A web-application middleware transforms an application.
    $$\text{Middleware} = \text{Application} \to \text{Application}$$ -/
abbrev Middleware := Application → Application

end Network.WebApp
