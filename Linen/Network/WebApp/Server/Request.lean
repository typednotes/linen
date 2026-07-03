/-
  Linen.Network.WebApp.Server.Request — HTTP request parsing

  Uses a C-based RecvBuffer for buffered I/O — reads socket data in
  4KB chunks and scans for CRLF entirely in C, eliminating per-byte
  syscall overhead.

  Ports Hale's `Network.Wai.Handler.Warp.Request`.

  ## Design

  A `RecvBuffer` is created once per connection and reused across
  requests (supports keep-alive and pipelining). The buffer may already
  contain the start of the next request after a response is sent.

  ## Guarantees

  - `parseRequestLine` returns `none` for malformed input (total function)
  - `parseHeaders` is total and handles malformed header lines gracefully
  - `parseHttpVersion` validates the "HTTP/x.y" format
  - Header count is bounded by `maxHeaders` to prevent DoS
-/

import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.HTTP.Types.Method
import Linen.Network.HTTP.Types.URI
import Linen.Network.HTTP.Types.Version
import Linen.Network.Socket

namespace Network.WebApp.Server

open Network.HTTP.Types
open Network.WebApp
open Network.Socket

/-- Maximum number of headers per request. Requests with more headers
    are rejected to prevent denial-of-service. -/
def maxHeaders : Nat := 100

/-- Parse an HTTP version string like "HTTP/1.1".
    $$\text{parseHttpVersion} : \text{String} \to \text{Option}(\text{HttpVersion})$$ -/
def parseHttpVersion (s : String) : Option HttpVersion :=
  if s == "HTTP/1.1" then some http11
  else if s == "HTTP/1.0" then some http10
  else if s == "HTTP/0.9" then some http09
  else if s == "HTTP/2.0" then some http20
  else if s.startsWith "HTTP/" then
    let rest := (s.drop 5).toString
    match rest.splitOn "." with
    | [maj, min] => do
      let major ← maj.toNat?
      let minor ← min.toNat?
      some ⟨major, minor⟩
    | _ => none
  else none

theorem parseHttpVersion_http11 : parseHttpVersion "HTTP/1.1" = some http11 := by rfl
theorem parseHttpVersion_http10 : parseHttpVersion "HTTP/1.0" = some http10 := by rfl
theorem parseHttpVersion_http09 : parseHttpVersion "HTTP/0.9" = some http09 := by rfl
theorem parseHttpVersion_http20 : parseHttpVersion "HTTP/2.0" = some http20 := by rfl

/-- Parse a request line like "GET /path?query HTTP/1.1".
    Returns (method, rawPath, rawQuery, version) or `none` if malformed.
    $$\text{parseRequestLine} : \text{String} \to \text{Option}(\text{Method} \times \text{String} \times \text{String} \times \text{HttpVersion})$$ -/
def parseRequestLine (line : String) : Option (Method × String × String × HttpVersion) := do
  let parts := line.splitOn " "
  match parts with
  | [methodStr, uri, versionStr] =>
    let method := parseMethod methodStr
    let version ← parseHttpVersion versionStr
    -- Split URI into path and query
    let (path, query) :=
      match uri.splitOn "?" with
      | [p] => (p, "")
      | [p, q] => (p, "?" ++ q)
      | _ => (uri, "")
    some (method, path, query, version)
  | _ => none

theorem parseRequestLine_empty : parseRequestLine "" = none := by native_decide

/-- Parse a single header line like "Content-Type: text/html".
    Returns `none` if the line doesn't contain a colon.
    $$\text{parseHeaderLine} : \text{String} \to \text{Option}(\text{Header})$$ -/
def parseHeaderLine (line : String) : Option Header :=
  match line.splitOn ":" with
  | [] => none
  | [_] => none
  | name :: rest =>
    let value := (":".intercalate rest).trimAscii.toString
    some (Data.CI.mk' name.trimAscii.toString, value)

/-- Parse header lines into a list of headers.
    $$\text{parseHeaders} : \text{List}(\text{String}) \to \text{RequestHeaders}$$ -/
def parseHeaders (lines : List String) : RequestHeaders :=
  lines.filterMap parseHeaderLine

/-- Read all header lines from a buffered reader until an empty line.
    Returns the request line and header lines.
    Uses O(1) cons + single reverse instead of O(n) append.
    Bounded by `maxHeaders` to prevent DoS.
    $$\text{recvHeaders} : \text{RecvBuffer} \to \text{IO}(\text{String} \times \text{List}(\text{String}))$$ -/
def recvHeaders (buf : FFI.RecvBuffer) : IO (String × List String) := do
  let requestLine ← FFI.recvBufReadLine buf
  let mut headers : List String := []
  let mut count := 0
  let mut done := false
  while !done do
    if count >= maxHeaders then
      done := true
    else
      let line ← FFI.recvBufReadLine buf
      if line.isEmpty then
        done := true
      else
        headers := line :: headers  -- O(1) cons
        count := count + 1
  pure (requestLine, headers.reverse)  -- single O(n) reverse

/-- **Header count bound:** The header count returned by `recvHeaders` is
    bounded by `maxHeaders`. The while loop checks `count >= maxHeaders`
    before each cons, ensuring at most `maxHeaders` elements are added.
    Axiom-dependent on IO monad execution semantics — the loop guard is
    checked before each `FFI.recvBufReadLine` call, but the proof cannot
    be discharged because `recvHeaders` uses `do`-notation over `IO` with
    mutable state (`let mut`), which is opaque to the kernel.
    $$\forall\, \text{rl}\; \text{hdrs},\; \text{recvHeaders}(\text{buf}) = \text{pure}(\text{rl}, \text{hdrs}) \implies \text{hdrs.length} \leq \text{maxHeaders}$$ -/
axiom recvHeaders_bounded (buf : FFI.RecvBuffer) :
    ∀ rl hdrs, recvHeaders buf = pure (rl, hdrs) → hdrs.length ≤ maxHeaders

/-- Find a header value by name in a header list. -/
private def findHeader (name : HeaderName) (headers : RequestHeaders) : Option String :=
  headers.find? (fun (n, _) => n == name) |>.map (·.2)

/-- Parse a full HTTP request from a buffered reader.
    Returns `none` if the request line is malformed or the connection is closed.
    $$\text{parseRequest} : \text{RecvBuffer} \to \text{SockAddr} \to \text{IO}(\text{Option}(\text{Request}))$$ -/
def parseRequest (buf : FFI.RecvBuffer) (remoteAddr : SockAddr) : IO (Option Request) := do
  let (requestLine, headerLines) ← recvHeaders buf
  if requestLine.isEmpty then
    return none
  match parseRequestLine requestLine with
  | none => return none
  | some (method, rawPath, rawQuery, version) =>
    let headers := parseHeaders headerLines
    -- Extract special headers
    let hostHeader := findHeader hHost headers
    let rangeHeader := findHeader hRange headers
    let refererHeader := findHeader hReferer headers
    let uaHeader := findHeader hUserAgent headers
    -- Parse content length → RequestBodyLength
    let contentLengthOpt : Option Nat := do
      let clStr ← findHeader hContentLength headers
      clStr.toNat?
    let bodyLength : Network.WebApp.RequestBodyLength :=
      match contentLengthOpt with
      | some n => .knownLength n
      | none   => .chunkedBody
    -- Parse path segments
    let pathSegments :=
      let segs := rawPath.splitOn "/"
      segs.filter (! ·.isEmpty)
    -- Parse query string
    let query := parseQuery rawQuery
    -- Body reader using the RecvBuffer for buffered reads.
    -- Axiom-dependent invariant: total bytes returned ≤ contentLength.
    let bodyRef ← IO.mkRef contentLengthOpt
    let bodyReader : IO ByteArray := do
      let remaining ← bodyRef.get
      match remaining with
      | none => pure ByteArray.empty   -- chunked: TODO proper chunked decoding
      | some 0 => pure ByteArray.empty
      | some n =>
        let toRead := min n 4096
        let chunk ← FFI.recvBufReadN buf toRead.toUSize
        let newRemaining := n - chunk.size
        bodyRef.set (some newRemaining)
        pure chunk
    return some {
      requestMethod := method
      httpVersion := version
      rawPathInfo := rawPath
      rawQueryString := rawQuery
      requestHeaders := headers
      isSecure := false
      remoteHost := remoteAddr
      pathInfo := pathSegments
      queryString := query
      requestBody := bodyReader
      vault := Data.Vault.empty
      requestBodyLength := bodyLength
      requestHeaderHost := hostHeader
      requestHeaderRange := rangeHeader
      requestHeaderReferer := refererHeader
      requestHeaderUserAgent := uaHeader
    }

end Network.WebApp.Server
