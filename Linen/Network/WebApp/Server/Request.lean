/-
  Linen.Network.WebApp.Server.Request — HTTP request parsing

  Uses a C-based RecvBuffer for buffered I/O — reads socket data in
  4KB chunks and scans for CRLF entirely in C, eliminating per-byte
  syscall overhead.

  Ports `Network.Wai.Handler.Warp.Request`.

  ## Design

  A `RecvBuffer` is created once per connection and reused across
  requests (supports keep-alive and pipelining). The buffer may already
  contain the start of the next request after a response is sent.

  ## Guarantees

  - `parseRequestLine` returns `none` for malformed input (total function)
  - `parseHeaders` is total and handles malformed header lines gracefully
  - `parseHttpVersion` validates the "HTTP/x.y" format
  - Header count is bounded by `maxHeaders` (in `HeaderLines`' type); a head
    with more is rejected, not truncated
  - Body framing follows RFC 9112 §6: `Content-Length`, or the chunked
    transfer coding (decoded by `chunkedBodyReader`), or no body. Ambiguous
    framing — the raw material of request smuggling — is rejected
    (`requestFraming`). Through 1.8.0 a chunked body was silently read as
    empty and its bytes were then parsed as the next request.
-/

import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.HTTP.Types.Method
import Linen.Network.HTTP.Types.URI
import Linen.Network.HTTP.Types.Version
import Linen.Network.Socket
import Linen.Data.Hex

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

-- ── Byte sources ──────────────────────────────────────────────────

/-- Where a request is read from: the two reads the parser needs. A plain
    socket uses the C `RecvBuffer` (`ByteSource.ofRecvBuffer`); a TLS session, or a
    test, uses a buffer over any chunk-receiving action (`ByteSource.buffered`). -/
structure ByteSource where
  /-- A CRLF-terminated line without its CRLF; the rest of the input when it
      ends without one; `""` at the end of input. -/
  readLine : IO String
  /-- Between `1` and `n` bytes; empty only at the end of input. -/
  readN : Nat → IO ByteArray

/-- The source over a socket's C-side `RecvBuffer`. -/
def ByteSource.ofRecvBuffer (buf : FFI.RecvBuffer) : ByteSource where
  readLine := FFI.recvBufReadLine buf
  readN n := FFI.recvBufReadN buf n.toUSize

/-- The longest line `ByteSource.buffered` accepts, matching the C `RecvBuffer`'s
    limit. -/
def maxLineBytes : Nat := 8192

/-- The first CRLF in `bytes` at or after `start`, by its index. -/
def findCRLF (bytes : ByteArray) (start : Nat) : Option Nat := Id.run do
  for i in [start:bytes.size] do
    if bytes.get! i == 13 && i + 1 < bytes.size && bytes.get! (i + 1) == 10 then
      return some i
  return none

/-- A buffered reader: a `ByteSource`, plus `readSome` for callers that take the
    connection over after the head (a WebSocket upgrade), which must see any
    bytes already buffered before new ones. -/
structure BufferedSource where
  source : ByteSource
  /-- Whatever is buffered, or else one `recv`; empty only at end of input. -/
  readSome : IO ByteArray
  /-- Append bytes received some other way — e.g. on a green thread, which
      is how the event-loop server buffers a request head without blocking a
      pool thread — so that reads see them next. -/
  feed : ByteArray → IO Unit
  /-- The bytes buffered and not yet read. -/
  unread : IO ByteArray

/-- Buffer `recv` — which returns the next bytes available, empty at end of
    input — into a `ByteSource`. A line longer than `maxLineBytes` is an error,
    as it is for the C `RecvBuffer`, so a peer cannot grow the buffer without
    bound by never sending CRLF. -/
def ByteSource.buffered (recv : IO ByteArray) : IO BufferedSource := do
  -- The buffer and the index of its first unread byte.
  let state ← IO.mkRef (ByteArray.empty, 0)
  -- Append one `recv` to the unread bytes; `false` at end of input.
  let fill : IO Bool := do
    let chunk ← recv
    if chunk.isEmpty then return false
    let (bytes, pos) ← state.get
    state.set (bytes.extract pos bytes.size ++ chunk, 0)
    return true
  let decode (bytes : ByteArray) : IO String :=
    match String.fromUTF8? bytes with
    | some line => pure line
    | none => throw (IO.userError "request head is not valid UTF-8")
  let readLine : IO String := do
    let mut scanned := 0  -- unread bytes already searched for a CRLF
    repeat
      let (bytes, pos) ← state.get
      match findCRLF bytes (pos + scanned) with
      | some i =>
        state.set (bytes, i + 2)
        return ← decode (bytes.extract pos i)
      | none =>
        let unread := bytes.size - pos
        if unread > maxLineBytes then
          throw (IO.userError s!"request line too long (>{maxLineBytes} bytes)")
        -- A CR at the very end may be completed by the next byte.
        scanned := unread - 1
        unless ← fill do
          state.set (bytes, bytes.size)
          return ← decode (bytes.extract pos bytes.size)
    return ""
  let readN (n : Nat) : IO ByteArray := do
    let (bytes, pos) ← state.get
    if pos == bytes.size then
      unless ← fill do return ByteArray.empty
    let (bytes, pos) ← state.get
    let stop := min bytes.size (pos + n)
    state.set (bytes, stop)
    return bytes.extract pos stop
  let readSome : IO ByteArray := do
    let (bytes, pos) ← state.get
    if pos == bytes.size then
      recv
    else
      state.set (ByteArray.empty, 0)
      return bytes.extract pos bytes.size
  let feed (chunk : ByteArray) : IO Unit := do
    let (bytes, pos) ← state.get
    state.set (bytes.extract pos bytes.size ++ chunk, 0)
  let unread : IO ByteArray := do
    let (bytes, pos) ← state.get
    return bytes.extract pos bytes.size
  return { source := { readLine, readN }, readSome, feed, unread }

-- ── Request head ──────────────────────────────────────────────────

/-- The most a request head may take, as bytes: every line the parser
    accepts at its longest. What the event-loop server buffers at most
    before giving up on a head. -/
def maxHeadBytes : Nat := (maxHeaders + 1) * (maxLineBytes + 2) + 2

/-- Whether `bytes` holds a complete request head — its blank line — or
    already more than any head the parser accepts (so waiting for more is
    pointless). Either way the parser can run without waiting for input. -/
def headComplete (bytes : ByteArray) : Bool :=
  bytes.size > maxHeadBytes || Id.run do
    for i in [0:bytes.size] do
      if i + 3 < bytes.size && bytes.get! i == 13 && bytes.get! (i + 1) == 10 &&
          bytes.get! (i + 2) == 13 && bytes.get! (i + 3) == 10 then
        return true
    return false

/-- Header lines of one request, at most `maxHeaders` of them — the bound is
    part of the type, so it holds for every value `recvHeaders` can return. -/
abbrev HeaderLines := { lines : List String // lines.length ≤ maxHeaders }

/-- Read the request line and then header lines up to the blank line that
    ends the head. `none` when the head has more than `maxHeaders` header
    lines: the request is rejected rather than truncated, since the unread
    header lines would otherwise be taken for the body or the next request.
    Reads at most `maxHeaders + 1` header lines, however long the head.
    $$\text{recvHeadersFrom} : \text{ByteSource} \to \text{IO}(\text{Option}(\text{String} \times \text{HeaderLines}))$$ -/
def recvHeadersFrom (src : ByteSource) : IO (Option (String × HeaderLines)) := do
  let requestLine ← src.readLine
  let mut headers : List String := []
  let mut ended := false
  for _ in [0:maxHeaders + 1] do
    let line ← src.readLine
    if line.isEmpty then
      ended := true
      break
    headers := line :: headers  -- O(1) cons
  if ended then
    if h : headers.reverse.length ≤ maxHeaders then
      return some (requestLine, ⟨headers.reverse, h⟩)
  return none

/-- `recvHeadersFrom` over a socket's `RecvBuffer`. -/
def recvHeaders (buf : FFI.RecvBuffer) : IO (Option (String × HeaderLines)) :=
  recvHeadersFrom (.ofRecvBuffer buf)

/-- Find a header value by name in a header list. -/
private def findHeader (name : HeaderName) (headers : RequestHeaders) : Option String :=
  headers.find? (fun (n, _) => n == name) |>.map (·.2)

-- ── Message framing (RFC 9112 §6) ─────────────────────────────────

/-- How a request body is delimited on the wire. -/
inductive BodyFraming where
  /-- `Content-Length: n` (a request with neither header has `length 0`). -/
  | length (bytes : Nat)
  /-- `Transfer-Encoding` whose final coding is `chunked`. -/
  | chunked
deriving BEq, Repr

/-- The `RequestBodyLength` an application sees for a framing. -/
def BodyFraming.toBodyLength : BodyFraming → Network.WebApp.RequestBodyLength
  | .length n => .knownLength n
  | .chunked => .chunkedBody

/-- Every comma-separated element of every `name` header, trimmed, with empty
    elements dropped (RFC 9110 §5.3: repeated fields are one list). -/
private def headerList (name : HeaderName) (headers : RequestHeaders) : List String :=
  headers.filter (·.1 == name) |>.flatMap fun (_, v) =>
    (v.splitOn ",").map (·.trimAscii.toString) |>.filter (!·.isEmpty)

/-- Parse a `Content-Length` element: one or more ASCII digits and nothing
    else (no sign, no `_`, no whitespace — `String.toNat?` alone is laxer). -/
def parseContentLength (s : String) : Option Nat :=
  if !s.isEmpty && s.all Char.isDigit then s.toNat? else none

/-- Decide how the body of a request with `headers` is framed, per
    RFC 9112 §6.1–6.3, or say why the request must be rejected. Rejection is
    the answer to every ambiguity an intermediary could resolve differently,
    because that disagreement is what request smuggling exploits:

    - `Transfer-Encoding` **and** `Content-Length` together (§6.3 ¶3);
    - `Transfer-Encoding` on an HTTP/1.0 request (§6.1);
    - a transfer coding whose final element is not `chunked`, or `chunked`
      applied more than once (§6.1, §7);
    - a `Content-Length` that is not all digits, or several that disagree
      (§6.3 ¶5).

    A request with neither header has no body (§6.3 ¶6). -/
def requestFraming (version : HttpVersion) (headers : RequestHeaders) :
    Except String BodyFraming :=
  let codings := (headerList hTransferEncoding headers).map (·.toLower)
  let lengths := headerList hContentLength headers
  if !codings.isEmpty then
    if !lengths.isEmpty then
      .error "both Transfer-Encoding and Content-Length"
    else if version.major == 0 || version == http10 then
      .error "Transfer-Encoding on an HTTP/1.0 request"
    else if codings.getLast? == some "chunked" && codings.count "chunked" == 1 then
      .ok .chunked
    else
      .error s!"unsupported Transfer-Encoding: {", ".intercalate codings}"
  else
    match lengths.map parseContentLength with
    | [] => .ok (.length 0)
    | some n :: rest =>
      if rest.all (· == some n) then .ok (.length n)
      else .error "conflicting Content-Length values"
    | none :: _ => .error "invalid Content-Length"

-- ── Chunked transfer coding (RFC 9112 §7.1) ───────────────────────

/-- The most hex digits a chunk size may have (a 64-bit size). A longer size
    line is rejected before it is turned into a `Nat`. -/
def maxChunkSizeDigits : Nat := 16

/-- Parse a `chunk-size [chunk-ext]` line: hex digits, optionally followed
    (after optional whitespace) by `;`-introduced extensions, which are
    ignored. `none` for anything else. -/
def parseChunkSize (line : String) : Option Nat := do
  let size := ((line.splitOn ";").headD "").trimAscii.toString
  if size.isEmpty || size.length > maxChunkSizeDigits then none
  size.foldl (fun acc c => do pure ((← acc) * 16 + (← Data.Hex.digitVal c))) (some 0)

/-- Where a chunked-body reader is in the stream. -/
inductive ChunkedState where
  /-- Expecting a chunk-size line. -/
  | header
  /-- Inside a chunk's data, with this many bytes still to read. -/
  | data (remaining : Nat)
  /-- The last chunk and the trailer section have been read. -/
  | done
deriving BEq, Repr

/-- Read up to `min n 4096` bytes of chunk data, then — at a chunk's end —
    the CRLF that closes it. An early end of input is an error: returning a
    short body as if it were complete would hand the application a
    truncated request. -/
private def chunkedData (readLine : IO String) (readN : Nat → IO ByteArray)
    (state : IO.Ref ChunkedState) (n : Nat) : IO ByteArray := do
  let bytes ← readN (min n 4096)
  if bytes.isEmpty then
    throw (IO.userError "chunked request body: unexpected end of input")
  let left := n - bytes.size
  if left == 0 then
    unless (← readLine).isEmpty do
      throw (IO.userError "chunked request body: missing CRLF after chunk data")
    state.set .header
  else
    state.set (.data left)
  pure bytes

/-- One read from a chunked body: the next non-empty piece of decoded data,
    or `ByteArray.empty` once the last chunk and the trailer section have
    been consumed — the stream's end, as for every `requestBody`. Trailer
    fields are read and discarded (at most `maxHeaders` of them). -/
def chunkedStep (readLine : IO String) (readN : Nat → IO ByteArray)
    (state : IO.Ref ChunkedState) : IO ByteArray := do
  match ← state.get with
  | .done => pure ByteArray.empty
  | .data n => chunkedData readLine readN state n
  | .header =>
    match parseChunkSize (← readLine) with
    | none => throw (IO.userError "chunked request body: malformed chunk size")
    | some 0 =>
      let mut ended := false
      for _ in [0:maxHeaders + 1] do
        if (← readLine).isEmpty then
          ended := true
          break
      unless ended do
        throw (IO.userError "chunked request body: too many trailer fields")
      state.set .done
      pure ByteArray.empty
    | some n => chunkedData readLine readN state n

/-- A `requestBody` reader decoding the chunked transfer coding from
    `readLine` (a CRLF-terminated line without its CRLF, `""` at end of
    input) and `readN` (up to `n` bytes, fewer only at end of input). The
    readers are parameters so the decoder is tested without a socket. -/
def chunkedBodyReader (readLine : IO String) (readN : Nat → IO ByteArray) :
    IO (IO ByteArray) := do
  let state ← IO.mkRef ChunkedState.header
  pure (chunkedStep readLine readN state)

/-- Read and discard whatever the application left of a request body, so
    that on a kept-alive connection the next request starts where this one's
    body ends. Relies on every body reader returning `ByteArray.empty` only
    at the end of its body. -/
def drainBody (req : Network.WebApp.Request) : IO Unit := do
  let mut more := true
  while more do
    more := !(← req.requestBody).isEmpty

-- ── Request parsing ───────────────────────────────────────────────

/-- Parse a full HTTP request from a byte source.
    Returns `none` — and the caller closes the connection — if the
    connection is closed, the request line is malformed, the head has too
    many header lines, or the body framing is rejected by `requestFraming`.
    $$\text{parseRequestFrom} : \text{ByteSource} \to \text{SockAddr} \to \text{IO}(\text{Option}(\text{Request}))$$ -/
def parseRequestFrom (src : ByteSource) (remoteAddr : SockAddr) : IO (Option Request) := do
  let some (requestLine, ⟨headerLines, _⟩) ← recvHeadersFrom src | return none
  if requestLine.isEmpty then
    return none
  match parseRequestLine requestLine with
  | none => return none
  | some (method, rawPath, rawQuery, version) =>
    let headers := parseHeaders headerLines
    let .ok framing := requestFraming version headers | return none
    -- Extract special headers
    let hostHeader := findHeader hHost headers
    let rangeHeader := findHeader hRange headers
    let refererHeader := findHeader hReferer headers
    let uaHeader := findHeader hUserAgent headers
    -- Parse path segments
    let pathSegments :=
      let segs := rawPath.splitOn "/"
      segs.filter (! ·.isEmpty)
    -- Parse query string
    let query := parseQuery rawQuery
    let readN := src.readN
    let bodyReader : IO ByteArray ← match framing with
      | .chunked => chunkedBodyReader src.readLine readN
      | .length contentLength => do
        -- Returns at most `contentLength` bytes in total.
        let remainingRef ← IO.mkRef contentLength
        pure do
          let remaining ← remainingRef.get
          if remaining == 0 then
            pure ByteArray.empty
          else
            let chunk ← readN (min remaining 4096)
            remainingRef.set (remaining - chunk.size)
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
      requestBodyLength := framing.toBodyLength
      requestHeaderHost := hostHeader
      requestHeaderRange := rangeHeader
      requestHeaderReferer := refererHeader
      requestHeaderUserAgent := uaHeader
    }

/-- `parseRequestFrom` over a socket's `RecvBuffer`. -/
def parseRequest (buf : FFI.RecvBuffer) (remoteAddr : SockAddr) : IO (Option Request) :=
  parseRequestFrom (.ofRecvBuffer buf) remoteAddr

end Network.WebApp.Server
