/-
  Linen.Network.WebApp.Server.Response — HTTP response rendering

  Renders status lines, headers, and dispatches on the `Response` type
  to send the appropriate body.

  Ports `Network.Wai.Handler.Warp.Response`.

  ## Design

  For `.responseBuilder`: builds the entire response as a single ByteArray
  and sends it in one call (efficient for small responses).

  For `.responseFile`: sends status + headers, then delegates to `Network.Sendfile`
  for efficient file transfer.

  For `.responseStream`: sends status + headers with chunked transfer encoding,
  then invokes the streaming body callback.

  All EventDispatcher-mode sends use `disp.sendAllGreen` for non-blocking I/O —
  the Green thread yields when the socket would block and resumes when writable.

  ## Guarantees

  - **Connected socket required:** `sendResponse` takes `Socket .connected`,
    enforced by Lean 4's phantom type parameter. Passing a listening or
    fresh socket is a compile-time error.
  - **ResponseReceived token:** `sendResponse` returns `ResponseReceived`,
    the opaque token that the `AppM` indexed monad requires the application
    to produce.
  - **Header preservation proven:** `addAutoHeaders_length_ge` proves that
    auto-added headers never remove user-provided headers (monotonicity).
-/

import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.HTTP.Types.Status
import Linen.Network.HTTP.Types.Version
import Linen.Network.Socket
import Linen.Network.Socket.EventDispatcher
import Linen.Network.Sendfile
import Linen.Control.Concurrent.Green
import Linen.Network.WebApp.Server.Settings

namespace Network.WebApp.Server

open Network.HTTP.Types
open Network.WebApp
open Network.Socket
open Network.Sendfile
open Control.Concurrent.Green (Green)

/-- Render an HTTP status line.
    $$\text{renderStatusLine}(\text{ver}, \text{st}) = \text{"HTTP/x.y code message\\r\\n"}$$ -/
def renderStatusLine (version : HttpVersion) (status : Status) : String :=
  s!"{version} {status.statusCode} {status.statusMessage}\r\n"

/-- Render a list of headers as a string, each terminated by CRLF.
    $$\text{renderHeaders}(hs) = \prod_{(n,v) \in hs} n \cdot \text{": "} \cdot v \cdot \text{"\\r\\n"}$$ -/
def renderHeaders (headers : ResponseHeaders) : String :=
  let lines := headers.map fun (name, value) =>
    s!"{name}: {value}\r\n"
  String.join lines

private def crlfBytes : ByteArray := "\r\n".toUTF8
private def colonSpaceBytes : ByteArray := ": ".toUTF8

/-- Render the HTTP status line directly as ByteArray, avoiding String intermediaries. -/
def renderStatusLineBytes (version : HttpVersion) (status : Status) : ByteArray :=
  s!"{version} {status.statusCode} {status.statusMessage}\r\n".toUTF8

/-- Render response headers directly as ByteArray, avoiding String intermediaries.
    Each header is rendered as `name: value\r\n` using ByteArray concatenation
    instead of building a String and converting at the end. -/
def renderHeadersBytes (headers : ResponseHeaders) : ByteArray :=
  headers.foldl (fun acc (name, value) =>
    acc ++ name.original.toUTF8 ++ colonSpaceBytes ++ value.toUTF8 ++ crlfBytes
  ) ByteArray.empty

/-- Check if a header name is present in a header list. -/
private def hasHeader (name : HeaderName) (headers : ResponseHeaders) : Bool :=
  headers.any fun (n, _) => n == name

/-- Add automatic headers based on settings and response type.
    Does not overwrite user-provided headers. -/
private def addAutoHeaders (settings : Settings) (extraHeaders : ResponseHeaders)
    (userHeaders : ResponseHeaders) : ResponseHeaders :=
  let headers := userHeaders
  -- Add Server header if configured and not already present
  let headers :=
    if settings.settingsAddServerHeader && !hasHeader hServer headers then
      (hServer, settings.settingsServerName) :: headers
    else headers
  -- Add extra headers (Content-Length, Transfer-Encoding) if not present
  let headers := extraHeaders.foldl (fun acc (n, v) =>
    if hasHeader n acc then acc else (n, v) :: acc) headers
  headers

/-- addAutoHeaders preserves the count of user headers (only adds, never removes). -/
private theorem addAutoHeaders_length_ge (settings : Settings) (extra user : ResponseHeaders) :
    user.length ≤ (addAutoHeaders settings extra user).length := by
  simp only [addAutoHeaders]
  -- After the server-header step, length is ≥ user.length
  have h1 : user.length ≤
    (if settings.settingsAddServerHeader && !hasHeader hServer user
     then (hServer, settings.settingsServerName) :: user
     else user).length := by
    split <;> simp_all [List.length_cons]
  -- foldl that only prepends preserves ≥
  suffices ∀ (acc : ResponseHeaders) (es : ResponseHeaders),
    acc.length ≤ (es.foldl (fun a (n, v) => if hasHeader n a then a else (n, v) :: a) acc).length from
    Nat.le_trans h1 (this _ extra)
  intro acc es
  induction es generalizing acc with
  | nil => exact Nat.le_refl _
  | cons hd tl ih =>
    simp only [List.foldl]
    apply Nat.le_trans _ (ih _)
    split <;> simp_all [List.length_cons]

/-- The number of bytes `sendFile path part` sends: the file's size, or
    the part of it that exists — `count = 0` meaning to the end, as in
    `Network.Sendfile.FilePart`.
    $$\text{filePartLength}(size, part) = \min(count, size - offset)$$ -/
def filePartLength (size : Nat) : Option FilePart → Nat
  | none => size
  | some fp =>
    let available := size - fp.offset
    if fp.count == 0 then available else min fp.count available

/-- The body length of a file response, from the file's metadata. A file
    response always carries `Content-Length`: without it (as through 1.8.0)
    a kept-alive client cannot tell where the file ends and the next
    response begins. -/
def fileBodyLength (path : String) (part : Option FilePart) : IO Nat := do
  let size := (← System.FilePath.metadata path).byteSize.toNat
  return filePartLength size part

/-- Where a response goes: the writes `sendResponseTo` needs, so the same
    rendering serves a plain socket (blocking or event-driven) and a TLS
    session, which must not be bypassed by writing to its socket. -/
structure ResponseSink where
  /-- Send all of these bytes (the head, a whole body). -/
  send : ByteArray → Green Unit
  /-- The same, from inside a streaming body's `IO` callback. -/
  sendIO : ByteArray → IO Unit
  /-- Send a file, or the `FilePart` of it. -/
  sendFile : String → Option FilePart → IO Unit
  /-- For `responseRaw`: read from the connection. -/
  rawRecv : IO ByteArray
  /-- For `responseRaw`: write to the connection. -/
  rawSend : ByteArray → IO Unit

/-- Send a full HTTP response to `sink`.
    $$\text{sendResponseTo} : \text{ResponseSink} \to \text{Settings} \to \text{Request} \to \text{Response} \to \text{Green ResponseReceived}$$ -/
def sendResponseTo (sink : ResponseSink) (settings : Settings) (req : Request)
    (resp : Response) : Green ResponseReceived := do
  let head (status : Status) (headers : ResponseHeaders) : ByteArray :=
    renderStatusLineBytes req.httpVersion status ++ renderHeadersBytes headers ++ crlfBytes
  match resp with
  | .responseBuilder status userHeaders body =>
    let allHeaders := addAutoHeaders settings [(hContentLength, toString body.size)] userHeaders
    sink.send (head status allHeaders ++ body)
    pure ResponseReceived.done

  | .responseFile status userHeaders path part =>
    let length ← (fileBodyLength path part : IO _)
    let allHeaders := addAutoHeaders settings [(hContentLength, toString length)] userHeaders
    sink.send (head status allHeaders)
    (sink.sendFile path part : IO _)
    pure ResponseReceived.done

  | .responseStream status userHeaders body =>
    let allHeaders := addAutoHeaders settings [(hTransferEncoding, "chunked")] userHeaders
    sink.send (head status allHeaders)
    let writeChunk : ByteArray → IO Unit := fun chunk => do
      if chunk.size > 0 then
        let sizeStr := String.ofList (Nat.toDigits 16 chunk.size)
        sink.sendIO ((sizeStr ++ "\r\n").toUTF8 ++ chunk ++ "\r\n".toUTF8)
    (body writeChunk (pure ()) : IO _)
    sink.send "0\r\n\r\n".toUTF8
    pure ResponseReceived.done

  | .responseRaw rawAction _fallback =>
    (rawAction sink.rawRecv sink.rawSend : IO _)
    pure ResponseReceived.done

/-- The sink for a connected socket in blocking mode (`Blocking.sendAll`,
    each wait for writability at most `timeoutMillis`). `rawRecv` should read
    through the connection's buffer when there is one, so a `responseRaw`
    handler sees bytes already received. -/
def ResponseSink.ofSocket (sock : Socket .connected)
    (timeoutMillis : Nat := Blocking.defaultTimeoutMillis)
    (rawRecv : IO ByteArray := Blocking.recv sock 4096 timeoutMillis) : ResponseSink where
  send bytes := do (Blocking.sendAll sock bytes timeoutMillis : IO _)
  sendIO bytes := Blocking.sendAll sock bytes timeoutMillis
  sendFile path part := Network.Sendfile.sendFileWith (Blocking.sendAll sock · timeoutMillis) path part
  rawRecv := rawRecv
  rawSend bytes := Blocking.sendAll sock bytes timeoutMillis

/-- The sink for a connected socket in EventDispatcher mode: every wait goes
    through the dispatcher — suspending the green thread for whole writes,
    and `IO.wait` on its promise for writes from `IO` callbacks (streamed
    bodies, files, `responseRaw`), which does not starve the pool as a
    `poll` would. Each wait is at most `timeoutMillis`. -/
def ResponseSink.ofSocketEL (sock : Socket .connected) (disp : EventDispatcher)
    (timeoutMillis : Nat := Blocking.defaultTimeoutMillis)
    (rawRecv : IO ByteArray := disp.recvAwait sock timeoutMillis) : ResponseSink where
  send bytes := disp.sendAllGreenFor sock bytes timeoutMillis
  sendIO bytes := disp.sendAllAwait sock bytes timeoutMillis
  sendFile path part := Network.Sendfile.sendFileWith (disp.sendAllAwait sock · timeoutMillis) path part
  rawRecv := rawRecv
  rawSend bytes := disp.sendAllAwait sock bytes timeoutMillis

/-- Send a full HTTP response over a connected socket (blocking mode).
    Uses `Blocking.sendAll` for reliable full writes.
    $$\text{sendResponse} : \text{Socket .connected} \to \text{Settings} \to \text{Request} \to \text{Response} \to \text{Green ResponseReceived}$$ -/
def sendResponse (sock : Socket .connected) (settings : Settings) (req : Request)
    (resp : Response) : Green ResponseReceived :=
  sendResponseTo (.ofSocket sock) settings req resp

/-- Send a full HTTP response (EventDispatcher mode, non-blocking).
    Uses `sendAllGreen` for non-blocking sends via the event loop. -/
def sendResponseEL (sock : Socket .connected) (settings : Settings) (req : Request)
    (resp : Response) (disp : EventDispatcher) : Green ResponseReceived :=
  sendResponseTo (.ofSocketEL sock disp) settings req resp

end Network.WebApp.Server
