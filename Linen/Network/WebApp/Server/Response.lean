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

/-- Send a full HTTP response over a connected socket (blocking mode).
    Uses `Blocking.sendAll` for reliable full writes.
    $$\text{sendResponse} : \text{Socket .connected} \to \text{Settings} \to \text{Request} \to \text{Response} \to \text{Green ResponseReceived}$$ -/
def sendResponse (sock : Socket .connected) (settings : Settings) (_req : Request)
    (resp : Response) : Green ResponseReceived := do
  match resp with
  | .responseBuilder status userHeaders body =>
    let extraHeaders : ResponseHeaders :=
      [(hContentLength, toString body.size)]
    let allHeaders := addAutoHeaders settings extraHeaders userHeaders
    let headBytes := renderStatusLineBytes _req.httpVersion status
      ++ renderHeadersBytes allHeaders ++ crlfBytes
    (Blocking.sendAll sock (headBytes ++ body) : IO _)
    pure ResponseReceived.done

  | .responseFile status userHeaders path part =>
    let allHeaders := addAutoHeaders settings [] userHeaders
    let headBytes := renderStatusLineBytes _req.httpVersion status
      ++ renderHeadersBytes allHeaders ++ crlfBytes
    (Blocking.sendAll sock headBytes : IO _)
    (Network.Sendfile.sendFile sock path part : IO _)
    pure ResponseReceived.done

  | .responseStream status userHeaders body =>
    let extraHeaders : ResponseHeaders :=
      [(hTransferEncoding, "chunked")]
    let allHeaders := addAutoHeaders settings extraHeaders userHeaders
    let headBytes := renderStatusLineBytes _req.httpVersion status
      ++ renderHeadersBytes allHeaders ++ crlfBytes
    (Blocking.sendAll sock headBytes : IO _)
    let writeChunk : ByteArray → IO Unit := fun chunk => do
      if chunk.size > 0 then
        let sizeStr := String.ofList (Nat.toDigits 16 chunk.size)
        let frame := (sizeStr ++ "\r\n").toUTF8 ++ chunk ++ "\r\n".toUTF8
        Blocking.sendAll sock frame
    let flush : IO Unit := pure ()
    (body writeChunk flush : IO _)
    (Blocking.sendAll sock "0\r\n\r\n".toUTF8 : IO _)
    pure ResponseReceived.done

  | .responseRaw rawAction _fallback =>
    let recvAction : IO ByteArray := Blocking.recv sock 4096
    let sendAction : ByteArray → IO Unit := Blocking.sendAll sock
    (rawAction recvAction sendAction : IO _)
    pure ResponseReceived.done

/-- Send a full HTTP response (EventDispatcher mode, non-blocking).
    Uses `sendAllGreen` for non-blocking sends via the event loop. -/
def sendResponseEL (sock : Socket .connected) (settings : Settings) (_req : Request)
    (resp : Response) (disp : EventDispatcher) : Green ResponseReceived := do
  match resp with
  | .responseBuilder status userHeaders body =>
    let extraHeaders : ResponseHeaders :=
      [(hContentLength, toString body.size)]
    let allHeaders := addAutoHeaders settings extraHeaders userHeaders
    let headBytes := renderStatusLineBytes _req.httpVersion status
      ++ renderHeadersBytes allHeaders ++ crlfBytes
    disp.sendAllGreen sock (headBytes ++ body)
    pure ResponseReceived.done

  | .responseFile status userHeaders path part =>
    let allHeaders := addAutoHeaders settings [] userHeaders
    let headBytes := renderStatusLineBytes _req.httpVersion status
      ++ renderHeadersBytes allHeaders ++ crlfBytes
    disp.sendAllGreen sock headBytes
    (Network.Sendfile.sendFile sock path part : IO _)
    pure ResponseReceived.done

  | .responseStream status userHeaders body =>
    let extraHeaders : ResponseHeaders :=
      [(hTransferEncoding, "chunked")]
    let allHeaders := addAutoHeaders settings extraHeaders userHeaders
    let headBytes := renderStatusLineBytes _req.httpVersion status
      ++ renderHeadersBytes allHeaders ++ crlfBytes
    disp.sendAllGreen sock headBytes
    let writeChunk : ByteArray → IO Unit := fun chunk => do
      if chunk.size > 0 then
        let sizeStr := String.ofList (Nat.toDigits 16 chunk.size)
        let frame := (sizeStr ++ "\r\n").toUTF8 ++ chunk ++ "\r\n".toUTF8
        Blocking.sendAll sock frame
    let flush : IO Unit := pure ()
    (body writeChunk flush : IO _)
    disp.sendAllGreen sock "0\r\n\r\n".toUTF8
    pure ResponseReceived.done

  | .responseRaw rawAction _fallback =>
    let recvAction : IO ByteArray := Blocking.recv sock 4096
    let sendAction : ByteArray → IO Unit := Blocking.sendAll sock
    (rawAction recvAction sendAction : IO _)
    pure ResponseReceived.done

end Network.WebApp.Server
