/-
  Linen.Network.WebApp.Server.TLS — HTTPS support for the web application server

  Provides TLS (HTTPS) support for the server using OpenSSL via FFI.

  Ports `Network.Wai.Handler.WarpTLS`, renamed from the
  Haskell-specific `WarpTLS` to `Server.TLS` per this project's naming
  convention.

  ## Design

  Two modes, as for the plain server:

  - **`runTLS`** — a thread per connection (`Server.runSettings`'s model).
    The socket is made non-blocking after the first-byte peek so every wait —
    handshake, reads, writes — is a `poll` bounded by `settingsTimeout`
    (`Network.TLS.handshake`, `readWithin`, `writeWithin`).
  - **`runTLSEventLoop`** — green threads over an `EventDispatcher`
    (`Server.runSettingsEventLoop`'s model): the peek, the handshake and every
    head read suspend the green thread (`Network.TLS.Green`), so idle and slow
    connections hold no pool thread; body reads, which are the application's
    `IO` calls, wait on the dispatcher's promise (`Green.readIO`), which the
    task manager compensates for.

  Either way requests are parsed, and responses written, *through the
  session*: by HTTP/2 (`serveHttp2`, `Network.HTTP2.serve`) when ALPN chose
  `h2` — offered unless `TLSSettings.http2` is off — and otherwise by the
  same HTTP/1.1 loop as plain connections (`serveHttp`).
  Through 1.8.0 this module ran the handshake and then parsed and answered
  requests on the raw socket, bypassing the session entirely, so no HTTPS
  request could succeed; nothing tested it.

  Before the handshake, the first byte the client sends is *peeked*
  (`MSG_PEEK`, so OpenSSL still reads it): `0x16` opens a TLS handshake
  record; anything else is plain HTTP, handled per `OnInsecure` — the
  detection warp-tls uses (which consumes and replays the bytes instead).

  ## What is not supported (say it loudly)

  - **HTTP/2 without TLS** (`h2c`, by prior knowledge or `Upgrade`): HTTP/2
    is offered only over TLS, by ALPN — which is how browsers use it.

  ## No `partial`

  All loops here are `while` loops in `do`-notation, which desugar to the
  standard library's `Loop.forIn` combinator — no `partial def` or fuel
  parameter is used, per this project's coding conventions.

  ## Guarantees

  - Minimum TLS version enforced by the OpenSSL configuration behind `Network.TLS.Context`
  - TLS sessions are cleaned up on connection close
  - Certificate and key are validated at startup
-/
import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.Socket
import Linen.Network.Socket.Blocking
import Linen.Network.Sendfile
import Linen.Network.TLS.Context
import Linen.Network.TLS.Green
import Linen.Control.Concurrent
import Linen.Control.Concurrent.Green
import Linen.Network.WebApp.Server.Settings
import Linen.Network.WebApp.Server.Request
import Linen.Network.WebApp.Server.Response
import Linen.Network.WebApp.Server.Run
import Linen.Network.WebApp.Server.HTTP2

namespace Network.WebApp.Server.TLS

open Network.WebApp
open Network.HTTP.Types
open Network.Socket
open Network.TLS
open Network.WebApp.Server
open Control.Concurrent.Green (Green)

/-- How to handle non-TLS (plain HTTP) connections on the TLS port. -/
inductive OnInsecure where
  /-- Answer `426 Upgrade Required` with this message, then close
      (`insecureDenial`). -/
  | denyInsecure (message : String)
  /-- Serve plain HTTP on the TLS port too, as `Server.runConnection` would
      (the application sees `isSecure = false`). -/
  | allowInsecure
deriving BEq, Repr

/-- Certificate source. -/
inductive CertSettings where
  | certFile (certPath keyPath : String)
deriving Repr

/-- TLS-specific settings for `Server.TLS`. -/
structure TLSSettings where
  certSettings : CertSettings
  onInsecure : OnInsecure := .denyInsecure "This server requires HTTPS"
  /-- Offer HTTP/2: ALPN prefers `h2`, then `http/1.1`. Off, the server does
      not answer ALPN and every client speaks HTTP/1.1. -/
  http2 : Bool := true

/-- Answer ALPN on `ctx` with `h2` then `http/1.1` (`http2`), or not at all. -/
def configureAlpn (ctx : TLSContext) (http2 : Bool) : IO Unit :=
  Network.TLS.setServerAlpn ctx (if http2 then ["h2", "http/1.1"] else [])

/-- Serve a TLS connection after its handshake with the protocol ALPN chose:
    HTTP/2 for `h2`, HTTP/1.1 otherwise. -/
def serveNegotiated (session : TLSSession) (transport : HttpTransport) (remoteAddr : SockAddr)
    (settings : Settings) (app : Application) : Green Unit := do
  if (← (Network.TLS.getAlpn session : IO _)) == some "h2" then
    serveHttp2 transport remoteAddr settings app
  else
    serveHttp transport remoteAddr settings app

/-- Whether the first byte of a connection opens a TLS handshake record
    (content type 22, RFC 8446 §5.1). -/
def isTlsHandshakeByte (b : UInt8) : Bool := b == 0x16

/-- The answer to plain HTTP under `denyInsecure message`: RFC 2817 §4.2's
    `426 Upgrade Required`, as warp-tls sends it, with a `Content-Length`. -/
def insecureDenial (message : String) : ByteArray :=
  let body := message.toUTF8
  ("HTTP/1.1 426 Upgrade Required\r\nUpgrade: TLS/1.0, HTTP/1.1\r\n" ++
   "Connection: Upgrade\r\nContent-Type: text/plain\r\n" ++
   s!"Content-Length: {body.size}\r\n\r\n").toUTF8 ++ body

/-- What the first byte a client sends says about its connection. -/
inductive FirstByte where
  | tls
  | plain
  | closed
deriving BEq, Repr

/-- Classify a peeked first byte. -/
def FirstByte.ofPeek : RecvOutcome → Option FirstByte
  | .data bytes => some (if isTlsHandshakeByte (bytes.get! 0) then .tls else .plain)
  | .eof => some .closed
  | .error _ => some .closed
  | .wouldBlock => none

/-- Wait (at most `timeoutMillis`, with `poll`) for the client's first byte
    and peek at it, leaving it unread. A client that sends nothing in time
    counts as `closed`. -/
def peekFirstByte (sock : Socket .connected) (timeoutMillis : Nat) : IO FirstByte := do
  for _ in [0:100] do
    match ← Network.Socket.poll sock .read timeoutMillis with
    | .ready => if let some b := FirstByte.ofPeek (← Network.Socket.peek sock 1) then return b
    | _ => return .closed
  return .closed

/-- `peekFirstByte` on the dispatcher: the green thread is suspended while
    waiting. -/
def peekFirstByteEL (disp : EventDispatcher) (sock : Socket .connected) (timeoutMillis : Nat) :
    Green FirstByte := do
  for _ in [0:100] do
    if let some b := FirstByte.ofPeek (← (Network.Socket.peek sock 1 : IO _)) then return b
    unless ← disp.waitReadableFor sock timeoutMillis do return .closed
  return .closed

/-- Answer plain HTTP with `insecureDenial` — after reading its head, since
    closing with a request unread makes the kernel reset the connection,
    which can destroy the answer. -/
def denyInsecureOn (t : HttpTransport) (message : String) : Green Unit := do
  if ← t.bufferHead then t.sink.send (insecureDenial message)

-- ── Transports over a TLS session ──

/-- The transport of a TLS session in blocking mode, on a **non-blocking**
    socket so that every wait is a bounded `poll` (`readWithin`,
    `writeWithin`) — a blocking `SSL_read` could not time out. -/
def tlsTransport (session : TLSSession) (sock : Socket .connected) (settings : Settings) :
    IO HttpTransport := do
  let timeout := settings.timeoutMillis
  let recv : IO ByteArray := do
    match ← readWithin session sock.raw timeout with
    | some bytes => pure bytes
    | none => throw (IO.userError s!"TLS read timed out after {timeout}ms")
  let reader ← ByteSource.buffered recv
  let write := writeWithin session sock.raw timeout
  return {
    nextChunk := do (readWithin session sock.raw timeout : IO _)
    reader
    sink := {
      send := fun bytes => do (write bytes : IO _)
      sendIO := write
      sendFile := fun path part => Network.Sendfile.sendFileWith write path part
      rawRecv := reader.readSome
      rawSend := write }
    isSecure := true }

/-- The transport of a TLS session in EventDispatcher mode: heads read on the
    green thread (`Green.readFor`), bodies and `IO`-side writes through the
    dispatcher's promises (`readIO`/`writeIO`), each wait at most
    `settingsTimeout`. -/
def tlsTransportEL (session : TLSSession) (sock : Socket .connected) (settings : Settings)
    (disp : EventDispatcher) : IO HttpTransport := do
  let timeout := settings.timeoutMillis
  let reader ← ByteSource.buffered (Network.TLS.Green.readIO disp sock session timeout)
  let writeIO (bytes : ByteArray) := Network.TLS.Green.writeIO disp sock session bytes timeout
  return {
    nextChunk := Network.TLS.Green.readFor disp sock session (some timeout)
    reader
    sink := {
      send := fun bytes => Network.TLS.Green.write disp sock session bytes (some timeout)
      sendIO := writeIO
      sendFile := fun path part => Network.Sendfile.sendFileWith writeIO path part
      rawRecv := reader.readSome
      rawSend := writeIO }
    isSecure := true }

-- ── Blocking mode: a thread per connection ──

private def reportError (settings : Settings) (remoteAddr : SockAddr) (e : IO.Error) : IO Unit := do
  settings.settingsOnException (some remoteAddr)
  IO.eprintln s!"Server.TLS: connection error from {remoteAddr}: {e}"

/-- Serve one connection on the calling thread: the TLS handshake, then
    HTTP/1.1 requests through the session (`serveHttp` over `tlsTransport`).
    A connection that does not open with a TLS record is handled per
    `onInsecure`. Every error ends the connection only, reported through
    `settingsOnException`. -/
def tlsConnection (ctx : TLSContext) (clientSock : Socket .connected)
    (remoteAddr : SockAddr) (settings : Settings) (app : Application)
    (onInsecure : OnInsecure := .denyInsecure "This server requires HTTPS") : IO Unit := do
  let timeout := settings.timeoutMillis
  match ← peekFirstByte clientSock timeout, onInsecure with
  | .plain, .allowInsecure =>
    -- Plain HTTP on this socket, exactly as the plain server serves it.
    runConnection clientSock remoteAddr settings app
  | .tls, _ =>
    try
      setNonBlocking clientSock
      let session ← newServerSession ctx clientSock.raw
      try
        handshake session clientSock.raw timeout
        let transport ← tlsTransport session clientSock settings
        Green.block (serveNegotiated session transport remoteAddr settings app)
          (← Std.CancellationToken.new)
      finally
        Network.TLS.close session
    catch e => reportError settings remoteAddr e
    finally
      let _ ← Network.Socket.close clientSock
  | .plain, .denyInsecure message =>
    try
      let transport ← blockingTransport clientSock settings
      Green.block (denyInsecureOn transport message) (← Std.CancellationToken.new)
    catch _ => pure ()
    finally
      let _ ← Network.Socket.close clientSock
  | .closed, _ =>
    let _ ← Network.Socket.close clientSock

/-- Accept connections on `serverSock` and serve each on its own thread,
    until `stop` is cancelled. Cancellation is checked after every accept,
    so a stopped loop exits on the next connection attempt (the test's way
    of stopping it is to cancel and then connect once). -/
def runTLSSocket (ctx : TLSContext) (serverSock : Socket .listening) (settings : Settings)
    (app : Application) (stop : Std.CancellationToken)
    (onInsecure : OnInsecure := .denyInsecure "This server requires HTTPS") : IO Unit := do
  while !(← stop.isCancelled) do
    let (clientSock, remoteAddr) ← Network.Socket.Blocking.accept serverSock (timeoutMillis := 0)
    if ← stop.isCancelled then
      let _ ← Network.Socket.close clientSock
    else
      -- A thread of its own: `forkConnection` says why.
      forkConnection (tlsConnection ctx clientSock remoteAddr settings app onInsecure)

/-- Run a web application with TLS on the given port, a thread per
    connection.
    $$\text{runTLS} : \text{TLSSettings} \to \text{Settings} \to \text{Application} \to \text{IO}()$$ -/
def runTLS (tlsSettings : TLSSettings) (settings : Settings)
    (app : Application) : IO Unit := do
  let (certPath, keyPath) := match tlsSettings.certSettings with
    | .certFile c k => (c, k)
  let ctx ← Network.TLS.createContext certPath keyPath
  configureAlpn ctx tlsSettings.http2
  let serverSock ← Network.Socket.listenTCP
    settings.settingsHost settings.settingsPort settings.settingsBacklog
  try
    settings.settingsBeforeMainLoop
    runTLSSocket ctx serverSock settings app (← Std.CancellationToken.new) tlsSettings.onInsecure
  finally
    let _ ← Network.Socket.close serverSock

-- ── EventDispatcher mode: green threads ──

/-- Serve one connection on a green thread (`tlsConnection`'s event-loop
    form): the first-byte peek, the handshake (`Network.TLS.Green.accept`)
    and every head read suspend the green thread instead of holding a pool
    thread. -/
def tlsConnectionEL (ctx : TLSContext) (clientSock : Socket .connected)
    (remoteAddr : SockAddr) (settings : Settings) (app : Application) (disp : EventDispatcher)
    (onInsecure : OnInsecure := .denyInsecure "This server requires HTTPS") : Green Unit := do
  let timeout := settings.timeoutMillis
  -- OpenSSL reads and writes the fd itself, so it must not block (Linux does
  -- not carry the listener's O_NONBLOCK over to accepted sockets).
  try (setNonBlocking clientSock : IO _) catch _ => pure ()
  match ← peekFirstByteEL disp clientSock timeout, onInsecure with
  | .plain, .allowInsecure =>
    runConnectionEL clientSock remoteAddr settings app disp
  | .tls, _ =>
    try
      let session ← Network.TLS.Green.accept disp ctx clientSock (some timeout)
      try
        let transport ← (tlsTransportEL session clientSock settings disp : IO _)
        serveNegotiated session transport remoteAddr settings app
      finally
        (Network.TLS.close session : IO _)
    catch e => (reportError settings remoteAddr e : IO _)
    finally
      let _ ← (Network.Socket.close clientSock : IO _)
  | .plain, .denyInsecure message =>
    try
      let transport ← (eventLoopTransport clientSock settings disp : IO _)
      denyInsecureOn transport message
    catch _ => pure ()
    finally
      let _ ← (Network.Socket.close clientSock : IO _)
  | .closed, _ =>
    let _ ← (Network.Socket.close clientSock : IO _)

/-- The event-loop accept loop for TLS: each connection on its own green
    thread. Stops when the green thread's token is cancelled (checked on each
    iteration; `EventDispatcher.shutdown` wakes an idle wait). -/
def runTLSSocketEL (ctx : TLSContext) (serverSock : Socket .listening) (settings : Settings)
    (app : Application) (disp : EventDispatcher)
    (onInsecure : OnInsecure := .denyInsecure "This server requires HTTPS") : Green Unit := do
  while true do
    Control.Concurrent.Green.Green.checkCancelled
    match ← (Network.Socket.accept serverSock : IO _) with
    | .accepted clientSock remoteAddr =>
      let _ ← (Control.Concurrent.forkGreen
        (tlsConnectionEL ctx clientSock remoteAddr settings app disp onInsecure) : IO _)
    | .wouldBlock => disp.waitReadable serverSock
    | .error _ => Control.Concurrent.Green.Green.sleep 10  -- no pool thread held

/-- Run a web application with TLS on the given port, on green threads over
    an `EventDispatcher` (`Server.runSettingsEventLoop`'s TLS counterpart):
    idle and slow connections — including their TLS handshakes — hold no
    pool thread.
    $$\text{runTLSEventLoop} : \text{TLSSettings} \to \text{Settings} \to \text{Application} \to \text{IO}()$$ -/
def runTLSEventLoop (tlsSettings : TLSSettings) (settings : Settings)
    (app : Application) : IO Unit := do
  let (certPath, keyPath) := match tlsSettings.certSettings with
    | .certFile c k => (c, k)
  let ctx ← Network.TLS.createContext certPath keyPath
  configureAlpn ctx tlsSettings.http2
  let serverSock ← Network.Socket.listenTCP
    settings.settingsHost settings.settingsPort settings.settingsBacklog
  Network.Socket.setNonBlocking serverSock
  let disp ← EventDispatcher.create
  try
    settings.settingsBeforeMainLoop
    Green.block (runTLSSocketEL ctx serverSock settings app disp tlsSettings.onInsecure)
      (← Std.CancellationToken.new)
  finally
    disp.shutdown
    let _ ← Network.Socket.close serverSock

end Network.WebApp.Server.TLS
