/-
  Linen.Network.WebApp.Server.Run — Accept loop and connection handling

  Binds a TCP socket, accepts connections, and spawns a green thread per
  connection to handle HTTP requests.

  Ports `Network.Wai.Handler.Warp.Run`.

  ## Design

  Two execution modes are available:
  - **Blocking mode** (default): Uses blocking `accept`/`recv`/`send` with
    `forkIO` per connection. Maximizes throughput for I/O-bound workloads.
  - **EventDispatcher mode**: Uses non-blocking sockets with kqueue/epoll
    via `EventDispatcher` and `Green` threads. Better for high-concurrency
    scenarios where many connections are idle (e.g., WebSockets, long-polling).
    Accessible via `runSettingsEventLoop`.

  ## No `partial`

  All loops here are `while` loops in `do`-notation, which desugar to the
  standard library's `Loop.forIn` combinator — no `partial def` or fuel
  parameter is used, per this project's coding conventions.

  ## Dependent-Type Guarantees

  The socket phantom types flow through the entire call chain:
  - `acceptLoop` requires `Socket .listening` (compile-time)
  - `accept` returns `AcceptOutcome` with `Socket .connected` (compile-time)
  - `runConnection` requires `Socket .connected` (compile-time)
  - `close` requires `state ≠ .closed` proof (compile-time)

  Keep-alive semantics are proven correct:
  - `connAction_http10_default`: HTTP/1.0 defaults to close
  - `connAction_http11_default`: HTTP/1.1 defaults to keep-alive
-/

import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.HTTP.Types.Version
import Linen.Network.Socket
import Linen.Network.Socket.EventDispatcher
import Linen.Network.Socket.Blocking
import Linen.Control.Concurrent
import Linen.Control.Concurrent.Green
import Linen.Network.WebApp.Server.Settings
import Linen.Network.WebApp.Server.Request
import Linen.Network.WebApp.Server.Response

namespace Network.WebApp.Server

open Network.WebApp
open Network.Socket
open Network.HTTP.Types
open Control.Concurrent.Green (Green)

/-- Connection action after handling a request.
    Encodes the HTTP/1.1 keep-alive state machine. -/
inductive ConnAction where
  | keepAlive  -- continue reading next request on this connection
  | close      -- close the connection
deriving BEq, Repr

/-- Determine whether to keep the connection alive based on HTTP version
    and the Connection header. -/
def connAction (req : Network.WebApp.Request) : ConnAction :=
  let connHdr := req.requestHeaders.find? (fun (n, _) => n == hConnection)
    |>.map (·.2.toLower)
  if req.httpVersion == http11 then
    if connHdr == some "close" then .close else .keepAlive
  else
    if connHdr == some "keep-alive" then .keepAlive else .close

/-- HTTP/1.0 without Connection header defaults to close. -/
theorem connAction_http10_default (req : Network.WebApp.Request)
    (hVer : (req.httpVersion == http11) = false)
    (hNoConn : req.requestHeaders.find? (fun (n, _) => n == hConnection) = none) :
    connAction req = .close := by
  unfold connAction; simp [hVer, hNoConn]

/-- HTTP/1.1 without Connection header defaults to keep-alive. -/
theorem connAction_http11_default (req : Network.WebApp.Request)
    (hVer : (req.httpVersion == http11) = true)
    (hNoConn : req.requestHeaders.find? (fun (n, _) => n == hConnection) = none) :
    connAction req = .keepAlive := by
  unfold connAction; simp [hVer, hNoConn]

-- ══════════════════════════════════════════════════════════════
-- The HTTP/1.1 connection loop, over any transport
-- ══════════════════════════════════════════════════════════════

/-- A connection as the HTTP/1.1 loop sees it — the same for plain and TLS
    sockets, in blocking and event-loop mode. -/
structure HttpTransport where
  /-- The next bytes towards a request head: `some` bytes, `some` empty at
      end of input, `none` when `settingsTimeout` passed without any. The
      event-loop transports suspend the green thread here. -/
  nextChunk : Green (Option ByteArray)
  /-- The connection's buffered reader: request heads are parsed from it,
      and bodies read through it (in `IO`, throwing on timeout). -/
  reader : BufferedSource
  /-- Where responses go. -/
  sink : ResponseSink
  /-- Whether the connection is TLS (`Request.isSecure`). -/
  isSecure : Bool := false

/-- Receive until `t.reader` holds a complete request head, or the peer
    closes: `true` then; `false` when `settingsTimeout` passed first. -/
def HttpTransport.bufferHead (t : HttpTransport) : Green Bool := do
  let mut ended := false
  while !ended && !headComplete (← (t.reader.unread : IO _)) do
    match ← t.nextChunk with
    | none => return false
    | some chunk => if chunk.isEmpty then ended := true else (t.reader.feed chunk : IO _)
  return true

/-- Serve HTTP/1.1 requests on a transport until the peer closes, a request
    asks to close, or `settingsTimeout` passes waiting for a request head.

    Each head is buffered until complete (`headComplete`) before the parser
    runs, so the parser never waits, and bytes already buffered — pipelined
    requests — are served before any new wait. A timeout while waiting for a
    head (between requests, or a client sending one too slowly) closes the
    connection quietly, as Warp's does; one while reading a body is an error
    the application sees. After each response the unread body is drained, so
    the next head starts where it should. -/
def serveHttp (t : HttpTransport) (remoteAddr : SockAddr) (settings : Settings)
    (app : Application) : Green Unit := do
  let mut keepGoing := true
  while keepGoing do
    if !(← t.bufferHead) then
      keepGoing := false
    else
      match ← (parseRequestFrom t.reader.source remoteAddr : IO _) with
      | none => keepGoing := false
      | some req =>
        let req := { req with isSecure := t.isSecure }
        let action := connAction req
        let _received ← (app req fun resp => do
          let resp' := if action == .close then
            resp.mapResponseHeaders ((hConnection, "close") :: ·)
          else resp
          sendResponseTo t.sink settings req resp').run
        if action == .keepAlive then
          (drainBody req : IO _)
        else
          keepGoing := false

/-- `settingsTimeout`, in milliseconds. -/
def Settings.timeoutMillis (settings : Settings) : Nat := settings.settingsTimeout * 1000

-- ══════════════════════════════════════════════════════════════
-- Blocking mode (default, maximum throughput)
-- ══════════════════════════════════════════════════════════════

/-- The transport of a connected socket in blocking mode: every wait is a
    `poll` of at most `settingsTimeout`, on this connection's own thread. -/
def blockingTransport (sock : Socket .connected) (settings : Settings) : IO HttpTransport := do
  let timeout := settings.timeoutMillis
  let reader ← ByteSource.buffered (Blocking.recv sock 16384 timeout)
  return {
    nextChunk := do
      match ← (Network.Socket.poll sock .read timeout : IO _) with
      | .timeout => return none
      | .error e => throw e
      | .ready => return some (← (Blocking.recv sock 16384 timeout : IO _))
    reader
    sink := ResponseSink.ofSocket sock timeout reader.readSome }

/-- Handle a single HTTP connection with keep-alive support (blocking mode),
    on the calling thread.

    Through 1.8.0 this read through the C `RecvBuffer` with no timeout — an
    idle or stalled client held its thread forever — and `responseRaw`
    handlers (a WebSocket upgrade) read the socket directly, skipping bytes
    already buffered after the request. Both now go through
    `blockingTransport`. -/
def runConnection (clientSock : Socket .connected) (remoteAddr : SockAddr)
    (settings : Settings) (app : Application) : IO Unit := do
  try
    let transport ← blockingTransport clientSock settings
    Green.block (serveHttp transport remoteAddr settings app) (← Std.CancellationToken.new)
  catch e =>
    settings.settingsOnException (some remoteAddr)
    IO.eprintln s!"Server: connection error from {remoteAddr}: {e}"
  finally
    let _ ← Network.Socket.close clientSock

/-- Accept loop (blocking mode): blocking accept + forkIO per connection.
    A `while true` loop, not self-recursion — no `partial` needed. -/
def acceptLoop (serverSock : Socket .listening) (settings : Settings)
    (app : Application) : IO Unit := do
  while true do
    let (clientSock, remoteAddr) ← Network.Socket.Blocking.accept serverSock (timeoutMillis := 0)
    let _tid ← Control.Concurrent.forkIO (runConnection clientSock remoteAddr settings app)
    pure ()

/-- Run a WAI application with the given settings (blocking mode, default).
    Maximum throughput for I/O-bound workloads. -/
def runSettings (settings : Settings) (app : Application) : IO Unit := do
  let serverSock ← Network.Socket.listenTCP
    settings.settingsHost settings.settingsPort settings.settingsBacklog
  try
    settings.settingsBeforeMainLoop
    acceptLoop serverSock settings app
  finally
    let _ ← Network.Socket.close serverSock

-- ══════════════════════════════════════════════════════════════
-- EventDispatcher mode (high-concurrency, non-blocking)
-- ══════════════════════════════════════════════════════════════

/-- The transport of a connected socket in EventDispatcher mode: heads are
    received on the green thread (`recvFor`, suspending it), bodies from the
    application's `IO` through the dispatcher (`recvAwait`: `IO.wait`, which
    the task manager compensates for, so a slow body does not starve the
    pool as a `poll` would), responses through `ResponseSink.ofSocketEL`.
    Every wait is at most `settingsTimeout`. -/
def eventLoopTransport (sock : Socket .connected) (settings : Settings)
    (disp : EventDispatcher) : IO HttpTransport := do
  let timeout := settings.timeoutMillis
  let reader ← ByteSource.buffered (disp.recvAwait sock timeout)
  return {
    nextChunk := disp.recvFor sock timeout
    reader
    sink := ResponseSink.ofSocketEL sock disp timeout reader.readSome }

/-- Handle a single HTTP connection (EventDispatcher mode).

    Through 1.8.0 this parsed from the C `RecvBuffer`, whose reads fail after
    a few `EAGAIN` retries — a head or body split across packets dropped the
    connection — and it waited for readability before each request even when
    the next one was already buffered, so pipelined requests hung. -/
def runConnectionEL (clientSock : Socket .connected) (remoteAddr : SockAddr)
    (settings : Settings) (app : Application) (disp : EventDispatcher) : Green Unit := do
  try
    let transport ← (eventLoopTransport clientSock settings disp : IO _)
    serveHttp transport remoteAddr settings app
  catch e =>
    (settings.settingsOnException (some remoteAddr) : IO _)
    (IO.eprintln s!"Server: connection error from {remoteAddr}: {e}" : IO _)
  finally
    let _ ← (Network.Socket.close clientSock : IO _)

/-- Accept loop (EventDispatcher mode): try accept first, wait only on wouldBlock.

    Checks cancellation at the top of every iteration — not just relevant
    between connections, but also the exit path *out of* a `waitReadable`
    wait with no pending connection: `disp.shutdown` wakes that wait with a
    plain `()` (see `EventDispatcher.shutdown`), and this check is what turns
    that wake-up into the loop actually stopping, instead of looping straight
    back into another `accept`/`waitReadable` on a socket that's being torn
    down underneath it. -/
def acceptLoopEL (serverSock : Socket .listening) (settings : Settings)
    (app : Application) (disp : EventDispatcher) : Green Unit := do
  while true do
    Control.Concurrent.Green.Green.checkCancelled
    match ← (Network.Socket.accept serverSock : IO _) with
    | .accepted clientSock remoteAddr =>
      let _ ← (Control.Concurrent.forkGreen
        (runConnectionEL clientSock remoteAddr settings app disp) : IO _)
    | .wouldBlock =>
      -- No pending connections — wait for readability then retry
      disp.waitReadable serverSock
    | .error _ =>
      -- e.g. `EMFILE`: the listener stays readable, so retrying at once
      -- would spin; back off briefly.
      Control.Concurrent.Green.Green.sleep 10

/-- Run a WAI application with non-blocking EventDispatcher mode.
    Better for high-concurrency scenarios with many idle connections. -/
def runSettingsEventLoop (settings : Settings) (app : Application) : IO Unit := do
  let serverSock ← Network.Socket.listenTCP
    settings.settingsHost settings.settingsPort settings.settingsBacklog
  Network.Socket.setNonBlocking serverSock
  let disp ← Network.Socket.EventDispatcher.create
  let token ← Std.CancellationToken.new
  try
    settings.settingsBeforeMainLoop
    Green.block (acceptLoopEL serverSock settings app disp) token
  finally
    disp.shutdown
    let _ ← Network.Socket.close serverSock

end Network.WebApp.Server
