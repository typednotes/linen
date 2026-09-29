/-
  Linen.Network.WebApp.Server.TLS — HTTPS support for the web application server

  Provides TLS (HTTPS) support for the server using OpenSSL via FFI.

  Ports `Network.Wai.Handler.WarpTLS`, renamed from the
  Haskell-specific `WarpTLS` to `Server.TLS` per this project's naming
  convention.

  ## Design

  One thread per connection, as in `Server.runSettings`: the accept loop
  hands each connection to a dedicated thread, which performs the TLS
  handshake and then serves HTTP/1.1 requests *through the session* — a
  `ByteSource.buffered` over `TLS.read` for requests, a `ResponseSink` over
  `TLS.write` for responses. (`Network.TLS.Green` now makes an
  event-loop TLS server possible — the handshake is resumable — but this
  server does not use it yet.)

  Through 1.8.0 this module ran the handshake and then parsed and answered
  requests on the raw socket, bypassing the session entirely, so no HTTPS
  request could succeed; nothing tested it. It is now tested end to end
  with a real TLS client.

  Before the handshake, the first byte the client sends is *peeked*
  (`MSG_PEEK`, so OpenSSL still reads it): `0x16` opens a TLS handshake
  record; anything else is plain HTTP, handled per `OnInsecure` — the
  detection warp-tls uses (which consumes and replays the bytes instead).

  ## What is not supported (say it loudly)

  - **HTTP/2.** The server speaks HTTP/1.1 only, so it does not answer ALPN:
    a client offering `h2, http/1.1` falls back to HTTP/1.1. (It used to
    call `Network.TLS.setAlpn`, which *prefers `h2`* — every browser would
    have negotiated a protocol this server cannot speak.)

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
import Linen.Control.Concurrent
import Linen.Control.Concurrent.Green
import Linen.Network.WebApp.Server.Settings
import Linen.Network.WebApp.Server.Request
import Linen.Network.WebApp.Server.Response
import Linen.Network.WebApp.Server.Run

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

/-- Wait (at most `timeoutMillis`) for the client's first byte and peek at
    it, leaving it unread. -/
def peekFirstByte (sock : Socket .connected) (timeoutMillis : Nat) : IO FirstByte := do
  for _ in [0:100] do
    match ← Network.Socket.poll sock .read timeoutMillis with
    | .timeout => throw (IO.userError s!"no data from the client after {timeoutMillis}ms")
    | .error e => throw e
    | .ready =>
      match ← Network.Socket.peek sock 1 with
      | .data bytes => return if isTlsHandshakeByte (bytes.get! 0) then .tls else .plain
      | .eof => return .closed
      | .error e => throw e
      | .wouldBlock => pure ()  -- a spurious wake-up: poll again
  throw (IO.userError "the client's socket keeps waking without data")

/-- The sink writing a response through a TLS session. -/
def tlsSink (session : TLSSession) (reader : BufferedSource) : ResponseSink where
  send bytes := do (Network.TLS.write session bytes : IO _)
  sendIO := Network.TLS.write session
  sendFile path part := Network.Sendfile.sendFileWith (Network.TLS.write session) path part
  rawRecv := reader.readSome
  rawSend := Network.TLS.write session

/-- Serve one connection: the TLS handshake, then HTTP/1.1 requests through
    the session, with keep-alive, until either side closes. A connection
    that does not open with a TLS record is handled per `onInsecure`. Every
    error ends the connection only, reported through `settingsOnException`. -/
def tlsConnection (ctx : TLSContext) (clientSock : Socket .connected)
    (remoteAddr : SockAddr) (settings : Settings) (app : Application)
    (onInsecure : OnInsecure := .denyInsecure "This server requires HTTPS") : IO Unit := do
  let firstByte ← try peekFirstByte clientSock (settings.settingsTimeout * 1000)
    catch _ => pure .closed
  match firstByte, onInsecure with
  | .tls, _ => pure ()
  | .closed, _ =>
    let _ ← Network.Socket.close clientSock
    return
  | .plain, .allowInsecure =>
    -- Plain HTTP on this socket, exactly as the plain server serves it.
    return ← runConnection clientSock remoteAddr settings app
  | .plain, .denyInsecure message =>
    try
      -- Read the head first: closing with a request unread makes the
      -- kernel reset the connection, which can destroy the answer.
      let _ ← recvHeaders (← FFI.recvBufCreate clientSock.raw)
      Network.Socket.Blocking.sendAll clientSock (insecureDenial message)
    catch _ => pure ()
    let _ ← Network.Socket.close clientSock
    return
  try
    let session ← Network.TLS.acceptSocket ctx clientSock.raw
    try
      let reader ← ByteSource.buffered (Network.TLS.read session 16384)
      let sink := tlsSink session reader
      let token ← Std.CancellationToken.new
      let mut keepGoing := true
      while keepGoing do
        match ← parseRequestFrom reader.source remoteAddr with
        | none => keepGoing := false
        | some req =>
          let secureReq := { req with isSecure := true }
          let action := connAction secureReq
          let _received ← Green.block (app secureReq fun resp => do
            let resp' := if action == .close then
              resp.mapResponseHeaders ((hConnection, "close") :: ·)
            else resp
            sendResponseTo sink settings secureReq resp').run token
          if action == .keepAlive then
            drainBody secureReq
          else
            keepGoing := false
    finally
      Network.TLS.close session
  catch e =>
    settings.settingsOnException (some remoteAddr)
    IO.eprintln s!"Server.TLS: connection error from {remoteAddr}: {e}"
  finally
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
      let _tid ← Control.Concurrent.forkIO
        (tlsConnection ctx clientSock remoteAddr settings app onInsecure)

/-- Run a web application with TLS on the given port.
    $$\text{runTLS} : \text{TLSSettings} \to \text{Settings} \to \text{Application} \to \text{IO}()$$ -/
def runTLS (tlsSettings : TLSSettings) (settings : Settings)
    (app : Application) : IO Unit := do
  let (certPath, keyPath) := match tlsSettings.certSettings with
    | .certFile c k => (c, k)
  let ctx ← Network.TLS.createContext certPath keyPath
  let serverSock ← Network.Socket.listenTCP
    settings.settingsHost settings.settingsPort settings.settingsBacklog
  try
    settings.settingsBeforeMainLoop
    runTLSSocket ctx serverSock settings app (← Std.CancellationToken.new) tlsSettings.onInsecure
  finally
    let _ ← Network.Socket.close serverSock

end Network.WebApp.Server.TLS
