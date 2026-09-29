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
  `TLS.write` for responses.

  Through 1.8.0 this module ran the handshake and then parsed and answered
  requests on the raw socket, bypassing the session entirely, so no HTTPS
  request could succeed; nothing tested it. It is now tested end to end
  with a real TLS client.

  Why not the EventDispatcher: `Network.TLS.acceptSocketNB` frees its
  `SSL` object whenever the handshake would block, so a handshake needing
  more than one read — every real one — cannot be resumed. A blocking
  handshake on a per-connection thread is correct today.

  ## What is not supported (say it loudly)

  - **HTTP/2.** The server speaks HTTP/1.1 only, so it does not answer ALPN:
    a client offering `h2, http/1.1` falls back to HTTP/1.1. (It used to
    call `Network.TLS.setAlpn`, which *prefers `h2`* — every browser would
    have negotiated a protocol this server cannot speak.)
  - **`OnInsecure.allowInsecure`.** Serving plain HTTP on the TLS port needs
    to peek at the first byte, which is not implemented. `runTLS` refuses to
    start with it rather than silently behaving as `denyInsecure`. With
    `denyInsecure` a plaintext connection fails the handshake and is closed;
    its message is not sent.

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

/-- How to handle non-TLS (plain HTTP) connections.
    Only `denyInsecure` is supported — see the module header. -/
inductive OnInsecure where
  /-- Refuse plain HTTP: the connection fails the handshake and is closed.
      The message is **not** sent. -/
  | denyInsecure (message : String)
  /-- Serve plain HTTP on the TLS port. **Not implemented**: `runTLS`
      refuses to start with it. -/
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

/-- The sink writing a response through a TLS session. -/
def tlsSink (session : TLSSession) (reader : BufferedSource) : ResponseSink where
  send bytes := do (Network.TLS.write session bytes : IO _)
  sendIO := Network.TLS.write session
  sendFile path part := Network.Sendfile.sendFileWith (Network.TLS.write session) path part
  rawRecv := reader.readSome
  rawSend := Network.TLS.write session

/-- Serve one connection: the TLS handshake, then HTTP/1.1 requests through
    the session, with keep-alive, until either side closes. Every error ends
    the connection only, reported through `settingsOnException`. -/
def tlsConnection (ctx : TLSContext) (clientSock : Socket .connected)
    (remoteAddr : SockAddr) (settings : Settings) (app : Application) : IO Unit := do
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
    (app : Application) (stop : Std.CancellationToken) : IO Unit := do
  while !(← stop.isCancelled) do
    let (clientSock, remoteAddr) ← Network.Socket.Blocking.accept serverSock (timeoutMillis := 0)
    if ← stop.isCancelled then
      let _ ← Network.Socket.close clientSock
    else
      let _tid ← Control.Concurrent.forkIO (tlsConnection ctx clientSock remoteAddr settings app)

/-- Run a web application with TLS on the given port. Throws at startup for
    `OnInsecure.allowInsecure`, which is not implemented.
    $$\text{runTLS} : \text{TLSSettings} \to \text{Settings} \to \text{Application} \to \text{IO}()$$ -/
def runTLS (tlsSettings : TLSSettings) (settings : Settings)
    (app : Application) : IO Unit := do
  if tlsSettings.onInsecure == .allowInsecure then
    throw (IO.userError
      "Server.TLS: OnInsecure.allowInsecure is not implemented (plain HTTP on the TLS port)")
  let (certPath, keyPath) := match tlsSettings.certSettings with
    | .certFile c k => (c, k)
  let ctx ← Network.TLS.createContext certPath keyPath
  let serverSock ← Network.Socket.listenTCP
    settings.settingsHost settings.settingsPort settings.settingsBacklog
  try
    settings.settingsBeforeMainLoop
    runTLSSocket ctx serverSock settings app (← Std.CancellationToken.new)
  finally
    let _ ← Network.Socket.close serverSock

end Network.WebApp.Server.TLS
