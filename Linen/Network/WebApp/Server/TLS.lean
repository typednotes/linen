/-
  Linen.Network.WebApp.Server.TLS — HTTPS support for the web application server

  Provides TLS (HTTPS) support for the server using OpenSSL via FFI.
  Uses the EventDispatcher and Green monad for non-blocking I/O.

  Ports `Network.Wai.Handler.WarpTLS`, renamed from the
  Haskell-specific `WarpTLS` to `Server.TLS` per this project's naming
  convention.

  ## No `partial`

  All loops here are `while` loops in `do`-notation, which desugar to the
  standard library's `Loop.forIn` combinator — no `partial def` or fuel
  parameter is used, per this project's coding conventions.

  ## Guarantees

  - Minimum TLS version enforced by the OpenSSL configuration behind `Network.TLS.Context`
  - ALPN negotiation for HTTP/2 (when enabled)
  - TLS sessions are cleaned up on connection close
  - Certificate and key are validated at startup
-/
import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.Socket
import Linen.Network.Socket.EventDispatcher
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

/-- How to handle non-TLS (plain HTTP) connections. -/
inductive OnInsecure where
  | denyInsecure (message : String)
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
  alpn : Bool := true

/-- Handle a single TLS connection using the EventDispatcher. -/
private def tlsConnection (ctx : TLSContext) (clientSock : Socket .connected)
    (remoteAddr : SockAddr) (settings : Settings) (app : Application)
    (disp : EventDispatcher) : Green Unit := do
  try
    let session ← (Network.TLS.acceptSocket ctx clientSock.raw : IO _)
    try
      let buf ← (FFI.recvBufCreate clientSock.raw : IO _)
      let mut keepGoing := true
      while keepGoing do
        disp.waitReadable clientSock
        let reqOpt ← (parseRequest buf remoteAddr : IO _)
        match reqOpt with
        | none => keepGoing := false
        | some req =>
          let secureReq := { req with isSecure := true }
          let action := connAction secureReq
          let _received ← (app secureReq fun resp => do
            let resp' := if action == .close then
              resp.mapResponseHeaders ((hConnection, "close") :: ·)
            else resp
            sendResponseEL clientSock settings secureReq resp' disp).run
          if action != .keepAlive then keepGoing := false
    finally
      (Network.TLS.close session : IO _)
  catch e =>
    (settings.settingsOnException (some remoteAddr) : IO _)
    (IO.eprintln s!"Server.TLS: connection error from {remoteAddr}: {e}" : IO _)
  finally
    let _ ← (Network.Socket.close clientSock : IO _)

/-- Accept loop for TLS connections using the EventDispatcher. -/
private def tlsAcceptLoop (ctx : TLSContext) (serverSock : Socket .listening)
    (settings : Settings) (app : Application) (disp : EventDispatcher) : Green Unit := do
  while true do
    disp.waitReadable serverSock
    match ← (Network.Socket.accept serverSock : IO _) with
    | .accepted clientSock remoteAddr =>
      let _ ← (Control.Concurrent.forkGreen
        (tlsConnection ctx clientSock remoteAddr settings app disp) : IO _)
    | .wouldBlock => pure ()
    | .error _ => pure ()

/-- Run a web application with TLS on the given port. -/
def runTLS (tlsSettings : TLSSettings) (settings : Settings)
    (app : Application) : IO Unit := do
  let (certPath, keyPath) := match tlsSettings.certSettings with
    | .certFile c k => (c, k)
  let ctx ← Network.TLS.createContext certPath keyPath
  if tlsSettings.alpn then
    Network.TLS.setAlpn ctx
  let serverSock ← Network.Socket.listenTCP
    settings.settingsHost settings.settingsPort settings.settingsBacklog
  Network.Socket.setNonBlocking serverSock
  let disp ← EventDispatcher.create
  let token ← Std.CancellationToken.new
  try
    settings.settingsBeforeMainLoop
    Green.block (tlsAcceptLoop ctx serverSock settings app disp) token
  finally
    disp.shutdown
    let _ ← Network.Socket.close serverSock

end Network.WebApp.Server.TLS
