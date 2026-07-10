/-
  Linen.Network.WebApp.Server.QUIC — HTTP/3 / QUIC web application handler

  Bridges the web application interface with HTTP/3 over QUIC transport.
  Analogous to `Network.WebApp.Server` for HTTP/1.1+HTTP/2 over TCP, but uses
  QUIC/HTTP/3.

  Ports `Network.Wai.Handler.WarpQUIC`, renamed from the
  Haskell-specific `WarpQUIC` to `Server.QUIC` per this project's naming
  convention (matching `Network.QUIC.Server`).

  ## Design

  `runH3`/`runQUIC` create a QUIC server, accept connections, process
  HTTP/3 streams, and dispatch requests to the application.

  The flow is:
  1. Build QUIC `ServerConfig` from `Settings`
  2. Run a QUIC server accept loop
  3. For each connection: open HTTP/3 control stream, send SETTINGS
  4. For each request stream: decode QPACK headers → build request →
     call handler → encode response

  ## Guarantees

  - TLS is mandatory (QUIC always uses TLS 1.3)
  - `Settings.certFile` and `Settings.keyFile` are required (not `Option`)
  - Server socket cleanup follows try/finally pattern (inside `Network.QUIC.Server.run`)
-/

import Linen.Network.QUIC.Config
import Linen.Network.QUIC.Server
import Linen.Network.HTTP3.Server
import Linen.Network.HTTP3.Frame

namespace Network.WebApp.Server.QUIC

open Network.QUIC
open Network.HTTP3

/-- Settings for the HTTP/3 / QUIC web application server.
    $$\text{Settings} = \{ \text{port} : \text{UInt16},\; \text{certFile} : \text{String},\; \ldots \}$$ -/
structure Settings where
  /-- Port to listen on. Default: 443 (HTTPS). -/
  port : UInt16 := 443
  /-- Host to bind to. Default: all interfaces. -/
  host : String := "0.0.0.0"
  /-- Path to TLS certificate file. Required for QUIC. -/
  certFile : String
  /-- Path to TLS private key file. Required for QUIC. -/
  keyFile : String
  /-- Maximum concurrent HTTP/3 request streams per connection. Default: 100. -/
  maxConcurrentStreams : Nat := 100
  /-- QPACK maximum dynamic table capacity. Default: 4096. -/
  qpackMaxTableCapacity : Nat := 4096
  /-- QPACK maximum blocked streams. Default: 100. -/
  qpackBlockedStreams : Nat := 100
  /-- Server name for the `server` response header. -/
  serverName : String := "Linen/WebApp.Server.QUIC"
  /-- Called just before the server starts its accept loop. -/
  beforeMainLoop : IO Unit := pure ()

/-- Default settings (requires cert and key paths).
    $$\text{defaultSettings}(c, k) = \text{Settings}\{ \text{certFile} := c,\; \text{keyFile} := k \}$$ -/
def defaultSettings (certFile keyFile : String) : Settings :=
  { certFile, keyFile }

/-- Build a QUIC `ServerConfig` from `Settings`.
    $$\text{toQUICConfig} : \text{Settings} \to \text{ServerConfig}$$ -/
def toQUICConfig (settings : Settings) : ServerConfig :=
  { tlsConfig := {
      certFile := some settings.certFile
      keyFile := some settings.keyFile
      alpn := ["h3"]
    }
    transportParams := {
      initialMaxStreamsBidi := settings.maxConcurrentStreams
      initialMaxStreamsUni := settings.maxConcurrentStreams
    }
    host := settings.host
    port := settings.port
  }

/-- Build HTTP/3 settings from `Settings`.
    $$\text{toH3Settings} : \text{Settings} \to \text{H3Settings}$$ -/
def toH3Settings (settings : Settings) : H3Settings :=
  { qpackMaxTableCapacity := settings.qpackMaxTableCapacity
    qpackBlockedStreams := settings.qpackBlockedStreams
  }

/-- Convert an HTTP/3 request to a simplified header-list representation.
    $$\text{h3RequestToHeaders} : \text{H3Request} \to \text{List}(\text{String} \times \text{String})$$ -/
def h3RequestToHeaders (req : H3Request) : List (String × String) :=
  [(":method", req.method),
   (":path", req.path),
   (":scheme", req.scheme),
   (":authority", req.authority)] ++ req.headers

/-- Handle a single QUIC connection by processing HTTP/3 streams.
    $$\text{handleConnection} : \text{Settings} \to \text{Connection} \to \text{IO}(\text{Unit})$$
    Delegates to `HTTP3.handleConnection`. -/
def handleConnection (settings : Settings) (conn : Connection)
    (handler : H3Handler) : IO Unit := do
  let h3settings := toH3Settings settings
  Network.HTTP3.handleConnection conn h3settings handler

/-- Run a web application over HTTP/3 / QUIC.
    $$\text{runH3} : \text{Settings} \to \text{H3Handler} \to \text{IO}(\text{Unit})$$
    This is the main entry point for the Server.QUIC server. Creates a QUIC
    server and dispatches HTTP/3 requests to the handler. -/
def runH3 (settings : Settings) (handler : H3Handler) : IO Unit := do
  let quicConfig := toQUICConfig settings
  settings.beforeMainLoop
  Server.run quicConfig fun conn => do
    handleConnection settings conn handler

/-- Run a QUIC server with a custom QUIC config and an HTTP/3 handler.
    $$\text{runQUIC} : \text{ServerConfig} \to \text{H3Handler} \to \text{IO}(\text{Unit})$$ -/
def runQUIC (quicConfig : ServerConfig) (handler : H3Handler) : IO Unit := do
  Server.run quicConfig fun conn => do
    let h3settings := H3Settings.default
    Network.HTTP3.handleConnection conn h3settings handler

end Network.WebApp.Server.QUIC
