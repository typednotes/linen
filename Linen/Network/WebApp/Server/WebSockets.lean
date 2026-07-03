/-
  Linen.Network.WebApp.Server.WebSockets — WebApp/WebSocket bridge

  Upgrade `Network.WebApp` requests to WebSocket connections.

  Ports Hale's `Network.Wai.Handler.WebSockets`, renamed from the
  Haskell-specific `WaiWebSockets` to `Server.WebSockets` per this project's
  naming convention.
-/
import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header
import Linen.Network.HTTP.Types.Status
import Linen.Network.WebSockets

namespace Network.WebApp.Server.WebSockets

open Network.WebApp
open Network.HTTP.Types
open Network.WebSockets

/-- Check if a WebApp request is a WebSocket upgrade request. -/
def isWebSocketsReq (req : Request) : Bool :=
  let upgrade := req.requestHeaders.find? (fun (n, _) => n == Data.CI.mk' "Upgrade")
    |>.map (·.2)
  upgrade.any (·.toLower == "websocket")

/-- Try to upgrade a WebApp request to a WebSocket connection.
    Returns `some response` (a raw response that performs the handshake)
    if the request is a WebSocket upgrade, `none` otherwise. -/
def websocketsApp (_opts : ConnectionOptions) (wsApp : ServerApp)
    (req : Request) : Option Response :=
  if !isWebSocketsReq req then none
  else
    let clientKey := req.requestHeaders.find?
      (fun (n, _) => n == Data.CI.mk' "Sec-WebSocket-Key") |>.map (·.2)
    match clientKey with
    | none => none
    | some key =>
      some (.responseRaw (fun recv send => do
        -- Send handshake response
        let handshakeResp := buildHandshakeResponse key
        send handshakeResp.toUTF8
        -- Create WebSocket connection
        let conn ← mkConnection send recv
        let reqHead : RequestHead := {
          path := req.rawPathInfo
          headers := req.requestHeaders.map fun (n, v) => (toString n, v)
        }
        let pending : PendingConnection := {
          request := reqHead
          acceptIO := pure conn
        }
        wsApp pending)
      -- Fallback response (never used when raw is supported)
      (.responseBuilder status500 [] "WebSocket upgrade failed".toUTF8))

/-- Combine a WebSocket app with a regular WebApp app.
    WebSocket requests go to the WS app, everything else to the backup.
    $$\text{websocketsOr} : \text{ConnectionOptions} \to \text{ServerApp} \to \text{Application} \to \text{Application}$$ -/
def websocketsOr (opts : ConnectionOptions) (wsApp : ServerApp)
    (backup : Application) : Application :=
  fun req respond =>
    match websocketsApp opts wsApp req with
    | some resp => AppM.respond respond resp
    | none => backup req respond

end Network.WebApp.Server.WebSockets
