/-
  Tests for `Linen.Network.WebApp.Server.WebSockets`.
-/
import Linen.Network.WebApp.Server.WebSockets

open Network.WebApp.Server.WebSockets
open Network.WebApp
open Network.HTTP.Types
open Network.WebSockets

namespace Tests.Network.WebApp.Server.WebSockets

/-- A minimal request builder, varying only the headers. -/
private def mkReq (headers : RequestHeaders) : Request where
  requestMethod := .standard .GET
  httpVersion := http11
  rawPathInfo := "/ws"
  rawQueryString := ""
  requestHeaders := headers
  isSecure := false
  remoteHost := { host := "127.0.0.1", port := 0 }
  pathInfo := ["ws"]
  queryString := []
  requestBody := pure ByteArray.empty
  vault := Data.Vault.empty
  requestBodyLength := .knownLength 0
  requestHeaderHost := none
  requestHeaderRange := none
  requestHeaderReferer := none
  requestHeaderUserAgent := none

private def upgradeHeaders (key : Option String) : RequestHeaders :=
  [(Data.CI.mk' "Upgrade", "websocket")] ++
    key.elim [] (fun k => [(Data.CI.mk' "Sec-WebSocket-Key", k)])

/-! ### `isWebSocketsReq` -/

#guard isWebSocketsReq (mkReq (upgradeHeaders (some "abc"))) == true
#guard isWebSocketsReq (mkReq []) == false
#guard isWebSocketsReq (mkReq [(Data.CI.mk' "Upgrade", "h2c")]) == false

/-! ### `websocketsApp` -/

private def dummyWsApp : ServerApp := fun _pending => pure ()

#guard (websocketsApp defaultConnectionOptions dummyWsApp (mkReq [])).isNone
#guard (websocketsApp defaultConnectionOptions dummyWsApp (mkReq (upgradeHeaders none))).isNone

-- A valid upgrade request produces a `.responseRaw` whose fallback is a 500.
#guard match websocketsApp defaultConnectionOptions dummyWsApp (mkReq (upgradeHeaders (some "abc"))) with
  | some resp => resp.status == status500 && resp.headers == []
  | none => false

#guard match websocketsApp defaultConnectionOptions dummyWsApp (mkReq (upgradeHeaders (some "abc"))) with
  | some (.responseRaw _ _) => true
  | _ => false

/-! ### `websocketsOr` — signature (the raw handler needs a live connection) -/

example : ConnectionOptions → ServerApp → Application → Application := websocketsOr

end Tests.Network.WebApp.Server.WebSockets
