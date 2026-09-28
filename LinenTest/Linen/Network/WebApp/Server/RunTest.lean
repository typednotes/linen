/-
  Tests for `Linen.Network.WebApp.Server.Run`.

  `connAction` is pure and checked with `#guard` against hand-built
  `Request` values. The accept-loop / connection-handling entry points need
  a live socket and `EventDispatcher`, so they are pinned at the type level.
-/
import Linen.Network.WebApp.Server.Run

open Network.WebApp.Server
open Network.WebApp (Request RequestBodyLength Application)
open Network.HTTP.Types
open Network.Socket (Socket SockAddr EventDispatcher)
open Control.Concurrent.Green (Green)

namespace Tests.Network.WebApp.Server.Run

/-- A minimal request builder, varying only version and the `Connection` header. -/
private def mkReq (version : HttpVersion) (connHeader : Option String) : Request where
  requestMethod := .standard .GET
  httpVersion := version
  rawPathInfo := "/"
  rawQueryString := ""
  requestHeaders := connHeader.elim [] (fun v => [(hConnection, v)])
  isSecure := false
  remoteHost := { host := "127.0.0.1", port := 0 }
  pathInfo := []
  queryString := []
  requestBody := pure ByteArray.empty
  vault := Data.Vault.empty
  requestBodyLength := .knownLength 0
  requestHeaderHost := none
  requestHeaderRange := none
  requestHeaderReferer := none
  requestHeaderUserAgent := none

#guard connAction (mkReq http11 none) == .keepAlive
#guard connAction (mkReq http11 (some "close")) == .close
#guard connAction (mkReq http10 none) == .close
#guard connAction (mkReq http10 (some "keep-alive")) == .keepAlive

example (req : Request) (hVer : (req.httpVersion == http11) = false)
    (hNoConn : req.requestHeaders.find? (fun (n, _) => n == hConnection) = none) :
    connAction req = .close := connAction_http10_default req hVer hNoConn

example (req : Request) (hVer : (req.httpVersion == http11) = true)
    (hNoConn : req.requestHeaders.find? (fun (n, _) => n == hConnection) = none) :
    connAction req = .keepAlive := connAction_http11_default req hVer hNoConn

/-! ### IO / Green entry points — signatures (need a live socket) -/

example : Socket .connected → SockAddr → Settings → Application → IO Unit := runConnection
example : Socket .listening → Settings → Application → IO Unit := acceptLoop
example : Settings → Application → IO Unit := runSettings
example : Socket .connected → SockAddr → Settings → Application → EventDispatcher → Green Unit :=
  runConnectionEL
example : Socket .listening → Settings → Application → EventDispatcher → Green Unit := acceptLoopEL
example : Settings → Application → IO Unit := runSettingsEventLoop

end Tests.Network.WebApp.Server.Run
