/-
  Tests for `Linen.Network.WebApp.Server.Run`.

  `connAction` is pure and checked with `#guard` against hand-built
  `Request` values. The accept-loop / connection-handling entry points need
  a live socket and `EventDispatcher`, so they are pinned at the type level.
-/
import Linen.Network.WebApp.Server.Run
import Linen.Network.WebApp.Server.WithApplication
import Linen.Network.Socket.Blocking

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

/-! ### Event-loop mode, end to end (`withApplication` runs `acceptLoopEL`) -/

open _root_.Network.WebApp (AppM responseLBS) in
/-- Answers with the method, path and whole body. -/
private def echoApp : Application := fun req respond =>
  AppM.respondIO respond do
    let mut body := ByteArray.empty
    for _ in [0:10000] do
      let piece ← req.requestBody
      if piece.isEmpty then break
      body := body ++ piece
    pure (responseLBS status200 []
      s!"{req.requestMethod} {req.rawPathInfo} body={String.fromUTF8! body};")

/-- Everything the server sends until it closes. -/
private def recvAll (conn : Socket .connected) : IO String := do
  let mut received := ByteArray.empty
  for _ in [0:1000] do
    let piece ← try Network.Socket.Blocking.recv conn catch _ => pure ByteArray.empty
    if piece.isEmpty then break
    received := received ++ piece
  pure (String.fromUTF8! received)

private def contains (s part : String) : Bool := (s.splitOn part).length > 1

private def connectTo (port : UInt16) : IO (Socket .connected) := do
  Network.Socket.Blocking.connect (← Network.Socket.socket .inet .stream)
    { host := "127.0.0.1", port }

-- Pipelining: three requests in one write. The second and third are already
-- buffered when the first is answered; through 1.8.0 the loop then waited
-- for the socket to become readable, which it never did, and hung.
#eval show IO Unit from do
  withApplication (pure echoApp) fun port => do
    let conn ← connectTo port
    Network.Socket.Blocking.sendAll conn
      ("GET /one HTTP/1.1\r\nHost: x\r\n\r\n" ++
       "POST /two HTTP/1.1\r\nHost: x\r\nContent-Length: 3\r\n\r\nabc" ++
       "GET /three HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n").toUTF8
    let replies ← recvAll conn
    let _ ← Network.Socket.close conn
    unless contains replies "GET /one body=;" && contains replies "POST /two body=abc;" &&
        contains replies "GET /three body=;" do
      throw (IO.userError s!"pipelined: {repr replies}")

-- A head and a body that arrive in pieces, with pauses between them. Through
-- 1.8.0 the C `RecvBuffer` gave up after a few `EAGAIN` retries mid-request
-- and the connection was dropped.
#eval show IO Unit from do
  withApplication (pure echoApp) fun port => do
    let conn ← connectTo port
    for piece in ["POST /sl", "ow HTTP/1.1\r\nHost: x\r\nCon", "tent-Length: 10\r\n",
                  "Connection: close\r\n\r\n", "01234", "56789"] do
      Network.Socket.Blocking.sendAll conn piece.toUTF8
      IO.sleep 60
    let reply ← recvAll conn
    let _ ← Network.Socket.close conn
    unless contains reply "POST /slow body=0123456789;" do
      throw (IO.userError s!"split: {repr reply}")

-- A body larger than the socket buffers, read by the application while the
-- client is still sending.
#eval show IO Unit from do
  withApplication (pure echoApp) fun port => do
    let conn ← connectTo port
    let body := String.ofList (List.replicate 300000 'z')
    Network.Socket.Blocking.sendAll conn
      (s!"POST /big HTTP/1.1\r\nHost: x\r\nContent-Length: {body.length}\r\n" ++
       "Connection: close\r\n\r\n" ++ body).toUTF8
    let reply ← recvAll conn
    let _ ← Network.Socket.close conn
    unless contains reply s!"POST /big body={body};" do
      throw (IO.userError s!"big body: {reply.length} bytes of reply")

/-! ### IO / Green entry points — signatures (need a live socket) -/

example : Socket .connected → SockAddr → Settings → Application → IO Unit := runConnection
example : Socket .listening → Settings → Application → IO Unit := acceptLoop
example : Settings → Application → IO Unit := runSettings
example : Socket .connected → SockAddr → Settings → Application → EventDispatcher → Green Unit :=
  runConnectionEL
example : Socket .listening → Settings → Application → EventDispatcher → Green Unit := acceptLoopEL
example : Settings → Application → IO Unit := runSettingsEventLoop

end Tests.Network.WebApp.Server.Run
