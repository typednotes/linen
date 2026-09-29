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

/-! ### End to end, in both modes

Every test below runs against the blocking server (`runConnection`, a thread
per connection) and the event-loop server (`withApplication`, which runs
`acceptLoopEL`), since they now share one HTTP/1.1 loop (`serveHttp`) over
different transports. -/

inductive Mode where
  | blocking
  | eventLoop
deriving Repr

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

/-- `settings` with `settingsTimeout := seconds`. -/
private def withTimeout (seconds : Nat) (h : seconds > 0 := by decide) : Settings :=
  { defaultSettings with settingsTimeout := seconds, settingsTimeoutPos := h }

/-- Run `app` in `mode` on a loopback port for the duration of `client`. -/
private def withServer (mode : Mode) (app : Application) (client : UInt16 → IO α)
    (settings : Settings := defaultSettings) : IO α := do
  match mode with
  | .eventLoop => withApplicationSettings settings (pure app) client
  | .blocking =>
    let server ← Network.Socket.listenTCP "127.0.0.1" 0
    let port := (← Network.Socket.getSockName server).port
    let stop ← Std.CancellationToken.new
    let loop ← IO.asTask (prio := .dedicated) do
      while !(← stop.isCancelled) do
        let (conn, addr) ← Network.Socket.Blocking.accept server (timeoutMillis := 0)
        if ← stop.isCancelled then
          let _ ← Network.Socket.close conn
        else
          let _ ← Control.Concurrent.forkIO (runConnection conn addr settings app)
    try
      client port
    finally
      stop.cancel .cancel
      let wake ← Network.Socket.Blocking.connect (← Network.Socket.socket .inet .stream)
        { host := "127.0.0.1", port }
      let _ ← Network.Socket.close wake
      let _ ← IO.wait loop
      let _ ← Network.Socket.close server

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

private def bothModes (test : Mode → IO Unit) : IO Unit := do
  for mode in [Mode.blocking, .eventLoop] do
    try test mode catch e => throw (IO.userError s!"{repr mode}: {e}")

-- Pipelining: three requests in one write. The second and third are already
-- buffered when the first is answered; through 1.8.0 the event loop then
-- waited for the socket to become readable, which it never did, and hung.
#eval bothModes fun mode => withServer mode echoApp fun port => do
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
-- 1.8.0 the event loop's C `RecvBuffer` gave up after a few `EAGAIN` retries
-- mid-request and the connection was dropped.
#eval bothModes fun mode => withServer mode echoApp fun port => do
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
#eval bothModes fun mode => withServer mode echoApp fun port => do
  let conn ← connectTo port
  let body := String.ofList (List.replicate 300000 'z')
  Network.Socket.Blocking.sendAll conn
    (s!"POST /big HTTP/1.1\r\nHost: x\r\nContent-Length: {body.length}\r\n" ++
     "Connection: close\r\n\r\n" ++ body).toUTF8
  let reply ← recvAll conn
  let _ ← Network.Socket.close conn
  unless contains reply s!"POST /big body={body};" do
    throw (IO.userError s!"big body: {reply.length} bytes of reply")

-- Timeouts. An idle kept-alive connection is closed once `settingsTimeout`
-- passes — quietly, with no response — as is a client that never finishes
-- its head. Through 1.8.0 the blocking server never gave up on either: the
-- connection held its thread forever.
#eval bothModes fun mode => withServer mode echoApp (settings := withTimeout 1) fun port => do
  for opening in ["GET /idle HTTP/1.1\r\nHost: x\r\n\r\n", "GET /never-finished HTTP/1.1\r\nHo"] do
    let conn ← connectTo port
    Network.Socket.Blocking.sendAll conn opening.toUTF8
    let t0 ← IO.monoMsNow
    let got ← recvAll conn
    let waited := (← IO.monoMsNow) - t0
    let _ ← Network.Socket.close conn
    let answered := contains got "GET /idle body=;"
    unless answered == (opening.endsWith "\r\n\r\n") && waited ≥ 900 && waited < 5000 do
      throw (IO.userError s!"{repr opening}: closed after {waited} ms, got {repr got}")

-- A body that stalls: the application's read times out (an error it sees),
-- and the connection is closed without a response.
#eval bothModes fun mode => withServer mode echoApp (settings := withTimeout 1) fun port => do
  let conn ← connectTo port
  Network.Socket.Blocking.sendAll conn
    "POST /stall HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\n012".toUTF8
  let t0 ← IO.monoMsNow
  let got ← recvAll conn
  let waited := (← IO.monoMsNow) - t0
  let _ ← Network.Socket.close conn
  unless !contains got "200" && waited ≥ 900 && waited < 5000 do
    throw (IO.userError s!"stalled body: closed after {waited} ms, got {repr got}")

open _root_.Network.WebApp (AppM Response) in
/-- Takes the connection over (as a WebSocket upgrade does) and echoes the
    first bytes it receives. -/
private def rawApp : Application := fun _req respond =>
  AppM.respond respond (.responseRaw (fun recv send => do
      send "raw:".toUTF8
      send (← recv))
    (.responseBuilder status500 [] ByteArray.empty))

-- A `responseRaw` handler sees bytes the client sent right behind the head,
-- in the same packet — already in the server's buffer. Through 1.8.0 the
-- blocking server's handler read the socket directly and never saw them.
#eval bothModes fun mode => withServer mode rawApp fun port => do
  let conn ← connectTo port
  Network.Socket.Blocking.sendAll conn "GET /ws HTTP/1.1\r\nHost: x\r\n\r\nEARLY-FRAME".toUTF8
  let mut got := ByteArray.empty
  for _ in [0:10] do
    if got.size ≥ 15 then break
    got := got ++ (← Network.Socket.Blocking.recv conn 64)
  let _ ← Network.Socket.close conn
  unless String.fromUTF8! got == "raw:EARLY-FRAME" do
    throw (IO.userError s!"raw handler saw {repr (String.fromUTF8! got)}")

/-! ### IO / Green entry points — signatures (need a live socket) -/

example : Socket .connected → SockAddr → Settings → Application → IO Unit := runConnection
example : Socket .listening → Settings → Application → IO Unit := acceptLoop
example : Settings → Application → IO Unit := runSettings
example : Socket .connected → SockAddr → Settings → Application → EventDispatcher → Green Unit :=
  runConnectionEL
example : Socket .listening → Settings → Application → EventDispatcher → Green Unit := acceptLoopEL
example : Settings → Application → IO Unit := runSettingsEventLoop

end Tests.Network.WebApp.Server.Run
