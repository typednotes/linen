/-
  Tests for `Linen.Network.WebApp.Server.TLS`.

  End to end over real TLS: `runTLSSocket` serves on a loopback port with the
  shared test certificate, and a real TLS client (`Network.TLS.connectSocket`,
  trusting that certificate as its CA) sends HTTP/1.1 requests through the
  session. Through 1.8.0 the server parsed requests from the raw socket after
  the handshake, so every one of these failed; nothing tested it.
-/
import Linen.Network.WebApp.Server.TLS
import LinenTest.Linen.Network.TLS.TestSupport

open Network.WebApp.Server.TLS
open Network.WebApp (Application AppM responseLBS)
open Network.HTTP.Types
open Network.Socket
open Tests.Network.TLS.TestSupport (withTestCert)

namespace Tests.Network.WebApp.Server.TLS

/-! ### Settings -/

#guard OnInsecure.allowInsecure == OnInsecure.allowInsecure
#guard OnInsecure.denyInsecure "nope" == OnInsecure.denyInsecure "nope"
#guard OnInsecure.allowInsecure != OnInsecure.denyInsecure "nope"

private def sampleTLSSettings : TLSSettings where
  certSettings := .certFile "cert.pem" "key.pem"

#guard sampleTLSSettings.onInsecure == OnInsecure.denyInsecure "This server requires HTTPS"

-- `allowInsecure` is not implemented, so `runTLS` refuses to start with it —
-- before touching the (here nonexistent) certificate files.
#eval show IO Unit from do
  let settings := { sampleTLSSettings with onInsecure := .allowInsecure }
  let app : Application := fun _ respond => AppM.respond respond (responseLBS status200 [] "")
  try
    runTLS settings Network.WebApp.Server.defaultSettings app
    throw (IO.userError "runTLS started with allowInsecure")
  catch e =>
    unless (toString e).startsWith "Server.TLS: OnInsecure.allowInsecure is not implemented" do
      throw e

/-! ### End to end over TLS -/

/-- Echoes the method, path, `isSecure` and the whole body. -/
private def echoApp : Application := fun req respond =>
  AppM.respondIO respond do
    let mut body := ByteArray.empty
    for _ in [0:10000] do
      let piece ← req.requestBody
      if piece.isEmpty then break
      body := body ++ piece
    pure (responseLBS status200 []
      s!"{req.requestMethod} {req.rawPathInfo} secure={req.isSecure} body={String.fromUTF8! body};")

/-- Read from the session until `done` holds of what was received, or the
    peer closes; bounded so a test cannot hang the build. -/
private def readUntil (session : Network.TLS.TLSSession) (done : String → Bool) : IO String := do
  let mut received := ByteArray.empty
  for _ in [0:1000] do
    if done (String.fromUTF8! received) then break
    let piece ← Network.TLS.read session 16384
    if piece.isEmpty then break
    received := received ++ piece
  pure (String.fromUTF8! received)

private def contains (s part : String) : Bool := (s.splitOn part).length > 1

/-- Run a TLS server for `app` on a loopback port, run `client` against the
    port, then stop the server (cancel, and connect once so the blocked
    `accept` returns and sees it). -/
private def withTLSServer (certPath keyPath : String) (app : Application)
    (client : UInt16 → IO α) : IO α := do
  let ctx ← Network.TLS.createContext certPath keyPath
  let server ← listenTCP "127.0.0.1" 0
  let port := (← getSockName server).port
  let stop ← Std.CancellationToken.new
  let serverTask ← IO.asTask (prio := .dedicated)
    (runTLSSocket ctx server Network.WebApp.Server.defaultSettings app stop)
  try
    client port
  finally
    stop.cancel .cancel
    let wake ← Blocking.connect (← socket .inet .stream) { host := "127.0.0.1", port }
    let _ ← close wake
    let _ ← IO.wait serverTask
    let _ ← close server

/-- A TLS client connection to `port`, verifying the test certificate. -/
private def tlsConnect (certPath : String) (port : UInt16) :
    IO (Socket .connected × Network.TLS.TLSSession) := do
  let clientCtx ← Network.TLS.createClientContextWithCA certPath
  let conn ← Blocking.connect (← socket .inet .stream) { host := "127.0.0.1", port }
  let session ← Network.TLS.connectSocket clientCtx conn.raw "localhost"
  pure (conn, session)

-- Three requests on one kept-alive TLS connection: a GET, a chunked POST
-- whose body looks like a request, and a final GET that closes. Each answer
-- must be to the request the client sent, and the app must see `isSecure`.
#eval show IO Unit from withTestCert fun certPath keyPath => do
  withTLSServer certPath keyPath echoApp fun port => do
    let (conn, session) ← tlsConnect certPath port
    Network.TLS.write session "GET /first HTTP/1.1\r\nHost: localhost\r\n\r\n".toUTF8
    let first ← readUntil session (contains · ";")
    unless first.startsWith "HTTP/1.1 200" && contains first "GET /first secure=true body=;" do
      throw (IO.userError s!"first: {repr first}")
    Network.TLS.write session
      ("POST /upload HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n" ++
       "5\r\nhello\r\n18\r\nGET /smuggled HTTP/1.1\r\n\r\n0\r\n\r\n").toUTF8
    let second ← readUntil session (contains · ";")
    unless contains second "POST /upload secure=true body=helloGET /smuggled HTTP/1.1\r\n;" do
      throw (IO.userError s!"second: {repr second}")
    Network.TLS.write session
      "GET /third HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n".toUTF8
    let third ← readUntil session (fun _ => false)
    Network.TLS.close session
    let _ ← close conn
    unless contains third "GET /third secure=true body=;" && !contains third "/smuggled" do
      throw (IO.userError s!"third: {repr third}")

-- A request split across TLS records, byte by byte, is reassembled.
#eval show IO Unit from withTestCert fun certPath keyPath => do
  withTLSServer certPath keyPath echoApp fun port => do
    let (conn, session) ← tlsConnect certPath port
    let request := "POST /split HTTP/1.1\r\nHost: localhost\r\nContent-Length: 3\r\n\
                    Connection: close\r\n\r\nabc"
    for b in request.toUTF8.toList do
      Network.TLS.write session (ByteArray.mk #[b])
    let reply ← readUntil session (fun _ => false)
    Network.TLS.close session
    let _ ← close conn
    unless contains reply "POST /split secure=true body=abc;" do
      throw (IO.userError s!"split: {repr reply}")

-- A plaintext HTTP request on the TLS port gets no HTTP answer: the
-- handshake fails and the connection is closed (`denyInsecure`), and the
-- server keeps serving TLS afterwards.
#eval show IO Unit from withTestCert fun certPath keyPath => do
  withTLSServer certPath keyPath echoApp fun port => do
    let plain ← Blocking.connect (← socket .inet .stream) { host := "127.0.0.1", port }
    Blocking.sendAll plain "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n".toUTF8
    let mut reply := ByteArray.empty
    for _ in [0:100] do
      let piece ← try Blocking.recv plain catch _ => pure ByteArray.empty
      if piece.isEmpty then break
      reply := reply ++ piece
    let _ ← close plain
    if (String.fromUTF8? reply).any (·.startsWith "HTTP/") then
      throw (IO.userError "answered plain HTTP on the TLS port")
    let (conn, session) ← tlsConnect certPath port
    Network.TLS.write session "GET /after HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n".toUTF8
    let after ← readUntil session (fun _ => false)
    Network.TLS.close session
    let _ ← close conn
    unless contains after "GET /after secure=true" do
      throw (IO.userError s!"after: {repr after}")

/-! ### Signatures -/

example : TLSSettings → Network.WebApp.Server.Settings → Application → IO Unit := runTLS

end Tests.Network.WebApp.Server.TLS
