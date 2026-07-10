/-
  Tests for `Linen.Network.WebApp.Server.WithApplication`.

  End-to-end: `withApplication` starts a real server on an OS-assigned
  port, hands back the *actual* bound port (the `getSockName` fix this
  module documents), and a real TCP client round-trips an HTTP/1.1 request
  against it.
-/
import Linen.Network.WebApp.Server.WithApplication
import Linen.Network.Socket.Blocking

open Network.WebApp.Server
open Network.WebApp
open Network.HTTP.Types
open Network.Socket
open Network.Socket.Blocking
open Control.Concurrent.Green

namespace Tests.Network.WebApp.Server.WithApplication

/-- Always responds 200 with a fixed body. -/
def helloApp : Application :=
  fun _req respond => AppM.respondIO respond (pure (responseLBS status200 [] "hi"))

/-- Read from `conn` until the peer closes (or `Connection: close` finishes
the response), concatenating everything received. -/
def recvUntilClosed (conn : Socket .connected) : IO ByteArray := do
  let mut received := ByteArray.empty
  let mut more := true
  while more do
    let chunk ← Blocking.recv conn
    if chunk.isEmpty then
      more := false
    else
      received := received ++ chunk
  pure received

#eval show IO Unit from do
  withApplication (pure helloApp) fun port => do
    -- The reported port must be the OS-assigned one, not the literal 0
    -- passed to `listenTCP` (a source bug, fixed here via `getSockName`).
    assert! port != 0
    let client ← socket .inet .stream
    let connected ← Blocking.connect client { host := "127.0.0.1", port := port }
    Blocking.sendAll connected
      "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n".toUTF8
    let response ← recvUntilClosed connected
    let _ ← close connected
    let text := String.fromUTF8! response
    assert! text.startsWith "HTTP/1.1 200"
    assert! (text.splitOn "hi").length > 1

end Tests.Network.WebApp.Server.WithApplication
