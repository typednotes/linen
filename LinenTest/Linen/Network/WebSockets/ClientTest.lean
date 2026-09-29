/-
  Tests for `Linen.Network.WebSockets.Client`.

  End-to-end: a real WebSocket echo server (built from
  `Network.WebApp.Server.WebSockets.websocketsOr`, running on an
  OS-assigned port via `withApplication`) is exercised by `Client.runClient`
  performing a real client-side opening handshake and a text round-trip.
-/
import Linen.Network.WebSockets.Client
import Linen.Network.WebApp.Server.WebSockets
import Linen.Network.WebApp.Server.WithApplication

open Network.WebSockets
open Network.WebApp
open Network.WebApp.Server
open Network.WebApp.Server.WebSockets
open Network.HTTP.Types

namespace Tests.Network.WebSockets.Client

/-- Echoes a single received text message back with a `"-echo"` suffix. -/
private def echoApp : ServerApp := fun pending => do
  let conn ← pending.acceptIO
  let msg ← conn.receiveText
  conn.sendText (msg ++ "-echo")

private def notFound : Application :=
  fun _req respond => AppM.respond respond (.responseBuilder status404 [] ByteArray.empty)

private def app : Application :=
  websocketsOr defaultConnectionOptions echoApp notFound

/-! ### `checkHandshakeResponse` — RFC 6455 §4.1's client-side checks -/

open _root_.Network.WebSockets.Client (checkHandshakeResponse)

private def rfcKey : String := "dGhlIHNhbXBsZSBub25jZQ=="

private def ok? : Except String Unit → Bool
  | .ok () => true
  | .error _ => false

-- The server response from RFC 6455 §1.3 is accepted.
#guard ok? (checkHandshakeResponse rfcKey
  "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\
   Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
-- Header names and the `Upgrade`/`Connection` values are case-insensitive;
-- `Connection` may list several tokens.
#guard ok? (checkHandshakeResponse rfcKey
  "HTTP/1.1 101 Switching Protocols\r\nupgrade: WebSocket\r\nCONNECTION: keep-alive, upgrade\r\n\
   sec-websocket-accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
-- Our own server's response is accepted by our own client.
#guard ok? (checkHandshakeResponse rfcKey
  ((buildHandshakeResponse rfcKey).dropEnd 4).toString)

-- An accept key for a different nonce is rejected — the check that was
-- skipped while the handshake's SHA-1 was a placeholder.
#guard !ok? (checkHandshakeResponse "x3JJHMbDL1EzLkh9GBhXDw=="
  "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\
   Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
-- A missing accept header, a non-101 status, and missing Upgrade/Connection.
#guard !ok? (checkHandshakeResponse rfcKey
  "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade")
#guard !ok? (checkHandshakeResponse rfcKey
  "HTTP/1.1 200 OK\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\
   Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
#guard !ok? (checkHandshakeResponse rfcKey
  "HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\n\
   Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
#guard !ok? (checkHandshakeResponse rfcKey
  "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n\
   Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")

/-! ### End to end -/

-- The handshake now verifies the server's accept key, so this round trip
-- also checks that server and client agree on it.
#eval show IO Unit from do
  withApplication (pure app) fun port => do
    let reply ← Network.WebSockets.Client.runClient "127.0.0.1" port "/" fun conn => do
      conn.sendText "hello"
      conn.receiveText
    assert! reply == "hello-echo"

end Tests.Network.WebSockets.Client
