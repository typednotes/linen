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

#eval show IO Unit from do
  withApplication (pure app) fun port => do
    let reply ← Network.WebSockets.Client.runClient "127.0.0.1" port "/" fun conn => do
      conn.sendText "hello"
      conn.receiveText
    assert! reply == "hello-echo"

end Tests.Network.WebSockets.Client
