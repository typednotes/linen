/-
  Tests for `Linen.CDP.Runtime`.

  End-to-end: a fake "browser" (a real server started via `withApplication`,
  serving `/json/list` over plain HTTP and a CDP-shaped WebSocket endpoint at
  `/devtools/page/T1`) is driven by `CDP.Runtime.runClient` performing the
  full discovery → connect → send-command → receive-response round trip.
-/
import Linen.CDP.Runtime
import Linen.CDP.Domains.Performance
import Linen.Network.WebApp.Server.WebSockets
import Linen.Network.WebApp.Server.WithApplication

open CDP.Internal.Utils
open CDP.Runtime
open Network.WebSockets
open Network.WebApp.Server
open Network.WebApp
open Network.WebApp.Server.WebSockets
open Network.HTTP.Types

namespace Tests.CDP.Runtime

-- ── Unit-level: message parsing and dispatch, without a real connection ──

private def incomingCommandResponse : String := "{\"id\":7,\"result\":{\"ok\":true}}"
private def incomingEvent : String :=
  "{\"method\":\"Performance.metrics\",\"params\":{\"title\":\"x\"}}"

#guard match (Data.Json.Decode.decodeAs incomingCommandResponse : Except String IncomingMessage) with
  | .ok im => im.id == some { val := 7 } && im.method == none
  | .error _ => false

#guard match (Data.Json.Decode.decodeAs incomingEvent : Except String IncomingMessage) with
  | .ok im => im.method == some "Performance.metrics" && im.sessionId == none
  | .error _ => false

#eval show IO Unit from do
  -- `dispatchCommandResponse` delivers to the waiting promise and removes it
  -- from the buffer; a second delivery for the same id is a no-op.
  let commandNextId ← Control.Concurrent.MVar.new (⟨0⟩ : CommandId)
  let subscriptions ← IO.mkRef ({} : Subscriptions)
  let mv ← Control.Concurrent.MVar.newEmpty (Except ProtocolError Data.Json.Value)
  let commandBuffer ←
    IO.mkRef
      (({} : Std.HashMap CommandId (Control.Concurrent.MVar (Except ProtocolError Data.Json.Value))).insert
        (⟨7⟩ : CommandId) mv)
  let dummyConn ← Network.WebSockets.mkConnection (fun _ => pure ()) (pure ByteArray.empty)
  let listenTask ← IO.asTask (prio := .dedicated) (pure ())
  let handle : Handle :=
    { config := {}, commandNextId, subscriptions, commandBuffer, conn := dummyConn, listenTask }
  dispatchCommandResponse handle ⟨7⟩ none (some (.object [("ok", .bool true)]))
  let result ← Control.Concurrent.MVar.readSync mv
  assert! result == .ok (.object [("ok", .bool true)])
  let bufferAfter ← commandBuffer.get
  assert! bufferAfter.isEmpty

#eval show IO Unit from do
  -- `subscribe`/`unsubscribe` register and remove a handler.
  let commandNextId ← Control.Concurrent.MVar.new (⟨0⟩ : CommandId)
  let subscriptions ← IO.mkRef ({} : Subscriptions)
  let commandBuffer ←
    IO.mkRef (({} : Std.HashMap CommandId (Control.Concurrent.MVar (Except ProtocolError Data.Json.Value))))
  let dummyConn ← Network.WebSockets.mkConnection (fun _ => pure ()) (pure ByteArray.empty)
  let listenTask ← IO.asTask (prio := .dedicated) (pure ())
  let handle : Handle :=
    { config := {}, commandNextId, subscriptions, commandBuffer, conn := dummyConn, listenTask }
  let receivedRef ← IO.mkRef (none : Option CDP.Domains.Performance.Metrics)
  let sub ← subscribe CDP.Domains.Performance.Metrics handle (fun ev => receivedRef.set (some ev))
  dispatchEvent handle none "Performance.metrics"
    (some (.object [("title", .string "x"), ("metrics", .array #[])]))
  assert! (← receivedRef.get).isSome
  unsubscribe handle sub
  receivedRef.set none
  dispatchEvent handle none "Performance.metrics"
    (some (.object [("title", .string "x"), ("metrics", .array #[])]))
  assert! (← receivedRef.get).isNone

-- ── End-to-end: full discovery + connect + command round trip ──

private def targetJsonFor (port : UInt16) : String :=
  "[{\"description\":\"\",\"devtoolsFrontendUrl\":\"\",\"id\":\"T1\",\"title\":\"\",\"type\":\"page\"," ++
  "\"url\":\"about:blank\",\"webSocketDebuggerUrl\":\"ws://127.0.0.1:" ++ toString port ++
  "/devtools/page/T1\"}]"

private def wsApp : ServerApp := fun pending => do
  let conn ← pending.acceptIO
  let _ ← conn.receiveText
  conn.sendText "{\"id\":0,\"result\":{}}"

private def mkApp (portRef : IO.Ref UInt16) : Application :=
  websocketsOr defaultConnectionOptions wsApp fun req respond =>
    if req.rawPathInfo == "/json/list" then
      AppM.respondIO respond do
        let port ← portRef.get
        pure (responseLBS status200 [] (targetJsonFor port))
    else
      AppM.respond respond (.responseBuilder status404 [] ByteArray.empty)

#eval show IO Unit from do
  let portRef ← IO.mkRef (0 : UInt16)
  withApplication (pure (mkApp portRef)) fun port => do
    portRef.set port
    let config : Config := { hostPort := ("http://127.0.0.1", port.toNat) }
    let () ← runClient Unit config fun handle => do
      sendCommandWait CDP.Domains.Performance.PEnable handle {}
    pure ()

end Tests.CDP.Runtime
