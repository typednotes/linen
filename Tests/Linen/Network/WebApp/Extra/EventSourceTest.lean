import Linen.Network.WebApp.Extra.EventSource
import Linen.Control.Concurrent.Green

/-! ### Tests for `Linen.Network.WebApp.Extra.EventSource`

    Coverage: `ServerEvent.render`'s SSE wire format, and `eventSourceApp`
    streaming a sequence of events to the response body. -/

open Network.WebApp Network.WebApp.Extra.EventSource
open Network.HTTP.Types
open Control.Concurrent.Green

namespace Tests.Network.WebApp.Extra.EventSource

#guard ({ eventData := ["hi"] } : ServerEvent).render == "data: hi\n\n"
#guard ({ eventName := some "greet", eventData := ["hi"] } : ServerEvent).render == "event: greet\ndata: hi\n\n"
#guard ({ eventId := some "1", eventData := ["hi"] } : ServerEvent).render == "id: 1\ndata: hi\n\n"
#guard ({ eventName := some "n", eventId := some "1", eventData := ["a", "b"] } : ServerEvent).render
  == "event: n\nid: 1\ndata: a\ndata: b\n\n"

#eval show IO Unit from do
  let app := eventSourceApp fun send => do
    send { eventData := ["one"] }
    send { eventName := some "second", eventData := ["two"] }
  let tok ← Std.CancellationToken.new
  let captured ← IO.mkRef (none : Option Response)
  let respond : Response → Green ResponseReceived := fun resp =>
    (do captured.set (some resp); pure ResponseReceived.done : IO ResponseReceived)
  let _ ← Green.block (app defaultRequest respond).run tok
  match ← captured.get with
  | some (.responseStream _ headers body) =>
    unless headers.any (fun (n, v) => n == hContentType && v == "text/event-stream") do
      throw (IO.userError "expected Content-Type: text/event-stream")
    let bufRef ← IO.mkRef ByteArray.empty
    body (fun chunk => bufRef.modify (· ++ chunk)) (pure ())
    let content ← bufRef.get
    unless String.fromUTF8! content == "data: one\n\nevent: second\ndata: two\n\n" do
      throw (IO.userError s!"unexpected SSE stream content: {String.fromUTF8! content}")
  | _ => throw (IO.userError "expected a responseStream")

end Tests.Network.WebApp.Extra.EventSource
