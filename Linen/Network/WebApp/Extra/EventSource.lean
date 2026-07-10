/-
  Linen.Network.WebApp.Extra.EventSource — Server-Sent Events (SSE)

  Implements the W3C Server-Sent Events protocol, compatible with the
  JavaScript `EventSource` API. Ports `Network.Wai.EventSource`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.EventSource

open Network.WebApp
open Network.HTTP.Types
open Data (CI)

/-- A single server-sent event. -/
structure ServerEvent where
  eventName : Option String := none
  eventId : Option String := none
  eventData : List String := []

/-- Render a `ServerEvent` as SSE wire format. -/
def ServerEvent.render (ev : ServerEvent) : String :=
  let s := match ev.eventName with
    | some name => s!"event: {name}\n"
    | none => ""
  let s := s ++ match ev.eventId with
    | some id => s!"id: {id}\n"
    | none => ""
  let s := ev.eventData.foldl (fun acc line => acc ++ s!"data: {line}\n") s
  s ++ "\n"

/-- Create an SSE streaming response. `eventSource` receives a callback to
    send events for the lifetime of the connection.
    $$\text{eventSourceApp} : ((\text{ServerEvent} \to \text{IO}()) \to \text{IO}()) \to \text{Application}$$ -/
def eventSourceApp (eventSource : (ServerEvent → IO Unit) → IO Unit) : Application :=
  fun _req respond =>
    AppM.respond respond (.responseStream status200
      [(hContentType, "text/event-stream"),
       (CI.mk' "Cache-Control", "no-cache"),
       (hConnection, "keep-alive")]
      fun send flush => do
        eventSource fun event => do
          send event.render.toUTF8
          flush)

end Network.WebApp.Extra.EventSource
