/-
  Linen.Network.WebApp.Extra.EventSource.EventStream — SSE event stream framing

  Ports `Network.Wai.EventSource.EventStream`.
-/
import Linen.Network.WebApp.Extra.EventSource

namespace Network.WebApp.Extra.EventSource.EventStream

open Network.WebApp.Extra.EventSource

/-- Create a simple data-only event. -/
def dataEvent (data : String) : ServerEvent :=
  { eventData := data.splitOn "\n" }

/-- Create a named event with data. -/
def namedEvent (name : String) (data : String) : ServerEvent :=
  { eventName := some name, eventData := data.splitOn "\n" }

/-- Create a retry event (tells the client to reconnect after `ms` ms). -/
def retryEvent (ms : Nat) : String :=
  s!"retry: {ms}\n\n"

/-- Create a comment (keep-alive ping). -/
def commentEvent (text : String := "") : String :=
  s!": {text}\n\n"

end Network.WebApp.Extra.EventSource.EventStream
