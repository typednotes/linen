import Linen.Control.Resource
import Linen.Network.WebSockets.Client

/-! # Scoped WebSocket applications
Receiving a peer close frame changes which actions can follow. The transport
runner owns the underlying connection.
-/

namespace Network.WebSockets
open Control.Resource

private opaque webSocketAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- An action on a private WebSocket connection. -/
abbrev WebSocketM := Action webSocketAccess.val (fun _ : ConnectionState => Connection)

/-- A WebSocket action whose result selects the next state. -/
abbrev WebSocketProgram := Program webSocketAccess.val (fun _ : ConnectionState => Connection)

/-- A received payload or an observed close frame. -/
inductive Incoming where
  | data (bytes : ByteArray)
  | closed

/-- A data observation leaves the previous state intact. A close is terminal. -/
def Incoming.state (previous : ConnectionState) : Incoming → ConnectionState
  | .data _ => previous
  | .closed => .closed

namespace WebSocketM

private def eval (conn : Connection) (action : WebSocketProgram .open_ α post) : IO α :=
  (·.1) <$> Program.run webSocketAccess.property action conn

/-- Establish a client WebSocket and close its TCP transport on exit. -/
def runClient (host : String) (port : UInt16) (path : String)
    (action : WebSocketProgram .open_ α post) : IO α :=
  Client.runClient host port path fun conn => eval conn action

/-- Run a program on a new framed connection over a caller-owned transport.
The caller is responsible for releasing that transport after the program. -/
def runTransport (send : ByteArray → IO Unit) (recv : IO ByteArray)
    (action : WebSocketProgram .open_ α post) : IO α := do
  eval (← mkConnection send recv) action

private def use (f : Connection → IO α) : WebSocketM .open_ .open_ α :=
  Program.step webSocketAccess.property fun conn => do return ⟨← f conn, conn⟩

/-- Send a text message while the connection is open. -/
def sendText (text : String) : WebSocketM .open_ .open_ Unit :=
  use (·.sendText text)

/-- Send a binary message while the connection is open. -/
def sendBinary (data : ByteArray) : WebSocketM .open_ .open_ Unit :=
  use (·.sendBinary data)

/-- Send a ping while the connection is open. -/
def sendPing (data : ByteArray) : WebSocketM .open_ .open_ Unit :=
  use (·.sendPing data)

/-- Send a close frame. Further sends are unavailable in this program. -/
def sendClose (code : CloseCode := .normal) (reason : String := "") :
    WebSocketM .open_ .closing Unit :=
  Program.step webSocketAccess.property fun conn => do
    conn.sendClose code reason
    return ⟨(), conn⟩

/-- Read under the existing framing API. An observed peer close selects
`.closed`, including while waiting for a reply to our own close frame. -/
def receive (_ : state = .open_ ∨ state = .closing := by decide) :
    WebSocketProgram state Incoming (Incoming.state state) :=
  Program.step webSocketAccess.property fun conn => do
    let bytes ← conn.receiveData
    if (← conn.getState) == .closed then
      return ⟨.closed, conn⟩
    return ⟨.data bytes, conn⟩

end WebSocketM
end Network.WebSockets
