import Linen.Network.WebSockets.Scoped

open Network.WebSockets Control.Resource

example : True := by
  fail_if_success
    have _ : WebSocketM .open_ .open_ Unit :=
      Program.ibind WebSocketM.sendClose fun () => WebSocketM.sendText "late"
  fail_if_success
    have _ : WebSocketM .closed .closed Unit := WebSocketM.sendBinary ByteArray.empty
  trivial

-- Receiving a close frame selects the closed branch after a local close.
#eval show IO Unit from do
  let output ← IO.mkRef ByteArray.empty
  let peerClose : Frame := ⟨true, .close, none, ByteArray.empty⟩
  let action : WebSocketProgram .open_ Incoming (Incoming.state .closing) :=
    Program.bind WebSocketM.sendClose fun () => WebSocketM.receive
  let result ← WebSocketM.runTransport output.set (pure peerClose.encode) action
  match result with
  | .closed =>
    unless !(← output.get).isEmpty do throw (IO.userError "close frame was not sent")
  | .data _ => throw (IO.userError "peer close was treated as payload")
