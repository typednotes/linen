/-
  Tests for `Linen.Network.WebSockets` (protocol package aggregator).

  A pure re-export aggregator — verified by using one symbol from each of
  the modules it pulls in.
-/
import Linen.Network.WebSockets

open Network.WebSockets

namespace Tests.Network.WebSockets

#guard Opcode.toUInt8 .text == 0x1
#guard (Frame.encode { fin := true, opcode := .text, mask := none, payload := ByteArray.empty }).size == 2
#guard webSocketGUID == "258EAFA5-E914-47DA-95CA-5AB5DC76B45B"

#eval show IO Unit from do
  let conn ← mkConnection (fun _ => pure ()) (pure ByteArray.empty)
  let state ← conn.getState
  assert! state == .open_

end Tests.Network.WebSockets
