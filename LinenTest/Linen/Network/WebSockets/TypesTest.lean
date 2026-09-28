/-
  Tests for `Linen.Network.WebSockets.Types`.
-/
import Linen.Network.WebSockets.Types

open Network.WebSockets

namespace Tests.Network.WebSockets.Types

/-! ### `Opcode` -/

#guard Opcode.toUInt8 .continuation == 0x0
#guard Opcode.toUInt8 .text == 0x1
#guard Opcode.toUInt8 .binary == 0x2
#guard Opcode.toUInt8 .close == 0x8
#guard Opcode.toUInt8 .ping == 0x9
#guard Opcode.toUInt8 .pong == 0xA

#guard Opcode.fromUInt8 0x0 == .continuation
#guard Opcode.fromUInt8 0x1 == .text
#guard Opcode.fromUInt8 0x2 == .binary
#guard Opcode.fromUInt8 0x8 == .close
#guard Opcode.fromUInt8 0x9 == .ping
#guard Opcode.fromUInt8 0xA == .pong
#guard Opcode.fromUInt8 0x3 == .reserved ⟨3, by omega⟩

example : Opcode.fromUInt8 (Opcode.toUInt8 .text) = .text := opcode_roundtrip_text
example : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨5, by omega⟩)) = .reserved ⟨5, by omega⟩ :=
  opcode_roundtrip_reserved_5

/-! ### `CloseCode` -/

#guard CloseCode.normal.code == 1000
#guard CloseCode.goingAway.code == 1001
#guard CloseCode.protocolError.code == 1002
#guard CloseCode.unsupportedData.code == 1003
#guard CloseCode.normal != CloseCode.goingAway

/-! ### `ConnectionOptions` -/

#guard defaultConnectionOptions.maxMessageSize == 0
#guard defaultConnectionOptions.acceptUnmaskedFrames == false

/-! ### `Connection`/`PendingConnection` — signatures (need a live socket) -/

example : Connection → IO Unit := fun conn => conn.sendText "hi"
example : PendingConnection → IO Connection := fun p => p.acceptIO
example : ServerApp → PendingConnection → IO Unit := fun app => app

end Tests.Network.WebSockets.Types
