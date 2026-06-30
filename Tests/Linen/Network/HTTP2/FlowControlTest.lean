/-
  Tests for `Linen.Network.HTTP2.FlowControl`.

  Flow-control windows are pure, so behaviour is checked with `#guard`,
  including the RFC 9113 §5.2/§6.9 increment, overflow, and settings-adjust
  rules.
-/
import Linen.Network.HTTP2.FlowControl

open Network.HTTP2

namespace Tests.Network.HTTP2.FlowControl

-- Core has no `BEq (Except ε α)`; this local instance lets us compare the
-- `Except ErrorCode FlowWindow` results directly.
local instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .ok a, .ok b => a == b
    | .error a, .error b => a == b
    | _, _ => false

/-! ### FlowWindow basics -/

#guard FlowWindow.default.size == 65535
#guard (FlowWindow.ofSize 100).size == 100
#guard ((FlowWindow.ofSize 100).consume 30).size == 70
#guard ((FlowWindow.ofSize 5).consume 10).size == -5   -- window may go negative
#guard (FlowWindow.ofSize 100).available == 100
#guard (FlowWindow.ofSize (-5)).available == 0          -- negative ⇒ no space

/-! ### increment (WINDOW_UPDATE, RFC 9113 §6.9) -/

#guard (FlowWindow.ofSize 100).increment 50 == .ok (FlowWindow.ofSize 150)
#guard (FlowWindow.ofSize 100).increment 0 == .error ErrorCode.protocolError   -- 0 increment
#guard (FlowWindow.ofSize 2147483647).increment 1 == .error ErrorCode.flowControlError  -- overflow

/-! ### adjust (SETTINGS_INITIAL_WINDOW_SIZE change, RFC 9113 §6.9.2) -/

-- Larger new initial size grows the window.
#guard (FlowWindow.ofSize 1000).adjust 65535 70000 == .ok (FlowWindow.ofSize 5465)
-- Smaller new initial size shrinks it (signed difference; may go negative).
#guard (FlowWindow.ofSize 1000).adjust 70000 65535 == .ok (FlowWindow.ofSize (-3465))
-- Overflow past the maximum is rejected.
#guard (FlowWindow.ofSize 2147483647).adjust 0 1 == .error ErrorCode.flowControlError

/-! ### ConnectionFlowControl -/

#guard ConnectionFlowControl.default.sendWindow.size == 65535
#guard ConnectionFlowControl.default.recvWindow.size == 65535
#guard (ConnectionFlowControl.default.consumeSend 1000).sendWindow.size == 64535
#guard (ConnectionFlowControl.default.consumeRecv 2000).recvWindow.size == 63535
#guard (match ConnectionFlowControl.default.processWindowUpdate 100 with
        | .ok fc => fc.sendWindow.size == 65635 | .error _ => false)
#guard (match ConnectionFlowControl.default.processWindowUpdate 0 with
        | .error e => e == ErrorCode.protocolError | .ok _ => false)

/-! ### processStreamWindowUpdate -/

def si : StreamInfo :=
  { streamId := StreamId.fromWire 1, state := .open, sendWindow := 100, recvWindow := 100 }

#guard (match processStreamWindowUpdate si 50 with
        | .ok i => i.sendWindow == 150 | .error _ => false)
#guard (match processStreamWindowUpdate { si with sendWindow := 2147483647 } 1 with
        | .error e => e == ErrorCode.flowControlError | .ok _ => false)

end Tests.Network.HTTP2.FlowControl
