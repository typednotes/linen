/-
  Tests for `Linen.Network.HTTP2.Stream`.

  The stream state machine and table (over `Std.HashMap`) are pure, so the
  lifecycle operations are checked with `#guard`.
-/
import Linen.Network.HTTP2.Stream

open Network.HTTP2

namespace Tests.Network.HTTP2.Stream

/-! ### StreamState -/

#guard StreamState.idle == StreamState.idle
#guard (StreamState.open == StreamState.closed) == false
#guard toString StreamState.open == "open"
#guard toString StreamState.halfClosedLocal == "half-closed (local)"
#guard toString StreamState.reservedRemote == "reserved (remote)"

/-! ### Stream-id classification (RFC 9113 §5.1.1) -/

#guard isClientStream (StreamId.fromWire 1) == true
#guard isClientStream (StreamId.fromWire 2) == false
#guard isServerStream (StreamId.fromWire 2) == true
#guard isServerStream (StreamId.fromWire 1) == false
#guard isConnectionStream StreamId.zero == true
#guard isConnectionStream (StreamId.fromWire 1) == false
#guard validateStreamId (StreamId.fromWire 3) (StreamId.fromWire 1) == true
#guard validateStreamId (StreamId.fromWire 1) (StreamId.fromWire 3) == false

/-! ### Empty table -/

#guard StreamTable.empty.streams.size == 0
#guard StreamTable.empty.lastClientStreamId.val == 0
#guard StreamTable.empty.nextServerStreamId.val == 2
#guard StreamTable.empty.activeStreamCount == 0
#guard (StreamTable.empty.lookup (StreamId.fromWire 5)).isNone

/-! ### upsert / lookup -/

def si : StreamInfo :=
  { streamId := StreamId.fromWire 5, state := .open, sendWindow := 100, recvWindow := 100 }

#guard ((StreamTable.empty.upsert si).lookup (StreamId.fromWire 5)).map (·.sendWindow) == some 100
#guard ((StreamTable.empty.upsert si).lookup (StreamId.fromWire 5)).map (·.state) == some StreamState.open

/-! ### openClientStream -/

-- Odd, increasing client id succeeds and records the state/window/last-id.
#guard (StreamTable.empty.openClientStream (StreamId.fromWire 1) 65535).isSome
#guard ((StreamTable.empty.openClientStream (StreamId.fromWire 1) 65535).bind
          (fun t => t.lookup (StreamId.fromWire 1))).map (·.state) == some StreamState.open
#guard ((StreamTable.empty.openClientStream (StreamId.fromWire 1) 65535).bind
          (fun t => t.lookup (StreamId.fromWire 1))).map (·.sendWindow) == some 65535
#guard (StreamTable.empty.openClientStream (StreamId.fromWire 1) 65535).map
          (fun t => t.lastClientStreamId.val) == some 1
-- A server (even) id, the connection id (0), and a non-increasing id are rejected.
#guard (StreamTable.empty.openClientStream (StreamId.fromWire 2) 65535).isNone
#guard (StreamTable.empty.openClientStream StreamId.zero 65535).isNone

def t1 : StreamTable := (StreamTable.empty.openClientStream (StreamId.fromWire 3) 65535).getD StreamTable.empty
#guard (t1.openClientStream (StreamId.fromWire 1) 65535).isNone   -- 1 ≤ lastClient (3)
#guard (t1.openClientStream (StreamId.fromWire 5) 65535).isSome   -- 5 > 3

/-! ### updateState / activeStreamCount -/

#guard t1.activeStreamCount == 1
#guard (t1.updateState (StreamId.fromWire 3) .closed).activeStreamCount == 0
#guard (t1.updateState (StreamId.fromWire 3) .halfClosedRemote).activeStreamCount == 1
#guard ((t1.updateState (StreamId.fromWire 3) .closed).lookup (StreamId.fromWire 3)).map (·.state)
        == some StreamState.closed

/-! ### updatePriority (creates an idle entry if absent) -/

#guard ((t1.updatePriority (StreamId.fromWire 3) true (StreamId.fromWire 1) 200).lookup
          (StreamId.fromWire 3)).map (·.priorityWeight) == some 200
#guard ((StreamTable.empty.updatePriority (StreamId.fromWire 9) false StreamId.zero 7).lookup
          (StreamId.fromWire 9)).map (·.state) == some StreamState.idle

end Tests.Network.HTTP2.Stream
