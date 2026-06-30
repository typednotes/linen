/-
  Tests for `Linen.Network.HTTP2.Server`.

  The connection handler is `IO`-driven (recv/send callbacks), so the IO entry
  points are pinned at the type level.  The pure pieces — initial connection
  state and `processWindowUpdateFrame` — are checked with `#guard`.
-/
import Linen.Network.HTTP2.Server

open Network.HTTP2

namespace Tests.Network.HTTP2.Server

/-! ### Initial connection state -/

#guard ConnectionState.initial.peerSettingsReceived == false
#guard ConnectionState.initial.goawayReceived == false
#guard ConnectionState.initial.lastGoodStreamId.val == 0
#guard ConnectionState.initial.localSettings.headerTableSize == 4096
#guard ConnectionState.initial.decoderTable.maxSize == 4096
#guard ConnectionState.initial.encoderTable.maxSize == 4096
#guard ConnectionState.initial.streams.streams.size == 0
#guard ConnectionState.initial.flowControl.sendWindow.size == 65535
#guard ConnectionState.initial.headerBlockState.isAssembling == false

/-! ### processWindowUpdateFrame (pure) -/

def connHdr : FrameHeader :=
  { payloadLength := 4, frameType := .windowUpdate, flags := 0, streamId := StreamId.zero }
def streamHdr : FrameHeader :=
  { payloadLength := 4, frameType := .windowUpdate, flags := 0, streamId := StreamId.fromWire 1 }
def inc100 : ByteArray := ByteArray.mk #[0, 0, 0, 100]
def inc0 : ByteArray := ByteArray.mk #[0, 0, 0, 0]

-- Connection-level increment grows the connection send window.
#guard (match processWindowUpdateFrame ConnectionState.initial connHdr inc100 with
        | .ok s => s.flowControl.sendWindow.size == 65635 | _ => false)
-- A zero increment on the connection is a connection error (PROTOCOL_ERROR).
#guard (match processWindowUpdateFrame ConnectionState.initial connHdr inc0 with
        | .error (.inl e) => e.errorCode == ErrorCode.protocolError | _ => false)
-- A zero increment on a stream is a stream error (RST_STREAM).
#guard (match processWindowUpdateFrame ConnectionState.initial streamHdr inc0 with
        | .error (.inr e) => e.errorCode == ErrorCode.protocolError | _ => false)
-- A too-short payload is a frame-size connection error.
#guard (match processWindowUpdateFrame ConnectionState.initial connHdr (ByteArray.mk #[0, 0]) with
        | .error (.inl e) => e.errorCode == ErrorCode.frameSizeError | _ => false)
-- A WINDOW_UPDATE for an unknown stream is ignored (state unchanged, ok).
#guard (match processWindowUpdateFrame ConnectionState.initial streamHdr inc100 with
        | .ok _ => true | _ => false)

/-! ### IO handlers — signatures (need live recv/send callbacks) -/

example : (ByteArray → IO Unit) → StreamId → ErrorCode → String → IO Unit :=
  fun send sid ec msg => sendGoaway send sid ec msg
example : (ByteArray → IO Unit) → StreamId → ErrorCode → IO Unit := sendRstStream
example : ByteArray → (ByteArray → IO Unit) → IO Unit := processPing
example : ConnectionState → ByteArray → (ByteArray → IO Unit) →
            IO (Except ConnectionError ConnectionState) := processSettings
example : ConnectionState → (ByteArray → IO Unit) → StreamId → List (String × String) → ByteArray →
            IO ConnectionState := sendResponse
example : IO ByteArray → (ByteArray → IO Unit) →
            (List (String × String) → StreamId → IO (List (String × String) × ByteArray)) →
            IO (Except ConnectionError Unit) := runHTTP2Connection

end Tests.Network.HTTP2.Server
