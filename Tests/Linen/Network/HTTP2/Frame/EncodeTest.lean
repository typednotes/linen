/-
  Tests for `Linen.Network.HTTP2.Frame.Encode`.

  Encoders are pure `… → ByteArray`/`… → Frame`, so they are checked with
  `#guard`.  Many cases verify an encode/decode **round-trip** against
  `Frame.Decode`, which is the strongest correctness statement available here.
-/
import Linen.Network.HTTP2.Frame.Encode
import Linen.Network.HTTP2.Frame.Decode

open Network.HTTP2

namespace Tests.Network.HTTP2.FrameEncode

/-! ### Big-endian integers (+ round-trips) -/

#guard encodeUInt16BE 0x0102 == ByteArray.mk #[0x01, 0x02]
#guard encodeUInt32BE 0x01020304 == ByteArray.mk #[0x01, 0x02, 0x03, 0x04]
#guard decodeUInt16BE (encodeUInt16BE 0xABCD) == some 0xABCD
#guard decodeUInt32BE (encodeUInt32BE 0x12345678) == some 0x12345678

/-! ### Frame header (round-trips through Decode) -/

def hdr : FrameHeader :=
  { payloadLength := 100, frameType := .data, flags := 0, streamId := StreamId.fromWire 7 }

#guard (encodeFrameHeader hdr).size == 9
#guard decodeFrameHeader (encodeFrameHeader hdr) == some hdr
#guard (encodeFrame { header := hdr, payload := "hi".toUTF8 }).size == 9 + 2

/-! ### SETTINGS (param encoding + payload round-trip) -/

#guard encodeSettingsParam .headerTableSize 4096 == ByteArray.mk #[0, 1, 0, 0, 0x10, 0]
#guard decodeSettingsPayload (encodeSettingsPayload [(.headerTableSize, 4096), (.enablePush, 0)])
        == some [(.headerTableSize, 4096), (.enablePush, 0)]

#guard (buildSettingsFrame [(.enablePush, 1)]).header.frameType == FrameType.settings
#guard (buildSettingsFrame [(.enablePush, 1)]).header.streamId.val == 0
#guard (buildSettingsFrame [(.enablePush, 1)]).header.flags == FrameFlags.none
#guard (buildSettingsFrame [] true).header.flags == FrameFlags.ack
#guard (buildSettingsFrame [] true).payload.size == 0
-- the SETTINGS payload length matches the encoded params (1 param = 6 bytes)
#guard (buildSettingsFrame [(.enablePush, 1)]).header.payloadLength == 6

/-! ### PING -/

#guard (buildPingFrame (ByteArray.mk #[1, 2, 3, 4, 5, 6, 7, 8])).header.frameType == FrameType.ping
#guard (buildPingFrame (ByteArray.mk #[1, 2, 3, 4, 5, 6, 7, 8])).header.payloadLength == 8
#guard (buildPingFrame ByteArray.empty true).header.flags == FrameFlags.ack

/-! ### GOAWAY / WINDOW_UPDATE / RST_STREAM (round-trips) -/

def goaway : Frame := buildGoawayFrame (StreamId.fromWire 5) .protocolError
#guard goaway.header.frameType == FrameType.goaway
#guard (decodeGoaway goaway.payload).map (·.1) == some (StreamId.fromWire 5)
#guard (decodeGoaway goaway.payload).map (·.2.1) == some ErrorCode.protocolError

#guard (buildWindowUpdateFrame (StreamId.fromWire 1) 100).header.payloadLength == 4
#guard decodeWindowUpdate (buildWindowUpdateFrame (StreamId.fromWire 1) 100).payload == some 100
#guard decodeRstStream (buildRstStreamFrame (StreamId.fromWire 1) .cancel).payload == some ErrorCode.cancel

/-! ### HEADERS / DATA / CONTINUATION flags -/

#guard (buildHeadersFrame (StreamId.fromWire 1) "x".toUTF8 true true).header.frameType == FrameType.headers
#guard FrameFlags.test (buildHeadersFrame (StreamId.fromWire 1) "x".toUTF8 true true).header.flags FrameFlags.endStream == true
#guard FrameFlags.test (buildHeadersFrame (StreamId.fromWire 1) "x".toUTF8 true true).header.flags FrameFlags.endHeaders == true
#guard FrameFlags.test (buildHeadersFrame (StreamId.fromWire 1) "x".toUTF8 false false).header.flags FrameFlags.endStream == false
#guard (buildDataFrame (StreamId.fromWire 3) "abc".toUTF8 true).header.frameType == FrameType.data
#guard (buildDataFrame (StreamId.fromWire 3) "abc".toUTF8 true).header.flags == FrameFlags.endStream
#guard (buildContinuationFrame (StreamId.fromWire 3) "y".toUTF8 true).header.frameType == FrameType.continuation

/-! ### PRIORITY (round-trip) -/

def prio : ByteArray := encodePriority true (StreamId.fromWire 3) 0xFF
#guard prio.size == 5
#guard (decodePriority prio).map (·.1) == some true
#guard (decodePriority prio).map (fun x => x.2.1.val) == some 3
#guard (decodePriority prio).map (fun x => x.2.2) == some 0xFF

/-! ### Padding (round-trip recovers content) -/

#guard (encodePadding (ByteArray.mk #[65, 66]) 2).size == 5    -- 1 (padLen) + 2 (data) + 2 (pad)
#guard (encodePadding (ByteArray.mk #[65, 66]) 2)[0]! == 2
#guard (decodePadding (encodePadding (ByteArray.mk #[65, 66]) 2)).map (·.2) == some 2
#guard (decodePadding (encodePadding (ByteArray.mk #[65, 66]) 2)).map (fun x => x.1.size) == some 2

/-! ### splitHeaderBlock (fuel-free chunking) -/

#guard (splitHeaderBlock (ByteArray.mk #[1, 2, 3, 4, 5]) 2).length == 3
#guard ((splitHeaderBlock (ByteArray.mk #[1, 2, 3, 4, 5]) 2).map (·.size)) == [2, 2, 1]
#guard (splitHeaderBlock (ByteArray.mk #[1, 2, 3, 4]) 2).length == 2
#guard (splitHeaderBlock (ByteArray.mk #[1, 2, 3]) 0).length == 1   -- maxSize 0 ⇒ single chunk
#guard (splitHeaderBlock ByteArray.empty 5).length == 0
-- the chunks reassemble to the original block
#guard ((splitHeaderBlock (ByteArray.mk #[1, 2, 3, 4, 5]) 2).foldl (· ++ ·) ByteArray.empty)
        == ByteArray.mk #[1, 2, 3, 4, 5]

end Tests.Network.HTTP2.FrameEncode
