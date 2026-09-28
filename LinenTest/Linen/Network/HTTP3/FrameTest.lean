/-
  Tests for `Linen.Network.HTTP3.Frame`.

  Frame types, the QUIC variable-length integer codec (RFC 9000 §16), frame
  framing, and HTTP/3 settings are all pure, so behaviour is checked with
  `#guard` — heavily via encode→decode round-trips.
-/
import Linen.Network.HTTP3.Frame

open Network.HTTP3

namespace Tests.Network.HTTP3.Frame

/-! ### FrameType (RFC 9114 §7) -/

#guard FrameType.data.toId == 0x0
#guard FrameType.settings.toId == 0x4
#guard FrameType.maxPushId.toId == 0xD
#guard (FrameType.unknown 0x99).toId == 0x99
#guard FrameType.fromId 0x1 == FrameType.headers
#guard FrameType.fromId 0x7 == FrameType.goaway
#guard FrameType.fromId 0x42 == FrameType.unknown 0x42
#guard FrameType.fromId (FrameType.toId .pushPromise) == FrameType.pushPromise
#guard toString FrameType.settings == "SETTINGS"
#guard toString (FrameType.unknown 0x99) == "UNKNOWN(153)"

/-! ### Variable-length integers (RFC 9000 §16) — minimal encoding lengths -/

#guard (encodeVarInt 0).size == 1
#guard (encodeVarInt 63).size == 1
#guard (encodeVarInt 64).size == 2          -- needs the 2-byte form
#guard (encodeVarInt 16383).size == 2
#guard (encodeVarInt 16384).size == 4
#guard (encodeVarInt 1073741823).size == 4
#guard (encodeVarInt 1073741824).size == 8
-- RFC 9000 §16 sample: 37 fits in one byte as 0x25.
#guard encodeVarInt 37 == ByteArray.mk #[0x25]
-- RFC 9000 §16 sample: 15293 → 0x7b 0xbd (two-byte form).
#guard encodeVarInt 15293 == ByteArray.mk #[0x7b, 0xbd]

/-! ### Varint round-trips across all four widths -/

#guard (decodeVarInt (encodeVarInt 0)).map (·.1) == some 0
#guard (decodeVarInt (encodeVarInt 63)).map (·.1) == some 63
#guard (decodeVarInt (encodeVarInt 64)).map (·.1) == some 64
#guard (decodeVarInt (encodeVarInt 15293)).map (·.1) == some 15293
#guard (decodeVarInt (encodeVarInt 1000000)).map (·.1) == some 1000000
#guard (decodeVarInt (encodeVarInt 4611686018427387903)).map (·.1) == some 4611686018427387903
#guard (decodeVarInt (encodeVarInt 12345)).map (·.2) == some 2   -- consumed bytes
#guard (decodeVarInt ByteArray.empty).isNone

/-! ### Frame framing — varint(type) ++ varint(len) ++ payload -/

def frm : Frame := { frameType := .headers, payload := "hello".toUTF8 }

#guard (Frame.decode (Frame.encode frm)).map (fun x => x.1.frameType) == some FrameType.headers
#guard (Frame.decode (Frame.encode frm)).map (fun x => x.1.payload) == some "hello".toUTF8
#guard (Frame.decode (Frame.encode frm)).map (·.2) == some (Frame.encode frm).size
-- A DATA frame with an empty payload round-trips too.
#guard (Frame.decode (Frame.encode { frameType := .data, payload := ByteArray.empty })).map
        (fun x => x.1.frameType) == some FrameType.data
-- Truncated input fails to decode.
#guard (Frame.decode (ByteArray.mk #[0x01, 0x05, 0x61])).isNone   -- claims 5 bytes, only 1 present

/-! ### H3Settings -/

#guard H3Settings.default.maxFieldSectionSize == 0
def st : H3Settings := { maxFieldSectionSize := 4096, qpackMaxTableCapacity := 100, qpackBlockedStreams := 16 }
#guard H3Settings.decode (H3Settings.encode st) == some st
-- Only non-zero settings are emitted, but decode reconstructs the same record.
#guard H3Settings.decode (H3Settings.encode H3Settings.default) == some H3Settings.default
#guard H3Settings.decode (H3Settings.encode { maxFieldSectionSize := 8192 })
        == some { maxFieldSectionSize := 8192 }

end Tests.Network.HTTP3.Frame
