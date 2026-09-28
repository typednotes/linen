/-
  Tests for `Linen.Network.HTTP2.Frame.Types`.

  All of this module is pure (types + total `UInt8`/`UInt16`/`UInt32`
  conversions + proof-carrying constructors), so behaviour is checked with
  `#guard` and the RFC value constraints with `rfl`/`native_decide` examples.
  Stream IDs are built with the proof-free `StreamId.fromWire` (identity below
  `2^31`); `ofUInt32`/`mk24` are exercised with explicit proofs.
-/
import Linen.Network.HTTP2.Frame.Types

open Network.HTTP2

namespace Tests.Network.HTTP2.FrameTypes

/-! ### StreamId -/

#guard (StreamId.fromWire 5).val == 5
#guard (StreamId.ofUInt32 5 (by native_decide)).val == 5
#guard StreamId.zero.val == 0
-- fromWire clears the reserved high bit (RFC 9113 §4.1).
#guard (StreamId.fromWire 0xFFFFFFFF).val == 0x7FFFFFFF
#guard (StreamId.fromWire 0x80000005).val == 5
#guard (StreamId.fromWire 3).isClientInitiated == true
#guard (StreamId.fromWire 3).isServerInitiated == false
#guard (StreamId.fromWire 4).isServerInitiated == true
#guard StreamId.zero.isClientInitiated == false
#guard StreamId.zero.isServerInitiated == false
#guard (StreamId.fromWire 5) == StreamId.fromWire 5
#guard ((StreamId.fromWire 5) == StreamId.fromWire 6) == false
#guard toString (StreamId.fromWire 7) == "7"
#guard (compare (StreamId.fromWire 1) (StreamId.fromWire 2)) == Ordering.lt
#guard (compare (StreamId.fromWire 2) (StreamId.fromWire 2)) == Ordering.eq

-- Coercion to UInt32, and the erased 31-bit proof.
example : UInt32 := StreamId.fromWire 5
example : (StreamId.fromWire 5).val.toNat < 2 ^ 31 := (StreamId.fromWire 5).hBit

/-! ### FrameType -/

#guard FrameType.data.toUInt8 == 0
#guard FrameType.continuation.toUInt8 == 9
#guard (FrameType.unknown 200).toUInt8 == 200
#guard FrameType.fromUInt8 1 == FrameType.headers
#guard FrameType.fromUInt8 4 == FrameType.settings
#guard FrameType.fromUInt8 99 == FrameType.unknown 99
#guard (FrameType.fromUInt8 (FrameType.toUInt8 .ping)) == .ping
#guard (FrameType.data == FrameType.headers) == false
#guard (FrameType.unknown 5 == FrameType.unknown 5)
#guard toString FrameType.rstStream == "RST_STREAM"
#guard toString (FrameType.unknown 42) == "UNKNOWN(42)"

/-! ### ErrorCode -/

#guard ErrorCode.noError.toUInt32 == 0
#guard ErrorCode.http11Required.toUInt32 == 13
#guard (ErrorCode.unknown 500).toUInt32 == 500
#guard ErrorCode.fromUInt32 1 == ErrorCode.protocolError
#guard ErrorCode.fromUInt32 100 == ErrorCode.unknown 100
#guard toString ErrorCode.cancel == "CANCEL"

/-! ### SettingsKeyId -/

#guard SettingsKeyId.headerTableSize.toUInt16 == 1
#guard SettingsKeyId.maxHeaderListSize.toUInt16 == 6
#guard (SettingsKeyId.unknown 9).toUInt16 == 9
#guard SettingsKeyId.fromUInt16 2 == SettingsKeyId.enablePush
#guard SettingsKeyId.fromUInt16 99 == SettingsKeyId.unknown 99
#guard toString SettingsKeyId.maxFrameSize == "MAX_FRAME_SIZE"

/-! ### Settings — RFC defaults -/

#guard Settings.default.headerTableSize == 4096
#guard Settings.default.enablePush == true
#guard Settings.default.maxConcurrentStreams == none
#guard Settings.default.initialWindowSize == 65535
#guard Settings.default.maxFrameSize == 16384
#guard Settings.default.maxHeaderListSize == none
#guard Settings.default == Settings.default

/-! ### FrameFlags -/

#guard FrameFlags.none == 0
#guard FrameFlags.endStream == 0x1
#guard FrameFlags.endHeaders == 0x4
#guard FrameFlags.padded == 0x8
#guard FrameFlags.set FrameFlags.none FrameFlags.endStream == 0x1
#guard FrameFlags.test (FrameFlags.set FrameFlags.none FrameFlags.padded) FrameFlags.padded == true
#guard FrameFlags.test FrameFlags.none FrameFlags.padded == false
#guard FrameFlags.clear (FrameFlags.set FrameFlags.none FrameFlags.padded) FrameFlags.padded == FrameFlags.none
#guard FrameFlags.set FrameFlags.endStream FrameFlags.endHeaders == 0x5
#guard FrameFlags.test (FrameFlags.set FrameFlags.endStream FrameFlags.endHeaders) FrameFlags.endHeaders == true

/-! ### FrameHeader / Frame -/

def hdr : FrameHeader :=
  { payloadLength := 8, frameType := .ping, flags := FrameFlags.ack, streamId := StreamId.fromWire 3 }

#guard hdr.payloadLength == 8
#guard hdr.frameType == FrameType.ping
#guard hdr == hdr
#guard (toString hdr).startsWith "FrameHeader(type=PING, length=8, flags=0x1, stream=3)"
#guard (FrameHeader.mk24 8 .ping FrameFlags.ack (StreamId.fromWire 3) (by native_decide)).payloadLength == 8
#guard (Frame.mk hdr "abc".toUTF8) == Frame.mk hdr "abc".toUTF8
#guard ((Frame.mk hdr "abc".toUTF8) == Frame.mk hdr "xyz".toUTF8) == false

/-! ### Constants -/

#guard connectionPrefaceLength == 24
#guard frameHeaderSize == 9
#guard defaultInitialWindowSize == 65535
#guard maxWindowSize == 2147483647
#guard minMaxFrameSize == 16384
#guard maxMaxFrameSize == 16777215
#guard connectionPreface.size == 24

-- RFC 9113 numeric round-trips for defined values (compile-time).
example : FrameType.fromUInt8 (FrameType.toUInt8 .settings) = .settings :=
  FrameType.fromUInt8_toUInt8_settings

end Tests.Network.HTTP2.FrameTypes
