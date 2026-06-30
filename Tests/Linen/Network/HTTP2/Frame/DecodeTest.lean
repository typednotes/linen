/-
  Tests for `Linen.Network.HTTP2.Frame.Decode`.

  Every decoder is a pure `ByteArray → Option …` function, so all behaviour
  (including the size guards and the fuel-free SETTINGS loop) is checked with
  `#guard`.
-/
import Linen.Network.HTTP2.Frame.Decode

open Network.HTTP2

namespace Tests.Network.HTTP2.FrameDecode

/-! ### Big-endian integer decoders -/

#guard decodeUInt16BE (ByteArray.mk #[0x01, 0x02]) == some 0x0102
#guard decodeUInt16BE (ByteArray.mk #[0xFF, 0xFF]) == some 0xFFFF
#guard decodeUInt16BE (ByteArray.mk #[0x01]) == none
#guard decodeUInt32BE (ByteArray.mk #[0x01, 0x02, 0x03, 0x04]) == some 0x01020304
#guard decodeUInt32BE (ByteArray.mk #[0x00, 0x00, 0x00, 0x00]) == some 0
#guard decodeUInt32BE (ByteArray.mk #[0x01, 0x02, 0x03]) == none

/-! ### Frame header (9 bytes: len[3] type flags streamId[4]) -/

def hdrBytes : ByteArray := ByteArray.mk #[0, 0, 8, 6, 1, 0, 0, 0, 3]

#guard (decodeFrameHeader hdrBytes).map (·.payloadLength) == some 8
#guard (decodeFrameHeader hdrBytes).map (·.frameType) == some FrameType.ping
#guard (decodeFrameHeader hdrBytes).map (·.flags) == some 1
#guard (decodeFrameHeader hdrBytes).map (fun h => h.streamId.val) == some 3
#guard decodeFrameHeader (ByteArray.mk #[0, 0, 8]) == none
-- the reserved high bit of the stream id is cleared by `fromWire`
#guard (decodeFrameHeader (ByteArray.mk #[0, 0, 0, 0, 0, 0x80, 0, 0, 7])).map (fun h => h.streamId.val)
        == some 7

/-! ### SETTINGS (6-byte key/value params) -/

def settingsBytes : ByteArray :=
  ByteArray.mk #[0, 1, 0, 0, 0x10, 0,   -- headerTableSize = 4096
                 0, 2, 0, 0, 0, 0]      -- enablePush = 0

#guard decodeSettingsParam (ByteArray.mk #[0, 1, 0, 0, 0x10, 0]) == some (.headerTableSize, 4096)
#guard decodeSettingsPayload settingsBytes == some [(.headerTableSize, 4096), (.enablePush, 0)]
#guard (decodeSettingsPayload settingsBytes).map (·.length) == some 2
#guard decodeSettingsPayload (ByteArray.mk #[]) == some []
#guard decodeSettingsPayload (ByteArray.mk #[1, 2, 3]) == none  -- not a multiple of 6

/-! ### applySettings — proof-carrying field updates -/

#guard (applySettings Settings.default [(.headerTableSize, 8192)]).headerTableSize == 8192
#guard (applySettings Settings.default [(.enablePush, 0)]).enablePush == false
#guard (applySettings Settings.default [(.maxConcurrentStreams, 50)]).maxConcurrentStreams == some 50
#guard (applySettings Settings.default [(.initialWindowSize, 1000)]).initialWindowSize == 1000
#guard (applySettings Settings.default [(.maxFrameSize, 20000)]).maxFrameSize == 20000
-- out-of-range values are rejected, leaving the default intact
#guard (applySettings Settings.default [(.maxFrameSize, 100)]).maxFrameSize == 16384
#guard (applySettings Settings.default [(.initialWindowSize, 0xFFFFFFFF)]).initialWindowSize == 65535

/-! ### GOAWAY / WINDOW_UPDATE / RST_STREAM -/

def goawayBytes : ByteArray := ByteArray.mk #[0, 0, 0, 5, 0, 0, 0, 1, 100, 101]
#guard (decodeGoaway goawayBytes).map (·.1) == some (StreamId.fromWire 5)
#guard (decodeGoaway goawayBytes).map (·.2.1) == some ErrorCode.protocolError
#guard (decodeGoaway goawayBytes).map (fun x => x.2.2.size) == some 2
#guard decodeGoaway (ByteArray.mk #[0, 0, 0, 5]) == none

#guard decodeWindowUpdate (ByteArray.mk #[0x80, 0, 0, 5]) == some 5  -- high bit masked
#guard decodeWindowUpdate (ByteArray.mk #[0, 0]) == none
#guard decodeRstStream (ByteArray.mk #[0, 0, 0, 8]) == some ErrorCode.cancel
#guard decodeRstStream (ByteArray.mk #[0]) == none

/-! ### PRIORITY (exclusive bit + dependency + weight) -/

def prioBytes : ByteArray := ByteArray.mk #[0x80, 0, 0, 3, 0xFF]
#guard (decodePriority prioBytes).map (·.1) == some true        -- exclusive
#guard (decodePriority prioBytes).map (fun x => x.2.1.val) == some 3  -- dependency stream
#guard (decodePriority prioBytes).map (fun x => x.2.2) == some 0xFF   -- weight
#guard decodePriority (ByteArray.mk #[0, 0, 0, 3]) == none

/-! ### Padding -/

#guard (decodePadding (ByteArray.mk #[2, 65, 66, 67, 68])).map (·.2) == some 2
#guard (decodePadding (ByteArray.mk #[2, 65, 66, 67, 68])).map (fun x => x.1.size) == some 2
#guard decodePadding (ByteArray.mk #[5, 1, 2]) == none  -- padding longer than payload
#guard decodePadding (ByteArray.mk #[]) == none

/-! ### Frame-size validation -/

def mkHdr (len : UInt32) (ft : FrameType) (flags : FrameFlags) : FrameHeader :=
  { payloadLength := len, frameType := ft, flags := flags, streamId := StreamId.fromWire 1 }

#guard validateFrameSize (mkHdr 8 .ping 0) Settings.default == none
#guard validateFrameSize (mkHdr 7 .ping 0) Settings.default == some ErrorCode.frameSizeError
#guard validateFrameSize (mkHdr 4 .rstStream 0) Settings.default == none
#guard validateFrameSize (mkHdr 5 .priority 0) Settings.default == none
#guard validateFrameSize (mkHdr 20000 .data 0) Settings.default == some ErrorCode.frameSizeError
#guard validateFrameSize (mkHdr 0 .settings FrameFlags.ack) Settings.default == none
#guard validateFrameSize (mkHdr 12 .settings 0) Settings.default == none
#guard validateFrameSize (mkHdr 5 .settings 0) Settings.default == some ErrorCode.frameSizeError

end Tests.Network.HTTP2.FrameDecode
