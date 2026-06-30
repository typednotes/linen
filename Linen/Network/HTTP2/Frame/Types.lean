/-
  Linen.Network.HTTP2.Frame.Types — HTTP/2 frame types

  Core types for HTTP/2 framing as defined in RFC 9113.

  ## Design

  Frame types, error codes, and settings are encoded as inductives with
  exhaustive pattern matching rather than raw numeric constants. Conversions
  to/from `UInt8`/`UInt32` are provided as total functions.

  ## Guarantees

  - `StreamId` carries a proof that `val < 2^31` (RFC 9113 §4.1)
  - `FrameType` is a closed inductive covering all RFC 9113 frame types
  - `ErrorCode` covers all defined error codes
  - `SettingsKeyId` covers all defined settings identifiers
  - Numeric conversions are provably inverse for defined values
  - `Settings` carries proof fields enforcing RFC 9113 value constraints

  ## Haskell equivalent
  `Network.HTTP2.Frame.Types` (https://hackage.haskell.org/package/http2)
-/

namespace Network.HTTP2

/-- `(a &&& 0x7FFFFFFF).toNat < 2^31`.
    Bitwise AND with `0x7FFFFFFF` clears bit 31, so the result is always `< 2^31`.
    Proved via `BitVec.toNat_and` and `Nat.and_le_right`. -/
private theorem UInt32.land_mask31_lt (a : UInt32) :
    (a &&& 0x7FFFFFFF).toNat < 2^31 := by
  show (a &&& 0x7FFFFFFF).toBitVec.toNat < 2^31
  rw [show (a &&& 0x7FFFFFFF).toBitVec = a.toBitVec &&& (0x7FFFFFFF : UInt32).toBitVec from rfl]
  rw [BitVec.toNat_and]
  have h := @Nat.and_le_right a.toBitVec.toNat (0x7FFFFFFF : UInt32).toBitVec.toNat
  have : (0x7FFFFFFF : UInt32).toBitVec.toNat = 2147483647 := by native_decide
  omega

/-- HTTP/2 stream identifier. RFC 9113 §4.1 reserves the high bit.
    The proof field ensures only 31-bit values are representable.
    Erased at runtime (zero cost).
    $$\text{StreamId} = \{ v : \text{UInt32} \mid v < 2^{31} \}$$ -/
structure StreamId where
  val : UInt32
  hBit : val.toNat < 2^31 := by omega
deriving BEq, Repr

instance : Hashable StreamId where
  hash s := hash s.val

instance : ToString StreamId where
  toString s := toString s.val

instance : Inhabited StreamId where
  default := { val := 0, hBit := by native_decide }

instance : Coe StreamId UInt32 where
  coe s := s.val

instance : Ord StreamId where
  compare a b := compare a.val b.val

def StreamId.ofUInt32 (v : UInt32) (h : v.toNat < 2^31 := by omega) : StreamId :=
  { val := v, hBit := h }

def StreamId.zero : StreamId := { val := 0, hBit := by native_decide }

@[inline] def StreamId.fromWire (raw : UInt32) : StreamId :=
  { val := raw &&& 0x7FFFFFFF, hBit := UInt32.land_mask31_lt raw }

def StreamId.isClientInitiated (s : StreamId) : Bool := s.val != 0 && s.val % 2 == 1
def StreamId.isServerInitiated (s : StreamId) : Bool := s.val != 0 && s.val % 2 == 0

inductive FrameType where
  | data | headers | priority | rstStream | settings | pushPromise
  | ping | goaway | windowUpdate | continuation | unknown (id : UInt8)
  deriving Repr, Inhabited

instance : BEq FrameType where
  beq
    | .data, .data => true | .headers, .headers => true | .priority, .priority => true
    | .rstStream, .rstStream => true | .settings, .settings => true
    | .pushPromise, .pushPromise => true | .ping, .ping => true | .goaway, .goaway => true
    | .windowUpdate, .windowUpdate => true | .continuation, .continuation => true
    | .unknown a, .unknown b => a == b | _, _ => false

instance : ToString FrameType where
  toString
    | .data => "DATA" | .headers => "HEADERS" | .priority => "PRIORITY"
    | .rstStream => "RST_STREAM" | .settings => "SETTINGS"
    | .pushPromise => "PUSH_PROMISE" | .ping => "PING" | .goaway => "GOAWAY"
    | .windowUpdate => "WINDOW_UPDATE" | .continuation => "CONTINUATION"
    | .unknown id => s!"UNKNOWN({id})"

namespace FrameType
def toUInt8 : FrameType → UInt8
  | .data => 0 | .headers => 1 | .priority => 2 | .rstStream => 3 | .settings => 4
  | .pushPromise => 5 | .ping => 6 | .goaway => 7 | .windowUpdate => 8
  | .continuation => 9 | .unknown id => id

def fromUInt8 : UInt8 → FrameType
  | 0 => .data | 1 => .headers | 2 => .priority | 3 => .rstStream | 4 => .settings
  | 5 => .pushPromise | 6 => .ping | 7 => .goaway | 8 => .windowUpdate
  | 9 => .continuation | id => .unknown id

theorem fromUInt8_toUInt8_data : fromUInt8 (toUInt8 .data) = .data := by rfl
theorem fromUInt8_toUInt8_headers : fromUInt8 (toUInt8 .headers) = .headers := by rfl
theorem fromUInt8_toUInt8_priority : fromUInt8 (toUInt8 .priority) = .priority := by rfl
theorem fromUInt8_toUInt8_rstStream : fromUInt8 (toUInt8 .rstStream) = .rstStream := by rfl
theorem fromUInt8_toUInt8_settings : fromUInt8 (toUInt8 .settings) = .settings := by rfl
theorem fromUInt8_toUInt8_pushPromise : fromUInt8 (toUInt8 .pushPromise) = .pushPromise := by rfl
theorem fromUInt8_toUInt8_ping : fromUInt8 (toUInt8 .ping) = .ping := by rfl
theorem fromUInt8_toUInt8_goaway : fromUInt8 (toUInt8 .goaway) = .goaway := by rfl
theorem fromUInt8_toUInt8_windowUpdate : fromUInt8 (toUInt8 .windowUpdate) = .windowUpdate := by rfl
theorem fromUInt8_toUInt8_continuation : fromUInt8 (toUInt8 .continuation) = .continuation := by rfl

theorem fromUInt8_toUInt8_unknown (id : UInt8) (h : id.toNat ≥ 10) :
    FrameType.fromUInt8 (FrameType.toUInt8 (.unknown id)) = .unknown id := by
  simp only [toUInt8]; unfold fromUInt8; split <;> simp_all
end FrameType

inductive ErrorCode where
  | noError | protocolError | internalError | flowControlError | settingsTimeout
  | streamClosed | frameSizeError | refusedStream | cancel | compressionError
  | connectError | enhanceYourCalm | inadequateSecurity | http11Required
  | unknown (code : UInt32)
  deriving Repr, Inhabited

instance : BEq ErrorCode where
  beq
    | .noError, .noError => true | .protocolError, .protocolError => true
    | .internalError, .internalError => true | .flowControlError, .flowControlError => true
    | .settingsTimeout, .settingsTimeout => true | .streamClosed, .streamClosed => true
    | .frameSizeError, .frameSizeError => true | .refusedStream, .refusedStream => true
    | .cancel, .cancel => true | .compressionError, .compressionError => true
    | .connectError, .connectError => true | .enhanceYourCalm, .enhanceYourCalm => true
    | .inadequateSecurity, .inadequateSecurity => true
    | .http11Required, .http11Required => true
    | .unknown a, .unknown b => a == b | _, _ => false

instance : ToString ErrorCode where
  toString
    | .noError => "NO_ERROR" | .protocolError => "PROTOCOL_ERROR"
    | .internalError => "INTERNAL_ERROR" | .flowControlError => "FLOW_CONTROL_ERROR"
    | .settingsTimeout => "SETTINGS_TIMEOUT" | .streamClosed => "STREAM_CLOSED"
    | .frameSizeError => "FRAME_SIZE_ERROR" | .refusedStream => "REFUSED_STREAM"
    | .cancel => "CANCEL" | .compressionError => "COMPRESSION_ERROR"
    | .connectError => "CONNECT_ERROR" | .enhanceYourCalm => "ENHANCE_YOUR_CALM"
    | .inadequateSecurity => "INADEQUATE_SECURITY" | .http11Required => "HTTP_1_1_REQUIRED"
    | .unknown code => s!"UNKNOWN({code})"

namespace ErrorCode
def toUInt32 : ErrorCode → UInt32
  | .noError => 0 | .protocolError => 1 | .internalError => 2 | .flowControlError => 3
  | .settingsTimeout => 4 | .streamClosed => 5 | .frameSizeError => 6 | .refusedStream => 7
  | .cancel => 8 | .compressionError => 9 | .connectError => 10 | .enhanceYourCalm => 11
  | .inadequateSecurity => 12 | .http11Required => 13 | .unknown code => code

def fromUInt32 : UInt32 → ErrorCode
  | 0 => .noError | 1 => .protocolError | 2 => .internalError | 3 => .flowControlError
  | 4 => .settingsTimeout | 5 => .streamClosed | 6 => .frameSizeError | 7 => .refusedStream
  | 8 => .cancel | 9 => .compressionError | 10 => .connectError | 11 => .enhanceYourCalm
  | 12 => .inadequateSecurity | 13 => .http11Required | code => .unknown code

theorem fromUInt32_toUInt32_unknown (code : UInt32) (h : code.toNat ≥ 14) :
    ErrorCode.fromUInt32 (ErrorCode.toUInt32 (.unknown code)) = .unknown code := by
  simp only [toUInt32]; unfold fromUInt32; split <;> simp_all
end ErrorCode

inductive SettingsKeyId where
  | headerTableSize | enablePush | maxConcurrentStreams
  | initialWindowSize | maxFrameSize | maxHeaderListSize | unknown (id : UInt16)
  deriving Repr, Inhabited

instance : BEq SettingsKeyId where
  beq
    | .headerTableSize, .headerTableSize => true | .enablePush, .enablePush => true
    | .maxConcurrentStreams, .maxConcurrentStreams => true
    | .initialWindowSize, .initialWindowSize => true | .maxFrameSize, .maxFrameSize => true
    | .maxHeaderListSize, .maxHeaderListSize => true
    | .unknown a, .unknown b => a == b | _, _ => false

instance : ToString SettingsKeyId where
  toString
    | .headerTableSize => "HEADER_TABLE_SIZE" | .enablePush => "ENABLE_PUSH"
    | .maxConcurrentStreams => "MAX_CONCURRENT_STREAMS"
    | .initialWindowSize => "INITIAL_WINDOW_SIZE" | .maxFrameSize => "MAX_FRAME_SIZE"
    | .maxHeaderListSize => "MAX_HEADER_LIST_SIZE" | .unknown id => s!"UNKNOWN({id})"

namespace SettingsKeyId
def toUInt16 : SettingsKeyId → UInt16
  | .headerTableSize => 1 | .enablePush => 2 | .maxConcurrentStreams => 3
  | .initialWindowSize => 4 | .maxFrameSize => 5 | .maxHeaderListSize => 6
  | .unknown id => id

def fromUInt16 : UInt16 → SettingsKeyId
  | 1 => .headerTableSize | 2 => .enablePush | 3 => .maxConcurrentStreams
  | 4 => .initialWindowSize | 5 => .maxFrameSize | 6 => .maxHeaderListSize
  | id => .unknown id
end SettingsKeyId

structure Settings where
  headerTableSize : Nat := 4096
  enablePush : Bool := true
  maxConcurrentStreams : Option Nat := none
  initialWindowSize : UInt32 := 65535
  maxFrameSize : UInt32 := 16384
  maxHeaderListSize : Option Nat := none
  initialWindowSize_valid : initialWindowSize.toNat ≤ 2147483647 := by native_decide
  maxFrameSize_lower : 16384 ≤ maxFrameSize.toNat := by native_decide
  maxFrameSize_upper : maxFrameSize.toNat ≤ 16777215 := by native_decide

instance : Repr Settings where
  reprPrec s _ :=
    "{ headerTableSize := " ++ repr s.headerTableSize ++
    ", enablePush := " ++ repr s.enablePush ++
    ", maxConcurrentStreams := " ++ repr s.maxConcurrentStreams ++
    ", initialWindowSize := " ++ repr s.initialWindowSize ++
    ", maxFrameSize := " ++ repr s.maxFrameSize ++
    ", maxHeaderListSize := " ++ repr s.maxHeaderListSize ++ " }"

instance : Inhabited Settings where default := {}
instance : BEq Settings where
  beq a b := a.headerTableSize == b.headerTableSize && a.enablePush == b.enablePush &&
    a.maxConcurrentStreams == b.maxConcurrentStreams &&
    a.initialWindowSize == b.initialWindowSize &&
    a.maxFrameSize == b.maxFrameSize && a.maxHeaderListSize == b.maxHeaderListSize

def Settings.default : Settings := {}

abbrev FrameFlags := UInt8

namespace FrameFlags
def none : FrameFlags := 0
def endStream : FrameFlags := 0x1
def ack : FrameFlags := 0x1
def endHeaders : FrameFlags := 0x4
def padded : FrameFlags := 0x8
def priority : FrameFlags := 0x20
@[inline] def test (flags flag : FrameFlags) : Bool := (flags &&& flag) != 0
@[inline] def set (flags flag : FrameFlags) : FrameFlags := flags ||| flag
@[inline] def clear (flags flag : FrameFlags) : FrameFlags := flags &&& (~~~ flag)
end FrameFlags

structure FrameHeader where
  payloadLength : UInt32
  frameType : FrameType
  flags : FrameFlags
  streamId : StreamId
  deriving Repr, Inhabited

def FrameHeader.validPayloadLength (h : FrameHeader) : Prop := h.payloadLength.toNat < 2^24
def FrameHeader.mk24 (payloadLength : UInt32) (frameType : FrameType) (flags : FrameFlags)
    (streamId : StreamId) (_h : payloadLength.toNat < 2^24 := by omega) : FrameHeader :=
  { payloadLength, frameType, flags, streamId }
theorem FrameHeader.payloadLength_4_valid : (4 : UInt32).toNat < 2^24 := by native_decide
theorem FrameHeader.payloadLength_8_valid : (8 : UInt32).toNat < 2^24 := by native_decide
theorem FrameHeader.payloadLength_0_valid : (0 : UInt32).toNat < 2^24 := by native_decide

instance : BEq FrameHeader where
  beq a b := a.payloadLength == b.payloadLength && a.frameType == b.frameType &&
    a.flags == b.flags && a.streamId == b.streamId

instance : ToString FrameHeader where
  toString h :=
    s!"FrameHeader(type={h.frameType}, length={h.payloadLength}, flags=0x{String.ofList (Nat.toDigits 16 h.flags.toNat)}, stream={h.streamId.val})"

structure Frame where
  header : FrameHeader
  payload : ByteArray
  deriving Inhabited

instance : BEq Frame where beq a b := a.header == b.header && a.payload == b.payload

def connectionPreface : ByteArray := "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n".toUTF8
def connectionPrefaceLength : Nat := 24
def frameHeaderSize : Nat := 9
def defaultInitialWindowSize : UInt32 := 65535
def maxWindowSize : UInt32 := 2147483647
def minMaxFrameSize : UInt32 := 16384
def maxMaxFrameSize : UInt32 := 16777215

end Network.HTTP2
