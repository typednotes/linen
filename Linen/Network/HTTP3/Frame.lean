/-
  Linen.Network.HTTP3.Frame -- HTTP/3 frame types and variable-length integer encoding

  Defines HTTP/3 frame types (RFC 9114 Section 7) and the QUIC variable-length
  integer encoding (RFC 9000 Section 16) used throughout HTTP/3.

  ## Design

  - `FrameType` enumerates all standard HTTP/3 frame types
  - Variable-length integers use 1/2/4/8 bytes with a 2-bit length prefix
  - Frame encoding: varint(type) ++ varint(length) ++ payload
  - Settings are encoded as a sequence of varint(id) ++ varint(value) pairs

  ## Guarantees

  - `encodeVarInt`/`decodeVarInt` are inverse for values in range [0, 2^62 - 1]
  - `encodeVarInt` uses the minimum number of bytes for the value
  - Frame encode/decode roundtrip for well-formed frames

  ## Haskell equivalent
  HTTP/3 frame types from the `http3` package
-/

namespace Network.HTTP3

/-- HTTP/3 frame types (RFC 9114 Section 7).
    $$\text{FrameType}$$ enumerates the standard frame type identifiers. -/
inductive FrameType where
  /-- DATA frame (0x0). Carries request or response body data. -/
  | data
  /-- HEADERS frame (0x1). Carries an encoded header block. -/
  | headers
  /-- CANCEL_PUSH frame (0x3). Cancels a server push. -/
  | cancelPush
  /-- SETTINGS frame (0x4). Conveys configuration parameters. -/
  | settings
  /-- PUSH_PROMISE frame (0x5). Server push initiation. -/
  | pushPromise
  /-- GOAWAY frame (0x7). Initiates graceful shutdown. -/
  | goaway
  /-- MAX_PUSH_ID frame (0xD). Controls server push. -/
  | maxPushId
  /-- Unknown/extension frame type. -/
  | unknown (id : UInt64)
  deriving Repr, BEq

/-- Convert a frame type to its numeric identifier.
    $$\text{toId} : \text{FrameType} \to \text{UInt64}$$ -/
def FrameType.toId : FrameType → UInt64
  | .data        => 0x0
  | .headers     => 0x1
  | .cancelPush  => 0x3
  | .settings    => 0x4
  | .pushPromise => 0x5
  | .goaway      => 0x7
  | .maxPushId   => 0xD
  | .unknown id  => id

/-- Parse a frame type from its numeric identifier.
    $$\text{fromId} : \text{UInt64} \to \text{FrameType}$$ -/
def FrameType.fromId : UInt64 → FrameType
  | 0x0 => .data
  | 0x1 => .headers
  | 0x3 => .cancelPush
  | 0x4 => .settings
  | 0x5 => .pushPromise
  | 0x7 => .goaway
  | 0xD => .maxPushId
  | id  => .unknown id

instance : ToString FrameType where
  toString
    | .data        => "DATA"
    | .headers     => "HEADERS"
    | .cancelPush  => "CANCEL_PUSH"
    | .settings    => "SETTINGS"
    | .pushPromise => "PUSH_PROMISE"
    | .goaway      => "GOAWAY"
    | .maxPushId   => "MAX_PUSH_ID"
    | .unknown id  => s!"UNKNOWN({id})"

/-- Roundtrip theorems for FrameType fromId/toId. -/
theorem FrameType.roundtrip_data : FrameType.fromId (FrameType.toId .data) = .data := rfl
theorem FrameType.roundtrip_headers : FrameType.fromId (FrameType.toId .headers) = .headers := rfl
theorem FrameType.roundtrip_cancelPush : FrameType.fromId (FrameType.toId .cancelPush) = .cancelPush := rfl
theorem FrameType.roundtrip_settings : FrameType.fromId (FrameType.toId .settings) = .settings := rfl
theorem FrameType.roundtrip_pushPromise : FrameType.fromId (FrameType.toId .pushPromise) = .pushPromise := rfl
theorem FrameType.roundtrip_goaway : FrameType.fromId (FrameType.toId .goaway) = .goaway := rfl
theorem FrameType.roundtrip_maxPushId : FrameType.fromId (FrameType.toId .maxPushId) = .maxPushId := rfl

-- ============================================================================
-- Variable-length integer encoding (RFC 9000 Section 16)
-- ============================================================================

/-- The maximum value encodable as a QUIC variable-length integer.
    $$2^{62} - 1 = 4611686018427387903$$ -/
def maxVarInt : UInt64 := 4611686018427387903

/-- Helper: get byte at offset, returning 0 if out of bounds. -/
@[inline] private def getByte (buf : ByteArray) (i : Nat) : UInt8 :=
  if h : i < buf.size then buf[i] else 0

/-- Helper: convert a UInt64 to UInt8 by taking the low byte. -/
@[inline] private def u64toU8 (v : UInt64) : UInt8 := v.toUInt8

/-- Encode a value as a QUIC variable-length integer (RFC 9000 Section 16).
    Uses the minimum number of bytes for the given value.
    $$\text{encodeVarInt} : \text{UInt64} \to \text{ByteArray}$$ -/
def encodeVarInt (val : UInt64) : ByteArray :=
  if val ≤ 63 then
    ByteArray.mk #[u64toU8 val]
  else if val ≤ 16383 then
    let b0 := u64toU8 ((0x40 : UInt64) ||| (val >>> 8))
    let b1 := u64toU8 val
    ByteArray.mk #[b0, b1]
  else if val ≤ 1073741823 then
    let b0 := u64toU8 ((0x80 : UInt64) ||| (val >>> 24))
    let b1 := u64toU8 (val >>> 16)
    let b2 := u64toU8 (val >>> 8)
    let b3 := u64toU8 val
    ByteArray.mk #[b0, b1, b2, b3]
  else
    let b0 := u64toU8 ((0xC0 : UInt64) ||| (val >>> 56))
    let b1 := u64toU8 (val >>> 48)
    let b2 := u64toU8 (val >>> 40)
    let b3 := u64toU8 (val >>> 32)
    let b4 := u64toU8 (val >>> 24)
    let b5 := u64toU8 (val >>> 16)
    let b6 := u64toU8 (val >>> 8)
    let b7 := u64toU8 val
    ByteArray.mk #[b0, b1, b2, b3, b4, b5, b6, b7]

/-- Decode a QUIC variable-length integer from a ByteArray at the given offset.
    $$\text{decodeVarInt} : \text{ByteArray} \to \mathbb{N} \to \text{Option}(\text{UInt64} \times \mathbb{N})$$
    Returns the decoded value and the number of bytes consumed, or `none` on error. -/
def decodeVarInt (buf : ByteArray) (offset : Nat := 0) : Option (UInt64 × Nat) :=
  if offset ≥ buf.size then none
  else
    let firstByte := getByte buf offset
    let pfx := ((firstByte >>> 6) &&& 0x03).toNat
    match pfx with
    | 0 =>
      some (((firstByte &&& 0x3F).toUInt64 : UInt64), 1)
    | 1 =>
      if offset + 1 < buf.size then
        let b1 := getByte buf (offset + 1)
        let val : UInt64 := ((firstByte &&& 0x3F).toUInt64 <<< 8) ||| b1.toUInt64
        some (val, 2)
      else none
    | 2 =>
      if offset + 3 < buf.size then
        let b1 := getByte buf (offset + 1)
        let b2 := getByte buf (offset + 2)
        let b3 := getByte buf (offset + 3)
        let val : UInt64 := ((firstByte &&& 0x3F).toUInt64 <<< 24) |||
                   (b1.toUInt64 <<< 16) |||
                   (b2.toUInt64 <<< 8) |||
                   b3.toUInt64
        some (val, 4)
      else none
    | _ =>
      if offset + 7 < buf.size then
        let b1 := getByte buf (offset + 1)
        let b2 := getByte buf (offset + 2)
        let b3 := getByte buf (offset + 3)
        let b4 := getByte buf (offset + 4)
        let b5 := getByte buf (offset + 5)
        let b6 := getByte buf (offset + 6)
        let b7 := getByte buf (offset + 7)
        let val : UInt64 := ((firstByte &&& 0x3F).toUInt64 <<< 56) |||
                   (b1.toUInt64 <<< 48) |||
                   (b2.toUInt64 <<< 40) |||
                   (b3.toUInt64 <<< 32) |||
                   (b4.toUInt64 <<< 24) |||
                   (b5.toUInt64 <<< 16) |||
                   (b6.toUInt64 <<< 8) |||
                   b7.toUInt64
        some (val, 8)
      else none

-- ============================================================================
-- HTTP/3 Frame encoding/decoding
-- ============================================================================

/-- An HTTP/3 frame: type + payload.
    $$\text{Frame} = \text{FrameType} \times \text{ByteArray}$$ -/
structure Frame where
  /-- The frame type. -/
  frameType : FrameType
  /-- The frame payload (raw bytes). -/
  payload : ByteArray

/-- Encode an HTTP/3 frame to wire format: varint(type) ++ varint(length) ++ payload.
    $$\text{encode} : \text{Frame} \to \text{ByteArray}$$ -/
def Frame.encode (f : Frame) : ByteArray :=
  let typeEnc := encodeVarInt f.frameType.toId
  let lenEnc := encodeVarInt f.payload.size.toUInt64
  typeEnc ++ lenEnc ++ f.payload

/-- Decode an HTTP/3 frame from wire format at the given offset.
    $$\text{decode} : \text{ByteArray} \to \mathbb{N} \to \text{Option}(\text{Frame} \times \mathbb{N})$$
    Returns the decoded frame and total bytes consumed, or `none` on error. -/
def Frame.decode (buf : ByteArray) (offset : Nat := 0) : Option (Frame × Nat) := do
  let (typeId, typeLen) ← decodeVarInt buf offset
  let (payloadLen, lenLen) ← decodeVarInt buf (offset + typeLen)
  let payloadStart := offset + typeLen + lenLen
  let payloadEnd := payloadStart + payloadLen.toNat
  if payloadEnd ≤ buf.size then
    let payload := buf.extract payloadStart payloadEnd
    let frame := { frameType := FrameType.fromId typeId, payload }
    some (frame, typeLen + lenLen + payloadLen.toNat)
  else none

-- ============================================================================
-- HTTP/3 Settings (RFC 9114 Section 7.2.4.1; QPACK identifiers from RFC 9204 Section 5)
-- ============================================================================

/-- HTTP/3 settings identifiers.
    `settingsMaxFieldSectionSize` is defined by RFC 9114 Section 7.2.4.1;
    `settingsQpackMaxTableCapacity`/`settingsQpackBlockedStreams` are defined
    by RFC 9204 (QPACK) Section 5. -/
def settingsQpackMaxTableCapacity : UInt64 := 0x1
def settingsMaxFieldSectionSize   : UInt64 := 0x6
def settingsQpackBlockedStreams    : UInt64 := 0x7

/-- HTTP/3 settings values.
    $$\text{H3Settings} = \{ \text{maxFieldSectionSize} : \mathbb{N},\; \ldots \}$$ -/
structure H3Settings where
  /-- Maximum size of a header section (SETTINGS_MAX_FIELD_SECTION_SIZE, 0x6). -/
  maxFieldSectionSize : Nat := 0
  /-- Maximum QPACK dynamic table capacity (SETTINGS_QPACK_MAX_TABLE_CAPACITY, 0x1). -/
  qpackMaxTableCapacity : Nat := 0
  /-- Maximum number of blocked QPACK streams (SETTINGS_QPACK_BLOCKED_STREAMS, 0x7). -/
  qpackBlockedStreams : Nat := 0
  deriving Repr, BEq

/-- Default HTTP/3 settings.
    $$\text{defaultH3Settings} = \text{H3Settings}\{\}$$ -/
def H3Settings.default : H3Settings := {}

/-- Encode HTTP/3 settings as a sequence of varint(id) ++ varint(value) pairs.
    Only non-zero settings are included.
    $$\text{encode} : \text{H3Settings} \to \text{ByteArray}$$ -/
def H3Settings.encode (s : H3Settings) : ByteArray := Id.run do
  let mut buf := ByteArray.empty
  if s.qpackMaxTableCapacity > 0 then
    buf := buf ++ encodeVarInt settingsQpackMaxTableCapacity ++ encodeVarInt s.qpackMaxTableCapacity.toUInt64
  if s.maxFieldSectionSize > 0 then
    buf := buf ++ encodeVarInt settingsMaxFieldSectionSize ++ encodeVarInt s.maxFieldSectionSize.toUInt64
  if s.qpackBlockedStreams > 0 then
    buf := buf ++ encodeVarInt settingsQpackBlockedStreams ++ encodeVarInt s.qpackBlockedStreams.toUInt64
  return buf

/-- Every successful varint decode consumes at least one byte. -/
theorem decodeVarInt_consumed {buf : ByteArray} {off : Nat} {v : UInt64} {n : Nat}
    (h : decodeVarInt buf off = some (v, n)) : 0 < n := by
  rw [decodeVarInt] at h
  dsimp only at h
  split at h
  · exact absurd h (by simp)
  · split at h
    · simp only [Option.some.injEq, Prod.mk.injEq] at h; omega
    · split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h; omega
      · exact absurd h (by simp)
    · split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h; omega
      · exact absurd h (by simp)
    · split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h; omega
      · exact absurd h (by simp)

set_option linter.unusedVariables false in
/-- Worker for decoding settings: processes `varint(id) ++ varint(value)` pairs
    until the buffer is exhausted.  The original `http3` source used a
    fuel-bounded recursion; this is well-founded on `buf.size - pos`, since each
    pair begins with a varint consuming ≥ 1 byte (`decodeVarInt_consumed`). -/
private def decodeSettingsPairs (buf : ByteArray) (pos : Nat) (settings : H3Settings) :
    Option H3Settings :=
  if pos ≥ buf.size then some settings
  else
    match h : decodeVarInt buf pos with
    | none => none
    | some (settingId, idLen) =>
      match decodeVarInt buf (pos + idLen) with
      | none => none
      | some (settingVal, valLen) =>
        let settings' :=
          if settingId == settingsQpackMaxTableCapacity then
            { settings with qpackMaxTableCapacity := settingVal.toNat }
          else if settingId == settingsMaxFieldSectionSize then
            { settings with maxFieldSectionSize := settingVal.toNat }
          else if settingId == settingsQpackBlockedStreams then
            { settings with qpackBlockedStreams := settingVal.toNat }
          else settings
        decodeSettingsPairs buf (pos + idLen + valLen) settings'
termination_by buf.size - pos
decreasing_by
  have hc := decodeVarInt_consumed h
  simp_wf
  omega

/-- Decode HTTP/3 settings from a byte buffer.
    $$\text{decode} : \text{ByteArray} \to \text{Option}(\text{H3Settings})$$ -/
def H3Settings.decode (buf : ByteArray) : Option H3Settings :=
  decodeSettingsPairs buf 0 H3Settings.default

end Network.HTTP3
