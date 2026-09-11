/-
  Linen.Network.HTTP2.Frame.Decode — HTTP/2 frame decoding

  Parses HTTP/2 frames from wire format, per **RFC 9113** — the frame header is
  §4.1, the frame definitions §6, and the settings parameters §6.5.2.

  ## Every input is hostile

  This module reads bytes off a socket before anything has been authenticated,
  so a malformed, truncated or maliciously-sized frame is the *expected* input,
  not an exceptional one. Two consequences run through the whole file:

  - **Every decoder answers `Option`, and never raises.** A frame that cannot be
    read is `none`, which the caller turns into the connection or stream error
    RFC 9113 prescribes. A panic here would be a remote denial of service.
  - **Bounds safety is structural, not by inspection.** Byte access goes through
    `byte?`, which is `ByteArray`'s total `[i]?`, so a read past the end
    *cannot* panic regardless of what the arithmetic above it does. The
    decoders previously used `bs[i]!` behind hand-written bounds checks: correct
    as written, but safe only for as long as every future edit kept the check
    and the indexing in agreement, which is not a property worth relying on in
    a parser exposed to the network.

  The explicit length checks that remain (`decodeGoaway`, `decodeWindowUpdate`,
  `decodeRstStream`) are **semantic** rather than defensive: they enforce the
  minimum payload sizes RFC 9113 §6 specifies, and are not what keeps the
  indexing in bounds.
-/
import Linen.Network.HTTP2.Frame.Types

namespace Network.HTTP2

-- ── Reading bytes ───────────────────────────────────────────────────────────

/-- One byte, or `none` past the end.

    `ByteArray`'s total indexing. Every decoder below reads through this, so no
    arithmetic mistake in an offset can turn into a panic — it turns into a
    `none`, which is a value the caller already handles. -/
@[inline] private def byte? (bs : ByteArray) (i : Nat) : Option UInt8 := bs[i]?

-- ── Integers ────────────────────────────────────────────────────────────────

/-- A big-endian 16-bit integer at `offset`, or `none` if fewer than two bytes
    remain. -/
def decodeUInt16BE (bs : ByteArray) (offset : Nat := 0) : Option UInt16 := do
  let b0 ← byte? bs offset
  let b1 ← byte? bs (offset + 1)
  return (b0.toUInt16 <<< 8) ||| b1.toUInt16

/-- A big-endian 32-bit integer at `offset`, or `none` if fewer than four bytes
    remain. -/
def decodeUInt32BE (bs : ByteArray) (offset : Nat := 0) : Option UInt32 := do
  let b0 ← byte? bs offset
  let b1 ← byte? bs (offset + 1)
  let b2 ← byte? bs (offset + 2)
  let b3 ← byte? bs (offset + 3)
  return (b0.toUInt32 <<< 24) ||| (b1.toUInt32 <<< 16)
       ||| (b2.toUInt32 <<< 8) ||| b3.toUInt32

-- ── The frame header ────────────────────────────────────────────────────────

/-- The nine-octet frame header of RFC 9113 §4.1: a 24-bit length, an 8-bit
    type, 8 bits of flags, and a 31-bit stream id with its reserved high bit.

    `none` unless all nine octets are present. The reserved bit is discarded by
    `StreamId.fromWire`, as §4.1 requires. -/
def decodeFrameHeader (bs : ByteArray) (offset : Nat := 0) : Option FrameHeader := do
  let l0 ← byte? bs offset
  let l1 ← byte? bs (offset + 1)
  let l2 ← byte? bs (offset + 2)
  let tyByte ← byte? bs (offset + 3)
  let flags ← byte? bs (offset + 4)
  let s0 ← byte? bs (offset + 5)
  let s1 ← byte? bs (offset + 6)
  let s2 ← byte? bs (offset + 7)
  let s3 ← byte? bs (offset + 8)
  let len : UInt32 := l0.toUInt32 <<< 16 ||| l1.toUInt32 <<< 8 ||| l2.toUInt32
  let rawSid : UInt32 :=
    s0.toUInt32 <<< 24 ||| s1.toUInt32 <<< 16 ||| s2.toUInt32 <<< 8 ||| s3.toUInt32
  return { payloadLength := len
         , frameType := FrameType.fromUInt8 tyByte
         , flags := flags
         , streamId := StreamId.fromWire rawSid }

def decodeSettingsParam (bs : ByteArray) (offset : Nat := 0) : Option (SettingsKeyId × UInt32) := do
  let key ← decodeUInt16BE bs offset
  let value ← decodeUInt32BE bs (offset + 2)
  some (SettingsKeyId.fromUInt16 key, value)

/-- Decode a SETTINGS payload (a sequence of 6-byte key/value parameters).
    The original `http2` source used an explicit fuel-bounded recursion; here
    the bounded loop is just `List.mapM` over `Option` (total, no fuel). -/
def decodeSettingsPayload (bs : ByteArray) : Option (List (SettingsKeyId × UInt32)) :=
  if bs.size % 6 != 0 then none
  else
    (List.range (bs.size / 6)).mapM (fun i => decodeSettingsParam bs (i * 6))

def applySettings (s : Settings) (params : List (SettingsKeyId × UInt32)) : Settings :=
  params.foldl (fun s (k, v) =>
    match k with
    | .headerTableSize => { s with headerTableSize := v.toNat }
    | .enablePush => { s with enablePush := v != 0 }
    | .maxConcurrentStreams => { s with maxConcurrentStreams := some v.toNat }
    | .initialWindowSize =>
      if h : v.toNat ≤ 2147483647 then
        { s with initialWindowSize := v, initialWindowSize_valid := h }
      else s
    | .maxFrameSize =>
      if h1 : 16384 ≤ v.toNat then
        if h2 : v.toNat ≤ 16777215 then
          { s with maxFrameSize := v, maxFrameSize_lower := h1, maxFrameSize_upper := h2 }
        else s
      else s
    | .maxHeaderListSize => { s with maxHeaderListSize := some v.toNat }
    | .unknown _ => s
  ) s

def decodeGoaway (bs : ByteArray) : Option (StreamId × ErrorCode × ByteArray) :=
  if bs.size < 8 then none
  else do
    let lastStreamRaw ← decodeUInt32BE bs 0
    let errCode ← decodeUInt32BE bs 4
    let debugData := bs.extract 8 bs.size
    some (StreamId.fromWire lastStreamRaw, ErrorCode.fromUInt32 errCode, debugData)

def decodeWindowUpdate (bs : ByteArray) : Option UInt32 :=
  if bs.size < 4 then none
  else do
    let inc ← decodeUInt32BE bs 0
    some (inc &&& 0x7FFFFFFF)

def decodeRstStream (bs : ByteArray) : Option ErrorCode :=
  if bs.size < 4 then none
  else do
    let code ← decodeUInt32BE bs 0
    some (ErrorCode.fromUInt32 code)

/-- The five-octet priority field of RFC 9113 §6.3: an exclusive flag, a
    31-bit stream dependency, and a weight. -/
def decodePriority (bs : ByteArray) (offset : Nat := 0) :
    Option (Bool × StreamId × UInt8) := do
  let first ← decodeUInt32BE bs offset
  let weight ← byte? bs (offset + 4)
  let exclusive := (first &&& 0x80000000) != 0
  return (exclusive, StreamId.fromWire first, weight)

-- ── Padding ─────────────────────────────────────────────────────────────────

/-- Strip the padding of RFC 9113 §6.1: a pad-length octet, the content, then
    that many padding octets. Answers the content and the padding length.

    `none` when the payload is empty, or when the declared padding does not fit
    — §6.1 requires a recipient to treat padding at least as long as the
    payload as a `PROTOCOL_ERROR`, which is what refusing to decode reports. -/
def decodePadding (bs : ByteArray) : Option (ByteArray × Nat) := do
  let padLen ← (byte? bs 0).map UInt8.toNat
  if padLen + 1 > bs.size then
    none
  else
    return (bs.extract 1 (bs.size - padLen), padLen)

def validateFrameSize (h : FrameHeader) (s : Settings) : Option ErrorCode :=
  let len := h.payloadLength
  if len > s.maxFrameSize then some .frameSizeError
  else match h.frameType with
  | .ping => if len != 8 then some .frameSizeError else none
  | .rstStream => if len != 4 then some .frameSizeError else none
  | .priority => if len != 5 then some .frameSizeError else none
  | .settings =>
    if FrameFlags.test h.flags FrameFlags.ack then
      if len != 0 then some .frameSizeError else none
    else if len.toNat % 6 != 0 then some .frameSizeError else none
  | .windowUpdate => if len != 4 then some .frameSizeError else none
  | _ => none

end Network.HTTP2
