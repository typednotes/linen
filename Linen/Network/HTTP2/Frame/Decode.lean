/-
  Linen.Network.HTTP2.Frame.Decode — HTTP/2 frame decoding

  Parses HTTP/2 frames from wire format as defined in RFC 9113 Section 4.
-/
import Linen.Network.HTTP2.Frame.Types

namespace Network.HTTP2

def decodeUInt16BE (bs : ByteArray) (offset : Nat := 0) : Option UInt16 :=
  if offset + 2 > bs.size then none
  else
    let b0 := bs[offset]!
    let b1 := bs[offset + 1]!
    some ((b0.toUInt16 <<< 8) ||| b1.toUInt16)

def decodeUInt32BE (bs : ByteArray) (offset : Nat := 0) : Option UInt32 :=
  if offset + 4 > bs.size then none
  else
    let b0 := bs[offset]!
    let b1 := bs[offset + 1]!
    let b2 := bs[offset + 2]!
    let b3 := bs[offset + 3]!
    some ((b0.toUInt32 <<< 24) ||| (b1.toUInt32 <<< 16) ||| (b2.toUInt32 <<< 8) ||| b3.toUInt32)

def decodeFrameHeader (bs : ByteArray) (offset : Nat := 0) : Option FrameHeader :=
  if offset + 9 > bs.size then none
  else
    let len : UInt32 :=
      (bs[offset]!).toUInt32 <<< 16 |||
      (bs[offset + 1]!).toUInt32 <<< 8 |||
      (bs[offset + 2]!).toUInt32
    let ft := FrameType.fromUInt8 (bs[offset + 3]!)
    let flags := bs[offset + 4]!
    let rawSid : UInt32 :=
      bs[offset + 5]!.toUInt32 <<< 24 |||
      bs[offset + 6]!.toUInt32 <<< 16 |||
      bs[offset + 7]!.toUInt32 <<< 8 |||
      bs[offset + 8]!.toUInt32
    some { payloadLength := len
           frameType := ft
           flags := flags
           streamId := StreamId.fromWire rawSid }

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

def decodePriority (bs : ByteArray) (offset : Nat := 0) : Option (Bool × StreamId × UInt8) :=
  if offset + 5 > bs.size then none
  else do
    let first ← decodeUInt32BE bs offset
    let exclusive := (first &&& 0x80000000) != 0
    let weight := bs[offset + 4]!
    some (exclusive, StreamId.fromWire first, weight)

def decodePadding (bs : ByteArray) : Option (ByteArray × Nat) :=
  if bs.size == 0 then none
  else
    let padLen := bs[0]!.toNat
    if padLen + 1 > bs.size then none
    else
      let content := bs.extract 1 (bs.size - padLen)
      some (content, padLen)

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
