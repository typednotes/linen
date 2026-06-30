/-
  Linen.Network.HTTP2.Frame.Encode — HTTP/2 frame encoding

  Serialises HTTP/2 frames to wire format as defined in RFC 9113 Section 4.
-/
import Linen.Network.HTTP2.Frame.Types

namespace Network.HTTP2

@[inline] def encodeUInt16BE (n : UInt16) : ByteArray :=
  ByteArray.empty.push (n >>> 8).toUInt8 |>.push n.toUInt8

@[inline] def encodeUInt32BE (n : UInt32) : ByteArray :=
  ByteArray.empty
    |>.push (n >>> 24).toUInt8
    |>.push (n >>> 16).toUInt8
    |>.push (n >>> 8).toUInt8
    |>.push n.toUInt8

def encodeFrameHeader (h : FrameHeader) : ByteArray :=
  let sid := h.streamId.val
  ByteArray.empty
    |>.push (h.payloadLength >>> 16).toUInt8
    |>.push (h.payloadLength >>> 8).toUInt8
    |>.push h.payloadLength.toUInt8
    |>.push h.frameType.toUInt8
    |>.push h.flags
    |>.push (sid >>> 24).toUInt8
    |>.push (sid >>> 16).toUInt8
    |>.push (sid >>> 8).toUInt8
    |>.push sid.toUInt8

def encodeFrame (f : Frame) : ByteArray := encodeFrameHeader f.header ++ f.payload

def encodeSettingsParam (key : SettingsKeyId) (value : UInt32) : ByteArray :=
  encodeUInt16BE key.toUInt16 ++ encodeUInt32BE value

def encodeSettingsPayload (params : List (SettingsKeyId × UInt32)) : ByteArray :=
  params.foldl (fun acc (k, v) => acc ++ encodeSettingsParam k v) ByteArray.empty

def buildSettingsFrame (params : List (SettingsKeyId × UInt32)) (isAck : Bool := false) : Frame :=
  let payload := if isAck then ByteArray.empty else encodeSettingsPayload params
  let flags := if isAck then FrameFlags.ack else FrameFlags.none
  { header :=
      { payloadLength := payload.size.toUInt32
        frameType := .settings
        flags := flags
        streamId := StreamId.zero }
    payload := payload }

def buildPingFrame (opaqueData : ByteArray) (isAck : Bool := false) : Frame :=
  let flags := if isAck then FrameFlags.ack else FrameFlags.none
  { header :=
      { payloadLength := opaqueData.size.toUInt32
        frameType := .ping
        flags := flags
        streamId := StreamId.zero }
    payload := opaqueData }

def buildGoawayFrame (lastStreamId : StreamId) (errorCode : ErrorCode)
    (debugData : ByteArray := ByteArray.empty) : Frame :=
  let payload := encodeUInt32BE lastStreamId.val ++ encodeUInt32BE errorCode.toUInt32 ++ debugData
  { header :=
      { payloadLength := payload.size.toUInt32
        frameType := .goaway
        flags := FrameFlags.none
        streamId := StreamId.zero }
    payload := payload }

def buildWindowUpdateFrame (streamId : StreamId) (increment : UInt32) : Frame :=
  let payload := encodeUInt32BE (increment &&& 0x7FFFFFFF)
  { header :=
      { payloadLength := 4
        frameType := .windowUpdate
        flags := FrameFlags.none
        streamId := streamId }
    payload := payload }

def buildRstStreamFrame (streamId : StreamId) (errorCode : ErrorCode) : Frame :=
  let payload := encodeUInt32BE errorCode.toUInt32
  { header :=
      { payloadLength := 4
        frameType := .rstStream
        flags := FrameFlags.none
        streamId := streamId }
    payload := payload }

def buildHeadersFrame (streamId : StreamId) (headerBlock : ByteArray)
    (endStream : Bool := false) (endHeaders : Bool := true) : Frame :=
  let flags := FrameFlags.none
  let flags := if endStream then FrameFlags.set flags FrameFlags.endStream else flags
  let flags := if endHeaders then FrameFlags.set flags FrameFlags.endHeaders else flags
  { header :=
      { payloadLength := headerBlock.size.toUInt32
        frameType := .headers
        flags := flags
        streamId := streamId }
    payload := headerBlock }

def buildDataFrame (streamId : StreamId) (payload : ByteArray)
    (endStream : Bool := false) : Frame :=
  let flags := if endStream then FrameFlags.endStream else FrameFlags.none
  { header :=
      { payloadLength := payload.size.toUInt32
        frameType := .data
        flags := flags
        streamId := streamId }
    payload := payload }

def encodePriority (exclusive : Bool) (dependency : StreamId) (weight : UInt8) : ByteArray :=
  let dep := dependency.val
  let first := if exclusive then dep ||| 0x80000000 else dep
  encodeUInt32BE first |>.push weight

def encodePadding (payload : ByteArray) (padLen : Nat) : ByteArray :=
  let padLenByte := (min padLen 255).toUInt8
  let result := ByteArray.empty.push padLenByte ++ payload
  (List.range padLen).foldl (fun (acc : ByteArray) _ => acc.push 0) result

def buildContinuationFrame (streamId : StreamId) (headerBlock : ByteArray)
    (endHeaders : Bool := false) : Frame :=
  let flags := if endHeaders then FrameFlags.endHeaders else FrameFlags.none
  { header :=
      { payloadLength := headerBlock.size.toUInt32
        frameType := .continuation
        flags := flags
        streamId := streamId }
    payload := headerBlock }

/-- Split a header block into `maxSize`-byte chunks (for HEADERS +
    CONTINUATION fragmentation).  The original `http2` source used an explicit
    fuel-bounded recursion; here the chunk count is a ceiling division and the
    pieces are extracted with a total `List.range` map. -/
def splitHeaderBlock (block : ByteArray) (maxSize : Nat) : List ByteArray :=
  if maxSize == 0 then [block]
  else
    let n := (block.size + maxSize - 1) / maxSize
    (List.range n).map (fun i =>
      block.extract (i * maxSize) (min ((i + 1) * maxSize) block.size))

end Network.HTTP2
