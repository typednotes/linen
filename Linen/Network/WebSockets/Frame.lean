/-
  Linen.Network.WebSockets.Frame — WebSocket frame encoding/decoding (RFC 6455 §5)

  Ports `Network.WebSockets.Frame`.

  ## Guarantees
  - Masking key is exactly 4 bytes (when present)
  - Payload length encoding follows the 7/16/64-bit threshold rules
  - Masking is its own inverse (XOR self-inverse)
-/
import Linen.Network.WebSockets.Types

namespace Network.WebSockets

/-- A parsed WebSocket frame. -/
structure Frame where
  fin : Bool
  opcode : Opcode
  mask : Option ByteArray  -- 4 bytes if present
  payload : ByteArray

/-- Apply/remove XOR masking to payload data (same operation for mask and unmask). -/
def applyMask (maskKey : ByteArray) (data : ByteArray) : ByteArray :=
  if maskKey.size != 4 then data
  else Id.run do
    let mut result := ByteArray.empty
    for i in [:data.size] do
      let masked := data.get! i ^^^ maskKey.get! (i % 4)
      result := result.push masked
    return result

/-- Encode a frame to bytes for sending.
    Server → client frames are NOT masked (per spec). -/
def Frame.encode (frame : Frame) : ByteArray := Id.run do
  let mut buf := ByteArray.empty
  -- Byte 1: FIN + opcode
  let byte1 := (if frame.fin then 0x80 else 0x00) ||| frame.opcode.toUInt8
  buf := buf.push byte1
  -- Byte 2: payload length (no mask bit for server→client)
  let len := frame.payload.size
  if len < 126 then
    buf := buf.push len.toUInt8
  else if len < 65536 then
    buf := buf.push 126
    buf := buf.push (len / 256).toUInt8
    buf := buf.push (len % 256).toUInt8
  else
    buf := buf.push 127
    for i in [:8] do
      buf := buf.push ((len >>> ((7 - i) <<< 3)) % 256).toUInt8
  -- Payload (unmasked for server-to-client)
  buf := buf ++ frame.payload
  return buf

/-- Decode a frame from raw bytes.
    Returns the frame and remaining bytes, or none on incomplete data. -/
def Frame.decode (data : ByteArray) : Option (Frame × ByteArray) := do
  guard (data.size >= 2)
  let byte1 := data.get! 0
  let byte2 := data.get! 1
  let fin := byte1 &&& 0x80 != 0
  let opcode := Opcode.fromUInt8 (byte1 &&& 0x0F)
  let masked := byte2 &&& 0x80 != 0
  let lenByte := (byte2 &&& 0x7F).toNat
  let (payloadLen, headerEnd) ←
    if lenByte < 126 then pure (lenByte, 2)
    else if lenByte == 126 then do
      guard (data.size >= 4)
      let hi := (data.get! 2).toNat
      let lo := (data.get! 3).toNat
      pure (hi * 256 + lo, 4)
    else do
      guard (data.size >= 10)
      let mut len := 0
      for i in [2:10] do
        len := len * 256 + (data.get! i).toNat
      pure (len, 10)
  let (maskKey, dataStart) ←
    if masked then do
      guard (data.size >= headerEnd + 4)
      pure (some (data.extract headerEnd (headerEnd + 4)), headerEnd + 4)
    else
      pure (none, headerEnd)
  guard (data.size >= dataStart + payloadLen)
  let rawPayload := data.extract dataStart (dataStart + payloadLen)
  let payload := match maskKey with
    | some key => applyMask key rawPayload
    | none => rawPayload
  let remaining := data.extract (dataStart + payloadLen) data.size
  pure (⟨fin, opcode, maskKey, payload⟩, remaining)

end Network.WebSockets
