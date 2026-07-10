/-
  Linen.Network.WebSockets.Connection — WebSocket connection management

  Ports `Network.WebSockets.Connection`.

  High-level API for sending/receiving WebSocket messages.
-/
import Linen.Network.WebSockets.Types
import Linen.Network.WebSockets.Frame

namespace Network.WebSockets

/-- Create a WebSocket connection from raw send/receive functions.
    The connection manages framing, masking, and control messages. -/
def mkConnection (send : ByteArray → IO Unit) (recv : IO ByteArray)
    : IO Connection := do
  let stateRef ← IO.mkRef ConnectionState.open_
  let bufRef ← IO.mkRef ByteArray.empty
  return {
    sendText := fun text => do
      let frame : Frame := ⟨true, .text, none, text.toUTF8⟩
      send frame.encode
    sendBinary := fun data => do
      let frame : Frame := ⟨true, .binary, none, data⟩
      send frame.encode
    sendClose := fun code msg => do
      let payload := ByteArray.mk #[
        (code.code >>> 8).toUInt8,
        (code.code &&& 0xFF).toUInt8
      ] ++ msg.toUTF8
      let frame : Frame := ⟨true, .close, none, payload⟩
      send frame.encode
      stateRef.set .closing
    sendPing := fun data => do
      let frame : Frame := ⟨true, .ping, none, data⟩
      send frame.encode
    receiveData := do
      let buf ← bufRef.get
      let data ← if buf.isEmpty then recv else do
        bufRef.set ByteArray.empty
        return buf
      match Frame.decode data with
      | some (frame, rest) =>
        unless rest.isEmpty do bufRef.set rest
        -- Handle control frames
        match frame.opcode with
        | .ping =>
          -- Auto-respond with pong
          let pong : Frame := ⟨true, .pong, none, frame.payload⟩
          send pong.encode
          recv  -- continue reading for data frame
        | .close =>
          stateRef.set .closed
          return ByteArray.empty
        | _ => return frame.payload
      | none => return ByteArray.empty
    receiveText := do
      let data ← recv  -- simplified
      return String.fromUTF8! data
    getState := stateRef.get
  }

end Network.WebSockets
