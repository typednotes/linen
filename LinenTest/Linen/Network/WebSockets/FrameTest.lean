/-
  Tests for `Linen.Network.WebSockets.Frame`.

  `Frame` has no `BEq` instance, so decoded frames are compared field by
  field via `frameEq` rather than with `==`.
-/
import Linen.Network.WebSockets.Frame

open Network.WebSockets

namespace Tests.Network.WebSockets.Frame

private def frameEq (a b : Frame) : Bool :=
  a.fin == b.fin && a.opcode == b.opcode && a.mask == b.mask && a.payload == b.payload

/-! ### `applyMask` — XOR is its own inverse -/

private def maskKey : ByteArray := ByteArray.mk #[0x01, 0x02, 0x03, 0x04]

#guard applyMask maskKey (applyMask maskKey "hello".toUTF8) == "hello".toUTF8
#guard applyMask (ByteArray.mk #[0x00, 0x00]) "hello".toUTF8 == "hello".toUTF8  -- wrong-size mask: no-op

/-! ### `Frame.encode`/`Frame.decode` roundtrip -/

private def textFrame : Frame := { fin := true, opcode := .text, mask := none, payload := "hi".toUTF8 }

#guard match Frame.decode textFrame.encode with
  | some (f, rest) => frameEq f textFrame && rest.isEmpty
  | none => false

-- A short (<126-byte) payload uses the 1-byte length encoding.
#guard textFrame.encode == ByteArray.mk #[0x81, 0x02, 0x68, 0x69]  -- FIN|text, len=2, "hi"

-- A payload of exactly 200 bytes switches to the 16-bit length encoding (0x7E marker).
private def mediumFrame : Frame :=
  { fin := true, opcode := .binary, mask := none, payload := ByteArray.mk (Array.replicate 200 0xAB) }

#guard mediumFrame.encode.get! 1 == 126
#guard match Frame.decode mediumFrame.encode with
  | some (f, rest) => frameEq f mediumFrame && rest.isEmpty
  | none => false

-- Masked client→server frames are unmasked on decode.
private def maskedEncoded : ByteArray :=
  ByteArray.mk #[0x81, 0x82] ++ maskKey ++ applyMask maskKey "hi".toUTF8

#guard match Frame.decode maskedEncoded with
  | some (f, rest) =>
    frameEq f { fin := true, opcode := .text, mask := some maskKey, payload := "hi".toUTF8 } && rest.isEmpty
  | none => false

-- Incomplete data decodes to `none`.
#guard Frame.decode ByteArray.empty |>.isNone
#guard Frame.decode (ByteArray.mk #[0x81, 0x02]) |>.isNone  -- header says 2 bytes payload, none present

-- Trailing bytes after one frame are returned as leftover.
#guard match Frame.decode (textFrame.encode ++ "more".toUTF8) with
  | some (_, rest) => rest == "more".toUTF8
  | none => false

end Tests.Network.WebSockets.Frame
