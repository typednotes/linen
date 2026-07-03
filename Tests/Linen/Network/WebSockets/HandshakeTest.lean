/-
  Tests for `Linen.Network.WebSockets.Handshake`.

  `sha1` is an honest placeholder (returns only the SHA-1 initial hash
  constants, ignoring its input — a real SHA-1 is out of scope, see the
  module's TODO), so `computeAcceptKey` is constant across client keys.
  These tests document that limitation rather than a real handshake.
-/
import Linen.Network.WebSockets.Handshake

open Network.WebSockets

namespace Tests.Network.WebSockets.Handshake

#guard webSocketGUID == "258EAFA5-E914-47DA-95CA-5AB5DC76B45B"

-- Placeholder `sha1`/`computeAcceptKey`: same output regardless of the key.
#guard computeAcceptKey "dGhlIHNhbXBsZSBub25jZQ==" == computeAcceptKey "a-different-key"
#guard computeAcceptKey "any-key" == Data.Base64.encode
  (ByteArray.mk #[0x67, 0x45, 0x23, 0x01, 0xEF, 0xCD, 0xAB, 0x89, 0x98, 0xBA, 0xDC, 0xFE,
                  0x10, 0x32, 0x54, 0x76, 0xC3, 0xD2, 0xE1, 0xF0])

/-! ### `isValidHandshake` -/

private def validHeaders : List (String × String) :=
  [("Upgrade", "websocket"), ("Connection", "Upgrade"),
   ("Sec-WebSocket-Version", "13"), ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")]

#guard isValidHandshake validHeaders == true
#guard isValidHandshake (validHeaders.filter (·.1 != "Upgrade")) == false
#guard isValidHandshake (validHeaders.filter (·.1 != "Sec-WebSocket-Key")) == false
#guard isValidHandshake [] == false
-- Case-insensitive header names and a comma-separated Connection value.
#guard isValidHandshake
  [("Upgrade", "websocket"), ("connection", "keep-alive, Upgrade"),
   ("sec-websocket-version", "13"), ("sec-websocket-key", "k")] == true

/-! ### `buildHandshakeResponse` -/

#guard (buildHandshakeResponse "dGhlIHNhbXBsZSBub25jZQ==").startsWith "HTTP/1.1 101 Switching Protocols\r\n"
#guard ((buildHandshakeResponse "k").splitOn "Sec-WebSocket-Accept: ").length > 1
#guard (buildHandshakeResponse "k").endsWith "\r\n\r\n"

end Tests.Network.WebSockets.Handshake
