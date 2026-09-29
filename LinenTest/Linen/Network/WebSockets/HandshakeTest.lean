/-
  Tests for `Linen.Network.WebSockets.Handshake`.

  The accept-key vectors are the ones real peers check, so a wrong GUID or a
  wrong SHA-1 fails here rather than at a browser. (Both were wrong through
  1.8.0: the GUID's last two groups were garbled and `sha1` ignored its
  input — and the old tests asserted that constant output.)
-/
import Linen.Network.WebSockets.Handshake

open Network.WebSockets

namespace Tests.Network.WebSockets.Handshake

-- RFC 6455 §1.3 / §4.2.2.
#guard webSocketGUID == "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

/-! ### `computeAcceptKey` -/

-- The worked example in RFC 6455 §1.3.
#guard computeAcceptKey "dGhlIHNhbXBsZSBub25jZQ==" == "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
-- A second key (checked against an independent SHA-1), and the key matters.
#guard computeAcceptKey "x3JJHMbDL1EzLkh9GBhXDw==" == "HSmrc0sMlYUkAGmm5OPpG2HaGWk="
#guard computeAcceptKey "dGhlIHNhbXBsZSBub25jZQ==" != computeAcceptKey "x3JJHMbDL1EzLkh9GBhXDw=="

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
#guard ((buildHandshakeResponse "dGhlIHNhbXBsZSBub25jZQ==").splitOn
  "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n").length == 2
#guard (buildHandshakeResponse "k").endsWith "\r\n\r\n"

end Tests.Network.WebSockets.Handshake
