/-
  Linen.Network.WebSockets.Handshake — WebSocket upgrade handshake (RFC 6455 §4)

  Ports `Network.WebSockets.Handshake`.

  The WebSocket handshake uses SHA-1 hash of the client's key concatenated
  with a magic GUID, Base64-encoded. The SHA-1 is `Linen.Crypto.SHA1` —
  through 1.8.0 this module carried a placeholder that ignored its input, so
  every accept key was the same constant and real peers rejected the
  handshake.
-/
import Linen.Network.WebSockets.Types
import Linen.Data.Base64
import Linen.Crypto.SHA1

namespace Network.WebSockets

/-- The WebSocket magic GUID used in the handshake (RFC 6455 §4.2.2). -/
def webSocketGUID : String := "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

/-- Compute the WebSocket accept key from the client's Sec-WebSocket-Key.
    $$\text{acceptKey}(k) = \text{base64}(\text{SHA-1}(k \mathbin\Vert \text{GUID}))$$ -/
def computeAcceptKey (clientKey : String) : String :=
  let combined := clientKey ++ webSocketGUID
  Data.Base64.encode (Crypto.SHA1.hash combined.toUTF8)

/-- Validate that a request is a valid WebSocket upgrade request. -/
def isValidHandshake (headers : List (String × String)) : Bool :=
  let findHeader (name : String) := headers.find? (fun (n, _) => n.toLower == name.toLower)
    |>.map (·.2)
  let upgrade := findHeader "upgrade"
  let connection := findHeader "connection"
  let version := findHeader "sec-websocket-version"
  let key := findHeader "sec-websocket-key"
  upgrade == some "websocket" &&
  connection.any (fun s => (s.toLower.splitOn ",").any (fun part => part.trimAscii.toString == "upgrade")) &&
  version == some "13" &&
  key.isSome

/-- Build the HTTP 101 Switching Protocols response for WebSocket upgrade. -/
def buildHandshakeResponse (clientKey : String) : String :=
  let acceptKey := computeAcceptKey clientKey
  "HTTP/1.1 101 Switching Protocols\r\n" ++
  "Upgrade: websocket\r\n" ++
  "Connection: Upgrade\r\n" ++
  s!"Sec-WebSocket-Accept: {acceptKey}\r\n" ++
  "\r\n"

end Network.WebSockets
