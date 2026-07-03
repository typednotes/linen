/-
  Linen.Network.WebSockets.Handshake — WebSocket upgrade handshake (RFC 6455 §4)

  Ports Hale's `Network.WebSockets.Handshake`.

  The WebSocket handshake uses SHA-1 hash of the client's key concatenated
  with a magic GUID, Base64-encoded.
-/
import Linen.Network.WebSockets.Types
import Linen.Data.Base64

namespace Network.WebSockets

/-- The WebSocket magic GUID used in the handshake (RFC 6455 §4.2.2). -/
def webSocketGUID : String := "258EAFA5-E914-47DA-95CA-5AB5DC76B45B"

/-- Placeholder SHA-1 for WebSocket handshake.
    TODO: Implement full SHA-1 (FIPS 180-4) for production use.
    This returns the SHA-1 initial hash values as a placeholder. -/
private def sha1 (input : ByteArray) : ByteArray := Id.run do
  -- SHA-1 initial hash values
  let vals : Array UInt32 := #[0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0]
  let mut hash := ByteArray.empty
  for b in vals do
    hash := hash.push ((b >>> 24).toUInt8)
    hash := hash.push ((b >>> 16).toUInt8)
    hash := hash.push ((b >>> 8).toUInt8)
    hash := hash.push b.toUInt8
  -- TODO: actual SHA-1 computation using `input`
  let _ := input  -- suppress unused warning
  return hash

/-- Compute the WebSocket accept key from the client's Sec-WebSocket-Key.
    $$\text{acceptKey}(k) = \text{base64}(\text{SHA-1}(k \mathbin\Vert \text{GUID}))$$ -/
def computeAcceptKey (clientKey : String) : String :=
  let combined := clientKey ++ webSocketGUID
  let hash := sha1 combined.toUTF8
  Data.Base64.encode hash

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
