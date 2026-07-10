/-
  Linen.Network.WebSockets.Client — outbound (client-side) WebSocket connections

  Ports `Network.WebSockets.runClient` (see
  `docs/imports/websockets/dependencies.md`). Establishes a plain-TCP
  connection via `Network.HTTP.Client.Connection`, performs the RFC 6455 §4.1
  opening handshake as a client, and hands the caller a fully-framed
  `Network.WebSockets.Connection`.

  The `Sec-WebSocket-Accept` value the server returns is not verified against
  `computeAcceptKey`: `Network.WebSockets.Handshake`'s underlying SHA-1 is
  already a documented placeholder in this codebase (see its header comment),
  so accept-key verification is skipped here for the same reason it would
  always trivially fail — this client only checks that the server answered
  `101 Switching Protocols`.
-/
import Linen.Network.WebSockets.Connection
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Request
import Linen.Data.Base64
import Linen.Data.CaseInsensitive

namespace Network.WebSockets.Client

open Network.HTTP.Client
open Network.HTTP.Types
open Data (CI)

/-- A simple linear congruential generator, used only to vary the
    `Sec-WebSocket-Key` nonce byte-to-byte; the server never checks its
    entropy (see the module header). -/
private def lcgNext (x : UInt64) : UInt64 :=
  x * 6364136223846793005 + 1442695040888963407

/-- Generate a 16-byte `Sec-WebSocket-Key` nonce (RFC 6455 §4.1). -/
private def generateKeyBytes : IO ByteArray := do
  let seed ← IO.monoNanosNow
  let mut x : UInt64 := seed.toUInt64
  let mut bytes := ByteArray.empty
  for _ in [0:16] do
    x := lcgNext x
    bytes := bytes.push (x >>> 56).toUInt8
  return bytes

/-- The bytes `"\r\n\r\n"`, marking the end of an HTTP response's headers. -/
private def headerTerminator : ByteArray :=
  ByteArray.mk #[0x0D, 0x0A, 0x0D, 0x0A]

/-- Find `pat` as a contiguous subsequence of `buf`, if present. -/
private def findSubarray (buf pat : ByteArray) : Option Nat := Id.run do
  if pat.isEmpty || buf.size < pat.size then return none
  for i in [0:buf.size - pat.size + 1] do
    let mut isMatch := true
    for j in [0:pat.size] do
      if buf.get! (i + j) != pat.get! j then
        isMatch := false
    if isMatch then return some i
  return none

/-- Read from `conn` until the HTTP response head (status line + headers) is
    fully buffered. Returns the status line and any bytes read past the
    terminating blank line — these belong to the WebSocket layer, not the
    HTTP response, and must be fed to the connection as already-received
    data. -/
private def readResponseHead (conn : Network.HTTP.Client.Connection)
    : IO (String × ByteArray) := do
  let mut buf := ByteArray.empty
  let mut result : Option (String × ByteArray) := none
  while result.isNone do
    match findSubarray buf headerTerminator with
    | some idx =>
      let head := buf.extract 0 idx
      let rest := buf.extract (idx + headerTerminator.size) buf.size
      let statusLine := (String.fromUTF8! head).splitOn "\r\n" |>.headD ""
      result := some (statusLine, rest)
    | none =>
      let chunk ← conn.connRead 4096
      if chunk.isEmpty then
        result := some (String.fromUTF8! buf, ByteArray.empty)
      else
        buf := buf ++ chunk
  return result.get!

/-- Run a client `WebSocket` application against `host:port/path`.
    Connects over plain TCP, performs the client opening handshake, then
    passes the resulting `Connection` to `app`. The underlying TCP connection
    is closed once `app` returns (or throws).

    $$\text{runClient} : \text{String} \to \text{UInt16} \to \text{String} \to (\text{Connection} \to \text{IO}\ \alpha) \to \text{IO}\ \alpha$$ -/
def runClient (host : String) (port : UInt16) (path : String)
    (app : Network.WebSockets.Connection → IO α) : IO α := do
  let httpConn ← Network.HTTP.Client.connect host port false
  try
    let key := Data.Base64.encode (← generateKeyBytes)
    let req : Network.HTTP.Client.Request :=
      { method := Method.standard .GET
      , host, port, path
      , headers :=
          [ (CI.mk' "Upgrade", "websocket")
          , (hConnection, "Upgrade")
          , (CI.mk' "Sec-WebSocket-Key", key)
          , (CI.mk' "Sec-WebSocket-Version", "13")
          ] }
    Network.HTTP.Client.sendRequest httpConn req
    let (statusLine, leftover) ← readResponseHead httpConn
    unless (statusLine.splitOn " ").getD 1 "" == "101" do
      throw (IO.Error.userError s!"WebSocket handshake failed: {statusLine}")
    let leftoverRef ← IO.mkRef leftover
    let wsConn ← Network.WebSockets.mkConnection
      httpConn.connWrite
      (do
        let cur ← leftoverRef.get
        if cur.isEmpty then
          httpConn.connRead 65536
        else
          leftoverRef.set ByteArray.empty
          pure cur)
    app wsConn
  finally
    httpConn.connClose

end Network.WebSockets.Client
