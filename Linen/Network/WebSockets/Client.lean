/-
  Linen.Network.WebSockets.Client — outbound (client-side) WebSocket connections

  Ports `Network.WebSockets.runClient` (see
  `docs/imports/websockets/dependencies.md`). Establishes a plain-TCP
  connection via `Network.HTTP.Client.Connection`, performs the RFC 6455 §4.1
  opening handshake as a client, and hands the caller a fully-framed
  `Network.WebSockets.Connection`.

  The server's answer is checked as RFC 6455 §4.1 requires of a client
  (`checkHandshakeResponse`): status `101`, `Upgrade: websocket`,
  `Connection: Upgrade`, and a `Sec-WebSocket-Accept` equal to
  `computeAcceptKey` of the key that was sent. Through 1.8.0 only the status was
  checked, because the handshake's SHA-1 was a placeholder.
-/
import Linen.Network.WebSockets.Connection
import Linen.Network.WebSockets.Handshake
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

/-- Check a server's opening-handshake response head (status line and header
    lines, without the terminating blank line) against the `Sec-WebSocket-Key`
    the client sent, per RFC 6455 §4.1's client requirements:

    1. the status code is `101`;
    2. `Upgrade` is `websocket` (case-insensitively);
    3. `Connection` contains the token `upgrade` (case-insensitively);
    4. `Sec-WebSocket-Accept` is exactly `computeAcceptKey key`.

    Header names are matched case-insensitively. Returns the reason on
    failure, so the client can say *why* it is failing the connection. -/
def checkHandshakeResponse (key : String) (head : String) : Except String Unit := do
  let lines := head.splitOn "\r\n"
  let statusLine := lines.headD ""
  unless (statusLine.splitOn " ").getD 1 "" == "101" do
    throw s!"WebSocket handshake failed: {statusLine}"
  let headers : List (String × String) := lines.drop 1 |>.filterMap fun line =>
    match line.splitOn ":" with
    | name :: rest@(_ :: _) =>
      some (name.trimAscii.toString.toLower, (":".intercalate rest).trimAscii.toString)
    | _ => none
  let header (name : String) : Option String := headers.lookup name
  unless (header "upgrade").any (·.toLower == "websocket") do
    throw "WebSocket handshake failed: missing `Upgrade: websocket`"
  unless (header "connection").any (fun v =>
      (v.toLower.splitOn ",").any (·.trimAscii.toString == "upgrade")) do
    throw "WebSocket handshake failed: missing `Connection: Upgrade`"
  match header "sec-websocket-accept" with
  | none => throw "WebSocket handshake failed: missing `Sec-WebSocket-Accept`"
  | some accept =>
    unless accept == computeAcceptKey key do
      throw s!"WebSocket handshake failed: `Sec-WebSocket-Accept: {accept}` does not match the key sent"

/-- Read from `conn` until the HTTP response head (status line + headers) is
    fully buffered. Returns the head (without its terminating blank line) and
    any bytes read past it — these belong to the WebSocket layer, not the
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
      result := some (String.fromUTF8! head, rest)
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
    let (head, leftover) ← readResponseHead httpConn
    match checkHandshakeResponse key head with
    | .error reason => throw (IO.Error.userError reason)
    | .ok () => pure ()
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
