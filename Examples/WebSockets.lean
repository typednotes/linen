/-
  Examples.WebSockets — `Network.WebSockets` (RFC 6455) end-to-end, bridged
  onto a real `Network.WebApp.Server` connection via
  `Network.WebApp.Server.WebSockets.websocketsOr`.

  The demo `Application` routes upgrade requests to `echoWsApp` (a
  `Network.WebSockets.ServerApp` that echoes every text message back) and
  everything else to a plain 404 `backupApp` -- exactly the
  `websocketsOr opts wsApp backup` composition the bridge module exists
  for. `Network.WebApp.Server.withApplication` then runs it on a real,
  OS-assigned TCP port, same as `Examples.Server`'s plain HTTP demo; the
  only difference is that a WebSocket request's connection gets hijacked
  into frame-based I/O after the opening handshake, via `.responseRaw`.

  The client side hand-rolls the RFC 6455 §4 opening handshake (a bare
  `GET .. Upgrade: websocket` request) and then reads/writes frames
  directly with `Frame.encode`/`Frame.decode` -- `Network.WebSockets` only
  ships a server-facing `Connection` (built from an already-accepted
  socket), so a client has to speak the wire protocol itself.

  Note: outgoing client frames below are sent unmasked. RFC 6455 requires
  clients to mask every frame, but `Frame.encode` only ever produces
  unmasked wire bytes (see that module's docstring: masking is only
  undone on `decode`), and `mkConnection`'s `receiveData` decodes masked
  and unmasked frames identically -- so this is the honest capability of
  the current port, not a demo shortcut around a requirement the server
  actually enforces.

  Args: (none) -- runs a few round trips and exits non-zero on any mismatch
-/
import Linen.Network.WebApp.Server.WebSockets
import Linen.Network.WebApp.Server.WithApplication
import Linen.Network.Socket.Blocking

open Network.WebApp
open Network.WebApp.Server
open Network.WebApp.Server.WebSockets
open Network.WebSockets
open Network.HTTP.Types
open Network.Socket
open Network.Socket.Blocking

namespace Examples.WebSockets

-- ── The demo WebSocket app ──

/-- Echo every text/binary message back, until the peer sends a close frame
    (or the connection drops) -- `receiveData` returns an empty payload in
    both cases. -/
def echoWsApp : ServerApp := fun pending => do
  let conn ← pending.acceptIO
  let mut go := true
  while go do
    let data ← conn.receiveData
    if data.isEmpty then
      go := false
    else
      conn.sendText (String.fromUTF8! data)

/-- Everything that isn't a WebSocket upgrade gets a fixed 404. -/
def backupApp : Application :=
  fun _req respond =>
    AppM.respondIO respond (pure (responseLBS status404 [] "not a websocket endpoint"))

/-- `echoWsApp` on WebSocket upgrades, `backupApp` on everything else. -/
def demoApplication : Application :=
  websocketsOr defaultConnectionOptions echoWsApp backupApp

-- ── A hand-rolled WebSocket client ──

/-- Send the RFC 6455 §4 opening handshake. -/
def sendHandshake (conn : Socket .connected) (path key : String) : IO Unit := do
  let req := s!"GET {path} HTTP/1.1\r\nHost: 127.0.0.1\r\nUpgrade: websocket\r\n" ++
    s!"Connection: Upgrade\r\nSec-WebSocket-Key: {key}\r\nSec-WebSocket-Version: 13\r\n\r\n"
  Blocking.sendAll conn req.toUTF8

/-- Read bytes off `conn` until the header/body separator, returning the
    header text and any bytes already buffered past it -- those belong to
    the WebSocket stream that follows and must be fed to the first
    `readFrame` call. Mirrors `Examples.WebApp.readRequest`'s framing. -/
def readHandshakeResponse (conn : Socket .connected) : IO (String × ByteArray) := do
  let mut buf := ByteArray.empty
  let mut header := (none : Option String)
  while header.isNone do
    let chunk ← Blocking.recv conn
    buf := buf ++ chunk
    let parts := (String.fromUTF8! buf).splitOn "\r\n\r\n"
    if parts.length > 1 then
      header := some (parts.getD 0 "")
  let h := header.get!
  pure (h, buf.extract (h.toUTF8.size + 4) buf.size)

/-- Read one WebSocket frame off `conn`, buffering leftover bytes in
    `bufRef` across calls (a `recv` can straddle frame boundaries in
    either direction). -/
def readFrame (conn : Socket .connected) (bufRef : IO.Ref ByteArray) : IO Frame := do
  let mut result := (none : Option Frame)
  while result.isNone do
    let buf ← bufRef.get
    match Frame.decode buf with
    | some (frame, rest) => bufRef.set rest; result := some frame
    | none =>
      let chunk ← Blocking.recv conn
      bufRef.set (buf ++ chunk)
  match result with
  | some frame => pure frame
  | none => unreachable!

/-- Send a text frame (unmasked -- see the module docstring). -/
def sendText (conn : Socket .connected) (text : String) : IO Unit :=
  Blocking.sendAll conn (Frame.encode ⟨true, .text, none, text.toUTF8⟩)

def demoRoundTrip : IO Bool := do
  IO.println "── Network.WebSockets + Server.WebSockets bridge over a real server ──"
  withApplication (pure demoApplication) fun port => do
    IO.println s!"  server listening on 127.0.0.1:{port}"
    let client ← socket .inet .stream
    let conn ← Blocking.connect client { host := "127.0.0.1", port := port }

    sendHandshake conn "/chat" "dGhlIHNhbXBsZSBub25jZQ=="
    let (header, leftover) ← readHandshakeResponse conn
    let statusLine := (header.splitOn "\r\n").getD 0 ""
    IO.println s!"  handshake -> {statusLine}"
    let upgraded := statusLine.startsWith "HTTP/1.1 101"

    let bufRef ← IO.mkRef leftover
    sendText conn "hello websockets"
    let echoed ← readFrame conn bufRef
    let echoedText := String.fromUTF8! echoed.payload
    IO.println s!"  sent \"hello websockets\" -> echoed {echoedText.quote}"

    sendText conn "a second message"
    let echoed2 ← readFrame conn bufRef
    let echoedText2 := String.fromUTF8! echoed2.payload
    IO.println s!"  sent \"a second message\" -> echoed {echoedText2.quote}"

    let _ ← close conn
    pure (upgraded && echoed.opcode == .text && echoedText == "hello websockets" &&
          echoed2.opcode == .text && echoedText2 == "a second message")

def run (_args : List String) : IO Unit := do
  if ← demoRoundTrip then
    IO.println "\nwebsockets demo done · all checks passed"
  else
    throw (IO.userError "websockets demo done · some checks failed")

end Examples.WebSockets
