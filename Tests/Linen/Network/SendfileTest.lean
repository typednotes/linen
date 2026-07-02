/-
  Tests for `Linen.Network.Sendfile`.

  Exercises `sendFile`/`sendFileSimple` end-to-end over a real loopback TCP
  connection: a background task accepts and drains the connection with
  `Blocking.recv` until EOF, while the foreground streams a real scratch file
  (via `IO.FS.createTempFile`) into the connected socket.
-/
import Linen.Network.Sendfile

open Network.Socket
open Network.Socket.Blocking
open Network.Sendfile

namespace Tests.Network.Sendfile

/-- Accept once, then read every chunk with `Blocking.recv` until EOF,
concatenating what was received. -/
def drainOnce (server : Socket .listening) : IO ByteArray := do
  let (conn, _peer) ← Blocking.accept server
  let mut received := ByteArray.empty
  let mut more := true
  while more do
    let chunk ← Blocking.recv conn
    if chunk.isEmpty then
      more := false
    else
      received := received ++ chunk
  let _ ← close conn
  pure received

/-- Connect to `addr`, run `action` on the connected socket, then close it
(driving the server's `recv` to EOF). -/
def withConnection (addr : SockAddr) (action : Socket .connected → IO Unit) : IO Unit := do
  let client ← socket .inet .stream
  let connected ← Blocking.connect client addr
  action connected
  let _ ← close connected
  pure ()

-- sendFileSimple streams an entire scratch file to the peer.
#eval show IO Unit from do
  let (handle, path) ← IO.FS.createTempFile
  handle.putStr "hello, sendfile!"
  handle.flush
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) (drainOnce server)
  withConnection addr (fun conn => sendFileSimple conn path.toString)
  let _ ← close server
  let received ←
    match serverTask.get with
    | .ok bytes => pure bytes
    | .error e => throw e
  IO.FS.removeFile path
  unless received == "hello, sendfile!".toUTF8 do
    throw (IO.userError s!"sendFileSimple: expected the whole file, got {received.size} bytes")

-- sendFile with a `FilePart` sends only the requested offset/count slice.
#eval show IO Unit from do
  let (handle, path) ← IO.FS.createTempFile
  handle.putStr "0123456789ABCDEF"
  handle.flush
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) (drainOnce server)
  withConnection addr (fun conn =>
    sendFile conn path.toString (some ⟨4, 6⟩))
  let _ ← close server
  let received ←
    match serverTask.get with
    | .ok bytes => pure bytes
    | .error e => throw e
  IO.FS.removeFile path
  unless received == "456789".toUTF8 do
    throw (IO.userError s!"sendFile with FilePart: expected '456789', got {String.fromUTF8! received}")

end Tests.Network.Sendfile
