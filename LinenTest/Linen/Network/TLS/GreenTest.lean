/-
  Tests for `Linen.Network.TLS.Green` — and, through it, the resumable
  non-blocking primitives of `Network.TLS.Context`.

  Every test runs over a real loopback TLS connection whose sockets are
  **non-blocking**, which is what the old `acceptSocketNB`/`connectSocketNB`
  could never complete a handshake on: the first would-block freed the
  `SSL`, and the retry started a new handshake mid-stream.

  - The server side runs on green threads (`Green.accept`/`read`/`write` on
    an `EventDispatcher`); the client side uses `Context.connectSocket`
    (`handshake` with `poll`) on a non-blocking socket too.
  - Transfers of several MiB force both would-block directions: the sender's
    socket buffer fills (`wantWrite`), and the reader sees many TLS records,
    some already decrypted inside OpenSSL when it asks (`SSL_pending`).
-/
import Linen.Network.TLS.Green
import Linen.Network.Socket.Blocking
import LinenTest.Linen.Network.TLS.TestSupport

open Network.Socket
open Network.TLS
open Tests.Network.TLS.TestSupport (withTestCert)
open Control.Concurrent.Green (Green)

namespace Tests.Network.TLS.Green

/-- `n` bytes of a repeating, position-dependent pattern, so a reordered,
    dropped or duplicated chunk is detected. -/
private def pattern (n : Nat) : ByteArray := Id.run do
  let mut out := ByteArray.emptyWithCapacity n
  for i in [0:n] do
    out := out.push (i * 7 + i / 251).toUInt8
  return out

/-- Read exactly `n` bytes (or up to end of input) with `readOnce`. -/
private def readExactly (n : Nat) (readOnce : IO ByteArray) : IO ByteArray := do
  let mut got := ByteArray.empty
  for _ in [0:n + 1] do
    if got.size ≥ n then break
    let piece ← readOnce
    if piece.isEmpty then break
    got := got ++ piece
  return got

/-- A client read over a non-blocking socket: `readNB`, and `poll` when it
    asks to wait. -/
private def clientRead (sock : RawSocket) (session : TLSSession) : IO ByteArray := do
  repeat
    let outcome ← readNB session 16384
    match outcome with
    | .ok bytes => return bytes
    | .error e => throw e
    | .wantWrite => let _ ← FFI.socketPoll sock PollMode.write.toUInt8 30000
    | .wantRead => let _ ← FFI.socketPoll sock PollMode.read.toUInt8 30000
  return ByteArray.empty

/-- A client write over a non-blocking socket: `writeNB`, repeating the same
    write after each wait. -/
private def clientWrite (sock : RawSocket) (session : TLSSession) (data : ByteArray) : IO Unit := do
  repeat
    let outcome ← writeNB session data
    match outcome with
    | .ok () => return
    | .error e => throw e
    | .wantWrite => let _ ← FFI.socketPoll sock PollMode.write.toUInt8 30000
    | .wantRead => let _ ← FFI.socketPoll sock PollMode.read.toUInt8 30000

/-- Serve one connection on green threads: accept, then `server disp sock
    session`, all non-blocking. Returns the server's result. -/
private def withGreenServer (certPath keyPath : String)
    (server : EventDispatcher → Socket .connected → TLSSession → Green α)
    (client : UInt16 → IO β) : IO (α × β) := do
  let ctx ← createContext certPath keyPath
  let listener ← listenTCP "127.0.0.1" 0
  let port := (← getSockName listener).port
  let disp ← EventDispatcher.create
  let token ← Std.CancellationToken.new
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (conn, _) ← Blocking.accept listener
    setNonBlocking conn
    try
      Green.block (do
        let session ← Network.TLS.Green.accept disp ctx conn
        let result ← server disp conn session
        (close session : IO _)
        return result) token
    finally
      let _ ← Network.Socket.close conn
  try
    let b ← client port
    let a ← IO.ofExcept (← IO.wait serverTask)
    return (a, b)
  finally
    disp.shutdown
    let _ ← Network.Socket.close listener

/-- A client TLS connection on a **non-blocking** socket. -/
private def nonBlockingClient (certPath : String) (port : UInt16) :
    IO (Socket .connected × TLSSession) := do
  let clientCtx ← createClientContextWithCA certPath
  let conn ← Blocking.connect (← socket .inet .stream) { host := "127.0.0.1", port }
  setNonBlocking conn
  let session ← connectSocket clientCtx conn.raw "localhost"
  return (conn, session)

-- Handshake on non-blocking sockets at both ends, then a small echo.
#eval show IO Unit from withTestCert fun certPath keyPath => do
  let (serverGot, clientGot) ← withGreenServer certPath keyPath
    (fun disp sock session => do
      let request ← Network.TLS.Green.read disp sock session
      Network.TLS.Green.write disp sock session (request ++ "!".toUTF8)
      return request)
    (fun port => do
      let (conn, session) ← nonBlockingClient certPath port
      clientWrite conn.raw session "ping".toUTF8
      let reply ← clientRead conn.raw session
      close session
      let _ ← Network.Socket.close conn
      return reply)
  unless serverGot == "ping".toUTF8 do
    throw (IO.userError s!"server got {String.fromUTF8! serverGot}")
  unless clientGot == "ping!".toUTF8 do
    throw (IO.userError s!"client got {String.fromUTF8! clientGot}")

-- Several MiB each way. The server writes 4 MiB in one `write` while the
-- client is not reading yet, so the server's socket buffer fills and the
-- write must wait and repeat (`wantWrite`); the client then reads it back in
-- 16 KiB records. The client sends 3 MiB, read by the green server.
#eval show IO Unit from withTestCert fun certPath keyPath => do
  let down := pattern (4 * 1024 * 1024)
  let up := pattern (3 * 1024 * 1024 + 17)
  let (serverGot, clientGot) ← withGreenServer certPath keyPath
    (fun disp sock session => do
      Network.TLS.Green.write disp sock session down
      let mut got := ByteArray.empty
      for _ in [0:up.size + 1] do
        if got.size ≥ up.size then break
        let piece ← Network.TLS.Green.read disp sock session
        if piece.isEmpty then break
        got := got ++ piece
      return got)
    (fun port => do
      let (conn, session) ← nonBlockingClient certPath port
      IO.sleep 200  -- let the server's write run into a full socket buffer
      let got ← readExactly down.size (clientRead conn.raw session)
      clientWrite conn.raw session up
      -- Wait for the server to finish reading before closing.
      let _ ← clientRead conn.raw session
      close session
      let _ ← Network.Socket.close conn
      return got)
  unless clientGot == down do
    throw (IO.userError s!"client received {clientGot.size} bytes, not the {down.size} sent")
  unless serverGot == up do
    throw (IO.userError s!"server received {serverGot.size} bytes, not the {up.size} sent")

-- Full duplex: one reader and one writer on each SSL object at once,
-- with getters racing them too. HTTP/2 needs this when consuming an upload
-- while its stream handler returns WINDOW_UPDATE or response DATA. Without
-- per-session serialization OpenSSL intermittently corrupts its state.
#eval show IO Unit from withTestCert fun certPath keyPath => do
  let up := pattern (2 * 1024 * 1024 + 23)
  let down := pattern (2 * 1024 * 1024 + 47)
  let inspect (session : TLSSession) : IO Unit := do
    for _ in [0:1000] do
      let version ← getVersion session
      unless version == "TLSv1.2" || version == "TLSv1.3" do
        throw (IO.userError s!"concurrent getter: {version}")
      let _ ← getAlpn session
  let (serverGot, clientGot) ← withGreenServer certPath keyPath
    (fun disp sock session => do
      let writer ← (IO.asTask (prio := .dedicated) do
        Network.TLS.Green.writeIO disp sock session down 10000 : IO _)
      let getter ← (IO.asTask (prio := .dedicated) (inspect session) : IO _)
      let mut got := ByteArray.empty
      while got.size < up.size do
        let piece ← Network.TLS.Green.readFor disp sock session (some 10000)
        let some piece := piece | throw (IO.userError "duplex server read timed out")
        if piece.isEmpty then throw (IO.userError "duplex server EOF")
        got := got ++ piece
      (IO.ofExcept (← IO.wait writer) : IO _)
      (IO.ofExcept (← IO.wait getter) : IO _)
      return got)
    (fun port => do
      let (conn, session) ← nonBlockingClient certPath port
      try
        let writer ← IO.asTask (prio := .dedicated) (writeWithin session conn.raw 10000 up)
        let getter ← IO.asTask (prio := .dedicated) (inspect session)
        let got ← readExactly down.size do
          let some bytes ← readWithin session conn.raw 10000
            | throw (IO.userError "duplex client read timed out")
          return bytes
        IO.ofExcept (← IO.wait writer)
        IO.ofExcept (← IO.wait getter)
        return got
      finally
        close session
        let _ ← Network.Socket.close conn)
  unless serverGot == up && clientGot == down do
    throw (IO.userError s!"duplex mismatch: {serverGot.size}/{up.size}, {clientGot.size}/{down.size}")

-- End of input: a peer that closes (with close_notify) reads as empty.
#eval show IO Unit from withTestCert fun certPath keyPath => do
  let (serverGot, _) ← withGreenServer certPath keyPath
    (fun disp sock session => Network.TLS.Green.read disp sock session)
    (fun port => do
      let (conn, session) ← nonBlockingClient certPath port
      close session
      let _ ← Network.Socket.close conn)
  unless serverGot.isEmpty do throw (IO.userError "expected end of input")

-- A handshake that fails (the client does not trust the server) is an error
-- on both sides, not a hang.
#eval show IO Unit from withTestCert fun certPath keyPath => do
  let ctx ← createContext certPath keyPath
  let listener ← listenTCP "127.0.0.1" 0
  let port := (← getSockName listener).port
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (conn, _) ← Blocking.accept listener
    try
      let _ ← acceptSocket ctx conn.raw
    finally
      let _ ← Network.Socket.close conn
  let untrusting ← createClientContextWithCA keyPath  -- not a certificate
    <|> createClientContext
  let conn ← Blocking.connect (← socket .inet .stream) { host := "127.0.0.1", port }
  let clientFailed ← try
      let _ ← connectSocket untrusting conn.raw "localhost"
      pure false
    catch _ => pure true
  let _ ← Network.Socket.close conn
  let serverFailed := (← IO.wait serverTask) matches .error _
  let _ ← Network.Socket.close listener
  unless clientFailed && serverFailed do
    throw (IO.userError s!"expected both handshakes to fail: client {clientFailed}, server {serverFailed}")

end Tests.Network.TLS.Green
