/-
  Tests for `Linen.Database.Redis.ProtocolPipelining`.

  Runs against a real loopback TCP connection: a background task plays the
  role of a (very dumb) Redis server, replying with canned RESP2 bytes to
  whatever it receives, so `send`/`recv`/`request` exercise the real
  `ConnectionContext`/`Protocol` machinery end-to-end.
-/
import Linen.Database.Redis.ProtocolPipelining
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.ProtocolPipelining
open Database.Redis.Protocol (Reply)

namespace Tests.Database.Redis.ProtocolPipelining

/-! ### Compile-time: public API shape -/

example : Database.Redis.ConnectionContext.ConnectAddr → IO Connection := connect
example : Connection → ByteArray → IO Unit := send
example : Connection → IO Reply := recv
example : Connection → ByteArray → IO Reply := request
example : Connection → IO Unit := flush
example : Connection → IO Unit := disconnect
example : Connection → IO Unit := beginReceiving

/-! ### Runtime: loopback round-trip against a canned RESP2 server -/

-- Send two requests before reading either reply (genuine pipelining), then
-- `recv` them back in order. The "server" task replies `+PONG\r\n` to the
-- first bytes it reads and `:42\r\n` to the second, regardless of what was
-- actually sent, split arbitrarily across `recv` calls to exercise
-- `parseOneReply`'s buffer-growing loop.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    -- Drain whatever the client sent (two pipelined requests).
    let _ ← Network.Socket.Blocking.recv accepted 256
    -- Reply to both, but split the second reply into two separate socket
    -- writes to force `parseOneReply` to grow its buffer mid-parse.
    Network.Socket.sendAll accepted "+PONG\r\n".toUTF8
    Network.Socket.sendAll accepted ":4".toUTF8
    Network.Socket.sendAll accepted "2\r\n".toUTF8
    Network.Socket.close accepted
  let conn ← connect (.hostPort addr.host addr.port)
  send conn "*1\r\n$4\r\nPING\r\n".toUTF8
  send conn "*1\r\n$4\r\nINCR\r\n".toUTF8
  let r1 ← recv conn
  let r2 ← recv conn
  match r1 with
  | .singleLine s => unless s == "PONG".toUTF8 do throw (IO.userError "expected PONG")
  | _ => throw (IO.userError "expected a singleLine reply")
  match r2 with
  | .integer n => unless n == 42 do throw (IO.userError s!"expected 42, got {n}")
  | _ => throw (IO.userError "expected an integer reply")
  disconnect conn
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do
    throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure ()
  let _ ← Network.Socket.close server
  pure ()

-- `request` = `send` then `recv`.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv accepted 256
    Network.Socket.sendAll accepted "+OK\r\n".toUTF8
    Network.Socket.close accepted
  let conn ← connect (.hostPort addr.host addr.port)
  let r ← request conn "*1\r\n$4\r\nPING\r\n".toUTF8
  match r with
  | .singleLine s => unless s == "OK".toUTF8 do throw (IO.userError "expected OK")
  | _ => throw (IO.userError "expected a singleLine reply")
  -- `flush`/`beginReceiving` are no-ops in this strict-IO port; just check
  -- they don't throw.
  flush conn
  beginReceiving conn
  disconnect conn
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do
    throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure ()
  let _ ← Network.Socket.close server
  pure ()

-- The server closing the connection before a full reply arrives surfaces as
-- an `IO.userError`, not a silent/incorrect parse.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv accepted 256
    -- Send a truncated reply, then close.
    Network.Socket.sendAll accepted "+PAR".toUTF8
    Network.Socket.close accepted
  let conn ← connect (.hostPort addr.host addr.port)
  send conn "*1\r\n$4\r\nPING\r\n".toUTF8
  try
    let _ ← recv conn
    throw (IO.userError "expected recv to fail on a truncated reply")
  catch _ =>
    pure ()
  disconnect conn
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do
    throw (IO.userError "server task did not finish within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok _ => pure ()
  let _ ← Network.Socket.close server
  pure ()

end Tests.Database.Redis.ProtocolPipelining
