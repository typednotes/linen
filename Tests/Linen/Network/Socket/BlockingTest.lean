/-
  Tests for `Linen.Network.Socket.Blocking`.

  * compile-time `example`s pin the blocking-style signatures.
  * one `#eval` round-trip exercises the real retry loops end-to-end over a
    loopback TCP connection: a background task blocks in `accept`/`recv`
    while the foreground blocks in `connect`/`sendAll`. Bounded — the
    background side is polled via `IO.hasFinished` for at most ~2 s, so a
    wiring bug fails the build instead of hanging it.
-/
import Linen.Network.Socket.Blocking

open Network.Socket Network.Socket.Blocking

namespace Tests.Network.Socket.Blocking

/-! ### Compile-time: blocking-style signatures -/

example : Socket .listening → IO (Socket .connected × SockAddr) := accept
example : Socket .fresh → SockAddr → IO (Socket .connected)     := connect
example : Socket .connected → ByteArray → IO Nat                := send
example : Socket .connected → ByteArray → IO Unit               := Blocking.sendAll
example : Socket .connected → IO ByteArray                      := (recv ·)

/-! ### Runtime: blocking accept/connect/send/recv round-trip on loopback -/

#eval show IO Unit from do
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (conn, _peer) ← Blocking.accept server
    let bytes ← Blocking.recv conn
    let _ ← close conn
    pure bytes
  let client ← socket .inet .stream
  let connected ← Blocking.connect client addr
  Blocking.sendAll connected "hello".toUTF8
  let _ ← close connected
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do
    throw (IO.userError "blocking accept/recv did not complete within ~2s")
  match serverTask.get with
  | .ok bytes =>
    unless bytes == "hello".toUTF8 do
      throw (IO.userError s!"expected 'hello', got {bytes.size} bytes")
  | .error e => throw e
  let _ ← close server

end Tests.Network.Socket.Blocking
