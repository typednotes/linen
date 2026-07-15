/-
  Tests for `Linen.Database.Redis.ConnectionContext`.

  Two kinds of checks:

  * compile-time `example`s pin down the public API shape against upstream
    `hedis`'s `ConnectionContext`.
  * `#eval` round-trips exercise a real loopback TCP connection (ephemeral
    port, localhost only, bounded polling) — no external network, no hangs.
-/
import Linen.Database.Redis.ConnectionContext
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.ConnectionContext
open Network.Socket (Socket)

namespace Tests.Database.Redis.ConnectionContext

/-! ### Compile-time: public API shape -/

example : ConnectAddr → IO ConnectionContext := connect
example : ConnectAddr → Network.TLS.TLSContext → IO ConnectionContext := connectTLS
example : ConnectionContext → ByteArray → IO Unit := send
example : ConnectionContext → Nat → IO ByteArray := (recv · ·)
example : ConnectionContext → IO Unit := flush
example : ConnectionContext → IO Unit := disconnect
example : Network.TLS.TLSContext → String → ConnectionContext → IO (Except String ConnectionContext) :=
  enableTLS

/-! ### Runtime: loopback round-trip -/

-- `connect (.unixSocket _)` is rejected explicitly (no `sockaddr_un` support
-- in the FFI layer -- see the module doc-comment).
#eval show IO Unit from do
  try
    let _ ← connect (.unixSocket "/tmp/does-not-matter.sock")
    throw (IO.userError "expected connect to a Unix socket to fail")
  catch _ =>
    pure ()

-- `connect (.hostPort ..)` against a real (loopback, ephemeral-port) listener,
-- then a `send`/`recv` round trip, then `disconnect`. Mirrors the loopback
-- pattern established in `Tests/Linen/Network/SocketTest.lean`.
#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) (Network.Socket.Blocking.accept server)
  let cc ← connect (.hostPort addr.host addr.port)
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do
    throw (IO.userError "accept did not complete within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok (accepted, _peer) =>
    -- Server -> client.
    Network.Socket.sendAll accepted "hello".toUTF8
    let bytes ← recv cc 16
    unless bytes == "hello".toUTF8 do
      throw (IO.userError s!"expected 'hello', got {bytes.size} bytes")
    -- Client -> server.
    send cc "world".toUTF8
    let reply ← Network.Socket.Blocking.recv accepted 16
    unless reply == "world".toUTF8 do
      throw (IO.userError s!"expected 'world', got {reply.size} bytes")
    flush cc
    let _ ← Network.Socket.close accepted
    pure ()
  disconnect cc
  let _ ← Network.Socket.close server
  pure ()

end Tests.Database.Redis.ConnectionContext
