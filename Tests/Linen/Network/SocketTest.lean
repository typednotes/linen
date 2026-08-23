/-
  Tests for `Linen.Network.Socket`.

  Two kinds of checks:

  * compile-time `example`s pin down the phantom-typed state machine — each
    transition has exactly the right pre/post states in its type. (If the
    protocol types ever drift, these stop elaborating.)
  * `#eval` round-trips exercise the real syscalls — all **local and
    non-blocking** (loopback, ephemeral port, 0 ms waits), so no network and no
    hangs. They need the `linenffi` native lib, which `precompileModules`
    makes available to the interpreter.
-/
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Network.Socket

namespace Tests.Network.Socket

/-! ### Compile-time: the POSIX lifecycle is encoded in the types -/

example : Family → SocketType → IO (Socket .fresh)     := socket
example : Socket .fresh → SockAddr → IO (Socket .bound) := bind
example : Socket .bound → IO (Socket .listening)        := (listen ·)
example : Socket .listening → IO AcceptOutcome          := accept
example : Socket .fresh → SockAddr → IO ConnectOutcome  := connect
example : Socket .connecting → IO ConnectOutcome        := connectFinish
example : Socket .connected → ByteArray → IO SendOutcome := send
example : Socket .connected → IO RecvOutcome            := (recv ·)
example : Socket .connected → ByteArray → IO Unit       := sendAll
example : Socket .connected → ByteArray → SockAddr → IO Nat := sendTo
example : Socket .connected → IO (ByteArray × SockAddr) := (recvFrom ·)
-- `close` accepts any non-closed state; the `state ≠ .closed` proof is
-- discharged by `decide` for a concrete state. (Closing a `Socket .closed`
-- is rejected at compile time — no proof of `.closed ≠ .closed` exists.)
example : Socket .fresh → IO (Socket .closed)           := (close ·)
example : Socket .connected → IO (Socket .closed)       := (close ·)

/-! ### Runtime: typed lifecycle round-trip on loopback -/

-- listenTCP (socket → reuseaddr → bind ephemeral → listen) then introspect the
-- kernel-assigned address; a non-blocking accept on an idle listener wouldBlocks.
#eval show IO Unit from do
  withListenTCP "127.0.0.1" 0 fun s => do
    setNonBlocking s
    let addr ← getSockName s
    unless addr.host == "127.0.0.1" do
      throw (IO.userError s!"expected 127.0.0.1, got {addr.host}")
    unless addr.port > 0 do
      throw (IO.userError s!"expected an ephemeral port > 0, got {addr.port}")
    match ← accept s with
      | .wouldBlock   => pure ()
      | .accepted _ _ => throw (IO.userError "unexpected connection on idle listener")
      | .error e      => throw (IO.userError s!"accept failed: {e}")

-- withEventLoop + EventLoop.add/wait/del over a registered, idle listener.
#eval show IO Unit from do
  let s ← listenTCP "127.0.0.1" 0
  setNonBlocking s
  withEventLoop fun el => do
    EventLoop.add el s EventType.readable
    let ready ← EventLoop.wait el 0
    unless ready.isEmpty do
      throw (IO.userError s!"idle listener reported {ready.length} ready events")
    EventLoop.del el s
  let _ ← close s

-- getAddrInfo maps the FFI triples into typed `AddrInfo` (numeric ⇒ no network).
#eval show IO Unit from do
  let infos ← getAddrInfo "127.0.0.1" "80"
  unless infos.length > 0 do
    throw (IO.userError "getAddrInfo returned no results for 127.0.0.1:80")
  match infos.head? with
    | some info =>
      unless info.family == Family.inet do
        throw (IO.userError s!"expected inet family, got {repr info.family}")
      unless info.host == "127.0.0.1" do
        throw (IO.userError s!"expected host 127.0.0.1, got {info.host}")
    | none => throw (IO.userError "unreachable: non-empty list has a head")

-- sendAll (FFI-looped, TCP): establish a loopback connection via `Blocking`
-- (already tested against these same non-blocking primitives), then verify
-- the peer receives exactly the bytes sent through `sendAll`. Bounded: the
-- accept side is polled via `IO.hasFinished` for at most ~2s.
#eval show IO Unit from do
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) (Blocking.accept server)
  let client ← socket .inet .stream
  let conn ← Blocking.connect client addr
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then done := true; break
    IO.sleep 10
  unless done do
    throw (IO.userError "accept did not complete within ~2s")
  match serverTask.get with
  | .error e => throw e
  | .ok (accepted, _peer) =>
    sendAll accepted "hello".toUTF8
    let bytes ← Blocking.recv conn 16
    unless bytes == "hello".toUTF8 do
      throw (IO.userError s!"expected 'hello', got {bytes.size} bytes")
    let _ ← close accepted
  let _ ← close conn
  let _ ← close server

-- recvFrom (UDP): `recvFrom` requires a `Socket .connected`, but `connect`
-- only accepts `Socket .fresh`, and there is no `.bound → .connected`
-- transition -- so a single socket can never be both "has a known fixed
-- address" (via `bind`) and `.connected`. The sender below stays merely
-- `.bound` (a real, addressable UDP socket) and is driven directly through
-- the raw FFI; only the receiver reaches `.connected` and exercises the
-- `recvFrom` wrapper under test.
--
-- `sendTo` itself is exercised only at compile time (line 32 above): on
-- BSD/Darwin, `sendto(2)` on an already-connected `SOCK_DGRAM` socket fails
-- with `EISCONN` even when the supplied address matches the connected peer
-- (confirmed empirically on this host) -- unlike Linux, where it is
-- permitted. Since `sendTo`'s type requires `Socket .connected`, no runtime
-- call to it can succeed portably across the platforms this library targets.
--
-- `connect` performs a *non-blocking* connect (`socketConnectNB`), which
-- leaves `connQ` in non-blocking mode. Datagram delivery on loopback isn't
-- instantaneous, so calling `recvFrom` right after the peer's `sendto(2)`
-- races the kernel: without waiting for readability first, `recvFrom` can
-- hit EAGAIN before the packet lands. `poll .read` blocks (via `select`)
-- until the datagram has actually arrived, removing the race.
#eval show IO Unit from do
  let p ← socket .inet .datagram
  let p ← bind p ⟨"127.0.0.1", 0⟩
  let addrP ← getSockName p
  let q ← socket .inet .datagram
  let connQ ← match ← connect q addrP with
    | .connected s  => pure s
    | .inProgress _ => throw (IO.userError "unexpected inProgress connecting a UDP socket")
    | .refused e    => throw e
  let addrQ ← getSockName connQ
  let _ ← Network.Socket.FFI.socketSendTo p.raw "pong".toUTF8 addrQ.host addrQ.port
  match ← poll connQ .read 2000 with
    | .ready    => pure ()
    | .timeout  => throw (IO.userError "datagram did not arrive within 2s")
    | .error e  => throw e
  let (data, from_) ← recvFrom connQ
  unless data == "pong".toUTF8 do
    throw (IO.userError s!"expected 'pong', got {data.size} bytes")
  unless from_.host == "127.0.0.1" do
    throw (IO.userError s!"expected sender host 127.0.0.1, got {from_.host}")
  let _ ← close connQ
  let _ ← close p

-- Timeouts: a peer that connects and then says nothing must not hang the
-- reader, and must not busy-spin either. This is the defect the timeout work
-- exists to fix, so it is checked behaviourally — the server accepts and
-- deliberately never sends, and `recv` must give up on its own.
#eval show IO Unit from do
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) (Blocking.accept server)
  let client ← socket .inet .stream
  let conn ← Blocking.connect client addr
  let mut accepted := false
  for _ in [0:200] do
    if ← IO.hasFinished serverTask then accepted := true; break
    IO.sleep 10
  unless accepted do throw (IO.userError "accept did not complete within ~2s")
  -- 300 ms is plenty to tell "gave up" from "blocked forever".
  let start ← IO.monoNanosNow
  let outcome ← (Blocking.recv conn 16 300).toBaseIO
  let elapsedMillis := ((← IO.monoNanosNow) - start) / 1000000
  match outcome with
  | .ok bytes => throw (IO.userError
      s!"expected recv to time out on a silent peer, got {bytes.size} bytes")
  | .error _ => pure ()
  -- Bounded above: it really did give up. Bounded below: it waited rather
  -- than failing instantly, so the deadline is doing the work.
  unless elapsedMillis ≥ 250 && elapsedMillis < 5000 do
    throw (IO.userError s!"recv returned after {elapsedMillis}ms, expected ~300ms")
  match serverTask.get with
  | .error e => throw e
  | .ok (a, _) => let _ ← close a
  let _ ← close conn
  let _ ← close server

-- The socket-level options accept both a real deadline and zero ("no
-- deadline"), on every platform the FFI builds for.
#eval show IO Unit from do
  let s ← socket .inet .stream
  setRecvTimeout s 1500
  setSendTimeout s 1500
  setRecvTimeout s 0
  setSendTimeout s 0
  let _ ← close s

end Tests.Network.Socket
