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

end Tests.Network.Socket
