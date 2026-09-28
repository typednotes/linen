/-
  Tests for `Linen.Network.Socket.FFI`.

  These bindings are `@[extern]` IO actions (real syscalls), so behaviour is
  checked with `#eval` (a thrown error fails the build), as in the other
  concurrency/IO tests. Everything here is **local and non-blocking** — no
  network access: we bind a TCP listener to an ephemeral loopback port, prove
  a non-blocking `accept` reports `wouldBlock`, exercise the kqueue/epoll event
  loop, and resolve a numeric address. Running these requires the `linenffi`
  native library, which `precompileModules` makes available to the interpreter.
-/
import Linen.Network.Socket.FFI

open Network.Socket Network.Socket.FFI

namespace Tests.Network.Socket.FFI

-- Create → options → bind(ephemeral, loopback) → listen → read back the
-- kernel-assigned address; a non-blocking accept on an idle listener must
-- report `wouldBlock` rather than hang.
#eval show IO Unit from do
  let s ← socketCreate Family.inet.toUInt8 SocketType.stream.toUInt8
  let fd ← socketGetFd s
  unless fd > 0 do throw (IO.userError s!"expected a positive fd, got {fd}")
  setReuseAddr s 1
  setNonBlocking s 1
  socketBind s "127.0.0.1" 0          -- port 0 ⇒ kernel picks an ephemeral port
  socketListen s 16
  let port ← getSockNamePort s
  unless port > 0 do throw (IO.userError s!"expected an ephemeral port > 0, got {port}")
  let host ← getSockNameHost s
  unless host == "127.0.0.1" do throw (IO.userError s!"expected 127.0.0.1, got {host}")
  match ← socketAcceptNB s with
    | .wouldBlock   => pure ()
    | .accepted _ _ => throw (IO.userError "unexpected connection on an idle listener")
    | .error e      => throw (IO.userError s!"accept_nb failed: {e}")
  socketClose s

-- The event loop (kqueue on macOS, epoll on Linux) can be created and closed.
#eval show IO Unit from do
  let loop ← eventLoopCreate
  eventLoopClose loop

-- A registered, idle socket produces no ready events within a short timeout.
#eval show IO Unit from do
  let s ← socketCreate Family.inet.toUInt8 SocketType.stream.toUInt8
  setNonBlocking s 1
  socketBind s "127.0.0.1" 0
  socketListen s 16
  let loop ← eventLoopCreate
  eventLoopAdd loop s EventType.readable.flags
  let ready ← eventLoopWait loop 0          -- 0 ms ⇒ return immediately
  unless ready.isEmpty do throw (IO.userError s!"idle listener reported {ready.length} ready events")
  eventLoopDel loop s
  eventLoopClose loop
  socketClose s

-- `getAddrInfo` resolves a numeric address without touching the network.
#eval show IO Unit from do
  let infos ← getAddrInfo "127.0.0.1" "80"
  unless infos.length > 0 do
    throw (IO.userError "getAddrInfo returned no results for 127.0.0.1:80")

end Tests.Network.Socket.FFI
