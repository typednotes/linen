/-
  Tests for `Linen.Network.Socket.EventDispatcher`.

  * compile-time `example`s pin the bridge's signatures (Green-returning ops).
  * `#eval` checks exercise the real dispatcher: create/shutdown, and an
    end-to-end proof that the kqueue/epoll loop **wakes a suspended `Green`
    thread** when a loopback socket becomes readable.

  Everything is local (loopback, ephemeral port) and **bounded** — the
  integration check polls `IO.hasFinished` for at most ~2 s, so a wiring bug
  fails the build instead of hanging it. Needs the `linenffi` native library,
  available to the interpreter via `precompileModules`.
-/
import Linen.Network.Socket.EventDispatcher

open Network.Socket Control.Concurrent.Green

namespace Tests.Network.Socket.EventDispatcher

/-! ### Compile-time: the bridge suspends in `Green` -/

example (st : SocketState) : EventDispatcher → Socket st → Green Unit :=
  EventDispatcher.waitReadable
example (st : SocketState) : EventDispatcher → Socket st → Green Unit :=
  EventDispatcher.waitWritable
example : EventDispatcher → Socket .connected → ByteArray → Green Unit :=
  EventDispatcher.sendAllGreen
example : EventDispatcher → Socket .connected → Green RecvOutcome :=
  (EventDispatcher.recvGreen · ·)

/-! ### Runtime: create / shutdown -/

#eval show IO Unit from do
  let disp ← EventDispatcher.create
  EventDispatcher.shutdown disp

/-! ### Runtime: the dispatcher wakes a Green waiter on readiness -/

-- A client connecting to a loopback listener makes the listener readable; a
-- Green thread parked in `waitReadable` must be resumed by the dispatch loop.
#eval show IO Unit from do
  let disp ← EventDispatcher.create
  try
    let server ← listenTCP "127.0.0.1" 0
    setNonBlocking server
    let addr ← getSockName server
    -- kick a connection so the listener becomes readable
    let client ← socket .inet .stream
    setNonBlocking client
    let _ ← connect client addr
    -- park a Green thread on readability; it frees its pool worker until woken
    let tok ← Std.CancellationToken.new
    let waitTask ← Green.run (EventDispatcher.waitReadable disp server) tok
    -- bounded wait (≤ ~2 s): never hang the build
    let mut woke := false
    for _ in [0:200] do
      if ← IO.hasFinished waitTask then woke := true; break
      IO.sleep 10
    unless woke do
      throw (IO.userError "dispatcher did not wake the Green waiter within ~2s")
    match ← IO.wait waitTask with
      | .ok ()   => pure ()
      | .error e => throw (IO.userError s!"waitReadable errored: {e}")
    let _ ← close client
    let _ ← close server
  finally
    EventDispatcher.shutdown disp

end Tests.Network.Socket.EventDispatcher
