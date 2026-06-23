/-
  Linen.Network.Socket.EventDispatcher — Event loop ↔ Green monad bridge

  Routes kqueue/epoll readiness events to `IO.Promise`-based waiters, letting
  `Green` threads suspend **without blocking pool threads** while waiting for
  socket I/O readiness. This is the payoff of the green-thread model: a thread
  blocked on a socket is a heap object, not an OS thread.

  ## Design

  A single dedicated OS thread runs the dispatch loop, calling `EventLoop.wait`
  in a tight loop. When a socket becomes ready, the dispatcher resolves the
  corresponding `IO.Promise`, which wakes the `Green` thread that was awaiting
  it (via `Green.await`, i.e. `BaseIO.bindTask` — never `IO.wait`).

  ## Guarantees (axiom-dependent on the FFI / `BaseIO.bindTask` contract)

  - **No pool starvation:** Green threads that call `waitReadable`/`waitWritable`
    free their pool thread via `Green.await`.
  - **One-shot semantics:** each waiter is resolved exactly once and removed.
  - **Thread safety:** the waiter map is protected by `Std.Mutex`; ready waiters
    are collected under the lock and resolved *after* releasing it.

  ## Differences from the Haskell/`hale` source

  - The dispatch loop and `sendAllGreen` are ordinary `def`s using `while`
    (which delegates iteration to the standard library's `Loop.forIn`), so the
    library keeps its **no-`partial`** invariant.
  - Mutex access uses Lean's `Std.Mutex.atomically` + `get`/`set` state idiom
    (as in `Linen.Control.Concurrent.MVar`).
-/

import Linen.Network.Socket
import Linen.Control.Concurrent.Green
import Std.Sync.Mutex
import Std.Data.HashMap

namespace Network.Socket

open Control.Concurrent.Green

/-- A waiter entry: the Promise to resolve and the event type being waited for. -/
private structure Waiter where
  promise : IO.Promise Unit
  events  : EventType

/-- Event dispatcher: bridges kqueue/epoll events to Green thread suspensions.

    Create with `EventDispatcher.create`, use `waitReadable`/`waitWritable`
    to suspend Green threads, and `shutdown` to stop the dispatch loop.

    The waiter map uses `Nat` keys (converted from `USize` fds) to avoid
    compiled-mode ABI issues with scalar types in closures. -/
structure EventDispatcher where
  private mk ::
  eventLoop : EventLoop
  waiters   : Std.Mutex (Std.HashMap Nat (List Waiter))
  running   : IO.Ref Bool

namespace EventDispatcher

/-- Register a waiter for a socket fd and add it to the event loop. Internal.
    Takes `RawSocket` directly to avoid compiled-mode issues with phantom-
    parameterized structure unwrapping. -/
private def register (disp : EventDispatcher) (raw : RawSocket)
    (evts : EventType) : IO (IO.Promise Unit) := do
  let fdNat ← FFI.socketGetFd raw
  let promise ← IO.Promise.new
  let waiter : Waiter := { promise := promise, events := evts }
  disp.waiters.atomically do
    let ws ← get
    set (ws.insert fdNat (waiter :: ws.getD fdNat []))
  -- Register interest with the event loop
  FFI.eventLoopAdd disp.eventLoop raw evts.flags
  pure promise

/-- Check if an event matches what a waiter is waiting for. -/
private def waiterMatches (evType : EventType) (w : Waiter) : Bool :=
  (evType.hasReadable && w.events.hasReadable) ||
  (evType.hasWritable && w.events.hasWritable) ||
  evType.hasError

/-- The dispatch loop. Runs on a dedicated OS thread until `running` is cleared.

    For each ready event we collect the matching waiters and update the map
    **under the mutex**, then resolve their promises **after** releasing it. -/
private def dispatchLoop (disp : EventDispatcher) : IO Unit := do
  while ← disp.running.get do
    let events ← EventLoop.wait disp.eventLoop 1
    for ev in events do
      let fd : Nat := ev.socketFd
      let evType := ev.events
      let toResolve ← disp.waiters.atomically do
        let ws ← get
        match ws[fd]? with
        | none => pure []
        | some waiterList =>
          let (toResolve, remaining) := waiterList.partition (waiterMatches evType)
          if remaining.isEmpty then
            set (ws.erase fd)
          else
            set (ws.insert fd remaining)
          pure toResolve
      for w in toResolve do
        w.promise.resolve ()

/-- Create a new EventDispatcher with a running dispatch loop. -/
def create : IO EventDispatcher := do
  let el ← EventLoop.create
  let waiters ← Std.Mutex.new (∅ : Std.HashMap Nat (List Waiter))
  let running ← IO.mkRef true
  let disp : EventDispatcher := EventDispatcher.mk el waiters running
  -- Start the dispatch loop on a dedicated OS thread
  let _ ← IO.asTask (prio := .dedicated) (dispatchLoop disp)
  pure disp

/-- Stop the dispatch loop and close the event loop. -/
def shutdown (disp : EventDispatcher) : IO Unit := do
  disp.running.set false
  EventLoop.close disp.eventLoop

/-- Wait for a socket to become readable. Suspends the Green thread
    (frees the pool thread) and resumes when the socket is readable.
    $$\text{waitReadable} : \text{EventDispatcher} \to \text{Socket}\ s \to \text{Green Unit}$$ -/
def waitReadable (disp : EventDispatcher) (s : Socket state) : Green Unit := do
  let promise : IO.Promise Unit ← (disp.register s.raw EventType.readable : IO _)
  Green.await promise.result!

/-- Wait for a socket to become writable. Suspends the Green thread
    (frees the pool thread) and resumes when the socket is writable.
    $$\text{waitWritable} : \text{EventDispatcher} \to \text{Socket}\ s \to \text{Green Unit}$$ -/
def waitWritable (disp : EventDispatcher) (s : Socket state) : Green Unit := do
  let promise : IO.Promise Unit ← (disp.register s.raw EventType.writable : IO _)
  Green.await promise.result!

/-- Send all bytes on a connected socket, using the event loop for
    backpressure (waits for writability on `wouldBlock`).
    $$\text{sendAllGreen} : \text{EventDispatcher} \to \text{Socket .connected} \to \text{ByteArray} \to \text{Green Unit}$$ -/
def sendAllGreen (disp : EventDispatcher) (s : Socket .connected)
    (data : ByteArray) : Green Unit := do
  let mut offset := 0
  while offset < data.size do
    let outcome : SendOutcome ← (Network.Socket.send s (data.extract offset data.size) : IO _)
    match outcome with
    | .sent n => offset := offset + n
    | .wouldBlock => disp.waitWritable s
    | .error e => throw (IO.userError s!"sendAllGreen: {e}")

/-- Receive data from a connected socket, waiting for readability first.
    $$\text{recvGreen} : \text{EventDispatcher} \to \text{Socket .connected} \to \text{Nat} \to \text{Green RecvOutcome}$$ -/
def recvGreen (disp : EventDispatcher) (s : Socket .connected)
    (maxlen : Nat := 4096) : Green RecvOutcome := do
  disp.waitReadable s
  let outcome : RecvOutcome ← (Network.Socket.recv s maxlen : IO _)
  pure outcome

end EventDispatcher

end Network.Socket
