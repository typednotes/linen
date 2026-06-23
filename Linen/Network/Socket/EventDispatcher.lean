/-
  Linen.Network.Socket.EventDispatcher — Event loop ↔ Green monad bridge

  Routes kqueue/epoll readiness events to `IO.Promise`-based waiters, letting
  `Green` threads suspend **without blocking pool threads** while waiting for
  socket I/O readiness. A thread blocked on a socket is a heap object, not an
  OS thread.

  ## Design — a sharded dispatcher

  The dispatcher is split into `N` independent **shards**, each with its own
  event loop (kqueue/epoll fd), its own waiter map, and its own dedicated
  dispatch thread. A socket fd is assigned to shard `fd % N`, so registration
  and dispatch for that fd always land on the same shard with no cross-shard
  coordination. This removes the single-thread dispatch bottleneck: `N` threads
  drain `kevent`/`epoll_wait` and resolve waiters in parallel, and the waiter
  mutex is per-shard so registrations contend far less.

  Each dispatch thread also processes a whole `kevent`/`epoll_wait` batch under
  **one** lock acquisition (not one per event), and resolves the woken promises
  *after* releasing it.

  When a socket becomes ready the shard resolves the corresponding `IO.Promise`,
  which wakes the `Green` thread that was awaiting it (via `Green.await`, i.e.
  `BaseIO.bindTask` — never `IO.wait`).

  ## Guarantees (axiom-dependent on the FFI / `BaseIO.bindTask` contract)

  - **No pool starvation:** `waitReadable`/`waitWritable` free their pool thread.
  - **One-shot semantics:** each waiter is resolved exactly once and removed.
  - **Thread safety:** each shard's waiter map is its own `Std.Mutex`.

  ## No `partial`

  The per-shard dispatch loop and `sendAllGreen` use `while` (which delegates
  iteration to the standard library's `Loop.forIn`), so the module keeps the
  library's no-`partial` invariant.
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

/-- One dispatcher shard: an event loop, the waiters registered on it (keyed by
fd), and a flag controlling its dispatch thread. -/
private structure Shard where
  eventLoop : EventLoop
  waiters   : Std.Mutex (Std.HashMap Nat (List Waiter))
  running   : IO.Ref Bool

/-- Event dispatcher: bridges kqueue/epoll events to Green thread suspensions,
sharded across several event loops + dispatch threads for parallel throughput.

    Create with `EventDispatcher.create`, use `waitReadable`/`waitWritable` to
    suspend Green threads, and `shutdown` to stop the dispatch loops. -/
structure EventDispatcher where
  private mk ::
  shards : Array Shard

namespace EventDispatcher

/-- Check if an event matches what a waiter is waiting for. -/
private def waiterMatches (evType : EventType) (w : Waiter) : Bool :=
  (evType.hasReadable && w.events.hasReadable) ||
  (evType.hasWritable && w.events.hasWritable) ||
  evType.hasError

/-- The dispatch loop for one shard, on its own dedicated OS thread. Drains a
whole `kevent`/`epoll_wait` batch, collects the matching waiters under a single
lock, then resolves their promises after releasing it. -/
private def dispatchShard (sh : Shard) : IO Unit := do
  while ← sh.running.get do
    let events ← EventLoop.wait sh.eventLoop 50
    if !events.isEmpty then
      let toResolve ← sh.waiters.atomically do
        let mut acc : List Waiter := []
        for ev in events do
          let ws ← get
          match ws[ev.socketFd]? with
          | none => pure ()
          | some waiterList =>
            let (matched, remaining) := waiterList.partition (waiterMatches ev.events)
            if remaining.isEmpty then
              set (ws.erase ev.socketFd)
            else
              set (ws.insert ev.socketFd remaining)
            acc := acc ++ matched
        pure acc
      for w in toResolve do
        w.promise.resolve ()

/-- Register a waiter for a socket fd on its shard (`fd % N`) and add it to that
shard's event loop. Internal. -/
private def register (disp : EventDispatcher) (raw : RawSocket)
    (evts : EventType) : IO (IO.Promise Unit) := do
  let fdNat ← FFI.socketGetFd raw
  let promise ← IO.Promise.new
  let waiter : Waiter := { promise := promise, events := evts }
  if h : 0 < disp.shards.size then
    let sh := disp.shards[fdNat % disp.shards.size]'(Nat.mod_lt fdNat h)
    sh.waiters.atomically do
      let ws ← get
      set (ws.insert fdNat (waiter :: ws.getD fdNat []))
    FFI.eventLoopAdd sh.eventLoop raw evts.flags
  pure promise

/-- Create a new `EventDispatcher` with `shards` independent event loops and
dispatch threads (default 4). -/
def create (shards : Nat := 4) : IO EventDispatcher := do
  let n := max 1 shards
  let mut arr : Array Shard := Array.mkEmpty n
  for _ in [0:n] do
    let eventLoop ← EventLoop.create
    let waiters ← Std.Mutex.new (∅ : Std.HashMap Nat (List Waiter))
    let running ← IO.mkRef true
    let sh : Shard := { eventLoop := eventLoop, waiters := waiters, running := running }
    let _ ← IO.asTask (prio := .dedicated) (dispatchShard sh)
    arr := arr.push sh
  pure (EventDispatcher.mk arr)

/-- Stop all dispatch loops and close every shard's event loop. -/
def shutdown (disp : EventDispatcher) : IO Unit := do
  for sh in disp.shards do
    sh.running.set false
  for sh in disp.shards do
    EventLoop.close sh.eventLoop

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
