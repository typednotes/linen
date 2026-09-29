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

  ## One-shot registrations

  Every registration is **one-shot** (`EventType.oneshot`: `EV_ONESHOT` /
  `EPOLLONESHOT`) and carries every direction still awaited on its fd, and
  the dispatcher re-arms an fd whose other waiters are still pending — the
  scheme of libuv, mio and GHC's IO manager. Through 1.8.0 registrations
  were level-triggered and never removed, so once a socket had been awaited
  it reported itself on every `kevent`/`epoll_wait` for as long as it stayed
  writable (almost always) or readable, and its shard spun a core at 100%
  with the server otherwise idle; with epoll, a second waiter's registration
  also replaced the first one's mask.

  ## Timeouts

  `waitReadableFor`/`waitWritableFor` (and their `IO` forms
  `awaitReadableFor`/`awaitWritableFor`) give up after a deadline: the
  dispatch loop sweeps expired waiters at least every `sweepIntervalMillis`
  and resolves them as timed out.

  ## Waiting from `IO`

  `awaitReadableFor` blocks its caller with `IO.wait` on the waiter's
  promise. Lean's task manager compensates for a worker blocked in `IO.wait`
  by letting another run (a worker blocked in a syscall such as `poll` is
  not compensated), so code that must wait from `IO` — a request body read
  by an application — does not starve the pool.

  ## Guarantees (axiom-dependent on the FFI / `BaseIO.bindTask` contract)

  - **No pool starvation:** `waitReadable`/`waitWritable` free their pool thread.
  - **One-shot semantics:** each waiter is resolved exactly once and removed,
    and each registration notifies once.
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

/-- A waiter entry: the promise to resolve — `true` when the socket is ready
    (or the dispatcher shuts down), `false` when the deadline passed — the
    events awaited, the socket (to re-arm its registration), and the
    deadline (`IO.monoMsNow` time), if any. -/
private structure Waiter where
  promise  : IO.Promise Bool
  events   : EventType
  raw      : RawSocket
  deadline : Option Nat

/-- One dispatcher shard: an event loop, the waiters registered on it (keyed by
fd), and a flag controlling its dispatch thread. -/
private structure Shard where
  eventLoop : EventLoop
  waiters   : Std.Mutex (Std.HashMap Nat (List Waiter))
  running   : IO.Ref Bool
  lastSweep : IO.Ref Nat

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

/-- How often, at least, the dispatch loop resolves waiters whose deadline
    has passed. A timeout is honoured to within this. -/
def sweepIntervalMillis : Nat := 100

/-- Every direction the waiters in `ws` await, as a one-shot registration. -/
private def armMask (ws : List Waiter) : EventType :=
  ws.foldl (fun acc w => acc ||| w.events) EventType.oneshot

/-- The dispatch loop for one shard, on its own dedicated OS thread. Drains a
whole `kevent`/`epoll_wait` batch, collects the matching waiters under a single
lock, then — after releasing it — resolves their promises and re-arms each fd
that still has waiters (its registration was one-shot). Every
`sweepIntervalMillis` it also resolves expired waiters as timed out. -/
private def dispatchShard (sh : Shard) : IO Unit := do
  while ← sh.running.get do
    let events ← EventLoop.wait sh.eventLoop 50
    let now ← IO.monoMsNow
    let sweep := now ≥ (← sh.lastSweep.get) + sweepIntervalMillis
    if sweep then sh.lastSweep.set now
    if !events.isEmpty || sweep then
      let (ready, expired, rearm) ← sh.waiters.atomically do
        let mut ready : List Waiter := []
        let mut rearm : Std.HashMap Nat (List Waiter) := {}
        for ev in events do
          let ws ← get
          match ws[ev.socketFd]? with
          | none => pure ()
          | some waiterList =>
            let (matched, remaining) := waiterList.partition (waiterMatches ev.events)
            if remaining.isEmpty then
              set (ws.erase ev.socketFd)
              rearm := rearm.erase ev.socketFd
            else
              set (ws.insert ev.socketFd remaining)
              -- Re-arm even when nothing matched: with epoll the one-shot
              -- registration is now spent for *every* direction.
              rearm := rearm.insert ev.socketFd remaining
            ready := ready ++ matched
        let mut expired : List Waiter := []
        if sweep then
          let ws ← get
          let mut kept : Std.HashMap Nat (List Waiter) := {}
          for (fd, waiterList) in ws.toList do
            let (late, onTime) := waiterList.partition fun w => w.deadline.any (· ≤ now)
            expired := expired ++ late
            if !onTime.isEmpty then kept := kept.insert fd onTime
          set kept
        pure (ready, expired, rearm)
      for w in ready do
        w.promise.resolve true
      for w in expired do
        w.promise.resolve false
      for (_, remaining) in rearm.toList do
        if let some w := remaining.head? then
          -- The fd may have been closed meanwhile; its waiters then time out
          -- or are woken by shutdown, so a failed re-arm is not an error.
          try FFI.eventLoopAdd sh.eventLoop w.raw (armMask remaining).flags catch _ => pure ()

/-- Register a waiter for a socket fd on its shard (`fd % N`), arming the fd
one-shot for every direction awaited on it. Internal. -/
private def register (disp : EventDispatcher) (raw : RawSocket)
    (evts : EventType) (timeoutMillis : Option Nat := none) : IO (IO.Promise Bool) := do
  let fdNat ← FFI.socketGetFd raw
  let promise ← IO.Promise.new
  let deadline ← timeoutMillis.mapM fun ms => return (← IO.monoMsNow) + ms
  let waiter : Waiter := { promise, events := evts, raw, deadline }
  if h : 0 < disp.shards.size then
    let sh := disp.shards[fdNat % disp.shards.size]'(Nat.mod_lt fdNat h)
    let all ← sh.waiters.atomically do
      let ws ← get
      let all := waiter :: ws.getD fdNat []
      set (ws.insert fdNat all)
      pure all
    FFI.eventLoopAdd sh.eventLoop raw (armMask all).flags
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
    let lastSweep ← IO.mkRef (← IO.monoMsNow)
    let sh : Shard := { eventLoop, waiters, running, lastSweep }
    let _ ← IO.asTask (prio := .dedicated) (dispatchShard sh)
    arr := arr.push sh
  pure (EventDispatcher.mk arr)

/-- Stop all dispatch loops, close every shard's event loop, and wake any
`waitReadable`/`waitWritable` callers still parked on a shard (resolving their
promise with a plain `()`, same as a real readiness event) so a shutdown
during an idle wait doesn't strand the awaiting `Green` thread — and the
`IO.asTask (prio := .dedicated)` thread underneath it — forever. Idempotent
and safe to call from multiple places (e.g. both the owner of the dispatcher
and a caller's own cleanup): each shard only drains/closes once, guarded by
its `running` flag. -/
def shutdown (disp : EventDispatcher) : IO Unit := do
  for sh in disp.shards do
    let wasRunning ← sh.running.modifyGet (fun r => (r, false))
    if wasRunning then
      let pending ← sh.waiters.atomically do
        let ws ← get
        set (∅ : Std.HashMap Nat (List Waiter))
        pure ws
      for (_, waiterList) in pending.toList do
        for w in waiterList do
          w.promise.resolve true
      EventLoop.close sh.eventLoop

/-- Wait for a socket to become readable. Suspends the Green thread
    (frees the pool thread) and resumes when the socket is readable.
    $$\text{waitReadable} : \text{EventDispatcher} \to \text{Socket}\ s \to \text{Green Unit}$$ -/
def waitReadable (disp : EventDispatcher) (s : Socket state) : Green Unit := do
  let promise ← (disp.register s.raw EventType.readable : IO _)
  let _ ← Green.await promise.result!

/-- Wait for a socket to become writable. Suspends the Green thread
    (frees the pool thread) and resumes when the socket is writable.
    $$\text{waitWritable} : \text{EventDispatcher} \to \text{Socket}\ s \to \text{Green Unit}$$ -/
def waitWritable (disp : EventDispatcher) (s : Socket state) : Green Unit := do
  let promise ← (disp.register s.raw EventType.writable : IO _)
  let _ ← Green.await promise.result!

/-- `waitReadable`, giving up after `timeoutMillis`: `true` when the socket
    became readable, `false` when the time ran out.
    $$\text{waitReadableFor} : \text{EventDispatcher} \to \text{Socket}\ s \to \mathbb{N} \to \text{Green Bool}$$ -/
def waitReadableFor (disp : EventDispatcher) (s : Socket state) (timeoutMillis : Nat) :
    Green Bool := do
  let promise ← (disp.register s.raw EventType.readable (some timeoutMillis) : IO _)
  Green.await promise.result!

/-- `waitWritable`, giving up after `timeoutMillis` (`false`). -/
def waitWritableFor (disp : EventDispatcher) (s : Socket state) (timeoutMillis : Nat) :
    Green Bool := do
  let promise ← (disp.register s.raw EventType.writable (some timeoutMillis) : IO _)
  Green.await promise.result!

/-- `waitReadableFor` from plain `IO`, blocking the caller with `IO.wait` on
    the dispatcher's promise — which Lean's task manager compensates for,
    unlike a blocking syscall (see the module header).
    $$\text{awaitReadableFor} : \text{EventDispatcher} \to \text{Socket}\ s \to \mathbb{N} \to \text{IO Bool}$$ -/
def awaitReadableFor (disp : EventDispatcher) (s : Socket state) (timeoutMillis : Nat) :
    IO Bool := do
  let promise ← disp.register s.raw EventType.readable (some timeoutMillis)
  return (← IO.wait promise.result?).getD false

/-- `waitWritableFor` from plain `IO` (see `awaitReadableFor`). -/
def awaitWritableFor (disp : EventDispatcher) (s : Socket state) (timeoutMillis : Nat) :
    IO Bool := do
  let promise ← disp.register s.raw EventType.writable (some timeoutMillis)
  return (← IO.wait promise.result?).getD false

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

/-- Receive from a connected socket, suspending the green thread until data
    arrives, for at most `timeoutMillis`: `some` bytes, `some` empty at end of
    input, `none` on timeout. The receive is tried before any wait.
    $$\text{recvFor} : \text{EventDispatcher} \to \text{Socket .connected} \to \mathbb{N} \to \text{Green (Option ByteArray)}$$ -/
def recvFor (disp : EventDispatcher) (s : Socket .connected) (timeoutMillis : Nat)
    (maxlen : Nat := 16384) : Green (Option ByteArray) := do
  repeat
    match ← (Network.Socket.recv s maxlen : IO _) with
    | .data bytes => return some bytes
    | .eof => return some ByteArray.empty
    | .error e => throw e
    | .wouldBlock => unless ← disp.waitReadableFor s timeoutMillis do return none
  return none

/-- `recvFor` from plain `IO` (`awaitReadableFor`); throws on timeout, empty
    at end of input. -/
def recvAwait (disp : EventDispatcher) (s : Socket .connected) (timeoutMillis : Nat)
    (maxlen : Nat := 16384) : IO ByteArray := do
  repeat
    match ← Network.Socket.recv s maxlen with
    | .data bytes => return bytes
    | .eof => return ByteArray.empty
    | .error e => throw e
    | .wouldBlock => unless ← disp.awaitReadableFor s timeoutMillis do
        throw (IO.userError s!"recv timed out after {timeoutMillis}ms")
  return ByteArray.empty

/-- `sendAllGreen`, throwing when one wait for writability exceeds
    `timeoutMillis` (a peer that stops reading). -/
def sendAllGreenFor (disp : EventDispatcher) (s : Socket .connected) (data : ByteArray)
    (timeoutMillis : Nat) : Green Unit := do
  let mut offset := 0
  while offset < data.size do
    match ← (Network.Socket.send s (data.extract offset data.size) : IO _) with
    | .sent n => offset := offset + n
    | .wouldBlock =>
      unless ← disp.waitWritableFor s timeoutMillis do
        throw (IO.userError s!"send timed out after {timeoutMillis}ms")
    | .error e => throw e

/-- `sendAllGreenFor` from plain `IO` (`awaitWritableFor`). -/
def sendAllAwait (disp : EventDispatcher) (s : Socket .connected) (data : ByteArray)
    (timeoutMillis : Nat) : IO Unit := do
  let mut offset := 0
  while offset < data.size do
    match ← Network.Socket.send s (data.extract offset data.size) with
    | .sent n => offset := offset + n
    | .wouldBlock =>
      unless ← disp.awaitWritableFor s timeoutMillis do
        throw (IO.userError s!"send timed out after {timeoutMillis}ms")
    | .error e => throw e

end EventDispatcher

end Network.Socket
