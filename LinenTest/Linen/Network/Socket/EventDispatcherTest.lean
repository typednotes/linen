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
import Linen.Network.Socket.Blocking

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

/-! ### One-shot registrations -/

/-- A connected loopback pair: (client, server side). -/
private def socketPair : IO (Socket .connected × Socket .connected × Socket .listening) := do
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  let client ← Blocking.connect (← socket .inet .stream) { host := "127.0.0.1", port := addr.port }
  let (conn, _) ← Blocking.accept server
  return (client, conn, server)

-- At the FFI: a level-triggered registration of a writable socket reports it
-- on every wait; a one-shot one reports it once, until re-added.
#eval show IO Unit from do
  let (client, conn, server) ← socketPair
  let level ← EventLoop.create
  FFI.eventLoopAdd level conn.raw EventType.writable.flags
  let l1 ← EventLoop.wait level 50
  let l2 ← EventLoop.wait level 50
  EventLoop.close level
  let once ← EventLoop.create
  FFI.eventLoopAdd once conn.raw (EventType.writable ||| EventType.oneshot).flags
  let o1 ← EventLoop.wait once 50
  let o2 ← EventLoop.wait once 50
  FFI.eventLoopAdd once conn.raw (EventType.writable ||| EventType.oneshot).flags
  let o3 ← EventLoop.wait once 50
  EventLoop.close once
  for s in [client, conn] do let _ ← close s
  let _ ← close server
  unless l1.length == 1 && l2.length == 1 do
    throw (IO.userError s!"level-triggered: {l1.length}, {l2.length} events")
  unless o1.length == 1 && o2.isEmpty && o3.length == 1 do
    throw (IO.userError s!"one-shot: {o1.length}, {o2.length}, {o3.length} events (want 1, 0, 1)")

/-- This process's CPU time in seconds, from `ps` (`[[DD-]HH:]MM:SS[.ss]`). -/
private def cpuSeconds : IO Float := do
  let out ← IO.Process.output
    { cmd := "ps", args := #["-o", "cputime=", "-p", toString (← IO.Process.getPID)] }
  let fields := ((out.stdout.trimAscii.toString.splitOn "-").getLast!.splitOn ":")
  let num (t : String) : Float :=
    match t.trimAscii.toString.splitOn "." with
    | [w, f] => w.toNat!.toFloat + f.toNat!.toFloat / (10.0 ^ f.length.toFloat)
    | [w] => w.toNat!.toFloat
    | _ => 0
  return fields.foldl (fun acc t => acc * 60 + num t) 0

-- The regression itself: after one `waitWritable` (and one `waitReadable`
-- that leaves data unread), an idle dispatcher must not spin. Through 1.8.0
-- this burnt a full core: 2.0 s of CPU in 2 s.
#eval show IO Unit from do
  let (client, conn, server) ← socketPair
  let disp ← EventDispatcher.create (shards := 1)
  let tok ← Std.CancellationToken.new
  Green.block (disp.waitWritable conn) tok
  Blocking.sendAll client "left unread".toUTF8
  Green.block (disp.waitReadable conn) tok
  let before ← cpuSeconds
  IO.sleep 2000
  let used := (← cpuSeconds) - before
  disp.shutdown
  for s in [client, conn] do let _ ← close s
  let _ ← close server
  if used > 1.0 then
    throw (IO.userError s!"idle dispatcher used {used} s of CPU in 2 s")

-- Waiting for both directions on one fd: a reader and a writer are both
-- woken, whichever direction fires first (with epoll, arming the second
-- registration used to replace the first's mask).
#eval show IO Unit from do
  let (client, conn, server) ← socketPair
  let disp ← EventDispatcher.create (shards := 1)
  let tok ← Std.CancellationToken.new
  let reader ← Green.run (disp.waitReadable conn) tok
  let writer ← Green.run (disp.waitWritable conn) tok
  IO.sleep 100
  Blocking.sendAll client "x".toUTF8
  let mut done := false
  for _ in [0:200] do
    if (← IO.hasFinished reader) && (← IO.hasFinished writer) then done := true; break
    IO.sleep 10
  disp.shutdown
  for s in [client, conn] do let _ ← close s
  let _ ← close server
  unless done do throw (IO.userError "a reader and a writer on one fd were not both woken")

/-! ### Timeouts -/

/-- Millisecond-resolution timers may resolve less than one millisecond
    before a nanosecond-measured duration reaches the requested integer.
    Round up only for the lower bound; keep the exact upper bound so late
    wakeups and the old 100 ms sweep are still caught. -/
private def inTimerRange (elapsedNanos : Nat) (minMillis maxMillis : Nat) : Bool :=
  (elapsedNanos + 999999) / 1000000 ≥ minMillis && elapsedNanos < maxMillis * 1000000

-- A sub-millisecond rounding difference is permitted, not an entire
-- millisecond or a widened upper bound.
#guard inTimerRange 119500000 120 145
#guard inTimerRange 120000000 120 145
#guard !inTimerRange 119000000 120 145
#guard !inTimerRange 118999999 120 145
#guard inTimerRange 144999999 120 145
#guard !inTimerRange 145000000 120 145

#eval show IO Unit from do
  let (client, conn, server) ← socketPair
  let disp ← EventDispatcher.create
  let tok ← Std.CancellationToken.new
  -- Nothing to read: the Green wait times out, near its deadline.
  let t0 ← IO.monoNanosNow
  let ready ← Green.block (disp.waitReadableFor conn 300) tok
  let elapsed := (← IO.monoNanosNow) - t0
  unless !ready && inTimerRange elapsed 300 350 do
    throw (IO.userError s!"waitReadableFor: ready={ready} after {elapsed} ns")
  -- The IO form, likewise; then with data, it is ready at once.
  unless !(← disp.awaitReadableFor conn 200) do throw (IO.userError "awaitReadableFor timed out?")
  Blocking.sendAll client "x".toUTF8
  unless ← disp.awaitReadableFor conn 5000 do throw (IO.userError "awaitReadableFor with data")
  unless ← Green.block (disp.waitWritableFor conn 5000) tok do
    throw (IO.userError "waitWritableFor on a writable socket")
  disp.shutdown
  for s in [client, conn] do let _ ← close s
  let _ ← close server

-- Waiting from `IO` does not starve the pool: 64 body-style waits blocked in
-- `awaitReadableFor` at once (more than the pool has workers), and a pool
-- task that wakes them still runs.
#eval show IO Unit from do
  let (client, conn, server) ← socketPair
  let disp ← EventDispatcher.create
  let waiters ← (List.range 64).mapM fun _ => IO.asTask (disp.awaitReadableFor conn 10000)
  IO.sleep 100
  let waker ← IO.asTask (Blocking.sendAll client "wake".toUTF8)
  let mut allReady := true
  for w in waiters do
    allReady := allReady && ((← IO.wait w).toOption.getD false)
  let _ ← IO.wait waker
  disp.shutdown
  for s in [client, conn] do let _ ← close s
  let _ ← close server
  unless allReady do throw (IO.userError "some IO waiters were not woken")

/-! ### Timer precision (libuv timers, no sweep) -/

-- Timeouts are honoured to within a few milliseconds, at several scales;
-- the old periodic sweep was only as good as its 100 ms interval.
#eval show IO Unit from do
  let (client, conn, server) ← socketPair
  let disp ← EventDispatcher.create
  let tok ← Std.CancellationToken.new
  for ms in [5, 20, 120] do
    let t0 ← IO.monoNanosNow
    let ready ← Green.block (disp.waitReadableFor conn ms) tok
    let elapsed := (← IO.monoNanosNow) - t0
    unless !ready && inTimerRange elapsed ms (ms + 25) do
      throw (IO.userError s!"a {ms} ms wait timed out after {elapsed} ns (ready={ready})")
  disp.shutdown
  for s in [client, conn] do let _ ← close s
  let _ ← close server

-- Readiness beats the timer: the wait returns `true` at once, and the timer
-- it armed is stopped — a later expiry does not disturb a new waiter on the
-- same socket.
#eval show IO Unit from do
  let (client, conn, server) ← socketPair
  let disp ← EventDispatcher.create
  let tok ← Std.CancellationToken.new
  Blocking.sendAll client "x".toUTF8
  let t0 ← IO.monoMsNow
  let ready ← Green.block (disp.waitReadableFor conn 200) tok
  let quick := decide ((← IO.monoMsNow) - t0 < 100)
  let _ ← Blocking.recv conn 1
  -- A second, longer wait on the same fd, spanning the first one's deadline.
  let waiter ← Green.run (disp.waitReadableFor conn 1000) tok
  IO.sleep 400
  let early ← IO.hasFinished waiter
  Blocking.sendAll client "y".toUTF8
  let second := (← IO.wait waiter).toOption.getD false
  disp.shutdown
  for s in [client, conn] do let _ ← close s
  let _ ← close server
  unless ready && quick && !early && second do
    throw (IO.userError s!"ready={ready} quick={quick} early={early} second={second}")

-- Thousands of deadlines at once, all expiring on time: 2000 green waits
-- with 100-150 ms deadlines finish in about 150 ms (the timers are libuv's,
-- not a sweep). (2000 *`IO`-blocked* waits would each cost the task manager a
-- compensating OS thread; that is why the server waits in `Green` wherever it
-- can.)
#eval show IO Unit from do
  let (client, conn, server) ← socketPair
  let disp ← EventDispatcher.create
  let tok ← Std.CancellationToken.new
  let t0 ← IO.monoMsNow
  let waits ← (List.range 2000).mapM fun i => Green.run (disp.waitReadableFor conn (100 + i % 50)) tok
  let mut timedOut := 0
  for w in waits do
    if (← IO.wait w).toOption == some false then timedOut := timedOut + 1
  let elapsed := (← IO.monoMsNow) - t0
  disp.shutdown
  for s in [client, conn] do let _ ← close s
  let _ ← close server
  unless timedOut == 2000 && elapsed < 1000 do
    throw (IO.userError s!"{timedOut} of 2000 waits timed out, in {elapsed} ms")

-- `Green.sleep` is precise and holds no pool thread: 200 green threads
-- sleeping 300 ms at once (far more than the pool has workers) all finish
-- in about 300 ms. With `IO.sleep` they would queue behind each other.
#eval show IO Unit from do
  let tok ← Std.CancellationToken.new
  let t0 ← IO.monoNanosNow
  let sleepers ← (List.range 200).mapM fun _ => Green.run (Green.sleep 300) tok
  for t in sleepers do let _ ← IO.wait t
  let elapsed := (← IO.monoNanosNow) - t0
  unless inTimerRange elapsed 300 1000 do
    throw (IO.userError s!"200 concurrent 300 ms sleeps took {elapsed} ns")

end Tests.Network.Socket.EventDispatcher
