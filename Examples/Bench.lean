/-
  Examples.Bench — network round-trips where each query takes a few ms,
  Green vs a blocking thread pool, with the SAME thread budget on both sides.

  We do `total` echo round-trips over `C` persistent connections (`total / C`
  rounds each). The shared echo server delays every reply by `delayMs` (a few
  ms) to model a real backend that *waits* — using an async timer so the green
  server holds many slow requests on ≈#cores threads (it suspends, never sleeps
  a worker).

  Both client sides get a pool of ≈#cores OS threads; only the concurrency model
  differs:

  * Green — `C` green threads (`forkGreen`), each driving one connection through
    the `EventDispatcher`. They suspend while a reply is pending, so all `C` are
    in flight at once. Wall time ≈ (rounds × delay).
  * Blocking pool — exactly #cores dedicated threads; each owns ⌈C/#cores⌉
    connections and runs them sequentially with blocking `recv`. Only #cores
    replies are awaited at once, so the C connections serialise into ⌈C/#cores⌉
    waves. Wall time ≈ (C/#cores × rounds × delay).

  With a per-query wait, Green's C-way concurrency hides the latency while the
  pool multiplies it by the number of waves — that's the regime where green
  threads win. Same ≈#cores threads on both sides, trivially under 1000.

  Args: `bench [C] [delayMs] [total]` (defaults: 512, 3, 20000).
-/
import Linen.Network.Socket.EventDispatcher
import Linen.Control.Concurrent

namespace Examples.Bench

open Network.Socket
open Control.Concurrent
open Control.Concurrent.Green

-- ── Async timer: suspend a green thread for `ms` without blocking a worker ──

/-- A timer driven by one dedicated thread that resolves promises as they fall
due, so `sleepGreen` suspends the green thread (frees its pool worker) instead
of blocking it. A general "timer source" for the green model. -/
structure Timer where
  private mk ::
  pending : Std.Mutex (Array (Nat × IO.Promise Unit))
  running : IO.Ref Bool

/-- The timer loop: every ~1 ms, resolve every promise whose deadline has passed. -/
private def timerLoop (t : Timer) : IO Unit := do
  while (← t.running.get) do
    IO.sleep 1
    let now ← IO.monoMsNow
    let due ← t.pending.atomically do
      let ps ← get
      let (expired, rest) := ps.partition (fun e => decide (e.1 ≤ now))
      set rest
      pure expired
    for e in due do e.2.resolve ()

/-- Create a timer with its dedicated driver thread. -/
def Timer.create : IO Timer := do
  let pending ← Std.Mutex.new (#[] : Array (Nat × IO.Promise Unit))
  let running ← IO.mkRef true
  let t : Timer := Timer.mk pending running
  let _ ← IO.asTask (prio := .dedicated) (timerLoop t)
  pure t

/-- Stop the timer's driver thread. -/
def Timer.shutdown (t : Timer) : IO Unit := t.running.set false

/-- Suspend the current green thread for at least `ms` milliseconds. -/
def Timer.sleepGreen (t : Timer) (ms : Nat) : Green Unit := do
  let now ← (IO.monoMsNow : IO _)
  let p : IO.Promise Unit ← (IO.Promise.new : IO _)
  (t.pending.atomically (do set ((← get).push (now + ms, p))) : IO _)
  Green.await p.result!

-- ── Delaying echo server (green; one handler per connection) ──

/-- Echo each request after a `delayMs` async pause, until EOF. -/
def handleDelayed (disp : EventDispatcher) (timer : Timer) (delayMs : Nat)
    (client : Socket .connected) : Green Unit := do
  let mut keepOpen := true
  while keepOpen do
    match ← disp.recvGreen client with
    | .data b =>
        if b.isEmpty then keepOpen := false
        else do
          if delayMs > 0 then timer.sleepGreen delayMs
          disp.sendAllGreen client b
    | .eof        => keepOpen := false
    | .wouldBlock => pure ()
    | .error _    => keepOpen := false
  let _ ← (Network.Socket.close client : IO _)

/-- Accept loop forking a delaying handler per connection. -/
def serverLoop (disp : EventDispatcher) (timer : Timer) (delayMs : Nat)
    (server : Socket .listening) (running : IO.Ref Bool) : Green Unit := do
  while (← (running.get : IO _)) do
    disp.waitReadable server
    let mut more := true
    while more do
      match ← (Network.Socket.accept server : IO _) with
      | .accepted client _addr =>
          (Network.Socket.setNonBlocking client : IO _)
          let _ ← (forkGreen (handleDelayed disp timer delayMs client) : IO _)
      | .wouldBlock => more := false
      | .error _    => more := false

-- ── One persistent connection, two ways ──

/-- One request→response round on a connected socket (no close). A readiness
notification can be spurious (epoll/kqueue may report ready, then `recv` yields
EAGAIN), so we re-wait on `wouldBlock` — as a correct non-blocking client must. -/
def greenRound (disp : EventDispatcher) (cs : Socket .connected) (payload : ByteArray) :
    Green Bool := do
  disp.sendAllGreen cs payload
  let mut result := false
  let mut done := false
  while !done do
    match ← disp.recvGreen cs payload.size with
    | .data b     => result := b == payload; done := true
    | .wouldBlock => pure ()
    | _           => done := true
  pure result

/-- Green client over one persistent connection: connect, `rounds` rounds, close. -/
def greenConn (disp : EventDispatcher) (addr : SockAddr) (payload : ByteArray) (rounds : Nat) :
    Green Bool := do
  let c ← (socket .inet .stream : IO _)
  (setNonBlocking c : IO _)
  let connected : Option (Socket .connected) ←
    match ← (connect c addr : IO _) with
    | .connected cs  => pure (some cs)
    | .inProgress cs =>
        disp.waitWritable cs
        match ← (connectFinish cs : IO _) with
        | .connected cs2 => pure (some cs2)
        | _              => pure none
    | .refused _     => pure none
  match connected with
  | none    => do let _ ← (Network.Socket.close c : IO _); pure false
  | some cs =>
      let mut allOk := true
      for _ in [0:rounds] do
        if !(← greenRound disp cs payload) then allOk := false
      let _ ← (Network.Socket.close cs : IO _)
      pure allOk

/-- Blocking client over one persistent connection: connect, `rounds` blocking
rounds, close. Holds its OS thread (waiting `delayMs` on each `recv`). -/
def poolConn (host : String) (port : UInt16) (payload : ByteArray) (rounds : Nat) : IO Bool := do
  let c ← socket .inet .stream
  FFI.socketConnect c.raw host port
  let mut allOk := true
  for _ in [0:rounds] do
    FFI.socketSendAll c.raw payload
    let got ← FFI.socketRecv c.raw payload.size.toUSize
    if got != payload then allOk := false
  let _ ← Network.Socket.close c
  pure allOk

-- ── Drivers: K connections × R rounds, return (wall-ms, connections-ok) ──

/-- `conns` green connections, all concurrent on ≈#cores threads. -/
def runGreen (disp : EventDispatcher) (addr : SockAddr) (payload : ByteArray)
    (conns rounds : Nat) : IO (Nat × Nat) := do
  let okRef ← Std.Mutex.new (0 : Nat)
  let t0 ← IO.monoMsNow
  let mut tids : Array ThreadId := Array.mkEmpty conns
  for _ in [0:conns] do
    tids := tids.push (← forkGreen (do
      if ← greenConn disp addr payload rounds then
        okRef.atomically do set ((← get) + 1)))
  for tid in tids do waitThread tid
  let t1 ← IO.monoMsNow
  let ok ← okRef.atomically get
  pure (t1 - t0, ok)

/-- `conns` connections over a fixed pool of `poolThreads` OS threads; each
worker handles ⌈conns/poolThreads⌉ connections sequentially. -/
def runPool (host : String) (port : UInt16) (payload : ByteArray)
    (conns rounds poolThreads : Nat) : IO (Nat × Nat) := do
  let k := max 1 (min poolThreads conns)
  let perWorker := (conns + k - 1) / k
  let t0 ← IO.monoMsNow
  let mut workers : Array (Task (Except IO.Error Nat)) := Array.mkEmpty k
  for w in [0:k] do
    let lo := w * perWorker
    let hi := min conns (lo + perWorker)
    if lo < hi then
      workers := workers.push (← IO.asTask (prio := .dedicated) (do
        let mut ok := 0
        for _ in [lo:hi] do
          if ← poolConn host port payload rounds then ok := ok + 1
        pure ok))
  let mut total := 0
  for t in workers do
    match ← IO.wait t with
    | .ok c => total := total + c
    | _     => pure ()
  let t1 ← IO.monoMsNow
  pure (t1 - t0, total)

-- ── Entry point ──

def run (args : List String) : IO Unit := do
  let arg := fun (i d : Nat) => (args[i]?.bind String.toNat?).getD d
  let cpus  ← FFI.numCpus
  let soft  ← FFI.setFdLimit (8192 : USize)
  let fdCap := if soft > 128 then (soft - 64) / 2 else 32
  let conns   := max 1 (min (arg 0 512) fdCap)
  let delayMs := arg 1 3
  let total   := arg 2 20000
  let nShards := max 1 (arg 3 4)
  let rounds  := max 1 (total / conns)
  let ops     := conns * rounds
  let pool    := cpus
  let waves   := (conns + pool - 1) / pool
  IO.println s!"{ops} round-trips · {conns} connections × {rounds} rounds · {delayMs} ms server delay per query"
  IO.println s!"pool = {pool} OS threads on BOTH sides · dispatcher shards = {nShards} (fd soft limit = {soft})\n"
  let disp ← EventDispatcher.create nShards
  let timer ← Timer.create
  let server ← listenTCP "127.0.0.1" 0 2048
  setNonBlocking server
  let addr ← getSockName server
  let running ← IO.mkRef true
  let _ ← forkGreen (serverLoop disp timer delayMs server running)
  IO.sleep 50
  let payload := "ping".toUTF8
  let (gms, gok) ← runGreen disp addr payload conns rounds
  let (pms, pok) ← runPool addr.host addr.port payload conns rounds pool
  running.set false
  timer.shutdown
  disp.shutdown
  let _ ← Network.Socket.close server
  let speed := if pms == 0 then 0 else (gms * 100) / pms
  IO.println s!"  Green threads : {gms} ms   ({gok}/{conns} ok · {conns}-way concurrent on {pool} threads)"
  IO.println s!"  Blocking pool : {pms} ms   ({pok}/{conns} ok · {pool}-way concurrent · {waves} waves)"
  IO.println s!"\n→ Each query waits {delayMs} ms. Green keeps all {conns} in flight → ≈ {rounds} × {delayMs} ms total."
  IO.println s!"  The pool runs {pool} at a time → ≈ {waves} waves × {rounds} × {delayMs} ms. Green is ~{speed}% of the pool's time."

end Examples.Bench
