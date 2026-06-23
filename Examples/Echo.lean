/-
  Examples.Echo — the whole `linen` socket stack end-to-end.

  Exercises `Network.Socket` (phantom-typed lifecycle) + `EventDispatcher`
  (kqueue/epoll → Green bridge) + `Control.Concurrent` (green threads):

  * one green **accept loop** parks in `EventDispatcher.waitReadable` on the
    listener (freeing its pool worker) and forks a green **handler** per
    connection;
  * each handler echoes with `recvGreen` / `sendAllGreen`, which suspend on
    `wouldBlock` rather than blocking an OS thread.

  So an unbounded number of connections are served by Lean's small worker pool
  — the payoff of the green-thread model.

  Args:
    (none)        -- self-checking demo: spins up clients, verifies echoes, exits
    serve [port]  -- run forever on `port` (default 9099); try:  nc 127.0.0.1 <port>
-/
import Linen.Network.Socket.EventDispatcher
import Linen.Control.Concurrent

open Network.Socket
open Control.Concurrent
open Control.Concurrent.Green

namespace Examples.Echo

-- ── Per-connection handler (a green thread) ──

/-- Echo everything the peer sends until EOF, then close. Suspends (frees its
pool worker) whenever the socket isn't ready, via the dispatcher. -/
def handleClient (disp : EventDispatcher) (client : Socket .connected) : Green Unit := do
  let mut keepOpen := true
  while keepOpen do
    match ← disp.recvGreen client with
    | .data bytes =>
        if bytes.isEmpty then keepOpen := false
        else disp.sendAllGreen client bytes
    | .eof        => keepOpen := false
    | .wouldBlock => pure ()                 -- spurious; recvGreen will re-wait
    | .error _    => keepOpen := false
  let _ ← (Network.Socket.close client : IO _)

-- ── Accept loop (a green thread) ──

/-- Park on listener readiness, then drain all pending connections, forking a
green handler for each. Loops while `running`. -/
def serverLoop (disp : EventDispatcher) (server : Socket .listening)
    (running : IO.Ref Bool) : Green Unit := do
  while (← (running.get : IO _)) do
    disp.waitReadable server
    let mut more := true
    while more do
      match ← (Network.Socket.accept server : IO _) with
      | .accepted client _addr =>
          (Network.Socket.setNonBlocking client : IO _)
          let _ ← (forkGreen (handleClient disp client) : IO _)
      | .wouldBlock => more := false
      | .error _    => more := false

-- ── Self-checking client ──

/-- Connect (blocking), send a message, wait up to 3 s for the echo, verify it.
Returns `true` on a correct round-trip. -/
def echoOnce (port : UInt16) (i : Nat) : IO Bool := do
  let c ← socket .inet .stream
  FFI.socketConnect c.raw "127.0.0.1" port
  let msg := s!"hello-{i}"
  FFI.socketSendAll c.raw msg.toUTF8
  let ok ←
    match ← Network.Socket.poll c .read 3000 with
    | .ready =>
        let echoed ← FFI.socketRecv c.raw msg.toUTF8.size.toUSize
        let got := String.fromUTF8! echoed
        if got == msg then
          IO.println s!"  client {i}: OK  (echoed {got.quote})"; pure true
        else
          IO.println s!"  client {i}: MISMATCH  sent={msg.quote} got={got.quote}"; pure false
    | .timeout => IO.println s!"  client {i}: TIMEOUT (no echo within 3s)"; pure false
    | .error e => IO.println s!"  client {i}: poll error: {e}"; pure false
  let _ ← Network.Socket.close c
  pure ok

-- ── Modes ──

/-- Self-contained demo: ephemeral port, N clients, verify, shut down, exit. -/
def runDemo (nClients : Nat) : IO Unit := do
  let disp ← EventDispatcher.create
  let server ← listenTCP "127.0.0.1" 0          -- port 0 ⇒ kernel picks one
  setNonBlocking server
  let addr ← getSockName server
  IO.println s!"echo demo · server on 127.0.0.1:{addr.port} · {nClients} clients (green-served)\n"
  let running ← IO.mkRef true
  let _ ← forkGreen (serverLoop disp server running)
  IO.sleep 50                                    -- let the accept loop register interest
  let mut ok := 0
  for i in [0:nClients] do
    if ← echoOnce addr.port i then ok := ok + 1
  running.set false
  disp.shutdown
  let _ ← Network.Socket.close server
  IO.println s!"\necho demo done · {ok}/{nClients} echoes verified"
  if ok != nClients then throw (IO.userError "some echoes failed")

/-- Run forever on `port`, serving every connection. For manual `nc` testing. -/
def runServer (port : UInt16) : IO Unit := do
  let disp ← EventDispatcher.create
  let server ← listenTCP "127.0.0.1" port
  setNonBlocking server
  let running ← IO.mkRef true
  IO.println s!"echo server listening on 127.0.0.1:{port}  ·  Ctrl-C to stop  ·  try: nc 127.0.0.1 {port}"
  let tok ← Std.CancellationToken.new
  Green.block (serverLoop disp server running) tok

def run (args : List String) : IO Unit := do
  match args with
  | "serve" :: rest =>
      let port : UInt16 := ((rest.head?.bind (·.toNat?)).getD 9099).toUInt16
      runServer port
  | _ => runDemo 4

end Examples.Echo
