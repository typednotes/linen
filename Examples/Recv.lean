/-
  Examples.Recv — `Network.Socket.Blocking`'s retry-loop wrappers end-to-end.

  The `recv` Hackage package (`Network.Socket.Recv`) is a pair of thin
  wrappers — `recv`/`recvString` — around a blocking-style socket `recv`. That
  functionality already exists in `linen` as
  `Network.Socket.Blocking.recv` (see `docs/imports/Recv/dependencies.md`,
  where it is marked a duplicate rather than re-ported), so this demo
  exercises that module directly instead of a redundant copy:

  * a background `Task` blocks in `Blocking.accept` and then loops in
    `Blocking.recv`, echoing every chunk back with `Blocking.sendAll`, until
    `recv` reports EOF (an empty `ByteArray`);
  * the foreground blocks in `Blocking.connect`, sends a few messages with
    `Blocking.sendAll`, and blocks in `Blocking.recv` for each echo before
    closing the connection to trigger that EOF.

  Args: (none) -- runs the round trip and exits non-zero on any mismatch
-/
import Linen.Network.Socket.Blocking

open Network.Socket
open Network.Socket.Blocking

namespace Examples.Recv

-- ── Server: blocking-accept once, then echo until EOF ──

/-- Accept a single connection, echoing every `Blocking.recv`'d chunk back via
`Blocking.sendAll` until `recv` returns EOF (`ByteArray.empty`). Returns the
chunks it saw, in order. -/
def serveOnce (server : Socket .listening) : IO (List ByteArray) := do
  let (conn, _peer) ← Blocking.accept server
  let mut seen : List ByteArray := []
  let mut more := true
  while more do
    let chunk ← Blocking.recv conn
    if chunk.isEmpty then
      more := false
    else
      Blocking.sendAll conn chunk
      seen := chunk :: seen
  let _ ← close conn
  pure seen.reverse

-- ── Client: blocking-connect, send a few messages, verify each echo ──

/-- Connect, send each of `messages` as a separate `sendAll`, block on `recv`
for the matching echo after every send, then close (which drives the server's
`recv` to EOF). Returns `true` iff every echo matched. -/
def sendAndVerify (addr : SockAddr) (messages : List String) : IO Bool := do
  let client ← socket .inet .stream
  let connected ← Blocking.connect client addr
  let mut allOk := true
  for msg in messages do
    Blocking.sendAll connected msg.toUTF8
    let echoed ← Blocking.recv connected
    let ok := echoed == msg.toUTF8
    IO.println s!"  sent {msg.quote}, echoed {(String.fromUTF8! echoed).quote}  [{if ok then "OK" else "MISMATCH"}]"
    allOk := allOk && ok
  let _ ← close connected
  pure allOk

def demoRoundTrip : IO Bool := do
  IO.println "── blocking accept/connect/send/recv round trip ──"
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  IO.println s!"  server listening on 127.0.0.1:{addr.port}"
  let serverTask ← IO.asTask (prio := .dedicated) (serveOnce server)
  let messages := ["hello", "from", "linen"]
  let clientOk ← sendAndVerify addr messages
  let _ ← close server
  let seenChunks ←
    match serverTask.get with
    | .ok chunks => pure chunks
    | .error e => throw e
  let echoedAll := seenChunks.map (String.fromUTF8! ·)
  IO.println s!"  server saw, in order: {echoedAll}"
  pure (clientOk && echoedAll == messages)

def run (_args : List String) : IO Unit := do
  if ← demoRoundTrip then
    IO.println "\nrecv demo done · all checks passed"
  else
    throw (IO.userError "recv demo done · some checks failed")

end Examples.Recv
