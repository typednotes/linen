/-
  Examples.StreamingCommons — `Data.Streaming.Network` end-to-end.

  * `demoAppDataRoundTrip` — a background task `bindPortTCP`s an ephemeral
    port, `acceptSafe`s one connection, and talks to it purely through the
    `AppData` interface (`mkAppData`'s `appRead`/`appWrite`/`appClose`); the
    foreground `getSocketTCP`s to that port, sends a few messages, and checks
    each echoed reply.
  * `serve [port]` — runs `runTCPServer` forever on `port` (default 9098),
    echoing every connection via the same `AppData` interface. For manual
    testing, e.g. `nc 127.0.0.1 <port>`.

  Args:
    (none)        -- self-checking demo: spins up one client, verifies echoes, exits
    serve [port]  -- run forever on `port` (default 9098); try:  nc 127.0.0.1 <port>
-/
import Linen.Data.Streaming.Network

open Network.Socket
open Data.Streaming.Network

namespace Examples.StreamingCommons

/-- Echo every chunk `appRead` returns back via `appWrite`, until EOF
(an empty `ByteArray`), then `appClose`. -/
def echoAppData (app : AppData) : IO Unit := do
  let mut more := true
  while more do
    let chunk ← app.appRead
    if chunk.isEmpty then
      more := false
    else
      app.appWrite chunk
  app.appClose

/-- Accept a single connection via `acceptSafe`, wrap it with `mkAppData`, and
echo through the `AppData` interface until EOF. -/
def serveOnce (server : Socket .listening) : IO Unit := do
  let (client, addr) ← acceptSafe server
  echoAppData (mkAppData client addr)

def demoAppDataRoundTrip : IO Bool := do
  IO.println "── bindPortTCP / getSocketTCP / acceptSafe / AppData round trip ──"
  let server ← bindPortTCP 0 "127.0.0.1"
  let addr ← getSockName server
  IO.println s!"  server listening on 127.0.0.1:{addr.port}"
  let serverTask ← IO.asTask (prio := .dedicated) (serveOnce server)
  let (client, _addr) ← getSocketTCP "127.0.0.1" addr.port
  let messages := ["hello", "from", "streaming-commons"]
  let mut allOk := true
  for msg in messages do
    Blocking.sendAll client msg.toUTF8
    let echoed ← Blocking.recv client
    let ok := echoed == msg.toUTF8
    IO.println s!"  sent {msg.quote}, echoed {(String.fromUTF8! echoed).quote}  [{if ok then "OK" else "MISMATCH"}]"
    allOk := allOk && ok
  let _ ← close client
  match serverTask.get with
  | .ok () => pure ()
  | .error e => throw e
  let _ ← close server
  pure allOk

/-- Run forever on `port`, echoing every connection via `runTCPServer`. For
manual `nc` testing. -/
def runServer (port : UInt16) : IO Unit := do
  IO.println s!"streaming-commons server listening on 0.0.0.0:{port}  ·  Ctrl-C to stop  ·  try: nc 127.0.0.1 {port}"
  runTCPServer port echoAppData

def run (args : List String) : IO Unit := do
  match args with
  | "serve" :: rest =>
      let port : UInt16 := ((rest.head?.bind (·.toNat?)).getD 9098).toUInt16
      runServer port
  | _ =>
    if ← demoAppDataRoundTrip then
      IO.println "\nstreaming-commons demo done · all checks passed"
    else
      throw (IO.userError "streaming-commons demo done · some checks failed")

end Examples.StreamingCommons
