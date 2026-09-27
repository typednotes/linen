/-
  `System.GitFn.Worker` — calling a built `GitFn`

  A worker (`System.GitFn.Build.build`) applies one function to JSON
  arguments. Two ways to talk to it:

  - **stdio** — `Built.spawn` starts the executable; each `call` writes one
    request line and reads one reply line. One process, many calls.
  - **HTTP** — `Built.serve` starts it as a small service on a local port
    (`POST /call` with a JSON-RPC request, `GET /health`); `callEndpoint`
    talks to any such service, local or deployed elsewhere.

  The wire protocol is **JSON-RPC 2.0**, with Lean core's own types for it
  (`Lean.JsonRpc`, which its language server speaks): a call is the request
  `{"jsonrpc": "2.0", "id": n, "method": "call", "params": [a₁, …, aₙ]}` —
  arguments by position — answered by a `result` or an `error` (`invalidParams`
  for arguments that do not decode, `internalError` for a function that fails).
  Over stdio, messages are one per line (compact JSON has no line breaks), and
  the notification `exit` (as in LSP) ends the session. Values are Lean core's
  JSON (`ToJson`/`FromJson`, the classes the remote types implement), so they
  keep their exact representation — including numbers, which are
  arbitrary-precision decimals. Any JSON-RPC client can call a worker.
-/
import Lean.Data.JsonRpc
import Linen.System.GitFn.Build
import Linen.Network.HTTP.Simple

namespace System.GitFn

open Lean (Json ToJson FromJson toJson fromJson?)
open Lean.JsonRpc (Message)

-- ── The protocol ────────────────────────────────────────────────────────────

/-- A call: the JSON-RPC request `call`, with the arguments by position. -/
def request (id : Nat) (args : List Json) : String :=
  (toJson (Message.request (.num id) "call" (some (.arr args.toArray)))).compress

/-- The end of a stdio session: the notification `exit`. -/
def exitNotification : String :=
  (toJson (Message.notification "exit" none)).compress

/-- A reply to the request `id`: its result, or the worker's error. -/
def reply (id : Nat) (s : String) : Except String Json := do
  let msg : Message ← Json.parse s >>= fromJson?
  match msg with
  | .response (.num n) r => if n == id then pure r else throw s!"a reply to another request: {s}"
  | .responseError _ _ message _ => throw message
  | _ => throw s!"malformed reply: {s}"

/-- Decode a result. -/
def decodeResult {β : Type} [FromJson β] : Except String Json → Except String β
  | .ok j => FromJson.fromJson? j
  | .error e => .error e

-- ── stdio ───────────────────────────────────────────────────────────────────

/-- The process configuration of a stdio worker. -/
abbrev stdioSpawn : IO.Process.StdioConfig := { stdin := .piped, stdout := .piped, stderr := .null }

/-- A running stdio worker. -/
structure StdioWorker where
  /-- The worker process. -/
  child : IO.Process.Child stdioSpawn
  /-- The id of the next request. -/
  nextId : IO.Ref Nat

/-- Start a stdio worker. -/
def Built.spawn (b : Built) : IO StdioWorker := do
  let child ← IO.Process.spawn { stdioSpawn with cmd := b.exe.toString, env := cleanEnv }
  pure ⟨child, ← IO.mkRef 0⟩

/-- Call the function on JSON arguments. -/
def StdioWorker.call (w : StdioWorker) (args : List Json) : IO (Except String Json) := do
  let id ← w.nextId.modifyGet fun n => (n, n + 1)
  w.child.stdin.putStrLn (request id args)
  w.child.stdin.flush
  let line ← w.child.stdout.getLine
  if line.isEmpty then return .error "the worker exited"
  return reply id line.trimAscii.toString

/-- Call the function and decode its result. -/
def StdioWorker.invoke {β : Type} [FromJson β] (w : StdioWorker) (args : List Json) :
    IO (Except String β) :=
  decodeResult <$> w.call args

/-- Stop the worker; its exit code.

    The worker is told to stop by the notification `exit`, not by end-of-file
    on its input: every process spawned later by this program inherits the
    write end of that pipe (`IO.Process.spawn` does not close unrelated
    descriptors in the child), so with several workers alive, closing our end
    alone would never deliver EOF, and waiting would hang. The input is closed
    as well, for a worker that already quit. -/
def StdioWorker.stop (w : StdioWorker) : IO UInt32 := do
  try
    w.child.stdin.putStrLn exitNotification
    w.child.stdin.flush
  catch _ => pure ()   -- the worker is already gone
  let (_, child) ← w.child.takeStdin
  child.wait

-- ── HTTP ────────────────────────────────────────────────────────────────────

/-- Call a worker's JSON-RPC endpoint (`<url>/call`, e.g. `http://127.0.0.1:8080`). -/
def callEndpoint (url : String) (args : List Json) : IO (Except String Json) := do
  let base ← Network.HTTP.Simple.parseUrl! (url ++ "/call")
  let req := { base with
    method := .standard .POST
    headers := [(Network.HTTP.Types.hContentType, "application/json")]
    body := some (request 0 args).toUTF8 }
  let resp ← Network.HTTP.Simple.httpBS req
  match String.fromUTF8? resp.body with
  | some body => return reply 0 body
  | none => return .error "the reply is not UTF-8"

/-- The process configuration of an HTTP worker. -/
abbrev httpSpawn : IO.Process.StdioConfig := { stdin := .null, stdout := .piped, stderr := .null }

/-- A running HTTP worker. -/
structure HttpWorker where
  /-- The worker process. -/
  child : IO.Process.Child httpSpawn
  /-- The port it listens on. -/
  port : Nat

/-- Its base URL. -/
def HttpWorker.url (w : HttpWorker) : String := s!"http://127.0.0.1:{w.port}"

/-- Start an HTTP worker on `port` (`0`: any free port, reported back). -/
def Built.serve (b : Built) (port : Nat := 0) : IO (Except String HttpWorker) := do
  let child ← IO.Process.spawn
    { httpSpawn with cmd := b.exe.toString, args := #["--http", toString port], env := cleanEnv }
  let line ← child.stdout.getLine
  match Json.parse line >>= (·.getObjValAs? Nat "listening") with
  | .ok p => return .ok ⟨child, p⟩
  | .error _ => child.kill; return .error s!"the worker did not start: {line}"

/-- Call the function on JSON arguments. -/
def HttpWorker.call (w : HttpWorker) (args : List Json) : IO (Except String Json) :=
  callEndpoint w.url args

/-- Call the function and decode its result. -/
def HttpWorker.invoke {β : Type} [FromJson β] (w : HttpWorker) (args : List Json) :
    IO (Except String β) :=
  decodeResult <$> w.call args

/-- Stop the worker. -/
def HttpWorker.stop (w : HttpWorker) : IO Unit := w.child.kill

end System.GitFn
