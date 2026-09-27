/-
  `System.GitFn.Worker` — calling a built `GitFn`

  A worker (`System.GitFn.Build.build`) applies one function to JSON
  arguments. Two ways to talk to it:

  - **stdio** — `Built.spawn` starts the executable; each `call` writes one
    request line and reads one reply line. One process, many calls.
  - **HTTP** — `Built.serve` starts it as a small REST service on a local port
    (`POST /call`, `GET /health`); `callEndpoint` talks to any such service,
    local or deployed elsewhere.

  The wire format is Lean core's JSON (`Lean.Json`), the same classes
  (`ToJson`/`FromJson`) the remote types implement, so values keep their exact
  representation — including numbers, which are arbitrary-precision decimals:
  `{"args": [a₁, …, aₙ]}` in, `{"ok": result}` or `{"error": message}` out.
-/
import Linen.System.GitFn.Build
import Linen.Network.HTTP.Simple

namespace System.GitFn

open Lean (Json ToJson FromJson)

-- ── The protocol ────────────────────────────────────────────────────────────

/-- A request: `{"args": [...]}`, on one line. -/
def request (args : List Json) : String :=
  (Json.mkObj [("args", Json.arr args.toArray)]).compress

/-- A reply: the result, or the worker's error. -/
def reply (s : String) : Except String Json := do
  let j ← Json.parse s
  match j.getObjVal? "ok" with
  | .ok v => pure v
  | .error _ => match j.getObjValAs? String "error" with
    | .ok e => throw e
    | .error _ => throw s!"malformed reply: {s}"

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

/-- Start a stdio worker. -/
def Built.spawn (b : Built) : IO StdioWorker := do
  let child ← IO.Process.spawn { stdioSpawn with cmd := b.exe.toString, env := cleanEnv }
  pure ⟨child⟩

/-- Call the function on JSON arguments. -/
def StdioWorker.call (w : StdioWorker) (args : List Json) : IO (Except String Json) := do
  w.child.stdin.putStrLn (request args)
  w.child.stdin.flush
  let line ← w.child.stdout.getLine
  if line.isEmpty then return .error "the worker exited"
  return reply line.trimAscii.toString

/-- Call the function and decode its result. -/
def StdioWorker.invoke {β : Type} [FromJson β] (w : StdioWorker) (args : List Json) :
    IO (Except String β) :=
  decodeResult <$> w.call args

/-- Stop the worker (closing its input ends it); its exit code. -/
def StdioWorker.stop (w : StdioWorker) : IO UInt32 := do
  let (_, child) ← w.child.takeStdin
  child.wait

-- ── HTTP ────────────────────────────────────────────────────────────────────

/-- Call a worker's REST endpoint (`<url>/call`, e.g. `http://127.0.0.1:8080`). -/
def callEndpoint (url : String) (args : List Json) : IO (Except String Json) := do
  let base ← Network.HTTP.Simple.parseUrl! (url ++ "/call")
  let req := { base with
    method := .standard .POST
    headers := [(Network.HTTP.Types.hContentType, "application/json")]
    body := some (request args).toUTF8 }
  let resp ← Network.HTTP.Simple.httpBS req
  match String.fromUTF8? resp.body with
  | some body => return reply body
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
