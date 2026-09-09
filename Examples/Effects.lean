/-
  Examples.Effects — the capability-restricted effects, end-to-end against
  real files, a real socket, and a real PostgreSQL server.

  `Control.Monad.Effect`'s row already says *which* effects a computation may
  perform. `Effect.FileSystem`, `Effect.HTTP` and `Effect.PostgreSQL` go one
  step further: the permission is a **value** indexing the effect, so the type
  also says which *operations* are allowed and which *arguments* they may be
  called on. This demo exercises all three against real resources, so that
  "authorised" and "actually happened" can be checked against each other:

  * `demoFileSystem` — a capability rooted at a scratch directory writes and
    reads real files through `IO.FS`, and a second capability (same root, one
    more bit) deletes them. Every path outside the root, and `deleteFile` under
    the first capability, are rejected *at elaboration time* — the module states
    those as theorems, since a program that failed to compile cannot be run;
  * `demoHTTP` — a hand-rolled loopback HTTP/1.1 server (the same pattern as
    `Examples.HTTPClient`) is driven by a capability that may `GET` anything
    under `/v1` but `POST` only to `/v1/events`;
  * `demoPostgreSQL` — a disposable `postgres` container is started with Podman,
    a table is created in it *outside* the effect (the query AST has no DDL, so
    `CREATE TABLE` is not expressible), and a capability scoped to that one
    table runs real `INSERT`/`UPDATE`/`SELECT` statements against it. A second
    table exists in the same database and is provably unreachable through the
    capability;
  * `demoRuntimeChecks` — the three `check?` functions, for paths, URLs and
    queries that are only known at runtime: validation returns the *evidence*,
    so the operations still cannot be reached without it;
  * `demoCombined` — one `Eff` computation over a four-effect row
    (`FileSystem`, `HTTP`, `PostgreSQL`, `Trace`) run by a single handler that
    dispatches each request to that effect's own interpreter.

  Args:
    (none)  -- run every demo below, starting a Podman PostgreSQL container
    no-db   -- skip the PostgreSQL and combined demos (no Podman needed)

  The PostgreSQL demo needs a working `podman` (on macOS, a started
  `podman machine`); if it is unreachable the section is reported as skipped
  rather than failing. The image is public, so if a corporate registry
  credential helper gets in the way of the anonymous pull, point
  `REGISTRY_AUTH_FILE` at a file containing `{"auths":{}}`.
-/
import Linen.Control.Monad.Effect.FileSystem
import Linen.Control.Monad.Effect.HTTP
import Linen.Control.Monad.Effect.PostgreSQL
import Linen.Control.Monad.Effect.Trace
import Linen.Network.Socket.Blocking

open Data.OpenUnion
open Control.Monad.Effect
open Network.HTTP.Client (Response)
open Database.SQL.Connection (Connection acquire release)

namespace Examples.Effects

-- ── The filesystem capability ───────────────────────────────────────────────

section FileSystemCap
open Control.Monad.Effect.FileSystem

/-- Everything this demo touches on disk lives under one root. -/
abbrev sandboxRoot : Path := p!"/tmp/linen-effects-demo"

/-- What the application code may do: read and write, under `sandboxRoot`
    only. `sandboxed` withholds the delete bit, so `deleteFile` is not merely
    discouraged here — it does not elaborate. -/
abbrev appFilesCap : Capability := sandboxed sandboxRoot

/-- The clean-up capability: the same root, one more bit. Two capabilities over
    the same *arguments* differing in their *operations*. -/
abbrev janitorCap : Capability :=
  { canRead := true, canWrite := true, canDelete := true
  , scopes := [under sandboxRoot] }

/-- Two permission sets in one capability: read and write under `data/`,
    read-only under `config/`, nothing anywhere else. Each `Scope` carries its
    own operation list, so this is one capability rather than two. -/
abbrev tieredCap : Capability :=
  { canRead := true, canWrite := true
  , scopes := [ under p!"/tmp/linen-effects-demo/data"   [.read, .write]
              , under p!"/tmp/linen-effects-demo/config" [.read] ] }

abbrev reportPath : Path := p!"/tmp/linen-effects-demo/report.txt"
abbrev auditPath  : Path := p!"/tmp/linen-effects-demo/audit.txt"
abbrev dataPath   : Path := p!"/tmp/linen-effects-demo/data/out.txt"
abbrev configPath : Path := p!"/tmp/linen-effects-demo/config/app.conf"

-- The application capability cannot delete, and the bit is provably absent —
-- which is exactly why no `CanDelete appFilesCap` instance can be synthesised.
example : appFilesCap.canRead = true := rfl
example : appFilesCap.canWrite = true := rfl
example : appFilesCap.canDelete = false := rfl
example : janitorCap.canDelete = true := rfl

-- Neither capability can name a path outside the sandbox: the obligation
-- `permits op p = true` is unsatisfiable, so no proof of it can exist.
theorem no_passwd : appFilesCap.permits .read p!"/etc/passwd" ≠ true := by decide
theorem no_home :
    janitorCap.permits .delete p!"/Users/someone/.ssh/id_rsa" ≠ true := by decide

-- A string prefix is *not* containment: `/tmp/linen-effects-demo-evil` extends
-- the root as a string but is not under it, and the component-wise check says so.
theorem no_sibling :
    appFilesCap.permits .write p!"/tmp/linen-effects-demo-evil/x" ≠ true := by decide

-- The tiered capability's two scopes really do differ: `config/` is readable
-- and not writable, `data/` is both, and the bits alone cannot tell them apart
-- (`tieredCap.canWrite` is `true` — it is the *scope* that withholds the write).
#guard tieredCap.consistent
example : tieredCap.canWrite = true := rfl
example : tieredCap.permits .read configPath = true := by decide
example : tieredCap.permits .write dataPath = true := by decide
theorem no_write_config : tieredCap.permits .write configPath ≠ true := by decide
theorem no_touch_report : tieredCap.permits .read reportPath ≠ true := by decide

/-- Write a report through the capability. Its type names no path — the paths
    it may use are fixed by `appFilesCap`. -/
def writeReport (body : String) : Eff [FileSystem appFilesCap] Unit :=
  writeFileString reportPath body

/-- Read it back, as UTF-8 or `none`. -/
def readReport : Eff [FileSystem appFilesCap] (Option String) :=
  readFileString? reportPath

/-- Remove it — only expressible under the janitor capability. -/
def deleteReport : Eff [FileSystem janitorCap] Unit :=
  deleteFile reportPath

/-- A path only known at runtime cannot have its scope proved by `decide`;
    `ScopedPath.check?` validates it and hands back the evidence, so the
    operation still cannot be reached without it. -/
def checkedWrite (p : Path) (contents : String) :
    Option (Eff [FileSystem appFilesCap] Unit) :=
  (ScopedPath.check? appFilesCap .write p).map (fun sp => writeFileAt sp contents.toUTF8)

/-- Under the tiered capability the evidence is per *operation*, so the same
    path can validate for reading and fail for writing. -/
def tieredRead (p : Path) : Option (Eff [FileSystem tieredCap] (Option String)) :=
  (ScopedPath.check? tieredCap .read p).map
    (fun sp => String.fromUTF8? <$> readFileAt sp)

def tieredWrite (p : Path) (contents : String) :
    Option (Eff [FileSystem tieredCap] Unit) :=
  (ScopedPath.check? tieredCap .write p).map (fun sp => writeFileAt sp contents.toUTF8)

end FileSystemCap

-- ── The HTTP capability ─────────────────────────────────────────────────────

section HTTPCap
open Control.Monad.Effect.HTTP

/-- The demo server's port. Fixed rather than OS-assigned because the
    capability's scope has to be a literal for `decide` to reduce it — the
    price of checking the URL at elaboration time rather than at run time. -/
abbrev demoPort : UInt16 := 18099

abbrev apiRoot   : Url := u!"http://127.0.0.1:18099/v1"
abbrev healthUrl : Url := u!"http://127.0.0.1:18099/v1/health"
abbrev eventsUrl : Url := u!"http://127.0.0.1:18099/v1/events"

/-- Read anything under `/v1`, but `POST` only to `/v1/events`: a restriction on
    (method, argument) pairs, not on arguments alone. -/
abbrev webCap : Capability := restClient apiRoot eventsUrl

example : webCap.canGet = true := rfl
example : webCap.canPost = true := rfl
example : webCap.canDelete = false := rfl

-- `GET` is allowed anywhere under `/v1`; `POST` only at `/v1/events`.
example : webCap.permits .GET healthUrl = true := by decide
example : webCap.permits .POST eventsUrl = true := by decide
theorem no_post_to_health : webCap.permits .POST healthUrl ≠ true := by decide

-- A different host is a different host, however it is spelled: label-wise
-- comparison rejects the `evil.com` suffix trick that a string prefix admits.
theorem no_evil :
    webCap.permits .GET u!"http://127.0.0.1.evil.com:18099/v1/health" ≠ true := by decide

-- And a sibling path is not a child: `/v1-admin` is not under `/v1`.
theorem no_v1_admin :
    webCap.permits .GET u!"http://127.0.0.1:18099/v1-admin/keys" ≠ true := by decide

def fetchHealth : Eff [HTTP webCap] (Option String) := getString? healthUrl

def recordEvent (payload : String) : Eff [HTTP webCap] Response :=
  postString eventsUrl payload

/-- The runtime-validated counterpart of `get`, for a URL built at run time. -/
def checkedGet (u : Url) : Option (Eff [HTTP webCap] Response) :=
  (ScopedUrl.check? webCap .GET u).map (fun su => getAt su)

/-- Build a URL at run time from a list of path segments — the point being
    that this value is *not* a literal, so its scope cannot be settled by
    `decide` and has to go through `ScopedUrl.check?` instead. -/
def apiUrl (segments : List String) : Url :=
  { secure := false, host := ["127", "0", "0", "1"], port := demoPort
  , path := segments }

end HTTPCap

-- ── The PostgreSQL capability ───────────────────────────────────────────────

section PostgreSQLCap
open Control.Monad.Effect.PostgreSQL

abbrev pgContainer : String := "linen-effects-demo-pg"
-- Fully qualified so Podman needs no short-name resolution.
abbrev pgImage     : String := "docker.io/library/postgres:17-alpine"
abbrev pgPort      : UInt16 := 15433
abbrev pgDatabase  : String := "linen_demo"
abbrev pgUser      : String := "postgres"
abbrev pgPassword  : String := "linen-demo-pw"

abbrev orders  : Table := { name := "orders" }
abbrev apiKeys : Table := { name := "api_keys" }

/-- One database, one role, three statement kinds, one table. `appWriter`
    withholds `DELETE`, and `tables := [orders]` withholds every table but
    `orders` — including `api_keys`, which really does exist in the same
    database, as the demo checks with `psql`. -/
abbrev pgCap : Capability :=
  appWriter pgDatabase pgUser [orders] (port := pgPort)

example : pgCap.canSelect = true := rfl
example : pgCap.canInsert = true := rfl
example : pgCap.canUpdate = true := rfl
example : pgCap.canDelete = false := rfl

-- The connection target is not a proof obligation but a structural fact: the
-- handler builds its connection string from the capability and nothing else,
-- so a computation cannot name another database or another role.
#guard (pgCap.settings pgPassword).connString
  == "host=localhost port=15433 user=postgres password=linen-demo-pw dbname=linen_demo"

-- `api_keys` is out of scope, whichever statement kind is tried, and `DELETE`
-- is out of scope even on the table the capability does name.
theorem no_read_keys  : pgCap.permits (.select apiKeys [] none) ≠ true := by decide
theorem no_write_keys :
    pgCap.permits (.insert apiKeys ["k"] [.text "x"]) ≠ true := by decide
theorem no_delete_orders : pgCap.permits (.delete orders none) ≠ true := by decide

/-- Two inserts, one update, one select — the whole demo transaction, with no
    connection named anywhere in its type. -/
def ordersProgram : Eff [PostgreSQL pgCap] (Nat × Nat × ResultSet) := do
  let a ← insertInto orders ["id", "customer", "total"]
            [.text "1", .text "ada", .text "42.00"]
  let b ← insertInto orders ["id", "customer", "total"]
            [.text "2", .text "grace", .text "17.50"]
  let updated ← update orders [("total", .text "43.00")]
                  (some (.eq "customer" (.text "ada")))
  let rows ← select orders ["id", "customer", "total"]
  pure (a + b, updated, rows)

/-- The runtime-validated counterpart of `select`. `check?_eq` is what carries
    the statement kind across the validation, so this cannot smuggle a write
    past `CanSelect`. -/
def checkedSelect (q : Query) (hop : q.op = .select) :
    Option (Eff [PostgreSQL pgCap] ResultSet) :=
  match h : ScopedQuery.check? pgCap q with
  | none    => none
  | some sq => some (selectAt sq (by rw [ScopedQuery.check?_eq h]; exact hop))

/-- Render a result set for printing. -/
def showRows (rs : ResultSet) : String :=
  "; ".intercalate (rs.rows.toList.map fun r =>
    " | ".intercalate (r.toList.map (·.getD "NULL")))

end PostgreSQLCap

-- ── A loopback HTTP/1.1 server for the HTTP demo ────────────────────────────

section Server
open Network.Socket
open Network.Socket.Blocking

/-- The `Content-Length` announced by a raw header block, or `0`. -/
def contentLengthOf (head : String) : Nat :=
  match (head.splitOn "\r\n").find? (fun l => l.toLower.startsWith "content-length:") with
  | some l => ((l.splitOn ":").getD 1 "").trimAscii.copy.toNat?.getD 0
  | none   => 0

/-- Read one request: header block first, then as many body bytes as
    `Content-Length` announces. Good enough for this demo's ASCII payloads. -/
def readRequest (conn : Socket .connected) : IO (String × String) := do
  let mut buf := ByteArray.empty
  let mut done := false
  while !done do
    let chunk ← Blocking.recv conn
    if chunk.isEmpty then
      done := true
    else
      buf := buf ++ chunk
      let text := String.fromUTF8! buf
      match text.splitOn "\r\n\r\n" with
      | head :: rest =>
        if !rest.isEmpty && ("\r\n\r\n".intercalate rest).utf8ByteSize ≥ contentLengthOf head then
          done := true
      | [] => pure ()
  let text := String.fromUTF8! buf
  match text.splitOn "\r\n\r\n" with
  | head :: rest => return (head, "\r\n\r\n".intercalate rest)
  | []           => return (text, "")

/-- The method and path of a raw request-line-and-headers block. -/
def requestLine (head : String) : String × String :=
  let line := (head.splitOn "\r\n").getD 0 ""
  ((line.splitOn " ").getD 0 "GET", (line.splitOn " ").getD 1 "/")

/-- `/v1/health` answers `ok`, `/v1/events` acknowledges the body it received,
    anything else is a 404. -/
def respondTo (method path body : String) : String :=
  let reply (status body : String) :=
    s!"HTTP/1.1 {status}\r\nContent-Type: text/plain\r\nContent-Length: {body.utf8ByteSize}\r\nConnection: close\r\n\r\n{body}"
  if path == "/v1/health" then reply "200 OK" "ok"
  else if path == "/v1/events" && method == "POST" then reply "201 Created" s!"recorded {body.length} bytes"
  else reply "404 Not Found" "no such route"

/-- Serve exactly `n` sequential connections, returning the `method path` of
    each one, in order. -/
def serveRequests (server : Socket .listening) (n : Nat) : IO (List String) := do
  let mut seen : List String := []
  for _ in [0:n] do
    let (conn, _peer) ← Blocking.accept server
    let (head, body) ← readRequest conn
    let (method, path) := requestLine head
    Blocking.sendAll conn (respondTo method path body).toUTF8
    let _ ← close conn
    seen := s!"{method} {path}" :: seen
  pure seen.reverse

/-- Bind the demo port, run `act` while a background task serves `n` requests,
    and return the paths the server saw alongside `act`'s result. -/
def withDemoServer (n : Nat) (act : IO α) : IO (α × List String) := do
  let server ← listenTCP "127.0.0.1" demoPort
  let task ← IO.asTask (prio := .dedicated) (serveRequests server n)
  try
    let a ← act
    match task.get with
    | .ok seen  => pure (a, seen)
    | .error e  => throw e
  finally
    let _ ← close server

end Server

-- ── Podman-managed PostgreSQL ───────────────────────────────────────────────

section Podman
open Control.Monad.Effect.PostgreSQL

def podman (args : Array String) : IO IO.Process.Output :=
  IO.Process.output { cmd := "podman", args }

/-- Is a container runtime reachable? On macOS this also requires a started
    `podman machine`, which is why it is probed rather than assumed. -/
def podmanAvailable : IO Bool := do
  try
    return (← podman #["info", "--format", "{{.Version.Version}}"]).exitCode == 0
  catch _ => return false

/-- Run one statement inside the container with `psql`. Used for the DDL the
    effect deliberately cannot express, and to prove `api_keys` really exists. -/
def psql (sql : String) : IO IO.Process.Output :=
  podman #["exec", pgContainer, "psql", "-U", pgUser, "-d", pgDatabase,
           "-v", "ON_ERROR_STOP=1", "-tAc", sql]

/-- Start a disposable container, replacing any leftover from a previous run. -/
def startPostgres : IO Unit := do
  let _ ← podman #["rm", "-f", pgContainer]
  let out ← podman #["run", "-d", "--rm", "--name", pgContainer,
    "-e", s!"POSTGRES_PASSWORD={pgPassword}",
    "-e", s!"POSTGRES_DB={pgDatabase}",
    "-p", s!"{pgPort}:5432", pgImage]
  if out.exitCode != 0 then
    throw (IO.userError s!"podman run failed: {out.stderr.trimAscii.copy}")

/-- Poll `pg_isready` until the server accepts connections, or give up. -/
def waitForPostgres (attempts : Nat := 60) : IO Bool := do
  let mut ready := false
  for _ in [0:attempts] do
    unless ready do
      let out ← podman #["exec", pgContainer, "pg_isready", "-U", pgUser, "-d", pgDatabase]
      if out.exitCode == 0 then ready := true else IO.sleep 500
  return ready

/-- Create both tables. `CREATE TABLE` is not in the `Query` AST — there is no
    DDL constructor and no `rawSql` escape hatch — so schema setup is the
    host's job, outside the capability, by construction. -/
def createSchema : IO Unit := do
  let out ← psql
    "CREATE TABLE orders (id int primary key, customer text, total numeric); \
     CREATE TABLE api_keys (id int primary key, secret text); \
     INSERT INTO api_keys VALUES (1, 'super-secret');"
  if out.exitCode != 0 then
    throw (IO.userError s!"schema setup failed: {out.stderr.trimAscii.copy}")

def stopPostgres : IO Unit := do
  let _ ← podman #["rm", "-f", pgContainer]
  pure ()

end Podman

-- ── Demo 1: the filesystem effect over real files ───────────────────────────

section DemoFS
open Control.Monad.Effect.FileSystem

def demoFileSystem : IO Bool := do
  IO.println "── Effect.FileSystem: a capability rooted at a scratch directory ──"
  -- Directory creation is not part of the effect's vocabulary at all, so it
  -- happens here, outside the capability.
  IO.FS.createDirAll sandboxRoot.toFilePath

  runFileSystem appFilesCap (writeReport "capabilities are values\n")
  let readBack ← runFileSystem appFilesCap readReport
  let onDisk ← IO.FS.readFile reportPath.toFilePath
  IO.println s!"  wrote and read back: {repr (readBack.getD "")}"
  IO.println s!"  the same bytes are really on disk: {readBack == some onDisk}"

  -- `deleteFile` is not expressible under `appFilesCap`; it is under the
  -- janitor capability, over the very same root.
  runFileSystem janitorCap deleteReport
  let gone := !(← System.FilePath.pathExists reportPath.toFilePath)
  IO.println s!"  deleted through janitorCap, file gone: {gone}"
  IO.println "  (appFilesCap.canDelete = false, so `deleteFile` there is a compile error)"

  -- One capability, two permission sets: read+write under `data/`, read-only
  -- under `config/`. The config file is seeded outside the effect, since the
  -- capability that reads it is not allowed to create it.
  IO.FS.createDirAll (sandboxRoot ++ ["data"] : Path).toFilePath
  IO.FS.createDirAll (sandboxRoot ++ ["config"] : Path).toFilePath
  IO.FS.writeFile configPath.toFilePath "mode = demo\n"
  runFileSystem tieredCap (writeFileString dataPath "written under data/\n")
  let conf ← runFileSystem tieredCap (readFileString? configPath)
  let dat ← runFileSystem tieredCap (readFileString? dataPath)
  IO.println s!"  tieredCap read config/: {repr (conf.getD "")} and wrote data/: {repr (dat.getD "")}"
  IO.println s!"  ScopedPath.check? on config/: read={(tieredRead configPath).isSome} \
write={(tieredWrite configPath "nope").isSome}"
  IO.println "  (tieredCap.canWrite = true, yet `writeFileString configPath` is a compile error)"
  IO.FS.removeFile configPath.toFilePath
  IO.FS.removeFile dataPath.toFilePath

  pure (readBack == some "capabilities are values\n" && readBack == some onDisk && gone
        && conf == some "mode = demo\n" && dat == some "written under data/\n"
        && (tieredRead configPath).isSome && !(tieredWrite configPath "nope").isSome)

end DemoFS

-- ── Demo 2: the HTTP effect over a real loopback socket ─────────────────────

section DemoHTTP
open Control.Monad.Effect.HTTP

def demoHTTP : IO Bool := do
  IO.println "── Effect.HTTP: GET anywhere under /v1, POST only to /v1/events ──"
  let ((health, event), seen) ← withDemoServer 2 do
    let health ← runHTTP webCap fetchHealth
    let event ← runHTTP webCap (recordEvent "{\"kind\":\"demo\"}")
    pure (health, event)
  IO.println s!"  GET  /v1/health -> {repr (health.getD "")}"
  IO.println s!"  POST /v1/events -> {event.statusCode.statusCode} {String.fromUTF8! event.body}"
  IO.println s!"  server saw, in order: {seen}"
  IO.println "  (POST /v1/health and GET //127.0.0.1.evil.com/ are compile errors)"
  pure (health == some "ok" && event.statusCode.statusCode == 201
        && seen == ["GET /v1/health", "POST /v1/events"])

end DemoHTTP

-- ── Demo 3: the PostgreSQL effect against a container ───────────────────────

section DemoPG
open Control.Monad.Effect.PostgreSQL

def demoPostgreSQL : IO Bool := do
  IO.println "── Effect.PostgreSQL: one database, one role, one table ──"

  -- The statements the program would send, without connecting to anything.
  IO.println "  dry run — the SQL this program is authorised to send:"
  for (sql, params) in renderedBy ordersProgram do
    IO.println s!"    {sql}  {params}"

  match ← runPostgreSQL pgCap pgPassword ordersProgram with
  | .error e =>
    IO.eprintln s!"  session failed: {e}"
    pure false
  | .ok (inserted, updated, rows) =>
    IO.println s!"  inserted {inserted} rows, updated {updated}"
    IO.println s!"  columns: {rows.columns}"
    IO.println s!"  rows: {showRows rows}"

    -- `api_keys` exists in this very database, and holds a row — the capability
    -- is what makes it unreachable, not the schema.
    let keys ← psql "SELECT count(*) FROM api_keys;"
    IO.println s!"  api_keys really exists ({keys.stdout.trimAscii.copy} row) and is still \
unreachable: no proof of pgCap.permits (select api_keys) can exist"

    let adaTotal := rows.rows.findSome? fun r =>
      if r[1]? == some (some "ada") then r[2]? else none
    IO.println s!"  ada's total after the UPDATE: {repr adaTotal}"
    pure (inserted == 2 && updated == 1 && rows.rows.size == 2
          && adaTotal == some (some "43.00") && keys.stdout.trimAscii.copy == "1")

end DemoPG

-- ── Demo 4: the three runtime `check?` functions ────────────────────────────

section DemoRuntime
open Control.Monad.Effect.FileSystem (runFileSystem readFileString?)
open Control.Monad.Effect.PostgreSQL (Query Op)

def demoRuntimeChecks : IO Bool := do
  IO.println "── check? — paths, URLs and queries only known at run time ──"

  -- A path assembled at run time: inside the sandbox it validates, outside it
  -- does not, and only the validated one can reach `writeFileAt`.
  let inside := sandboxRoot ++ ["runtime.txt"]
  let outside := ["etc", "passwd"]
  let okWrite := checkedWrite inside "written through a ScopedPath\n"
  let noWrite := checkedWrite outside "should never happen\n"
  match okWrite with
  | none     => IO.eprintln "  runtime path inside the sandbox was rejected"; pure ()
  | some act => runFileSystem appFilesCap act
  let wrote ← runFileSystem appFilesCap
    (readFileString? (sandboxRoot ++ ["runtime.txt"])
      (by decide : appFilesCap.permits .read (sandboxRoot ++ ["runtime.txt"]) = true))
  IO.println s!"  ScopedPath.check? inside={okWrite.isSome} outside={noWrite.isSome}, \
read back {repr (wrote.getD "")}"

  -- A URL assembled at run time, checked against the same capability.
  -- `/v1-admin` is a string-prefix extension of `/v1` but is not a path
  -- *under* it, and the segment-wise check is what tells them apart.
  let okUrl := checkedGet (apiUrl ["v1", "health"])
  let noUrl := checkedGet (apiUrl ["v1-admin", "keys"])
  let healthAgain ← match okUrl with
    | none     => pure none
    | some act => do
      let (resp, _) ← withDemoServer 1 (Control.Monad.Effect.HTTP.runHTTP webCap act)
      pure (String.fromUTF8? resp.body)
  IO.println s!"  ScopedUrl.check? /v1/health={okUrl.isSome} /v1-admin/keys={noUrl.isSome}, \
GET returned {repr (healthAgain.getD "")}"

  -- A query assembled at run time: the table is in scope or it is not.
  let okQuery := checkedSelect (Query.select orders ["id"] none) rfl
  let noQuery := checkedSelect (Query.select apiKeys ["secret"] none) rfl
  IO.println s!"  ScopedQuery.check? orders={okQuery.isSome} api_keys={noQuery.isSome}"

  IO.FS.removeFile (sandboxRoot ++ ["runtime.txt"] : Control.Monad.Effect.FileSystem.Path).toFilePath
  pure (okWrite.isSome && !noWrite.isSome
        && wrote == some "written through a ScopedPath\n"
        && okUrl.isSome && !noUrl.isSome && healthAgain == some "ok"
        && okQuery.isSome && !noQuery.isSome)

end DemoRuntime

-- ── Demo 5: one computation over all four effects ───────────────────────────

section DemoCombined

/-- The row *is* the whitelist, and each entry carries its own capability: this
    computation may read and write files under one directory, talk to one
    origin, run three statement kinds against one table, and trace. Nothing
    else — there is no `IO` in the row. -/
abbrev demoRow : List (Type → Type) :=
  [ FileSystem.FileSystem appFilesCap
  , HTTP.HTTP webCap
  , PostgreSQL.PostgreSQL pgCap
  , Trace.Trace ]

/-- Check the service, file the result, record it in the database, read the
    file back — one program, four effects, no `IO`. -/
def auditProgram : Eff demoRow (Option String) := do
  Trace.trace "    [trace] GET /v1/health"
  let health ← HTTP.getString? healthUrl
  let body := health.getD "<not utf-8>"
  Trace.trace s!"    [trace] service says {repr body}"
  FileSystem.writeFileString auditPath s!"health={body}\n"
  Trace.trace "    [trace] wrote the audit file"
  let _ ← PostgreSQL.insertInto orders ["id", "customer", "total"]
            [.text "3", .text "audit", .text "0.00"]
  Trace.trace "    [trace] recorded the audit row"
  FileSystem.readFileString? auditPath

/-- Answer one request of the row by handing it to that effect's own
    interpreter. Nothing is re-checked here: a request that exists carries the
    proofs that authorised it. -/
def performOne (conn : Connection) {β : Type} : Union demoRow β → IO β
  | .here e =>
      FileSystem.runFileSystem appFilesCap (.impure (.here e) .protect)
  | .there (.here e) =>
      HTTP.runHTTP webCap (.impure (.here e) .protect)
  | .there (.there (.here e)) => do
      match ← Database.SQL.Session.Session.run
               (PostgreSQL.runSession pgCap (.impure (.here e) .protect)) conn with
      | .ok b     => pure b
      | .error err => throw (IO.userError s!"postgres: {err}")
  | .there (.there (.there (.here e))) =>
      Trace.runTrace (.impure (.here e) .protect)
  | .there (.there (.there (.there u))) => u.elim0

/-- Run the whole row in `IO`: the same shape as `interpretM`, one level up.
    Structurally recursive on the request tree — no `partial`, no fuel. -/
def runRow (conn : Connection) {α : Type} : Eff demoRow α → IO α
  | .protect a  => pure a
  | .impure u k => performOne conn u >>= fun b => runRow conn (k b)

def demoCombined : IO Bool := do
  IO.println "── one Eff over [FileSystem, HTTP, PostgreSQL, Trace] ──"
  match ← acquire (pgCap.settings pgPassword) with
  | .error e => IO.eprintln s!"  could not connect: {e}"; pure false
  | .ok conn =>
    try
      let (readBack, seen) ← withDemoServer 1 (runRow conn auditProgram)
      IO.println s!"  audit file contains: {repr (readBack.getD "")}"
      IO.println s!"  server saw: {seen}"
      let rows ← runPostgreSQLCount conn
      IO.println s!"  orders now holds {rows} rows (2 from the earlier demo + 1 audit)"
      IO.FS.removeFile auditPath.toFilePath
      pure (readBack == some "health=ok\n" && seen == ["GET /v1/health"] && rows == 3)
    finally
      release conn
where
  /-- Count `orders` through the capability, on the connection already open. -/
  runPostgreSQLCount (conn : Connection) : IO Nat := do
    match ← Database.SQL.Session.Session.run
             (PostgreSQL.runSession pgCap
               (PostgreSQL.select orders ["id"])) conn with
    | .ok rs     => pure rs.rows.size
    | .error err => throw (IO.userError s!"postgres: {err}")

end DemoCombined

-- ── Entry point ─────────────────────────────────────────────────────────────

def run (args : List String) : IO Unit := do
  let withDb := !args.contains "no-db"

  let okFs ← demoFileSystem
  IO.println ""
  let okHttp ← demoHTTP
  IO.println ""

  let dbResult ←
    if !withDb then
      IO.println "── Effect.PostgreSQL / combined: skipped (`no-db`) ──"
      pure none
    else if !(← podmanAvailable) then
      IO.println "── Effect.PostgreSQL / combined: skipped (no reachable container runtime) ──"
      IO.println "   start podman (`podman machine start` on macOS) and re-run, or pass \
`no-db` to skip these on purpose"
      pure none
    else do
      IO.println s!"── starting {pgImage} as {pgContainer} on port {pgPort} ──"
      startPostgres
      try
        if !(← waitForPostgres) then
          throw (IO.userError "the container never became ready")
        createSchema
        IO.println "  container ready, schema created with psql (no DDL exists in the effect)"
        IO.println ""
        let okPg ← demoPostgreSQL
        IO.println ""
        let okCombined ← demoCombined
        pure (some (okPg && okCombined))
      finally
        stopPostgres

  IO.println ""
  let okRuntime ← demoRuntimeChecks
  IO.println ""

  let allOk := okFs && okHttp && okRuntime && dbResult.getD true
  let scope := if dbResult.isSome then "all checks passed"
               else "filesystem/HTTP/runtime checks passed · database section skipped"
  if allOk then
    IO.println s!"effects demo done · {scope}"
  else
    throw (IO.userError "effects demo done · some checks failed")

end Examples.Effects
