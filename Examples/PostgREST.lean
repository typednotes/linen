/-
  Examples.PostgREST — the `Linen.PostgREST.*` port end-to-end.

  Wires together the pieces ported from the `postgrest` Haskell package into a
  tiny in-memory API server:

  * an `AppState` (`IO.Ref`-backed schema cache + metrics + observer)
    seeded with a hand-built `SchemaCache` describing one `users` table
    (no socket, no database — the default, self-checking mode);
  * `App.handleRequest` dispatching a handful of simulated HTTP requests
    (root table listing, CORS preflight, table `GET`/`POST`, an unknown
    table, an unsupported method) through the same code path a real WAI
    `Application` would use;
  * `Response.OpenAPI.generateOpenAPISpec` rendering the schema cache as an
    OpenAPI 3.0 document;
  * `CLI.parseArgs` showing how a command line maps to a `Command`;
  * (`live` mode only) `Database.SQL.Connection`/`Session` connecting to a
    real PostgreSQL instance and `SchemaCache.tablesSql`/`columnsSql`
    introspecting its `public` schema for real, the same catalog queries
    PostgREST itself runs at startup.

  Args:
    (none)         -- self-checking demo: runs the requests above against an
                      in-memory schema, verifies the expected status codes,
                      prints the OpenAPI spec, exits
    spec           -- print just the generated OpenAPI spec for the demo schema
    live [connstr] -- connect to a real Postgres (default: a local instance
                      matching `docker run --rm -e POSTGRES_PASSWORD=postgres
                      -p 5432:5432 postgres`), introspect its `public` schema,
                      and serve a couple of requests against the real tables
-/
import Linen.PostgREST.App
import Linen.PostgREST.Response.OpenAPI
import Linen.PostgREST.CLI
import Linen.Database.SQL.Connection
import Linen.Database.SQL.Session

namespace Examples.PostgREST

open PostgREST.AppState
open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers
open PostgREST.App
open Database.SQL.Connection (Settings Connection ConnectionError)
open Database.SQL.Session (Session SessionError)
open Database.SQL.Session.Session (query run getRawConnection)

-- ── Demo schema: one `public.users` table ──

/-- The `public.users` table's qualified name, shared by its `Table` and
every `Column` below so they agree on which table they belong to. -/
def usersQi : QualifiedIdentifier := { qiSchema := "public", qiName := "users" }

/-- A hand-built `public.users` table: `id` (int4, primary key), `name`
(text, not null), and `email` (text, nullable). -/
def usersTable : Table :=
  { tableSchema := "public"
    tableName := "users"
    tableColumns :=
      #[ { colTable := usersQi, colName := "id", colNullable := false, colType := "int4"
           , colIsPrimaryKey := true }
       , { colTable := usersQi, colName := "name", colNullable := false, colType := "text" }
       , { colTable := usersQi, colName := "email", colNullable := true, colType := "text" } ]
    tablePrimaryKey :=
      #[ { colTable := usersQi, colName := "id", colNullable := false, colType := "int4"
           , colIsPrimaryKey := true } ] }

/-- The demo `SchemaCache`, containing only `usersTable` — everything the
in-memory demo and `spec` mode need, with no database involved. -/
def demoSchemaCache : SchemaCache :=
  { SchemaCache.empty with dbTables := [(usersQi, usersTable)] }

-- ── One simulated request, printed and checked against an expected status ──

/-- Send one request through `handleRequest`, print a summary line, and report
whether the response matched `expectedStatus`. -/
def tryRequest (st : AppState) (label : String) (req : SimpleRequest) (expectedStatus : Nat)
    : IO Bool := do
  let resp ← handleRequest st req
  let mark := if resp.status == expectedStatus then "OK" else "MISMATCH"
  IO.println s!"  {label}: {req.method} {req.path} -> {resp.status}  [{mark}]"
  pure (resp.status == expectedStatus)

/-- Self-contained demo: seed a schema cache, fire a handful of requests
through `handleRequest`, verify their status codes, then print the OpenAPI
spec and a `CLI.parseArgs` example. -/
def runDemo : IO Unit := do
  let st ← AppState.create (fun _ => pure ())
  st.putSchemaCache demoSchemaCache
  printBanner "127.0.0.1" 3000
  IO.println ""

  let checks : List (String × SimpleRequest × Nat) :=
    [ ("root table listing", { method := "GET", path := "/" }, 200)
    , ("CORS preflight", { method := "OPTIONS", path := "/", headers := [("Origin", "http://example.com")] }, 204)
    , ("read users", { method := "GET", path := "/users" }, 200)
    , ("insert into users", { method := "POST", path := "/users" }, 201)
    , ("unknown table", { method := "GET", path := "/ghosts" }, 404)
    , ("unsupported method", { method := "TRACE", path := "/users" }, 405)
    , ("unimplemented RPC", { method := "POST", path := "/rpc/greet" }, 501) ]

  let mut ok := 0
  for (label, req, expected) in checks do
    if ← tryRequest st label req expected then ok := ok + 1
  let total := checks.length

  let m ← st.stateMetrics.get
  IO.println s!"\nmetrics: {m.requestCount} requests recorded"

  IO.println "\nOpenAPI spec for the demo schema:"
  IO.println (PostgREST.Response.OpenAPI.generateOpenAPISpec demoSchemaCache)

  IO.println "\nCLI.parseArgs examples:"
  for cliArgs in [[], ["--version"], ["config.conf"], ["--bogus"]] do
    IO.println s!"  {cliArgs} -> {repr (PostgREST.CLI.parseArgs cliArgs)}"

  IO.println s!"\npostgrest demo done · {ok}/{total} requests matched their expected status"
  if ok != total then throw (IO.userError "some requests did not match their expected status")

-- ────────────────────────────────────────────────────────────────────
-- Live mode: introspect a real PostgreSQL instance
-- ────────────────────────────────────────────────────────────────────

/-- The connection string matching a plain local Postgres container:
    `docker run --rm -e POSTGRES_PASSWORD=postgres -p 5432:5432 postgres`. -/
def defaultLiveConnString : String :=
  "host=localhost port=5432 user=postgres password=postgres dbname=postgres"

/-- Run `queryStr` and materialize every cell as `some value` or `none` (SQL
    NULL), using the same `libpq` primitives `Session.query`'s result exposes. -/
def fetchRows (queryStr : String) : Session (Array (Array (Option String))) := do
  let result ← query queryStr #[]
  let nRows ← Database.PostgreSQL.LibPQ.ntuples result
  let nCols ← Database.PostgreSQL.LibPQ.nfields result
  let mut rows : Array (Array (Option String)) := #[]
  for r in [0:nRows.toNat] do
    let mut row : Array (Option String) := #[]
    for c in [0:nCols.toNat] do
      if ← Database.PostgreSQL.LibPQ.getIsNull result r.toUInt32 c.toUInt32 then
        row := row.push none
      else
        row := row.push (some (← Database.PostgreSQL.LibPQ.getvalue result r.toUInt32 c.toUInt32))
    rows := rows.push row
  return rows

/-- `libpq` renders booleans in text format as `"t"`/`"f"`. -/
def parsePgBool (cell : Option String) : Bool := cell == some "t"

/-- Introspect the `public` schema of a live connection, mirroring
    `App.handleRequest`'s expectations: one row per table from `tablesSql`,
    one row per column from `columnsSql`, joined by `(schema, table)`. -/
def introspectSchemaCache : Session SchemaCache := do
  let tableRows ← fetchRows (SchemaCache.tablesSql ["public"])
  let columnRows ← fetchRows (SchemaCache.columnsSql ["public"])

  let baseTables := tableRows.map fun row =>
    let schema := row[0]!.getD "public"
    let name := row[1]!.getD ""
    let qi : QualifiedIdentifier := { qiSchema := schema, qiName := name }
    (qi, schema, name, row[2]!, parsePgBool row[3]!, parsePgBool row[4]!,
      parsePgBool row[5]!, parsePgBool row[6]!)

  let columnsFor (schema name : String) : Array Column :=
    columnRows.filterMap fun row =>
      if row[0]!.getD "" == schema && row[1]!.getD "" == name then
        some
          { colTable := { qiSchema := schema, qiName := name }
            colName := row[2]!.getD ""
            colDescription := row[3]!
            colNullable := parsePgBool row[4]!
            colType := row[5]!.getD ""
            colMaxLen := row[6]!.bind (·.toNat?)
            colDefault := row[7]!
            colIsPrimaryKey := parsePgBool row[9]! }
      else none

  let dbTables := baseTables.toList.map fun (qi, schema, name, desc, insertable, updatable, deletable, isView) =>
    let cols := columnsFor schema name
    let table : Table :=
      { tableSchema := schema
        tableName := name
        tableDescription := desc
        tableInsertable := insertable
        tableUpdatable := updatable
        tableDeletable := deletable
        tableIsView := isView
        tableColumns := cols
        tablePrimaryKey := cols.filter (·.colIsPrimaryKey)
        -- `tablePrimaryKey` is filtered from `cols`, so it's a subset by
        -- construction; `grind` closes the existential this unfolds to.
        pk_subset := by intro c hc; grind }
    (qi, table)

  return { SchemaCache.empty with dbTables := dbTables }

/-- Print a friendly connection-failure hint instead of a raw stack trace,
    and exit non-zero. -/
def reportConnectFailure (detail : String) : IO Unit := do
  IO.eprintln s!"could not connect: {detail}"
  IO.eprintln ""
  IO.eprintln "start a local Postgres and try again, e.g.:"
  IO.eprintln "  docker run --rm -e POSTGRES_PASSWORD=postgres -p 5432:5432 postgres"
  IO.Process.exit 1

/-- Connect to `connString`, introspect its `public` schema, serve a couple of
    requests against it, and print the resulting OpenAPI spec. Prints a
    friendly hint (rather than crashing) if no server is reachable, since this
    mode depends on external infrastructure the self-checking demo doesn't.

    A rejected connection surfaces through `withConnection`'s documented
    `Except.error (.cantConnect _)`, handled below; the surrounding
    `try`/`catch` is defense-in-depth for anything else unexpected (e.g. the
    connection dropping mid-session). -/
def runLive (connString : String) : IO Unit := do
  IO.println s!"connecting to postgres · {connString}"
  (← IO.getStdout).flush
  -- `Settings.uri` requires a non-empty proof; `connString` is a runtime
  -- value (a CLI arg or the literal default), so the proof is checked here
  -- rather than relying on `Settings.uri`'s `by decide` default, which only
  -- discharges for compile-time literals (see PgError.pgCode_len upstream).
  if h : connString.length > 0 then
    let settings := Settings.uri connString h
    try
      let outcome ← Database.SQL.Connection.withConnection settings fun conn => do
        let sessionResult ← run introspectSchemaCache conn
        match sessionResult with
        | .error e => throw (IO.userError s!"schema introspection failed: {e}")
        | .ok sc => pure sc
      match outcome with
      | .error e => reportConnectFailure (toString e)
      | .ok sc =>
        IO.println s!"discovered {sc.dbTables.length} table(s) in schema 'public':"
        for (qi, table) in sc.dbTables do
          IO.println s!"  {qi} ({table.tableColumns.size} columns, {table.pkColumnNames})"

        let st ← AppState.create (fun _ => pure ())
        st.putSchemaCache sc
        IO.println ""

        let rootResp ← handleRequest st { method := "GET", path := "/" }
        IO.println s!"  GET / -> {rootResp.status}  body={rootResp.body}"
        match sc.dbTables.head? with
        | some (qi, _) =>
          let tableResp ← handleRequest st { method := "GET", path := s!"/{qi.qiName}" }
          IO.println s!"  GET /{qi.qiName} -> {tableResp.status}"
        | none => pure ()

        IO.println "\nOpenAPI spec for the live schema:"
        IO.println (PostgREST.Response.OpenAPI.generateOpenAPISpec sc)
    catch e =>
      reportConnectFailure (toString e)
  else
    IO.eprintln "connection string must not be empty"
    IO.Process.exit 1

def run (args : List String) : IO Unit := do
  match args with
  | ["spec"] => IO.println (PostgREST.Response.OpenAPI.generateOpenAPISpec demoSchemaCache)
  | ["live"] => runLive defaultLiveConnString
  | ["live", connString] => runLive connString
  | _ => runDemo

end Examples.PostgREST
