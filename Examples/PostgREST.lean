/-
  Examples.PostgREST — the `Linen.PostgREST.*` port end-to-end.

  Wires together the pieces ported from the `postgrest` Haskell package into a
  tiny in-memory API server, without a real socket or database connection:

  * an `AppState` (`IO.Ref`-backed schema cache + metrics + observer)
    seeded with a hand-built `SchemaCache` describing one `users` table;
  * `App.handleRequest` dispatching a handful of simulated HTTP requests
    (root table listing, CORS preflight, table `GET`/`POST`, an unknown
    table, an unsupported method) through the same code path a real WAI
    `Application` would use;
  * `Response.OpenAPI.generateOpenAPISpec` rendering the schema cache as an
    OpenAPI 3.0 document;
  * `CLI.parseArgs` showing how a command line maps to a `Command`.

  Args:
    (none)  -- self-checking demo: runs the requests above, verifies the
               expected status codes, prints the OpenAPI spec, exits
    spec    -- print just the generated OpenAPI spec for the demo schema
-/
import Linen.PostgREST.App
import Linen.PostgREST.Response.OpenAPI
import Linen.PostgREST.CLI

namespace Examples.PostgREST

open PostgREST.AppState
open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers
open PostgREST.App

-- ── Demo schema: one `public.users` table ──

def usersQi : QualifiedIdentifier := { qiSchema := "public", qiName := "users" }

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

def run (args : List String) : IO Unit := do
  match args with
  | ["spec"] => IO.println (PostgREST.Response.OpenAPI.generateOpenAPISpec demoSchemaCache)
  | _ => runDemo

end Examples.PostgREST
