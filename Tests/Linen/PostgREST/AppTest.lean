/-
  Tests for `Linen.PostgREST.App`.

  `handleRequest`/`printBanner` are IO-effectful (they read `AppState`'s
  `IO.Ref`s and the monotonic clock, or write to stderr), so they are
  exercised with `#eval show IO Unit from do ...` — a thrown error fails
  the build.
-/
import Linen.PostgREST.App

open PostgREST.App
open PostgREST.AppState
open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.App

def usersQi : QualifiedIdentifier := { qiSchema := "public", qiName := "users" }
def usersTable : Table := { tableSchema := "public", tableName := "users" }

#eval show IO Unit from do
  let st ← AppState.create (fun _ => pure ())
  st.putSchemaCache { SchemaCache.empty with dbTables := [(usersQi, usersTable)] }

  -- root listing
  let rootResp ← handleRequest st { method := "GET", path := "/" }
  unless rootResp.status == 200 do
    throw (IO.userError s!"expected GET / to report 200, got {rootResp.status}")
  unless rootResp.body == "[\"users\"]" do
    throw (IO.userError s!"expected GET / to list tables, got {rootResp.body}")

  -- root CORS preflight
  let optionsResp ← handleRequest st
    { method := "OPTIONS", path := "/", headers := [("Origin", "http://example.com")] }
  unless optionsResp.status == 204 do
    throw (IO.userError s!"expected OPTIONS / to report 204, got {optionsResp.status}")

  -- known table, GET
  let getResp ← handleRequest st { method := "GET", path := "/users" }
  unless getResp.status == 200 do
    throw (IO.userError s!"expected GET /users to report 200, got {getResp.status}")
  unless getResp.headers.any (·.1 == "Content-Range") do
    throw (IO.userError "expected GET /users to include a Content-Range header")

  -- known table, POST
  let postResp ← handleRequest st { method := "POST", path := "/users" }
  unless postResp.status == 201 do
    throw (IO.userError s!"expected POST /users to report 201, got {postResp.status}")

  -- unknown table
  let missingResp ← handleRequest st { method := "GET", path := "/ghosts" }
  unless missingResp.status == 404 do
    throw (IO.userError s!"expected GET /ghosts to report 404, got {missingResp.status}")

  -- unsupported method on a known table
  let unsupportedResp ← handleRequest st { method := "TRACE", path := "/users" }
  unless unsupportedResp.status == 405 do
    throw (IO.userError s!"expected TRACE /users to report 405, got {unsupportedResp.status}")

  -- unimplemented RPC
  let rpcResp ← handleRequest st { method := "POST", path := "/rpc/do_thing" }
  unless rpcResp.status == 501 do
    throw (IO.userError s!"expected POST /rpc/do_thing to report 501, got {rpcResp.status}")

  -- request completion is observed and counted
  let m ← st.stateMetrics.get
  unless m.requestCount == 7 do
    throw (IO.userError s!"expected 7 recorded requests, got {m.requestCount}")

  printBanner "localhost" 3000

end Tests.PostgREST.App
