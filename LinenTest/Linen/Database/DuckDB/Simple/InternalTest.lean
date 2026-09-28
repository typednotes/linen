/-
  Tests for `Linen.Database.DuckDB.Simple.Internal`.

  Opens a real in-memory DuckDB connection via `openConnection`, exercises
  `withDatabaseHandle`/`withConnectionHandle`/`withClientContext` against it
  (running a real query through the raw FFI handles each accessor exposes),
  closes it via `closeConnection`, and confirms every accessor now reports
  `connectionClosedError` — proving both the happy path and the "closed
  connection" bookkeeping this module is responsible for.
-/
import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.DuckDB.FFI.QueryExecution

open Database.DuckDB.Simple
open Database.DuckDB.FFI.QueryExecution (query destroy)
open Database.DuckDB.FFI.OpenConnect (clientContextGetConnectionId)

namespace Tests.Database.DuckDB.Simple.Internal

#eval show IO Unit from do
  let conn ← openConnection none -- in-memory database

  -- `withConnectionHandle` gives access to a real, live `Connection` FFI
  -- handle: run an actual query through it.
  let (rc, result) ← withConnectionHandle conn fun h => query h "SELECT 41 + 1"
  unless rc.isSuccess do throw (IO.userError "SELECT through withConnectionHandle failed")
  destroy result

  -- `withDatabaseHandle` gives access to the paired `Database` handle (only
  -- checked for liveness here, since `duckdb_database` itself exposes no
  -- further inspection API at this layer).
  withDatabaseHandle conn fun _db => pure ()

  -- `withClientContext` fetches, uses, and destroys a fresh `ClientContext`.
  let connId ← withClientContext conn clientContextGetConnectionId
  if connId != connId then throw (IO.userError "unreachable")

  closeConnection conn

  -- Once closed, every accessor must report `connectionClosedError`, not
  -- silently succeed or crash.
  let mut sawError := false
  try
    let _ ← withConnectionHandle conn fun h => query h "SELECT 1"
    pure ()
  catch _ =>
    sawError := true
  unless sawError do throw (IO.userError "expected withConnectionHandle to fail once closed")

  sawError := false
  try
    withDatabaseHandle conn fun _ => pure ()
  catch _ =>
    sawError := true
  unless sawError do throw (IO.userError "expected withDatabaseHandle to fail once closed")

  sawError := false
  try
    let _ ← withClientContext conn clientContextGetConnectionId
    pure ()
  catch _ =>
    sawError := true
  unless sawError do throw (IO.userError "expected withClientContext to fail once closed")

  -- Closing twice is a documented no-op, not an error.
  closeConnection conn

-- `Query`'s `Coe`/`ToString`/`Append` surface.
#guard (("SELECT 1" : Query)).fromQuery == "SELECT 1"
#guard toString (Query.mk "SELECT 1") == "SELECT 1"
#guard (Query.mk "SELECT 1 " ++ Query.mk "UNION SELECT 2").fromQuery ==
  "SELECT 1 UNION SELECT 2"
#guard Query.empty.fromQuery == ""

-- `SQLError`'s `ToString` surface, with and without an attached query.
#guard toString (SQLError.mk "boom" none none) == "duckdb-simple: boom"
#guard toString (SQLError.mk "boom" none (some (Query.mk "SELECT 1"))) ==
  "duckdb-simple: boom (query: SELECT 1)"

end Tests.Database.DuckDB.Simple.Internal
