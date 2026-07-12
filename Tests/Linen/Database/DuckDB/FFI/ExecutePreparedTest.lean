/-
  Tests for `Linen.Database.DuckDB.FFI.ExecutePrepared`.

  Prepares a real `INSERT`/`SELECT` against a real table (via
  `Tests.Linen.Database.DuckDB.FFI.TestSupport.prepareForTest`), binds a
  parameter with `Database.DuckDB.FFI.BindValues`, and checks that
  `execute` both reports success and produces the expected row count (via
  this batch's test-only `resultRowCount`, backed by `duckdb_row_count`) —
  not just a `State.success` code.
-/
import Linen.Database.DuckDB.FFI.BindValues
import Linen.Database.DuckDB.FFI.ExecutePrepared
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.BindValues
open Database.DuckDB.FFI.ExecutePrepared
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types
open Tests.Database.DuckDB.FFI.TestSupport

namespace Tests.Database.DuckDB.FFI.ExecutePrepared

#eval show IO Unit from do
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")
  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  let createState ← queryExec conn "CREATE TABLE ep(x INTEGER)"
  unless createState.isSuccess do throw (IO.userError "CREATE TABLE ep failed")
  let insertState ← queryExec conn "INSERT INTO ep VALUES (1), (2), (3)"
  unless insertState.isSuccess do throw (IO.userError "INSERT INTO ep failed")

  -- Bind a parameter, then execute the same prepared statement twice with
  -- different bindings, to confirm both the bind/execute round-trip and
  -- that a `PreparedStatement` may be re-executed after re-binding.
  let selResult ← prepareForTest conn "SELECT * FROM ep WHERE x > ?"
  let selStmt ← match selResult with
    | .ok stmt => pure stmt
    | .error msg => throw (IO.userError msg)

  let bindState ← bindInt32 selStmt 1 1
  unless bindState.isSuccess do throw (IO.userError "bindInt32 failed")
  let (state1, result1) ← execute selStmt
  unless state1.isSuccess do throw (IO.userError "execute failed")
  let rowCount1 ← resultRowCount result1
  unless rowCount1 == 2 do throw (IO.userError s!"expected 2 rows (x > 1), got {rowCount1}")
  destroy result1

  let rebindState ← bindInt32 selStmt 1 2
  unless rebindState.isSuccess do throw (IO.userError "bindInt32 (rebind) failed")
  let (state2, result2) ← execute selStmt
  unless state2.isSuccess do throw (IO.userError "execute (rebind) failed")
  let rowCount2 ← resultRowCount result2
  unless rowCount2 == 1 do throw (IO.userError s!"expected 1 row (x > 2), got {rowCount2}")
  destroy result2
  destroy result2 -- idempotent

  destroyPreparedForTest selStmt

  -- Error path: `execute`'s own failure surface, via a statement whose
  -- parameter is deliberately left unbound (a malformed query, in
  -- contrast, never reaches `execute` at all — it already fails at
  -- `prepareForTest`).
  let unboundResult ← prepareForTest conn "SELECT * FROM ep WHERE x > ?"
  let unboundStmt ← match unboundResult with
    | .ok stmt => pure stmt
    | .error msg => throw (IO.userError msg)
  let (unboundState, unboundResultObj) ← execute unboundStmt
  match unboundState.isSuccess with
  | true => throw (IO.userError "expected execute to fail for an unbound parameter")
  | false => pure ()
  destroy unboundResultObj
  destroyPreparedForTest unboundStmt

  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.ExecutePrepared
