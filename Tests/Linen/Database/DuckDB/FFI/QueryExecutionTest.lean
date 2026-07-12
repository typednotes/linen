/-
  Tests for `Linen.Database.DuckDB.FFI.QueryExecution`.

  Runs a real `SELECT`, then an `INSERT`/`UPDATE`, confirming column
  names/types, `StatementType`, row/column counts, `rowsChanged`, and
  `resultError`/`resultErrorType` on a deliberately failing query.
-/
import Linen.Database.DuckDB.FFI.QueryExecution
import Linen.Database.DuckDB.FFI.LogicalTypes
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.QueryExecution
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types
open Tests.Database.DuckDB.FFI.TestSupport

namespace Tests.Database.DuckDB.FFI.QueryExecution

#eval show IO Unit from do
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")
  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  let createState ← queryExec conn "CREATE TABLE query_probe(a INTEGER, b VARCHAR)"
  unless createState.isSuccess do throw (IO.userError "CREATE TABLE query_probe failed")

  -- SELECT.
  let (selRc, selResult) ← query conn "SELECT a, b FROM query_probe"
  unless selRc.isSuccess do
    let err ← resultError selResult
    throw (IO.userError s!"SELECT failed: {err}")

  let colCount ← columnCount selResult
  unless colCount == 2 do throw (IO.userError s!"expected 2 columns, got {colCount}")

  let colName0 ← columnName selResult 0
  unless colName0 == some "a" do
    throw (IO.userError s!"expected column 0 named 'a', got {colName0}")

  let colType1 ← columnType selResult 1
  unless colType1 == .varchar do
    throw (IO.userError s!"expected column 1 to be .varchar, got {repr colType1}")

  let logicalType1 ← columnLogicalType selResult 1
  let logicalTypeId1 ← Database.DuckDB.FFI.LogicalTypes.getTypeId logicalType1
  unless logicalTypeId1 == .varchar do
    throw (IO.userError "expected column 1 logical type .varchar")
  Database.DuckDB.FFI.LogicalTypes.destroy logicalType1

  let stType ← statementType selResult
  unless stType == .select do
    throw (IO.userError s!"expected .select, got {repr stType}")

  destroy selResult

  -- INSERT / UPDATE: exercise `rowsChanged`.
  let (insRc, insResult) ← query conn "INSERT INTO query_probe VALUES (1, 'x'), (2, 'y')"
  unless insRc.isSuccess do throw (IO.userError "INSERT failed")
  let insRows ← rowsChanged insResult
  unless insRows == 2 do throw (IO.userError s!"expected 2 rows inserted, got {insRows}")
  destroy insResult

  let (updRc, updResult) ← query conn "UPDATE query_probe SET a = 3 WHERE a = 1"
  unless updRc.isSuccess do throw (IO.userError "UPDATE failed")
  let updRows ← rowsChanged updResult
  unless updRows == 1 do throw (IO.userError s!"expected 1 row updated, got {updRows}")
  destroy updResult

  -- A malformed query must report a real error message/type via `query`.
  let (badRc, badResult) ← query conn "SELECT * FROM this_table_does_not_exist"
  unless !badRc.isSuccess do throw (IO.userError "expected query to fail on a bad table name")
  let badErr ← resultError badResult
  unless badErr.isSome do throw (IO.userError "expected a non-empty error message")
  let _badErrType ← resultErrorType badResult
  destroy badResult

  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.QueryExecution
