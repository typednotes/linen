/-
  Tests for `Linen.Database.DuckDB.FFI.PreparedStatements`.

  Prepares a real parameterized statement against a real table, and
  confirms its shape (parameter count/name/type, result-column
  count/name/type, and classified `StatementType`) — then confirms a
  malformed query reports a real error message via `error`.
-/
import Linen.Database.DuckDB.FFI.PreparedStatements
import Linen.Database.DuckDB.FFI.LogicalTypes
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.PreparedStatements
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types
open Tests.Database.DuckDB.FFI.TestSupport

namespace Tests.Database.DuckDB.FFI.PreparedStatements

#eval show IO Unit from do
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")
  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  let createState ← queryExec conn "CREATE TABLE prep_probe(a INTEGER, b VARCHAR)"
  unless createState.isSuccess do throw (IO.userError "CREATE TABLE prep_probe failed")

  let (rc, stmt) ← prepare conn "SELECT a, b FROM prep_probe WHERE a = ?"
  unless rc.isSuccess do
    let err ← error stmt
    throw (IO.userError s!"prepare failed: {err}")

  let n ← nparams stmt
  unless n == 1 do throw (IO.userError s!"expected 1 parameter, got {n}")

  let pType ← paramType stmt 1
  unless pType == .integer do
    throw (IO.userError s!"expected parameter 1 to be .integer, got {repr pType}")

  let pLogicalType ← paramLogicalType stmt 1
  let pTypeId ← Database.DuckDB.FFI.LogicalTypes.getTypeId pLogicalType

  unless pTypeId == .integer do throw (IO.userError "expected parameter logical type .integer")
  Database.DuckDB.FFI.LogicalTypes.destroy pLogicalType

  let clearState ← clearBindings stmt
  unless clearState.isSuccess do throw (IO.userError "clearBindings failed")

  let stType ← statementType stmt
  unless stType == .select do
    throw (IO.userError s!"expected .select, got {repr stType}")

  let colCount ← columnCount stmt
  unless colCount == 2 do throw (IO.userError s!"expected 2 result columns, got {colCount}")

  let colNameA ← columnName stmt 0
  unless colNameA == some "a" do
    throw (IO.userError s!"expected result column 0 named 'a', got {colNameA}")

  let colTypeB ← columnType stmt 1
  unless colTypeB == .varchar do
    throw (IO.userError s!"expected result column 1 to be .varchar, got {repr colTypeB}")

  let colLogicalTypeB ← columnLogicalType stmt 1
  let colLogicalTypeIdB ← Database.DuckDB.FFI.LogicalTypes.getTypeId colLogicalTypeB
  unless colLogicalTypeIdB == .varchar do
    throw (IO.userError "expected result column 1 logical type .varchar")
  Database.DuckDB.FFI.LogicalTypes.destroy colLogicalTypeB

  destroy stmt
  destroy stmt -- idempotent

  -- A malformed query must report a real error message via `error`.
  let (badRc, badStmt) ← prepare conn "SELECT * FROM this_table_does_not_exist"
  unless !badRc.isSuccess do throw (IO.userError "expected prepare to fail on a bad table name")
  let badErr ← error badStmt
  unless badErr.isSome do throw (IO.userError "expected a non-empty error message")
  destroy badStmt

  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.PreparedStatements
