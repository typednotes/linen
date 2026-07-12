/-
  Tests.Linen.Database.DuckDB.FFI.TestSupport — shared test-only helpers

  Not a mirror of any `Linen/` module: this file backs the three
  `TEST SUPPORT` entry points documented near the end of
  `ffi/duckdb_shim.c` (`duckdb_query`/`duckdb_prepare`/
  `duckdb_destroy_prepare` wrappers), used only by this batch's own
  `Tests/Linen/Database/DuckDB/FFI/{Appender,BindValues,Catalog,
  Configuration,DataChunk}Test.lean` to set up realistic end-to-end
  scenarios (a real table via `CREATE TABLE`, a real `PreparedStatement` to
  bind against) without prematurely porting
  `Database.DuckDB.FFI.QueryExecution`/`PreparedStatements` (out of scope
  for this batch — see `docs/imports/duckdb-ffi/dependencies.md`).
-/
import Linen.Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.Types

/-- Raw `duckdb_query`, run purely for side effects (DDL/DML); any result
    data is discarded by the C shim before this returns. -/
@[extern "linen_duckdb_test_query"]
opaque queryExecRaw (connection : @& Connection) (query : @& String) : IO UInt32

/-- Run `query` on `connection` for side effects (e.g. `CREATE TABLE`),
    decoded to a `State`. -/
def queryExec (connection : Connection) (query : String) : IO State :=
  State.ofUInt32 <$> queryExecRaw connection query

/-- Raw `duckdb_prepare`: `(state, preparedStatement?)`. -/
@[extern "linen_duckdb_test_prepare"]
opaque prepareForTestRaw (connection : @& Connection) (query : @& String) :
    IO (UInt32 × Option PreparedStatement)

/-- Prepare `query` on `connection`, for use as a
    `Database.DuckDB.FFI.BindValues` receiver in tests. -/
def prepareForTest (connection : Connection) (query : String) :
    IO (Except String PreparedStatement) := do
  let (rc, stmtOpt) ← prepareForTestRaw connection query
  match State.ofUInt32 rc, stmtOpt with
  | .success, some stmt => pure (.ok stmt)
  | _, _ => pure (.error s!"duckdb_prepare failed for {query}")

/-- Destroy a `PreparedStatement` obtained from `prepareForTest`.
    Idempotent. -/
@[extern "linen_duckdb_test_destroy_prepare"]
opaque destroyPreparedForTest : PreparedStatement → IO Unit

/-- Raw `duckdb_row_count` on a `Result` obtained from
    `Database.DuckDB.FFI.ExecutePrepared.execute`, used only by
    `ExecutePreparedTest` to check real row data rather than just the
    reported `State`. -/
@[extern "linen_duckdb_test_result_row_count"]
opaque resultRowCount (result : @& Result) : IO UInt64

end Tests.Database.DuckDB.FFI.TestSupport
