/-
  Linen.Database.DuckDB.FFI.PreparedStatements — preparing/inspecting SQL
  statements

  Mirrors Haskell's `Database.DuckDB.FFI.PreparedStatements` (the
  `duckdb-ffi` package). One of the batch of modules from
  `docs/imports/duckdb-ffi/dependencies.md` depending only on
  `Database.DuckDB.FFI.Types` (module #1).

  `Database.DuckDB.FFI.BindValues` (already ported) binds *parameters* to
  an existing `PreparedStatement`; this module is the complementary half —
  *creating* one from a SQL string (`prepare`) and inspecting its shape
  (parameter count/names/types, result-column count/names/types, and the
  classified `StatementType` of the statement it wraps). Executing a
  prepared statement remains `Database.DuckDB.FFI.ExecutePrepared`'s job. -/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.PreparedStatements

open Database.DuckDB.FFI.Types

/-! ── Creation / destruction ── -/

/-- Raw `duckdb_prepare`: `(state, preparedStatement)`. Per `duckdb.h`'s own
    doc comment, the resulting `PreparedStatement` must always be destroyed
    with `destroy`, even if preparation fails (`error` below then reports
    why). -/
@[extern "linen_duckdb_prepare"]
opaque prepareRaw (connection : @& Connection) (query : @& String) : IO (UInt32 × PreparedStatement)

/-- Prepare `query` for execution against `connection`. The resulting
    `PreparedStatement` must eventually be destroyed with `destroy` (or let
    its GC finalizer do so) regardless of whether preparation succeeded. -/
def prepare (connection : Connection) (query : String) : IO (State × PreparedStatement) := do
  let (rc, stmt) ← prepareRaw connection query
  pure (State.ofUInt32 rc, stmt)

/-- Destroy `preparedStatement`, deallocating all associated memory.
    Idempotent, like `Database.DuckDB.FFI.OpenConnect.close`. -/
@[extern "linen_duckdb_destroy_prepare"]
opaque destroy : PreparedStatement → IO Unit

/-! ── Inspection ── -/

/-- The error message associated with `preparedStatement`, if preparation
    failed. -/
@[extern "linen_duckdb_prepare_error"]
opaque error (preparedStatement : @& PreparedStatement) : IO (Option String)

/-- The number of parameters `preparedStatement` accepts (`0` if it was not
    successfully prepared). -/
@[extern "linen_duckdb_nparams"]
opaque nparams (preparedStatement : @& PreparedStatement) : IO Idx

/-- The name of the parameter at `paramIdx` (1-based, per `duckdb.h`). -/
@[extern "linen_duckdb_parameter_name"]
opaque parameterName (preparedStatement : @& PreparedStatement) (paramIdx : Idx) : IO (Option String)

/-- The raw `duckdb_type` of the parameter at `paramIdx`. -/
@[extern "linen_duckdb_param_type_raw"]
opaque paramTypeRaw (preparedStatement : @& PreparedStatement) (paramIdx : Idx) : IO UInt32

/-- The `duckdb_type` of the parameter at `paramIdx`, decoded. -/
def paramType (preparedStatement : PreparedStatement) (paramIdx : Idx) : IO Type_ := do
  pure (Type_.ofUInt32 (← paramTypeRaw preparedStatement paramIdx))

/-- The logical type of the parameter at `paramIdx`. -/
@[extern "linen_duckdb_param_logical_type"]
opaque paramLogicalType (preparedStatement : @& PreparedStatement) (paramIdx : Idx) : IO LogicalType

/-- Clear all parameters currently bound to `preparedStatement`. -/
@[extern "linen_duckdb_clear_bindings_raw"]
opaque clearBindingsRaw (preparedStatement : @& PreparedStatement) : IO UInt32

/-- Clear all parameters currently bound to `preparedStatement`. -/
def clearBindings (preparedStatement : PreparedStatement) : IO State := do
  pure (State.ofUInt32 (← clearBindingsRaw preparedStatement))

/-- The raw `duckdb_statement_type` of the statement `preparedStatement`
    will execute. -/
@[extern "linen_duckdb_prepared_statement_type_raw"]
opaque statementTypeRaw (preparedStatement : @& PreparedStatement) : IO UInt32

/-- The `StatementType` of the statement `preparedStatement` will execute. -/
def statementType (preparedStatement : PreparedStatement) : IO StatementType := do
  pure (StatementType.ofUInt32 (← statementTypeRaw preparedStatement))

/-- The number of result columns `preparedStatement` will produce. -/
@[extern "linen_duckdb_prepared_statement_column_count"]
opaque columnCount (preparedStatement : @& PreparedStatement) : IO Idx

/-- The name of the result column at `colIdx`. -/
@[extern "linen_duckdb_prepared_statement_column_name"]
opaque columnName (preparedStatement : @& PreparedStatement) (colIdx : Idx) : IO (Option String)

/-- The logical type of the result column at `colIdx`. -/
@[extern "linen_duckdb_prepared_statement_column_logical_type"]
opaque columnLogicalType (preparedStatement : @& PreparedStatement) (colIdx : Idx) : IO LogicalType

/-- The raw `duckdb_type` of the result column at `colIdx`. -/
@[extern "linen_duckdb_prepared_statement_column_type_raw"]
opaque columnTypeRaw (preparedStatement : @& PreparedStatement) (colIdx : Idx) : IO UInt32

/-- The `duckdb_type` of the result column at `colIdx`, decoded. -/
def columnType (preparedStatement : PreparedStatement) (colIdx : Idx) : IO Type_ := do
  pure (Type_.ofUInt32 (← columnTypeRaw preparedStatement colIdx))

end Database.DuckDB.FFI.PreparedStatements
