/-
  Linen.Database.DuckDB.FFI.QueryExecution — running a SQL query directly and
  inspecting its result

  Mirrors Haskell's `Database.DuckDB.FFI.QueryExecution` (the `duckdb-ffi`
  package). One of the batch of modules from
  `docs/imports/duckdb-ffi/dependencies.md` depending only on
  `Database.DuckDB.FFI.Types` (module #1).

  `query` is the direct, unparameterized counterpart to
  `Database.DuckDB.FFI.PreparedStatements.prepare` +
  `Database.DuckDB.FFI.ExecutePrepared.execute`; the rest of this module
  inspects the resulting `Types.Result` (column names/types, statement
  type, row/column counts, rows-changed, and error reporting).
  `duckdb_result_statement_type`/`duckdb_result_get_arrow_options` are
  bound directly against `duckdb_result` by value in this port's pinned
  `duckdb.h` (unlike `duckdb-haskell`, which routes them through
  `wrapped_duckdb_result_statement_type`/`wrapped_duckdb_result_get_arrow_options`
  C shims — apparently to dodge a GHC-FFI struct-by-value-argument
  limitation this port doesn't have), matching the same by-value handling
  `Types.lean`'s doc comment on `ResultHandle` and
  `Database.DuckDB.FFI.ExecutePrepared` already describe.

  ## Addendum: `fetchChunk`

  `fetchChunk` (`duckdb_fetch_chunk`) was added after the rest of this
  module, once `Linen.Database.DuckDB.Simple` (module #17 of
  `duckdb-simple`) needed it to walk a materialized result's rows.
  `docs/imports/duckdb-ffi/dependencies.md`'s original scope decision
  excluded it, having found it filed under upstream Haskell's
  `Database.DuckDB.FFI.StreamingResult` module — but the underlying
  `duckdb_fetch_chunk` C function itself works on *any* materialized
  `duckdb_result` (the ones `duckdb_query`/`duckdb_execute_prepared`
  return), not only on a `duckdb_pending_prepared_streaming` result; it
  sits in `duckdb.h`'s "Streaming Result Interface" section but, unlike its
  neighbour `duckdb_stream_fetch_chunk`, is not gated behind
  `DUCKDB_API_NO_DEPRECATED` and carries no deprecation notice — it is the
  only current, non-deprecated way to walk a materialized result's chunks
  (`duckdb_result_get_chunk`/`duckdb_result_chunk_count`, `duckdb.h`'s other
  candidate pair, are themselves marked for removal). `duckdb-simple`'s own
  `collectRows`/`streamNextRow` call it directly on ordinary query results,
  confirming this reading. -/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.QueryExecution

open Database.DuckDB.FFI.Types

/-! ── Running a query ── -/

/-- Raw `duckdb_query`: `(state, result)`. Per `duckdb.h`'s own doc comment,
    the resulting `Result` must always be destroyed with `destroy`, even if
    the query fails (`resultError` below then reports why). -/
@[extern "linen_duckdb_query"]
opaque queryRaw (connection : @& Connection) (query : @& String) : IO (UInt32 × Types.Result)

/-- Run `query` against `connection`, materializing the full result. The
    resulting `Result` must eventually be destroyed with `destroy` (or let
    its GC finalizer do so) regardless of whether the query succeeded. -/
def query (connection : Connection) (query : String) : IO (State × Types.Result) := do
  let (rc, result) ← queryRaw connection query
  pure (State.ofUInt32 rc, result)

/-- Destroy `result`, deallocating all associated memory. Idempotent, like
    `Database.DuckDB.FFI.OpenConnect.close`. -/
@[extern "linen_duckdb_destroy_result"]
opaque destroy : Types.Result → IO Unit

/-! ── Inspection ── -/

/-- The name of the result column at `col`. -/
@[extern "linen_duckdb_column_name"]
opaque columnName (result : @& Types.Result) (col : Idx) : IO (Option String)

/-- The raw `duckdb_type` of the result column at `col`. -/
@[extern "linen_duckdb_column_type_raw"]
opaque columnTypeRaw (result : @& Types.Result) (col : Idx) : IO UInt32

/-- The `duckdb_type` of the result column at `col`, decoded. -/
def columnType (result : Types.Result) (col : Idx) : IO Type_ := do
  pure (Type_.ofUInt32 (← columnTypeRaw result col))

/-- The raw `duckdb_statement_type` of the statement that produced
    `result`. -/
@[extern "linen_duckdb_result_statement_type_raw"]
opaque statementTypeRaw (result : @& Types.Result) : IO UInt32

/-- The `StatementType` of the statement that produced `result`. -/
def statementType (result : Types.Result) : IO StatementType := do
  pure (StatementType.ofUInt32 (← statementTypeRaw result))

/-- The logical type of the result column at `col`. -/
@[extern "linen_duckdb_column_logical_type"]
opaque columnLogicalType (result : @& Types.Result) (col : Idx) : IO LogicalType

/-- The Arrow options associated with `result`. -/
@[extern "linen_duckdb_result_get_arrow_options"]
opaque resultGetArrowOptions (result : @& Types.Result) : IO ArrowOptions

/-- The number of columns present in `result`. -/
@[extern "linen_duckdb_column_count"]
opaque columnCount (result : @& Types.Result) : IO Idx

/-- The number of rows changed by `result`'s query (`INSERT`/`UPDATE`/
    `DELETE` only; `0` for other statement types). -/
@[extern "linen_duckdb_rows_changed"]
opaque rowsChanged (result : @& Types.Result) : IO Idx

/-- The error message associated with `result`, if the query failed. -/
@[extern "linen_duckdb_result_error"]
opaque resultError (result : @& Types.Result) : IO (Option String)

/-- The raw `duckdb_error_type` associated with `result`, if the query
    failed. -/
@[extern "linen_duckdb_result_error_type_raw"]
opaque resultErrorTypeRaw (result : @& Types.Result) : IO UInt32

/-- The `ErrorType` associated with `result`, if the query failed. -/
def resultErrorType (result : Types.Result) : IO ErrorType := do
  pure (ErrorType.ofUInt32 (← resultErrorTypeRaw result))

/-- Fetch the next data chunk of `result`, `none` once exhausted. Must be
    called repeatedly, in order, to walk the whole result (see the module
    doc's "Addendum" above for why this binding lives here rather than in a
    dedicated streaming-result module). The returned `DataChunk` must
    eventually be destroyed with `Database.DuckDB.FFI.DataChunk.destroy`
    (or let its GC finalizer do so). -/
@[extern "linen_duckdb_fetch_chunk"]
opaque fetchChunk (result : @& Types.Result) : IO (Option DataChunk)

end Database.DuckDB.FFI.QueryExecution
