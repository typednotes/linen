/-
  Linen.Database.DuckDB.FFI.DataChunk — result data-chunk / column access

  Mirrors Haskell's `Database.DuckDB.FFI.DataChunk` (the `duckdb-ffi`
  package). Module #6 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1).

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`. A
  `DataChunk` is a materialized batch of column `Vector`s, either built
  directly via `createDataChunk` or returned from a query result (the latter
  is `Database.DuckDB.FFI.QueryExecution`'s job, out of scope for this
  batch). `getVector`'s return type, and `createDataChunk`'s `types`
  parameter, are the two places this module touches `LogicalType` — but
  *building* a `LogicalType` from scratch (`duckdb_create_logical_type` and
  friends) is `Database.DuckDB.FFI.LogicalTypes`'s job, also out of scope
  here; `createDataChunk (types := #[])` (a valid, real zero-column chunk —
  see `duckdb.h`'s own doc comment: `types`/`column_count` may legitimately
  be an empty array/`0`) is this port's only way to exercise `createDataChunk`
  without that module.

  `createDataChunk`'s upstream `Ptr DuckDBLogicalType` array parameter is
  bound here as a plain `Array LogicalType`: the C shim builds the transient
  C array of raw pointers from it and frees that scratch array immediately
  after the call returns (DuckDB copies each logical type into the chunk's
  own storage — the `LogicalType` handles the caller passed in remain
  independently owned and must still be destroyed by the caller, per
  `duckdb.h`'s own doc comment on `duckdb_create_data_chunk`).
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.DataChunk

open Database.DuckDB.FFI.Types

/-! ── Creation / destruction ── -/

/-- Create an empty data chunk with `types` as its column types (`types :=
    #[]` for a valid zero-column chunk — see the module doc comment).
    Column types may not contain `ANY`/`INVALID`. The result must eventually
    be destroyed with `destroy` (or let its GC finalizer do so). -/
@[extern "linen_duckdb_create_data_chunk"]
opaque createDataChunk (types : @& Array LogicalType) : IO DataChunk

/-- Destroy `chunk`, deallocating all memory associated with it. Idempotent,
    like `Database.DuckDB.FFI.OpenConnect.close`. -/
@[extern "linen_duckdb_destroy_data_chunk"]
opaque destroy : DataChunk → IO Unit

/-! ── Inspection ── -/

/-- Reset `chunk`, clearing its validity masks and setting its cardinality to
    `0`. After calling this, any previously retrieved `Vector` data/validity
    pointers must be re-fetched (this module doesn't yet bind those
    accessors — that's `Database.DuckDB.FFI.Vector`/`Validity`, out of scope
    here; this doc comment just carries upstream's own caveat forward
    faithfully). -/
@[extern "linen_duckdb_data_chunk_reset"]
opaque reset : DataChunk → IO Unit

/-- The number of columns in `chunk`. -/
@[extern "linen_duckdb_data_chunk_get_column_count"]
opaque getColumnCount : DataChunk → IO Idx

/-- The `Vector` at column index `colIdx` in `chunk`. The returned handle is
    valid only as long as `chunk` is alive and does *not* need to be
    destroyed (see `Types.lean`'s doc comment on `VectorHandle`). -/
@[extern "linen_duckdb_data_chunk_get_vector"]
opaque getVector (chunk : @& DataChunk) (colIdx : Idx) : IO Vector

/-- The current number of tuples (rows) in `chunk`. -/
@[extern "linen_duckdb_data_chunk_get_size"]
opaque getSize : DataChunk → IO Idx

/-- Set the current number of tuples (rows) in `chunk`. -/
@[extern "linen_duckdb_data_chunk_set_size"]
opaque setSize (chunk : @& DataChunk) (size : Idx) : IO Unit

end Database.DuckDB.FFI.DataChunk
