/-
  Tests for `Linen.Database.DuckDB.FFI.DataChunk`.

  Exercises both the documented zero-column case (`createDataChunk (types :=
  #[])`, this port's only way to call `createDataChunk` at all without
  `Database.DuckDB.FFI.LogicalTypes`, per the module's own doc comment) and a
  real one-column chunk, using a `LogicalType` borrowed from
  `Database.DuckDB.FFI.Appender.columnType` on a real table column — the same
  cross-module borrowing `ConfigurationTest` uses for its own `LogicalType`.
-/
import Linen.Database.DuckDB.FFI.DataChunk
import Linen.Database.DuckDB.FFI.Appender
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.DataChunk
open Database.DuckDB.FFI.Appender (columnType create)
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types
open Tests.Database.DuckDB.FFI.TestSupport

namespace Tests.Database.DuckDB.FFI.DataChunk

-- Zero-column chunk: a valid, real chunk per `duckdb.h`'s own doc comment on
-- `duckdb_create_data_chunk`.
#eval show IO Unit from do
  let chunk ← createDataChunk #[]
  let colCount ← getColumnCount chunk
  if colCount != 0 then
    throw (IO.userError s!"expected 0 columns, got {colCount}")

  let size ← getSize chunk
  if size != 0 then
    throw (IO.userError s!"expected a fresh chunk's size to be 0, got {size}")

  reset chunk -- a no-op here, but must not crash on a zero-column chunk
  destroy chunk
  destroy chunk -- idempotent

-- One-column chunk, with a real `LogicalType` borrowed via `Appender`.
#eval show IO Unit from do
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")

  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  let createState ← queryExec conn "CREATE TABLE chunk_probe(a INTEGER)"
  if !createState.isSuccess then
    throw (IO.userError "CREATE TABLE chunk_probe failed")

  let appResult ← create conn none "chunk_probe"
  let appender ← match appResult with
    | .ok app => pure app
    | .error msg => throw (IO.userError msg)

  let intType ← columnType appender 0

  let chunk ← createDataChunk #[intType]
  let colCount ← getColumnCount chunk
  if colCount != 1 then
    throw (IO.userError s!"expected 1 column, got {colCount}")

  let _vec ← getVector chunk 0 -- just proves the FFI round-trip works

  let sizeBefore ← getSize chunk
  if sizeBefore != 0 then
    throw (IO.userError s!"expected a fresh chunk's size to be 0, got {sizeBefore}")

  setSize chunk 3
  let sizeAfter ← getSize chunk
  if sizeAfter != 3 then
    throw (IO.userError s!"expected size 3 after setSize, got {sizeAfter}")

  reset chunk
  let sizeAfterReset ← getSize chunk
  if sizeAfterReset != 0 then
    throw (IO.userError s!"expected size 0 after reset, got {sizeAfterReset}")

  destroy chunk
  Database.DuckDB.FFI.Appender.destroy appender
  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.DataChunk
