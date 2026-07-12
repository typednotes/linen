/-
  Linen.Database.DuckDB.FFI.ErrorData — structured error object accessors

  Mirrors Haskell's `Database.DuckDB.FFI.ErrorData` (the `duckdb-ffi`
  package). Module #7 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1), which already declares
  the `ErrorData` handle itself (added there for
  `Database.DuckDB.FFI.Appender`'s `errorData` accessor).

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`,
  reusing the `linen_duckdb_error_data_t` wrapper/finalizer/`mk_duckdb_error_data`
  helper already registered for `ErrorData` — this module only adds the five
  raw entry points upstream exposes: `duckdb_create_error_data`,
  `duckdb_destroy_error_data`, `duckdb_error_data_error_type`,
  `duckdb_error_data_message`, `duckdb_error_data_has_error`.
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.ErrorData

open Database.DuckDB.FFI.Types

/-- Raw `duckdb_error_data_error_type`. -/
@[extern "linen_duckdb_error_data_error_type"]
opaque errorTypeRaw (errorData : @& Types.ErrorData) : IO UInt32

/-- The classification code of `errorData`. -/
def errorType (errorData : Types.ErrorData) : IO ErrorType :=
  ErrorType.ofUInt32 <$> errorTypeRaw errorData

/-- Raw `duckdb_error_data_message`. The returned string is owned by
    `errorData` and must not be freed separately (matches upstream's own
    "must not be freed" doc comment). -/
@[extern "linen_duckdb_error_data_message"]
opaque message (errorData : @& Types.ErrorData) : IO String

/-- Raw `duckdb_error_data_has_error`. -/
@[extern "linen_duckdb_error_data_has_error"]
opaque hasError (errorData : @& Types.ErrorData) : IO Bool

/-- Raw `duckdb_create_error_data`: takes the raw `duckdb_error_type` code
    (see `create`). -/
@[extern "linen_duckdb_error_data_create"]
opaque createRaw (type : UInt32) (message : @& String) : IO Types.ErrorData

/-- Build a standalone structured error object (e.g. to feed into a
    `Database.DuckDB.FFI.Logging` custom log-storage callback). The result
    must eventually be destroyed with `destroy` (or let its GC finalizer do
    so). -/
def create (type : ErrorType) (message : String) : IO Types.ErrorData :=
  createRaw type.toUInt32 message

/-- `duckdb_destroy_error_data`: release `errorData`'s underlying resources
    early. Idempotent — safe to call more than once, and safe to skip
    entirely (the GC finalizer calls it too). -/
@[extern "linen_duckdb_error_data_destroy"]
opaque destroy : Types.ErrorData → IO Unit

end Database.DuckDB.FFI.ErrorData
