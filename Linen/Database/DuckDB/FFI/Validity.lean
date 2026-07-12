/-
  Linen.Database.DuckDB.FFI.Validity — reading/writing a vector's validity
  (NULL) bitmask

  Mirrors Haskell's `Database.DuckDB.FFI.Validity` (the `duckdb-ffi`
  package). One of the batch of modules from
  `docs/imports/duckdb-ffi/dependencies.md` depending only on
  `Database.DuckDB.FFI.Types` (module #1).

  A `ValidityMask` (`Types.ValidityMaskHandle`) is obtained from
  `Database.DuckDB.FFI.Vector.getValidity`; per `duckdb.h`'s own doc
  comment it is a bitset of `uint64_t` words, one bit per row (`1` = valid/
  not-NULL, `0` = NULL). This module exposes the row-granular
  read/write helpers DuckDB itself provides as the ergonomic (if slower)
  alternative to bit-twiddling the raw words directly. -/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.Validity

open Database.DuckDB.FFI.Types

/-- Whether `row` is valid (not `NULL`) according to `validity`. -/
@[extern "linen_duckdb_validity_row_is_valid"]
opaque rowIsValid (validity : @& ValidityMask) (row : Idx) : IO Bool

/-- Set whether `row` is valid (not `NULL`) in `validity`. `validity` must
    have been obtained after ensuring the mask is writable (see
    `Database.DuckDB.FFI.Vector.ensureValidityWritable`). -/
@[extern "linen_duckdb_validity_set_row_validity"]
opaque setRowValidity (validity : @& ValidityMask) (row : Idx) (valid : Bool) : IO Unit

/-- Mark `row` as invalid (`NULL`) in `validity`. Equivalent to
    `setRowValidity validity row false`. -/
@[extern "linen_duckdb_validity_set_row_invalid"]
opaque setRowInvalid (validity : @& ValidityMask) (row : Idx) : IO Unit

/-- Mark `row` as valid (not `NULL`) in `validity`. Equivalent to
    `setRowValidity validity row true`. -/
@[extern "linen_duckdb_validity_set_row_valid"]
opaque setRowValid (validity : @& ValidityMask) (row : Idx) : IO Unit

end Database.DuckDB.FFI.Validity
