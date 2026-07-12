/-
  Linen.Database.DuckDB.FFI.Vector — creating and reading/writing
  `duckdb_vector` columns

  Mirrors Haskell's `Database.DuckDB.FFI.Vector` (the `duckdb-ffi`
  package). One of the batch of modules from
  `docs/imports/duckdb-ffi/dependencies.md` depending only on
  `Database.DuckDB.FFI.Types` (module #1).

  **Ownership caveat.** `Types.VectorHandle`'s GC finalizer is deliberately
  a no-op on the underlying `duckdb_vector` (see its doc comment): every
  *other* module that hands out a `Vector` (`Database.DuckDB.FFI.DataChunk`,
  `Database.DuckDB.FFI.ScalarFunctions`) does so as a *borrowed* pointer
  into a parent `DataChunk`, whose own destruction is what actually frees
  it. `createVector` below is the one exception — it allocates a genuinely
  *standalone* vector via `duckdb_create_vector`, which per `duckdb.h` must
  be released with `duckdb_destroy_vector`. Because `VectorHandle` is
  shared across both use-cases, that release is *not* automatic for a
  vector obtained from `createVector`: callers must call `destroy`
  explicitly. This is a deliberate, documented consequence of not
  splitting `Vector` into two handle types purely for this one entry
  point — `duckdb-haskell` itself makes the same simplification (its
  binding is a bare, un-wrapped foreign import with no RAII of any kind).

  **Raw data access.** `duckdb_vector_get_data` returns an untyped `void*`;
  rather than hand-roll pointer arithmetic and endianness-sensitive decoding
  in Lean, this port exposes both the faithful raw byte-copy primitives
  (`getDataBytes`/`setDataBytes`) and small typed convenience wrappers for
  the fixed-width types tests exercise (`getInt32`/`setInt32`, etc.),
  implemented directly in `ffi/duckdb_shim.c` to avoid any manual
  bit-twiddling bugs. Reading/writing `VARCHAR`/`BLOB` data composes
  `getDataBytes` (to copy out the 16-byte `duckdb_string_t` image at
  `idx*16`) with `Database.DuckDB.FFI.Helpers.stringTData` — both modules
  depend only on `Types`, so that composition happens in consumer code
  (e.g. this batch's own `Tests/`), not inside either module.

  **Excluded.** `duckdb_slice_vector`/`duckdb_vector_copy_sel` need a
  `duckdb_selection_vector` (`SelectionVector`, explicitly excluded by
  `docs/imports/duckdb-ffi/dependencies.md`), so are out of scope here. -/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.Vector

open Database.DuckDB.FFI.Types

/-! ── Creation / destruction ── -/

/-- Create a flat vector of `ty` with the given `capacity`. Must eventually
    be destroyed with `destroy` — see this module's doc comment on
    `VectorHandle`'s ownership caveat. -/
@[extern "linen_duckdb_create_vector"]
opaque createVector (ty : @& LogicalType) (capacity : Idx) : IO Vector

/-- Destroy a vector obtained from `createVector`, deallocating its memory.
    Must *not* be called on a borrowed `Vector` (one obtained from
    `Database.DuckDB.FFI.DataChunk.getVector` or the child-vector
    accessors below). -/
@[extern "linen_duckdb_destroy_vector"]
opaque destroy : Vector → IO Unit

/-! ── Inspection ── -/

/-- The logical type of `vector`. -/
@[extern "linen_duckdb_vector_get_column_type"]
opaque getColumnType (vector : @& Vector) : IO LogicalType

/-- Copy `length` bytes starting at `byteOffset` out of `vector`'s raw data
    array. See this module's doc comment for how to decode `VARCHAR`/`BLOB`
    entries from the result. -/
@[extern "linen_duckdb_vector_get_data_bytes"]
opaque getDataBytes (vector : @& Vector) (byteOffset : UInt64) (length : UInt64) : IO ByteArray

/-- Overwrite `data.size` bytes of `vector`'s raw data array starting at
    `byteOffset`. -/
@[extern "linen_duckdb_vector_set_data_bytes"]
opaque setDataBytes (vector : @& Vector) (byteOffset : UInt64) (data : @& ByteArray) : IO Unit

/-- Read the `Int32` at row `idx` of an `INTEGER`-typed vector's data
    array. -/
@[extern "linen_duckdb_vector_get_int32"]
opaque getInt32 (vector : @& Vector) (idx : Idx) : IO Int32

/-- Write the `Int32` at row `idx` of an `INTEGER`-typed vector's data
    array. -/
@[extern "linen_duckdb_vector_set_int32"]
opaque setInt32 (vector : @& Vector) (idx : Idx) (value : Int32) : IO Unit

/-- Read the `Int64` at row `idx` of a `BIGINT`-typed vector's data
    array. -/
@[extern "linen_duckdb_vector_get_int64"]
opaque getInt64 (vector : @& Vector) (idx : Idx) : IO Int64

/-- Write the `Int64` at row `idx` of a `BIGINT`-typed vector's data
    array. -/
@[extern "linen_duckdb_vector_set_int64"]
opaque setInt64 (vector : @& Vector) (idx : Idx) (value : Int64) : IO Unit

/-- Read the `Float` at row `idx` of a `DOUBLE`-typed vector's data
    array. -/
@[extern "linen_duckdb_vector_get_double"]
opaque getDouble (vector : @& Vector) (idx : Idx) : IO Float

/-- Write the `Float` at row `idx` of a `DOUBLE`-typed vector's data
    array. -/
@[extern "linen_duckdb_vector_set_double"]
opaque setDouble (vector : @& Vector) (idx : Idx) (value : Float) : IO Unit

/-- Read the `Bool` at row `idx` of a `BOOLEAN`-typed vector's data
    array. -/
@[extern "linen_duckdb_vector_get_bool"]
opaque getBool (vector : @& Vector) (idx : Idx) : IO Bool

/-- Write the `Bool` at row `idx` of a `BOOLEAN`-typed vector's data
    array. -/
@[extern "linen_duckdb_vector_set_bool"]
opaque setBool (vector : @& Vector) (idx : Idx) (value : Bool) : IO Unit

/-- The validity (`NULL`) bitmask of `vector`, or `none` if all values are
    valid and no mask has been allocated yet (see
    `duckdb.h`'s own doc comment, faithfully carried forward: this MIGHT
    return `none` even though the vector legitimately has room for
    `NULL`s — call `ensureValidityWritable` first if you need to write
    `NULL`s unconditionally). -/
@[extern "linen_duckdb_vector_get_validity"]
opaque getValidity (vector : @& Vector) : IO (Option ValidityMask)

/-- Ensure `vector` has an allocated, writable validity mask (after this,
    `getValidity` always returns `some`). -/
@[extern "linen_duckdb_vector_ensure_validity_writable"]
opaque ensureValidityWritable (vector : @& Vector) : IO Unit

/-! ── String/BLOB assignment ── -/

/-- Assign the NUL-terminated string `str` to `vector` at `index`. -/
@[extern "linen_duckdb_vector_assign_string_element"]
opaque assignStringElement (vector : @& Vector) (index : Idx) (str : @& String) : IO Unit

/-- Assign `bytes` (validated as UTF-8 by DuckDB) to `vector` at `index`;
    may also be used for `BLOB`s. -/
@[extern "linen_duckdb_vector_assign_string_element_len"]
opaque assignStringElementLen (vector : @& Vector) (index : Idx) (bytes : @& ByteArray) : IO Unit

/-- Like `assignStringElementLen`, but skips UTF-8 validation (use
    `Database.DuckDB.FFI.Helpers.validUtf8Check` first if validation is
    required). -/
@[extern "linen_duckdb_unsafe_vector_assign_string_element_len"]
opaque unsafeAssignStringElementLen (vector : @& Vector) (index : Idx) (bytes : @& ByteArray) :
    IO Unit

/-! ── Nested-type child vectors ── -/

/-- The child vector of a `LIST`-typed vector; valid as long as `vector`
    is. -/
@[extern "linen_duckdb_list_vector_get_child"]
opaque listVectorGetChild (vector : @& Vector) : IO Vector

/-- The current size of a `LIST`-typed vector's child vector. -/
@[extern "linen_duckdb_list_vector_get_size"]
opaque listVectorGetSize (vector : @& Vector) : IO Idx

/-- Set the total size of a `LIST`-typed vector's child vector. -/
@[extern "linen_duckdb_list_vector_set_size_raw"]
opaque listVectorSetSizeRaw (vector : @& Vector) (size : Idx) : IO UInt32

/-- Set the total size of a `LIST`-typed vector's child vector. -/
def listVectorSetSize (vector : Vector) (size : Idx) : IO State := do
  pure (State.ofUInt32 (← listVectorSetSizeRaw vector size))

/-- Reserve `requiredCapacity` entries in a `LIST`-typed vector's child
    vector. After this call, re-fetch the child's data/validity pointers. -/
@[extern "linen_duckdb_list_vector_reserve_raw"]
opaque listVectorReserveRaw (vector : @& Vector) (requiredCapacity : Idx) : IO UInt32

/-- Reserve `requiredCapacity` entries in a `LIST`-typed vector's child
    vector. -/
def listVectorReserve (vector : Vector) (requiredCapacity : Idx) : IO State := do
  pure (State.ofUInt32 (← listVectorReserveRaw vector requiredCapacity))

/-- The child vector of a `STRUCT`-typed vector at `index`; valid as long
    as `vector` is. -/
@[extern "linen_duckdb_struct_vector_get_child"]
opaque structVectorGetChild (vector : @& Vector) (index : Idx) : IO Vector

/-- The child vector of an `ARRAY`-typed vector (sized `vector`'s length
    times the array's fixed size); valid as long as `vector` is. -/
@[extern "linen_duckdb_array_vector_get_child"]
opaque arrayVectorGetChild (vector : @& Vector) : IO Vector

/-! ── Referencing ── -/

/-- Copy `value`'s contents into `vector`. -/
@[extern "linen_duckdb_vector_reference_value"]
opaque referenceValue (vector : @& Vector) (value : @& Value) : IO Unit

/-- Make `toVector` reference `fromVector`; after this call the two share
    ownership of the underlying data. -/
@[extern "linen_duckdb_vector_reference_vector"]
opaque referenceVector (toVector : @& Vector) (fromVector : @& Vector) : IO Unit

end Database.DuckDB.FFI.Vector
