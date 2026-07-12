/-
  Linen.Database.DuckDB.FFI.Types — low-level DuckDB C-API opaque handles +
  shared enums

  Mirrors Haskell's `Database.DuckDB.FFI.Types` (the `duckdb-ffi` package,
  version 1.5.0.0, checked against
  <https://github.com/Tritlo/duckdb-haskell>). Foundational module #1 of
  `docs/imports/duckdb-ffi/dependencies.md` — every other in-scope module
  imports only this one.

  **Scope.** Upstream's `Types.hs` also declares handles/structs used only by
  the 26 modules `docs/imports/duckdb-ffi/dependencies.md` excludes (Arrow
  interop, aggregate/cast/copy/table-function registration, the `Deprecated.*`
  tree, …). Following that same documented scope decision, this port includes
  only the opaque handles and enums needed by `Database.DuckDB.FFI.OpenConnect`
  (module #2, the only other module ported so far): `Database`, `Connection`,
  `InstanceCache`, `ClientContext`, `ArrowOptions`, `Value`, plus the
  `duckdb_state`/`duckdb_type`/`duckdb_error_type` enums. Handles needed by
  later modules (`Config`, `LogicalType`, `DataChunk`, `Vector`, …) are added
  to this module when those modules are ported, exactly as this module itself
  was only just now added — a shared foundational module is meant to grow
  incrementally with its consumers, not be front-loaded with everything it
  could ever need.

  Every handle wraps a DuckDB C API pointer via Lean's external-object
  mechanism (the same pattern as `Linen.Database.SQLite.Bindings.Types`'s
  `Database`/`Statement`, and `Linen.Database.PostgreSQL.LibPQ.Types`'s
  `PgConn`/`PgResult`): the runtime owns the pointer, and a C finalizer
  releases it (`duckdb_close`, `duckdb_disconnect`, …) if the caller never
  explicitly did.
-/

namespace Database.DuckDB.FFI.Types

/-! ── Opaque handles ── -/

/-- Opaque DuckDB database handle (wraps `duckdb_database`; GC finalizer
    calls `duckdb_close`). -/
opaque DatabaseHandle : NonemptyType

/-- A live (or formerly-live) DuckDB database, opened via `duckdb_open`/
    `duckdb_open_ext`/`duckdb_get_or_create_from_cache`. -/
def Database : Type := DatabaseHandle.type
instance : Nonempty Database := DatabaseHandle.property

/-- Opaque DuckDB connection handle (wraps `duckdb_connection`; GC finalizer
    calls `duckdb_disconnect`). -/
opaque ConnectionHandle : NonemptyType

/-- A live (or formerly-live) connection to a `Database`. -/
def Connection : Type := ConnectionHandle.type
instance : Nonempty Connection := ConnectionHandle.property

/-- Opaque DuckDB instance-cache handle (wraps `duckdb_instance_cache`; GC
    finalizer calls `duckdb_destroy_instance_cache`). Lets a process that
    (re)opens the same database file multiple times share one underlying
    instance instead of creating a fresh one each time. -/
opaque InstanceCacheHandle : NonemptyType

/-- A live (or formerly-live) DuckDB instance cache. -/
def InstanceCache : Type := InstanceCacheHandle.type
instance : Nonempty InstanceCache := InstanceCacheHandle.property

/-- Opaque DuckDB client-context handle (wraps `duckdb_client_context`; GC
    finalizer calls `duckdb_destroy_client_context`). -/
opaque ClientContextHandle : NonemptyType

/-- A connection's client context, retrieved via
    `duckdb_connection_get_client_context`. -/
def ClientContext : Type := ClientContextHandle.type
instance : Nonempty ClientContext := ClientContextHandle.property

/-- Opaque DuckDB Arrow-options handle (wraps `duckdb_arrow_options`; GC
    finalizer calls `duckdb_destroy_arrow_options`). Only the handle itself
    is in scope here — the Arrow interop API built on top of it
    (`Database.DuckDB.FFI.Arrow`) is one of the 26 excluded modules. -/
opaque ArrowOptionsHandle : NonemptyType

/-- A connection's Arrow options, retrieved via
    `duckdb_connection_get_arrow_options`. -/
def ArrowOptions : Type := ArrowOptionsHandle.type
instance : Nonempty ArrowOptions := ArrowOptionsHandle.property

/-- Opaque DuckDB value handle (wraps `duckdb_value`; GC finalizer calls
    `duckdb_destroy_value`). Only the handle itself is in scope here — the
    full boxed-value construction/decoding API
    (`Database.DuckDB.FFI.ValueInterface`) is one of the 26 excluded modules;
    this port only needs the handle type as `duckdb_get_table_names`'s
    return type. -/
opaque ValueHandle : NonemptyType

/-- A DuckDB boxed value, e.g. the `VARCHAR[]` returned by
    `duckdb_get_table_names`. -/
def Value : Type := ValueHandle.type
instance : Nonempty Value := ValueHandle.property

/-- Opaque DuckDB appender handle (wraps `duckdb_appender`; GC finalizer calls
    `duckdb_appender_destroy`). Added for `Database.DuckDB.FFI.Appender`
    (module #2). -/
opaque AppenderHandle : NonemptyType

/-- A live (or formerly-live) bulk-append handle, created via
    `duckdb_appender_create`/`_create_ext`/`_create_query`. -/
def Appender : Type := AppenderHandle.type
instance : Nonempty Appender := AppenderHandle.property

/-- Opaque DuckDB prepared-statement handle (wraps `duckdb_prepared_statement`;
    GC finalizer calls `duckdb_destroy_prepare`). Only the handle itself is in
    scope here — the full prepare/execute lifecycle
    (`Database.DuckDB.FFI.PreparedStatements`/`ExecutePrepared`) is out of
    scope for this batch; this port only needs the handle type as
    `Database.DuckDB.FFI.BindValues`'s receiver type. Added for
    `Database.DuckDB.FFI.BindValues` (module #3). -/
opaque PreparedStatementHandle : NonemptyType

/-- A prepared SQL statement, obtained via `duckdb_prepare`. -/
def PreparedStatement : Type := PreparedStatementHandle.type
instance : Nonempty PreparedStatement := PreparedStatementHandle.property

/-- Opaque DuckDB data-chunk handle (wraps `duckdb_data_chunk`; GC finalizer
    calls `duckdb_destroy_data_chunk`). Added for
    `Database.DuckDB.FFI.DataChunk` (module #6). -/
opaque DataChunkHandle : NonemptyType

/-- A materialized batch of column vectors, created via
    `duckdb_create_data_chunk` or returned from a query result. -/
def DataChunk : Type := DataChunkHandle.type
instance : Nonempty DataChunk := DataChunkHandle.property

/-- Opaque DuckDB vector handle (wraps `duckdb_vector`). Unlike every other
    handle in this module, a vector is a *non-owning* pointer into its
    parent `DataChunk` — per `duckdb.h`'s own doc comment on
    `duckdb_data_chunk_get_vector`, "It does NOT need to be destroyed" — so
    this handle's GC finalizer releases only the Lean-side wrapper, never the
    underlying pointer. Only the handle itself is in scope here — the
    data/validity accessors built on top of it
    (`Database.DuckDB.FFI.Vector`/`Validity`) are out of scope for this
    batch; this port only needs the handle type as
    `Database.DuckDB.FFI.DataChunk.getVector`'s return type. Added for
    `Database.DuckDB.FFI.DataChunk` (module #6). -/
opaque VectorHandle : NonemptyType

/-- A column vector within a `DataChunk`, retrieved via
    `duckdb_data_chunk_get_vector`. -/
def Vector : Type := VectorHandle.type
instance : Nonempty Vector := VectorHandle.property

/-- Opaque DuckDB logical-type handle (wraps `duckdb_logical_type`; GC
    finalizer calls `duckdb_destroy_logical_type`). Only the handle itself is
    in scope here — building/inspecting logical types
    (`Database.DuckDB.FFI.LogicalTypes`) is out of scope for this batch; this
    port only needs the handle type as a parameter/return type in
    `Appender`/`Configuration`/`DataChunk`. Added for
    `Database.DuckDB.FFI.Appender` (module #2). -/
opaque LogicalTypeHandle : NonemptyType

/-- A DuckDB logical (SQL) type descriptor. -/
def LogicalType : Type := LogicalTypeHandle.type
instance : Nonempty LogicalType := LogicalTypeHandle.property

/-- Opaque DuckDB configuration handle (wraps `duckdb_config`; GC finalizer
    calls `duckdb_destroy_config`). Added for
    `Database.DuckDB.FFI.Configuration` (module #5). -/
opaque ConfigHandle : NonemptyType

/-- A start-up configuration object for `duckdb_open_ext`, created via
    `duckdb_create_config`. -/
def Config : Type := ConfigHandle.type
instance : Nonempty Config := ConfigHandle.property

/-- Opaque DuckDB configuration-option-descriptor handle (wraps
    `duckdb_config_option`; GC finalizer calls
    `duckdb_destroy_config_option`). Added for
    `Database.DuckDB.FFI.Configuration` (module #5). -/
opaque ConfigOptionHandle : NonemptyType

/-- A custom configuration-option descriptor, created via
    `duckdb_create_config_option` and registered with
    `duckdb_register_config_option`. -/
def ConfigOption : Type := ConfigOptionHandle.type
instance : Nonempty ConfigOption := ConfigOptionHandle.property

/-- Opaque DuckDB error-data handle (wraps `duckdb_error_data`; GC finalizer
    calls `duckdb_destroy_error_data`). Added for
    `Database.DuckDB.FFI.Appender` (module #2). -/
opaque ErrorDataHandle : NonemptyType

/-- A structured error object, e.g. as returned by
    `duckdb_appender_error_data`. -/
def ErrorData : Type := ErrorDataHandle.type
instance : Nonempty ErrorData := ErrorDataHandle.property

/-- Opaque DuckDB catalog handle (wraps `duckdb_catalog`; GC finalizer calls
    `duckdb_destroy_catalog`). Added for `Database.DuckDB.FFI.Catalog`
    (module #4). -/
opaque CatalogHandle : NonemptyType

/-- A named catalog within a database, retrieved via
    `duckdb_client_context_get_catalog`. -/
def Catalog : Type := CatalogHandle.type
instance : Nonempty Catalog := CatalogHandle.property

/-- Opaque DuckDB catalog-entry handle (wraps `duckdb_catalog_entry`; GC
    finalizer calls `duckdb_destroy_catalog_entry`). Added for
    `Database.DuckDB.FFI.Catalog` (module #4). -/
opaque CatalogEntryHandle : NonemptyType

/-- A single entry (table, view, schema, …) within a `Catalog`, retrieved via
    `duckdb_catalog_get_entry`. -/
def CatalogEntry : Type := CatalogEntryHandle.type
instance : Nonempty CatalogEntry := CatalogEntryHandle.property

/-- Opaque DuckDB query-result handle. Unlike every other handle in this
    module, `duckdb_result` is *not* a pointer-typedef in `duckdb.h` — it is a
    flat-by-value struct (`{ deprecated_column_count; …; internal_data; }`).
    This port's `ffi/duckdb_shim.c` mallocs a small wrapper that embeds a
    `duckdb_result` by value, so from Lean's point of view it is still just
    an opaque owning handle whose GC finalizer calls `duckdb_destroy_result`
    (then frees the wrapper). Added for `Database.DuckDB.FFI.ExecutePrepared`
    (module #8). -/
opaque ResultHandle : NonemptyType

/-- A materialized query result, produced e.g. by `duckdb_execute_prepared`. -/
def Result : Type := ResultHandle.type
instance : Nonempty Result := ResultHandle.property

/-- Opaque DuckDB file-system handle (wraps `duckdb_file_system`; GC finalizer
    calls `duckdb_destroy_file_system`). Added for
    `Database.DuckDB.FFI.FileSystem` (module #9). -/
opaque FileSystemHandle : NonemptyType

/-- A client context's virtual/attached file-system, retrieved via
    `duckdb_client_context_get_file_system`. -/
def FileSystem : Type := FileSystemHandle.type
instance : Nonempty FileSystem := FileSystemHandle.property

/-- Opaque DuckDB file-open-options handle (wraps `duckdb_file_open_options`;
    GC finalizer calls `duckdb_destroy_file_open_options`). Added for
    `Database.DuckDB.FFI.FileSystem` (module #9). -/
opaque FileOpenOptionsHandle : NonemptyType

/-- A mutable set of flags for `FileSystem.open`, created via
    `duckdb_create_file_open_options`. -/
def FileOpenOptions : Type := FileOpenOptionsHandle.type
instance : Nonempty FileOpenOptions := FileOpenOptionsHandle.property

/-- Opaque DuckDB file-handle handle (wraps `duckdb_file_handle`; GC finalizer
    calls `duckdb_destroy_file_handle`, which per `duckdb.h`'s own doc comment
    "will also close the file if it is still open"). Added for
    `Database.DuckDB.FFI.FileSystem` (module #9). -/
opaque FileHandleHandle : NonemptyType

/-- An open file obtained via `duckdb_file_system_open`. -/
def FileHandle : Type := FileHandleHandle.type
instance : Nonempty FileHandle := FileHandleHandle.property

/-- Opaque DuckDB log-storage handle (wraps `duckdb_log_storage`; GC finalizer
    calls `duckdb_destroy_log_storage`). Added for
    `Database.DuckDB.FFI.Logging` (module #11). -/
opaque LogStorageHandle : NonemptyType

/-- A custom log-storage backend, created via `duckdb_create_log_storage` and
    installed on a `Database` via `duckdb_register_log_storage`. -/
def LogStorage : Type := LogStorageHandle.type
instance : Nonempty LogStorage := LogStorageHandle.property

/-- Opaque wrapper around a bare `void*` returned by `duckdb_malloc`. GC
    finalizer calls `duckdb_free`. Added for `Database.DuckDB.FFI.Helpers`
    (module #10); kept purely for round-trip testability of
    `duckdb_malloc`/`duckdb_free` — without a byte-level peek/poke API (out
    of scope; belongs to the not-yet-ported `Vector`/`DataChunk` data
    accessors) there is little further use for the raw pointer itself. -/
opaque RawMemoryHandle : NonemptyType

/-- A raw memory block allocated via `duckdb_malloc`. -/
def RawMemory : Type := RawMemoryHandle.type
instance : Nonempty RawMemory := RawMemoryHandle.property

/-- Opaque DuckDB scalar-function handle (wraps `duckdb_scalar_function`; GC
    finalizer calls `duckdb_destroy_scalar_function`). Added for
    `Database.DuckDB.FFI.ScalarFunctions` (module #16). -/
opaque ScalarFunctionHandle : NonemptyType

/-- A user-defined scalar function being built up via `duckdb_create_scalar_
    function` and friends, not yet (or already) registered on a
    `Connection`. -/
def ScalarFunction : Type := ScalarFunctionHandle.type
instance : Nonempty ScalarFunction := ScalarFunctionHandle.property

/-- Opaque DuckDB scalar-function-set handle (wraps
    `duckdb_scalar_function_set`; GC finalizer calls
    `duckdb_destroy_scalar_function_set`). Added for
    `Database.DuckDB.FFI.ScalarFunctions` (module #16). -/
opaque ScalarFunctionSetHandle : NonemptyType

/-- A named group of `ScalarFunction` overloads, registered together via
    `duckdb_register_scalar_function_set`. -/
def ScalarFunctionSet : Type := ScalarFunctionSetHandle.type
instance : Nonempty ScalarFunctionSet := ScalarFunctionSetHandle.property

/-- Opaque handle for the `duckdb_data_chunk` DuckDB hands a scalar
    function's native callback on every invocation (the function's *input*
    chunk). Unlike `DataChunk` above, this handle is *non-owning* — DuckDB
    itself owns and frees the chunk once the callback returns, so this
    port's GC finalizer releases only the Lean-side wrapper, never the
    underlying pointer (the same non-owning treatment `VectorHandle` already
    gets). Kept as its own handle, distinct from `DataChunk`, precisely so
    that distinction is enforced by the type system rather than left to
    caller discipline. Added for `Database.DuckDB.FFI.ScalarFunctions`
    (module #16). -/
opaque BorrowedDataChunkHandle : NonemptyType

/-- A scalar function invocation's borrowed input chunk — see
    `BorrowedDataChunkHandle`'s doc comment. -/
def BorrowedDataChunk : Type := BorrowedDataChunkHandle.type
instance : Nonempty BorrowedDataChunk := BorrowedDataChunkHandle.property

/-- Opaque handle for a `duckdb_vector`'s validity (NULL) bitmask pointer, as
    returned by `duckdb_vector_get_validity`. Non-owning, like
    `VectorHandle`/`BorrowedDataChunkHandle`: the mask is owned by its parent
    `Vector`, not independently allocated. Kept as its own handle (rather
    than e.g. `Vector` itself) so `Database.DuckDB.FFI.Validity`, which only
    ever receives this raw mask pointer upstream, can depend on nothing but
    `Types` — exactly the flat, no-cross-imports shape
    `docs/imports/duckdb-ffi/dependencies.md` documents for this whole
    batch. Added for `Database.DuckDB.FFI.Validity` (module #17). -/
opaque ValidityMaskHandle : NonemptyType

/-- A vector's validity (NULL) bitmask, retrieved via
    `Database.DuckDB.FFI.Vector.getValidity`. -/
def ValidityMask : Type := ValidityMaskHandle.type
instance : Nonempty ValidityMask := ValidityMaskHandle.property

/-! ── Scalar types ── -/

/-- Unsigned index/size type used throughout the DuckDB C API (mirrors
    `idx_t`). -/
abbrev Idx : Type := UInt64

/-! ── Fixed-width temporal/numeric value structs ── -/
/- Added for `Database.DuckDB.FFI.Appender`/`BindValues` (modules #2/#3): the
   by-value structs those two modules' `append_*`/`bind_*` functions pass
   `duckdb.h`'s date/time/interval/(u)hugeint types by. Each mirrors its C
   struct field-for-field (checked against the pinned `v1.5.4` header). -/

/-- DATE, stored as days since 1970-01-01 (mirrors `duckdb_date`). -/
structure Date where
  days : Int32
  deriving BEq, Repr, Inhabited

/-- TIME, stored as microseconds since 00:00:00 (mirrors `duckdb_time`). -/
structure Time where
  micros : Int64
  deriving BEq, Repr, Inhabited

/-- TIMESTAMP, stored as microseconds since 1970-01-01 (mirrors
    `duckdb_timestamp`). -/
structure Timestamp where
  micros : Int64
  deriving BEq, Repr, Inhabited

/-- An INTERVAL value, stored as months/days/microseconds (mirrors
    `duckdb_interval`). -/
structure Interval where
  months : Int32
  days : Int32
  micros : Int64
  deriving BEq, Repr, Inhabited

/-- A 128-bit signed integer, `upper * 2^64 + lower` (mirrors
    `duckdb_hugeint`). -/
structure HugeInt where
  lower : UInt64
  upper : Int64
  deriving BEq, Repr, Inhabited

/-- A 128-bit unsigned integer, `upper * 2^64 + lower` (mirrors
    `duckdb_uhugeint`). -/
structure UHugeInt where
  lower : UInt64
  upper : UInt64
  deriving BEq, Repr, Inhabited

/-- A DECIMAL value: a `width`/`scale` pair plus its `HugeInt`-encoded value
    (mirrors `duckdb_decimal`). -/
structure Decimal where
  width : UInt8
  scale : UInt8
  value : HugeInt
  deriving BEq, Repr, Inhabited

/-! ── Decomposed date/time value structs ── -/
/- Added for `Database.DuckDB.FFI.Helpers` (module #10): the field-decomposed
   counterparts of `Date`/`Time`/`Timestamp` above, mirrored field-for-field
   against `duckdb.h`'s `duckdb_date_struct`/`duckdb_time_struct`/
   `duckdb_time_tz`/`duckdb_time_tz_struct`/`duckdb_timestamp_struct`. -/

/-- A DATE decomposed into year/month/day (mirrors `duckdb_date_struct`). -/
structure DateStruct where
  year : Int32
  month : Int8
  day : Int8
  deriving BEq, Repr, Inhabited

/-- A TIME decomposed into hour/minute/second/microsecond (mirrors
    `duckdb_time_struct`). -/
structure TimeStruct where
  hour : Int8
  min : Int8
  sec : Int8
  micros : Int32
  deriving BEq, Repr, Inhabited

/-- A TIME_TZ value, packed as 40 bits of microseconds plus 24 bits of
    timezone offset (mirrors `duckdb_time_tz`; opaque bit pattern, decompose
    via `Helpers.fromTimeTz`). -/
structure TimeTz where
  bits : UInt64
  deriving BEq, Repr, Inhabited

/-- A TIME_TZ decomposed into its `TimeStruct` and timezone offset (mirrors
    `duckdb_time_tz_struct`). -/
structure TimeTzStruct where
  time : TimeStruct
  offset : Int32
  deriving BEq, Repr, Inhabited

/-- A TIMESTAMP decomposed into its `DateStruct` and `TimeStruct` (mirrors
    `duckdb_timestamp_struct`). -/
structure TimestampStruct where
  date : DateStruct
  time : TimeStruct
  deriving BEq, Repr, Inhabited

/-! ── Configuration-option scope (`duckdb_config_option_scope`) ── -/

/-- The scope at which a configuration option applies, per `duckdb.h`'s
    `duckdb_config_option_scope` enum. Added for
    `Database.DuckDB.FFI.Configuration` (module #5). -/
inductive ConfigOptionScope where
  | invalid
  | localScope -- ^ named `localScope`, not `local` (a Lean keyword)
  | session
  | global
  | other (code : UInt32)
  deriving BEq, Repr, Inhabited

/-- Decode a raw `duckdb_config_option_scope` code. Total, per the same
    rationale as `Type_.ofUInt32`. -/
def ConfigOptionScope.ofUInt32 : UInt32 → ConfigOptionScope
  | 0 => .invalid
  | 1 => .localScope
  | 2 => .session
  | 3 => .global
  | n => .other n

/-- Encode a `ConfigOptionScope` back to its raw `duckdb_config_option_scope`
    code (round-trips `.other` through its original value). -/
def ConfigOptionScope.toUInt32 : ConfigOptionScope → UInt32
  | .invalid => 0
  | .localScope => 1
  | .session => 2
  | .global => 3
  | .other n => n

/-! ── Catalog entry kind (`duckdb_catalog_entry_type`) ── -/

/-- The kind of a catalog entry, per `duckdb.h`'s `duckdb_catalog_entry_type`
    enum. Added for `Database.DuckDB.FFI.Catalog` (module #4). -/
inductive CatalogEntryType where
  | invalid
  | table
  | schema
  | view
  | index
  | preparedStatement
  | sequence
  | collation
  | type
  | database
  | other (code : UInt32)
  deriving BEq, Repr, Inhabited

/-- Decode a raw `duckdb_catalog_entry_type` code. Total, per the same
    rationale as `Type_.ofUInt32`. -/
def CatalogEntryType.ofUInt32 : UInt32 → CatalogEntryType
  | 0 => .invalid
  | 1 => .table
  | 2 => .schema
  | 3 => .view
  | 4 => .index
  | 5 => .preparedStatement
  | 6 => .sequence
  | 7 => .collation
  | 8 => .type
  | 9 => .database
  | n => .other n

/-- Encode a `CatalogEntryType` back to its raw `duckdb_catalog_entry_type`
    code (round-trips `.other` through its original value). -/
def CatalogEntryType.toUInt32 : CatalogEntryType → UInt32
  | .invalid => 0
  | .table => 1
  | .schema => 2
  | .view => 3
  | .index => 4
  | .preparedStatement => 5
  | .sequence => 6
  | .collation => 7
  | .type => 8
  | .database => 9
  | .other n => n

/-! ── File-open flags (`duckdb_file_flag`) ── -/

/-- A single file-open capability flag, per `duckdb.h`'s `duckdb_file_flag`
    enum. Note that despite the C name, these are mutually-exclusive modes
    rather than independently-combinable bit flags (each is set one at a
    time via `FileSystem.setOpenFlag`). Added for
    `Database.DuckDB.FFI.FileSystem` (module #9). -/
inductive FileFlag where
  | invalid
  | read
  | write
  | create
  | createNew
  | append
  | other (code : UInt32)
  deriving BEq, Repr, Inhabited

/-- Decode a raw `duckdb_file_flag` code. Total, per the same rationale as
    `Type_.ofUInt32`. -/
def FileFlag.ofUInt32 : UInt32 → FileFlag
  | 0 => .invalid
  | 1 => .read
  | 2 => .write
  | 3 => .create
  | 4 => .createNew
  | 5 => .append
  | n => .other n

/-- Encode a `FileFlag` back to its raw `duckdb_file_flag` code (round-trips
    `.other` through its original value). -/
def FileFlag.toUInt32 : FileFlag → UInt32
  | .invalid => 0
  | .read => 1
  | .write => 2
  | .create => 3
  | .createNew => 4
  | .append => 5
  | .other n => n

/-! ── Result state (`duckdb_state`) ── -/

/-- Result state returned by most DuckDB C API calls. -/
inductive State where
  | success
  | error
  deriving BEq, Repr, Inhabited

/-- Decode a raw `duckdb_state`. Total: any value other than `0`
    (`DuckDBSuccess`) decodes to `.error`, matching every other raw code in
    this enum's C-side range (`duckdb_state` only ever takes the values `0`
    or `1`, but nothing in the API contract rules out a future third value
    meaning "still an error"). -/
def State.ofUInt32 : UInt32 → State
  | 0 => .success
  | _ => .error

/-- Encode a `State` back to its raw `duckdb_state` code. -/
def State.toUInt32 : State → UInt32
  | .success => 0
  | .error => 1

/-- `DuckDBSuccess` is the only success code. -/
def State.isSuccess : State → Bool
  | .success => true
  | .error => false

theorem State.success_isSuccess : State.success.isSuccess = true := rfl
theorem State.error_not_isSuccess : State.error.isSuccess = false := rfl

/-! ── Physical value type tags (`duckdb_type`) ── -/

/-- DuckDB's physical value type tags, per `duckdb.h`'s `duckdb_type` enum
    (checked against the pinned `v1.5.4` header downloaded by
    `lakefile.lean`). -/
inductive Type_ where
  | invalid
  | boolean
  | tinyInt
  | smallInt
  | integer
  | bigInt
  | uTinyInt
  | uSmallInt
  | uInteger
  | uBigInt
  | float
  | double
  | timestamp
  | date
  | time
  | interval
  | hugeInt
  | varchar
  | blob
  | decimal
  | timestampS
  | timestampMs
  | timestampNs
  | enum
  | list
  | struct
  | map
  | uuid
  | union
  | bit
  | timeTz
  | timestampTz
  | uHugeInt
  | array
  | any
  | varInt
  | sqlNull
  | other (code : UInt32) -- ^ Any other raw code (extension/future types)
  deriving BEq, Repr, Inhabited

/-- Decode a raw `duckdb_type` code. Total: unrecognized codes decode to
    `.other`, the same total-decoding pattern as
    `Linen.Database.SQLite.Bindings.Types.Error.ofUInt32`. -/
def Type_.ofUInt32 : UInt32 → Type_
  | 0 => .invalid
  | 1 => .boolean
  | 2 => .tinyInt
  | 3 => .smallInt
  | 4 => .integer
  | 5 => .bigInt
  | 6 => .uTinyInt
  | 7 => .uSmallInt
  | 8 => .uInteger
  | 9 => .uBigInt
  | 10 => .float
  | 11 => .double
  | 12 => .timestamp
  | 13 => .date
  | 14 => .time
  | 15 => .interval
  | 16 => .hugeInt
  | 17 => .varchar
  | 18 => .blob
  | 19 => .decimal
  | 20 => .timestampS
  | 21 => .timestampMs
  | 22 => .timestampNs
  | 23 => .enum
  | 24 => .list
  | 25 => .struct
  | 26 => .map
  | 27 => .uuid
  | 28 => .union
  | 29 => .bit
  | 30 => .timeTz
  | 31 => .timestampTz
  | 32 => .uHugeInt
  | 33 => .array
  | 34 => .any
  | 35 => .varInt
  | 36 => .sqlNull
  | n => .other n

/-- Encode a `Type_` back to its raw `duckdb_type` code (round-trips
    `.other` through its original value). -/
def Type_.toUInt32 : Type_ → UInt32
  | .invalid => 0
  | .boolean => 1
  | .tinyInt => 2
  | .smallInt => 3
  | .integer => 4
  | .bigInt => 5
  | .uTinyInt => 6
  | .uSmallInt => 7
  | .uInteger => 8
  | .uBigInt => 9
  | .float => 10
  | .double => 11
  | .timestamp => 12
  | .date => 13
  | .time => 14
  | .interval => 15
  | .hugeInt => 16
  | .varchar => 17
  | .blob => 18
  | .decimal => 19
  | .timestampS => 20
  | .timestampMs => 21
  | .timestampNs => 22
  | .enum => 23
  | .list => 24
  | .struct => 25
  | .map => 26
  | .uuid => 27
  | .union => 28
  | .bit => 29
  | .timeTz => 30
  | .timestampTz => 31
  | .uHugeInt => 32
  | .array => 33
  | .any => 34
  | .varInt => 35
  | .sqlNull => 36
  | .other n => n

/-! ── Error classification codes (`duckdb_error_type`) ── -/

/-- DuckDB's error classification codes, per `duckdb.h`'s `duckdb_error_type`
    enum. -/
inductive ErrorType where
  | invalid
  | outOfRange
  | conversion
  | unknownType
  | decimal
  | mismatchType
  | divideByZero
  | objectSize
  | invalidType
  | serialization
  | transaction
  | notImplemented
  | expression
  | catalog
  | parser
  | planner
  | scheduler
  | executor
  | constraint
  | index
  | stat
  | connection
  | syntax
  | settings
  | binder
  | network
  | optimizer
  | nullPointer
  | io
  | interrupt
  | fatal
  | internal
  | invalidInput
  | outOfMemory
  | permission
  | parameterNotResolved
  | parameterNotAllowed
  | dependency
  | http
  | missingExtension
  | autoload
  | sequence
  | invalidConfiguration
  | other (code : UInt32)
  deriving BEq, Repr, Inhabited

/-- Decode a raw `duckdb_error_type` code. Total, per the same rationale as
    `Type_.ofUInt32`. -/
def ErrorType.ofUInt32 : UInt32 → ErrorType
  | 0 => .invalid
  | 1 => .outOfRange
  | 2 => .conversion
  | 3 => .unknownType
  | 4 => .decimal
  | 5 => .mismatchType
  | 6 => .divideByZero
  | 7 => .objectSize
  | 8 => .invalidType
  | 9 => .serialization
  | 10 => .transaction
  | 11 => .notImplemented
  | 12 => .expression
  | 13 => .catalog
  | 14 => .parser
  | 15 => .planner
  | 16 => .scheduler
  | 17 => .executor
  | 18 => .constraint
  | 19 => .index
  | 20 => .stat
  | 21 => .connection
  | 22 => .syntax
  | 23 => .settings
  | 24 => .binder
  | 25 => .network
  | 26 => .optimizer
  | 27 => .nullPointer
  | 28 => .io
  | 29 => .interrupt
  | 30 => .fatal
  | 31 => .internal
  | 32 => .invalidInput
  | 33 => .outOfMemory
  | 34 => .permission
  | 35 => .parameterNotResolved
  | 36 => .parameterNotAllowed
  | 37 => .dependency
  | 38 => .http
  | 39 => .missingExtension
  | 40 => .autoload
  | 41 => .sequence
  | 42 => .invalidConfiguration
  | n => .other n

/-- Encode an `ErrorType` back to its raw `duckdb_error_type` code
    (round-trips `.other` through its original value). -/
def ErrorType.toUInt32 : ErrorType → UInt32
  | .invalid => 0
  | .outOfRange => 1
  | .conversion => 2
  | .unknownType => 3
  | .decimal => 4
  | .mismatchType => 5
  | .divideByZero => 6
  | .objectSize => 7
  | .invalidType => 8
  | .serialization => 9
  | .transaction => 10
  | .notImplemented => 11
  | .expression => 12
  | .catalog => 13
  | .parser => 14
  | .planner => 15
  | .scheduler => 16
  | .executor => 17
  | .constraint => 18
  | .index => 19
  | .stat => 20
  | .connection => 21
  | .syntax => 22
  | .settings => 23
  | .binder => 24
  | .network => 25
  | .optimizer => 26
  | .nullPointer => 27
  | .io => 28
  | .interrupt => 29
  | .fatal => 30
  | .internal => 31
  | .invalidInput => 32
  | .outOfMemory => 33
  | .permission => 34
  | .parameterNotResolved => 35
  | .parameterNotAllowed => 36
  | .dependency => 37
  | .http => 38
  | .missingExtension => 39
  | .autoload => 40
  | .sequence => 41
  | .invalidConfiguration => 42
  | .other n => n

/-! ── Statement classification codes (`duckdb_statement_type`) ── -/

/-- DuckDB's SQL-statement classification codes, per `duckdb.h`'s
    `duckdb_statement_type` enum. Added for
    `Database.DuckDB.FFI.PreparedStatements`/`QueryExecution` (modules #15,
    #18). -/
inductive StatementType where
  | invalid
  | select
  | insert
  | update
  | explain
  | delete
  | prepare
  | create
  | execute
  | alter
  | transaction
  | copy
  | analyze
  | variableSet
  | createFunc
  | drop
  | export
  | pragma
  | vacuum
  | call
  | set
  | load
  | relation
  | extension
  | logicalPlan
  | attach
  | detach
  | multi
  | other (code : UInt32)
  deriving BEq, Repr, Inhabited

/-- Decode a raw `duckdb_statement_type` code. Total, per the same rationale
    as `Type_.ofUInt32`. -/
def StatementType.ofUInt32 : UInt32 → StatementType
  | 0 => .invalid
  | 1 => .select
  | 2 => .insert
  | 3 => .update
  | 4 => .explain
  | 5 => .delete
  | 6 => .prepare
  | 7 => .create
  | 8 => .execute
  | 9 => .alter
  | 10 => .transaction
  | 11 => .copy
  | 12 => .analyze
  | 13 => .variableSet
  | 14 => .createFunc
  | 15 => .drop
  | 16 => .export
  | 17 => .pragma
  | 18 => .vacuum
  | 19 => .call
  | 20 => .set
  | 21 => .load
  | 22 => .relation
  | 23 => .extension
  | 24 => .logicalPlan
  | 25 => .attach
  | 26 => .detach
  | 27 => .multi
  | n => .other n

/-- Encode a `StatementType` back to its raw `duckdb_statement_type` code
    (round-trips `.other` through its original value). -/
def StatementType.toUInt32 : StatementType → UInt32
  | .invalid => 0
  | .select => 1
  | .insert => 2
  | .update => 3
  | .explain => 4
  | .delete => 5
  | .prepare => 6
  | .create => 7
  | .execute => 8
  | .alter => 9
  | .transaction => 10
  | .copy => 11
  | .analyze => 12
  | .variableSet => 13
  | .createFunc => 14
  | .drop => 15
  | .export => 16
  | .pragma => 17
  | .vacuum => 18
  | .call => 19
  | .set => 20
  | .load => 21
  | .relation => 22
  | .extension => 23
  | .logicalPlan => 24
  | .attach => 25
  | .detach => 26
  | .multi => 27
  | .other n => n

/-! ── Query-progress snapshot (`duckdb_query_progress_type`) ── -/

/-- A snapshot of a running query's execution progress, per `duckdb.h`'s
    `duckdb_query_progress_type` (`{ percentage; rows_processed;
    total_rows_to_process }`). Returned by value from `duckdb_query_progress`
    — unlike `duckdb-haskell`, which routes this through an extra
    `wrapped_duckdb_query_progress` C shim (apparently to dodge a GHC-FFI
    struct-return-by-value limitation), this port's own `ffi/duckdb_shim.c`
    can just unpack the by-value result directly into three plain return
    values, so no such wrapper is needed here. -/
structure QueryProgress where
  /-- Percentage complete, or `-1` if no query is running / no progress is
      available. -/
  percentage : Float
  /-- Rows processed so far. -/
  rowsProcessed : UInt64
  /-- Total rows expected to be processed. -/
  totalRowsToProcess : UInt64
  deriving BEq, Repr, Inhabited

end Database.DuckDB.FFI.Types
