/-
  Linen.Database.DuckDB.FFI.Appender — bulk-append API

  Mirrors Haskell's `Database.DuckDB.FFI.Appender` (the `duckdb-ffi`
  package). Module #2 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1). *Load-bearing for
  `duckdb-simple`'s `Database.DuckDB.Simple.Copy`.*

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`. This
  is the largest module in this batch (upstream exposes 39 raw entry
  points); it splits cleanly into three groups:

  - **Lifecycle**: `create`/`createExt`/`createQuery` build an `Appender`
    bound to an existing table (or, for `createQuery`, a bespoke
    INSERT/UPDATE/DELETE/MERGE statement); `flush`/`close`/`destroy`/`clear`
    tear it back down; `columnCount`/`columnType`/`errorData` inspect it.
  - **Row shape**: `addColumn`/`clearColumns` adjust the active column list;
    `beginRow`/`endRow` delimit one row of appends.
  - **Per-cell appends**: one `append*` function per DuckDB physical type,
    plus `appendDefault(ToChunk)`/`appendNull`/`appendValue`/
    `appendDataChunk` for the non-scalar cases. Every one returns a raw
    `duckdb_state`, decoded to this port's `State` — mirroring
    `Database.DuckDB.FFI.BindValues`'s uniform treatment of its own
    `bind*` family, and for the same reason: upstream's C API surfaces no
    richer per-call error here either (`errorData` above is the appender's
    error channel once something goes wrong).

  `createQuery`'s upstream `Ptr DuckDBLogicalType` (`types`) and
  `Ptr CString` (`column_names`) array parameters are bound here as a plain
  `Array LogicalType` and `Option (Array String)` respectively — the C shim
  builds the transient C arrays from them and frees that scratch memory
  immediately after the call returns, the same treatment
  `Database.DuckDB.FFI.DataChunk.createDataChunk` gives its own
  `Ptr DuckDBLogicalType` array parameter.
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.Appender

open Database.DuckDB.FFI.Types

/-! ── Lifecycle: create ── -/

/-- Raw `duckdb_appender_create`: `(state, appender?)`. -/
@[extern "linen_duckdb_appender_create"]
opaque createRaw (connection : @& Connection) (schema : @& Option String) (table : @& String) :
    IO (UInt32 × Option Appender)

/-- Create an appender bound to `table` in `schema` (or the default schema,
    if `schema := none`) on `connection`. The resulting `Appender` must
    eventually be destroyed with `destroy` (or let its GC finalizer do so). -/
def create (connection : Connection) (schema : Option String) (table : String) :
    IO (Except String Appender) := do
  let (rc, appOpt) ← createRaw connection schema table
  match State.ofUInt32 rc, appOpt with
  | .success, some app => pure (.ok app)
  | _, _ => pure (.error "duckdb_appender_create failed")

/-- Raw `duckdb_appender_create_ext`: `(state, appender?)`. -/
@[extern "linen_duckdb_appender_create_ext"]
opaque createExtRaw (connection : @& Connection) (catalog : @& Option String)
    (schema : @& Option String) (table : @& String) : IO (UInt32 × Option Appender)

/-- Create an appender bound to `table` in `schema`/`catalog` (or the
    default schema/catalog, for either `none`) on `connection`. -/
def createExt (connection : Connection) (catalog : Option String) (schema : Option String)
    (table : String) : IO (Except String Appender) := do
  let (rc, appOpt) ← createExtRaw connection catalog schema table
  match State.ofUInt32 rc, appOpt with
  | .success, some app => pure (.ok app)
  | _, _ => pure (.error "duckdb_appender_create_ext failed")

/-- Raw `duckdb_appender_create_query`: `(state, appender?)`. -/
@[extern "linen_duckdb_appender_create_query"]
opaque createQueryRaw (connection : @& Connection) (query : @& String) (types : @& Array LogicalType)
    (tableName : @& Option String) (columnNames : @& Option (Array String)) :
    IO (UInt32 × Option Appender)

/-- Create an appender that executes `query` (an INSERT/DELETE/UPDATE/MERGE
    INTO statement) with whatever data is appended to it, appending
    `types`-typed columns. `tableName`/`columnNames` name the appended data
    within `query` (defaulting, per upstream, to `"appended_data"`/
    `"col1"`, `"col2"`, … when `none`). -/
def createQuery (connection : Connection) (query : String) (types : Array LogicalType)
    (tableName : Option String := none) (columnNames : Option (Array String) := none) :
    IO (Except String Appender) := do
  let (rc, appOpt) ← createQueryRaw connection query types tableName columnNames
  match State.ofUInt32 rc, appOpt with
  | .success, some app => pure (.ok app)
  | _, _ => pure (.error "duckdb_appender_create_query failed")

/-! ── Lifecycle: inspect / tear down ── -/

/-- The number of columns belonging to `appender` (its active column list,
    or the bound table's physical columns if there is none). -/
@[extern "linen_duckdb_appender_column_count"]
opaque columnCount : Appender → IO Idx

/-- The logical type of the column at `colIdx` in `appender`. The result
    must eventually be destroyed with
    `Database.DuckDB.FFI.Types.LogicalTypeHandle`'s GC finalizer (or an
    explicit `duckdb_destroy_logical_type` call, not yet bound in this
    batch — see `Database.DuckDB.FFI.DataChunk`'s doc comment for why). -/
@[extern "linen_duckdb_appender_column_type"]
opaque columnType (appender : @& Appender) (colIdx : Idx) : IO LogicalType

/-- The structured error data associated with `appender` (e.g. after a
    failed `flush`/`close`). Must eventually be destroyed (its handle's GC
    finalizer calls `duckdb_destroy_error_data` if never done explicitly). -/
@[extern "linen_duckdb_appender_error_data"]
opaque errorData : Appender → IO ErrorData

/-- Raw `duckdb_appender_clear`. -/
@[extern "linen_duckdb_appender_clear"]
opaque clearRaw : Appender → IO UInt32

/-- Clear `appender`'s buffered state without destroying it. -/
def clear (appender : Appender) : IO State := State.ofUInt32 <$> clearRaw appender

/-- Raw `duckdb_appender_flush`. -/
@[extern "linen_duckdb_appender_flush"]
opaque flushRaw : Appender → IO UInt32

/-- Flush `appender`'s cache to the table. On failure (e.g. a constraint
    violation), all buffered data is invalidated and no more values can be
    appended — call `errorData` to inspect the failure, then `destroy`. -/
def flush (appender : Appender) : IO State := State.ofUInt32 <$> flushRaw appender

/-- Raw `duckdb_appender_close`. -/
@[extern "linen_duckdb_appender_close"]
opaque closeRaw : Appender → IO UInt32

/-- Flush all of `appender`'s intermediate state and close it for further
    appends, without destroying it. On failure, same caveats as `flush`. -/
def close (appender : Appender) : IO State := State.ofUInt32 <$> closeRaw appender

/-- Flush `appender`'s intermediate state, close it, and destroy it,
    deallocating all associated memory. Idempotent, like
    `Database.DuckDB.FFI.OpenConnect.close`. Prefer `close` (to inspect
    `errorData` on failure) before this, if you need error detail. -/
@[extern "linen_duckdb_appender_destroy"]
opaque destroy : Appender → IO Unit

/-! ── Row shape ── -/

/-- Raw `duckdb_appender_add_column`. -/
@[extern "linen_duckdb_appender_add_column"]
opaque addColumnRaw (appender : @& Appender) (name : @& String) : IO UInt32

/-- Append `name` to `appender`'s active column list (immediately flushing
    all previously-buffered data). Any column absent from the active list
    is filled with its default value, or `NULL`, when flushed. -/
def addColumn (appender : Appender) (name : String) : IO State :=
  State.ofUInt32 <$> addColumnRaw appender name

/-- Raw `duckdb_appender_clear_columns`. -/
@[extern "linen_duckdb_appender_clear_columns"]
opaque clearColumnsRaw : Appender → IO UInt32

/-- Remove all columns from `appender`'s active column list, resetting it to
    treat every column as active (immediately flushing all previously
    -buffered data). -/
def clearColumns (appender : Appender) : IO State :=
  State.ofUInt32 <$> clearColumnsRaw appender

/-- Raw `duckdb_appender_begin_row`. A backwards-compatibility no-op —
    upstream documents only `endRow` as actually required. -/
@[extern "linen_duckdb_appender_begin_row"]
opaque beginRowRaw : Appender → IO UInt32

def beginRow (appender : Appender) : IO State := State.ofUInt32 <$> beginRowRaw appender

/-- Raw `duckdb_appender_end_row`. -/
@[extern "linen_duckdb_appender_end_row"]
opaque endRowRaw : Appender → IO UInt32

/-- Finish the row of appends started (implicitly, or via `beginRow`) since
    the last `endRow`. After this, the next row can be appended. -/
def endRow (appender : Appender) : IO State := State.ofUInt32 <$> endRowRaw appender

/-! ── Per-cell appends: defaults / NULL / boxed values ── -/

/-- Raw `duckdb_append_default`. -/
@[extern "linen_duckdb_append_default"]
opaque appendDefaultRaw : Appender → IO UInt32

/-- Append the column's `DEFAULT` value (or `NULL`, if it has none) to
    `appender`. -/
def appendDefault (appender : Appender) : IO State :=
  State.ofUInt32 <$> appendDefaultRaw appender

/-- Raw `duckdb_append_default_to_chunk`. -/
@[extern "linen_duckdb_append_default_to_chunk"]
opaque appendDefaultToChunkRaw (appender : @& Appender) (chunk : @& DataChunk) (col row : Idx) :
    IO UInt32

/-- Append the column's `DEFAULT` value (a constant only — no
    non-deterministic expressions like `nextval`/`random`) at `(col, row)`
    in `chunk`, using `appender`'s bound table to look the default up. -/
def appendDefaultToChunk (appender : Appender) (chunk : DataChunk) (col row : Idx) : IO State :=
  State.ofUInt32 <$> appendDefaultToChunkRaw appender chunk col row

/-- Raw `duckdb_append_null`. -/
@[extern "linen_duckdb_append_null"]
opaque appendNullRaw : Appender → IO UInt32

/-- Append a SQL `NULL` (of any type) to `appender`. -/
def appendNull (appender : Appender) : IO State := State.ofUInt32 <$> appendNullRaw appender

/-- Raw `duckdb_append_value`. -/
@[extern "linen_duckdb_append_value"]
opaque appendValueRaw (appender : @& Appender) (value : @& Value) : IO UInt32

/-- Append a boxed `Value` to `appender`. -/
def appendValue (appender : Appender) (value : Value) : IO State :=
  State.ofUInt32 <$> appendValueRaw appender value

/-- Raw `duckdb_append_data_chunk`. -/
@[extern "linen_duckdb_append_data_chunk"]
opaque appendDataChunkRaw (appender : @& Appender) (chunk : @& DataChunk) : IO UInt32

/-- Append a pre-filled `DataChunk` to `appender` in one call (casting
    between the chunk's and the appender's active types, if they don't
    already match). -/
def appendDataChunk (appender : Appender) (chunk : DataChunk) : IO State :=
  State.ofUInt32 <$> appendDataChunkRaw appender chunk

/-! ── Per-cell appends: scalars ── -/

/-- Raw `duckdb_append_bool`. -/
@[extern "linen_duckdb_append_bool"]
opaque appendBoolRaw (appender : @& Appender) (value : UInt8) : IO UInt32

def appendBool (appender : Appender) (value : Bool) : IO State :=
  State.ofUInt32 <$> appendBoolRaw appender (if value then 1 else 0)

/-- Raw `duckdb_append_int8`. -/
@[extern "linen_duckdb_append_int8"]
opaque appendInt8Raw (appender : @& Appender) (value : Int8) : IO UInt32

def appendInt8 (appender : Appender) (value : Int8) : IO State :=
  State.ofUInt32 <$> appendInt8Raw appender value

/-- Raw `duckdb_append_int16`. -/
@[extern "linen_duckdb_append_int16"]
opaque appendInt16Raw (appender : @& Appender) (value : Int16) : IO UInt32

def appendInt16 (appender : Appender) (value : Int16) : IO State :=
  State.ofUInt32 <$> appendInt16Raw appender value

/-- Raw `duckdb_append_int32`. -/
@[extern "linen_duckdb_append_int32"]
opaque appendInt32Raw (appender : @& Appender) (value : Int32) : IO UInt32

def appendInt32 (appender : Appender) (value : Int32) : IO State :=
  State.ofUInt32 <$> appendInt32Raw appender value

/-- Raw `duckdb_append_int64`. -/
@[extern "linen_duckdb_append_int64"]
opaque appendInt64Raw (appender : @& Appender) (value : Int64) : IO UInt32

def appendInt64 (appender : Appender) (value : Int64) : IO State :=
  State.ofUInt32 <$> appendInt64Raw appender value

/-- Raw `duckdb_append_hugeint`, with `value`'s `lower`/`upper` fields passed
    as separate scalar arguments (see
    `Database.DuckDB.FFI.BindValues.bindHugeIntRaw`'s doc comment for why —
    the same treatment applies to every multi-field value type appended
    below: `HugeInt`, `UHugeInt`, `Interval`). -/
@[extern "linen_duckdb_append_hugeint"]
opaque appendHugeIntRaw (appender : @& Appender) (lower : UInt64) (upper : Int64) : IO UInt32

def appendHugeInt (appender : Appender) (value : HugeInt) : IO State :=
  State.ofUInt32 <$> appendHugeIntRaw appender value.lower value.upper

/-- Raw `duckdb_append_uint8`. -/
@[extern "linen_duckdb_append_uint8"]
opaque appendUInt8Raw (appender : @& Appender) (value : UInt8) : IO UInt32

def appendUInt8 (appender : Appender) (value : UInt8) : IO State :=
  State.ofUInt32 <$> appendUInt8Raw appender value

/-- Raw `duckdb_append_uint16`. -/
@[extern "linen_duckdb_append_uint16"]
opaque appendUInt16Raw (appender : @& Appender) (value : UInt16) : IO UInt32

def appendUInt16 (appender : Appender) (value : UInt16) : IO State :=
  State.ofUInt32 <$> appendUInt16Raw appender value

/-- Raw `duckdb_append_uint32`. -/
@[extern "linen_duckdb_append_uint32"]
opaque appendUInt32Raw (appender : @& Appender) (value : UInt32) : IO UInt32

def appendUInt32 (appender : Appender) (value : UInt32) : IO State :=
  State.ofUInt32 <$> appendUInt32Raw appender value

/-- Raw `duckdb_append_uint64`. -/
@[extern "linen_duckdb_append_uint64"]
opaque appendUInt64Raw (appender : @& Appender) (value : UInt64) : IO UInt32

def appendUInt64 (appender : Appender) (value : UInt64) : IO State :=
  State.ofUInt32 <$> appendUInt64Raw appender value

/-- Raw `duckdb_append_uhugeint` (see `appendHugeIntRaw`'s doc comment). -/
@[extern "linen_duckdb_append_uhugeint"]
opaque appendUHugeIntRaw (appender : @& Appender) (lower upper : UInt64) : IO UInt32

def appendUHugeInt (appender : Appender) (value : UHugeInt) : IO State :=
  State.ofUInt32 <$> appendUHugeIntRaw appender value.lower value.upper

/-- Raw `duckdb_append_float`. -/
@[extern "linen_duckdb_append_float"]
opaque appendFloatRaw (appender : @& Appender) (value : Float32) : IO UInt32

def appendFloat (appender : Appender) (value : Float32) : IO State :=
  State.ofUInt32 <$> appendFloatRaw appender value

/-- Raw `duckdb_append_double`. -/
@[extern "linen_duckdb_append_double"]
opaque appendDoubleRaw (appender : @& Appender) (value : Float) : IO UInt32

def appendDouble (appender : Appender) (value : Float) : IO State :=
  State.ofUInt32 <$> appendDoubleRaw appender value

/-! ── Per-cell appends: temporal ── -/

/-- Raw `duckdb_append_date`. -/
@[extern "linen_duckdb_append_date"]
opaque appendDateRaw (appender : @& Appender) (value : @& Date) : IO UInt32

def appendDate (appender : Appender) (value : Date) : IO State :=
  State.ofUInt32 <$> appendDateRaw appender value

/-- Raw `duckdb_append_time`. -/
@[extern "linen_duckdb_append_time"]
opaque appendTimeRaw (appender : @& Appender) (value : @& Time) : IO UInt32

def appendTime (appender : Appender) (value : Time) : IO State :=
  State.ofUInt32 <$> appendTimeRaw appender value

/-- Raw `duckdb_append_timestamp`. -/
@[extern "linen_duckdb_append_timestamp"]
opaque appendTimestampRaw (appender : @& Appender) (value : @& Timestamp) : IO UInt32

def appendTimestamp (appender : Appender) (value : Timestamp) : IO State :=
  State.ofUInt32 <$> appendTimestampRaw appender value

/-- Raw `duckdb_append_interval` (see `appendHugeIntRaw`'s doc comment). -/
@[extern "linen_duckdb_append_interval"]
opaque appendIntervalRaw (appender : @& Appender) (months days : Int32) (micros : Int64) :
    IO UInt32

def appendInterval (appender : Appender) (value : Interval) : IO State :=
  State.ofUInt32 <$> appendIntervalRaw appender value.months value.days value.micros

/-! ── Per-cell appends: strings / blobs ── -/

/-- Raw `duckdb_append_varchar`. -/
@[extern "linen_duckdb_append_varchar"]
opaque appendVarcharRaw (appender : @& Appender) (value : @& String) : IO UInt32

def appendVarchar (appender : Appender) (value : String) : IO State :=
  State.ofUInt32 <$> appendVarcharRaw appender value

/-- Raw `duckdb_append_varchar_length` (see
    `Database.DuckDB.FFI.BindValues.bindVarcharLength`'s doc comment for why
    this always passes `value`'s own full UTF-8 byte length). -/
@[extern "linen_duckdb_append_varchar_length"]
opaque appendVarcharLengthRaw (appender : @& Appender) (value : @& String) (length : Idx) :
    IO UInt32

def appendVarcharLength (appender : Appender) (value : String) : IO State :=
  State.ofUInt32 <$> appendVarcharLengthRaw appender value value.utf8ByteSize.toUInt64

/-- Raw `duckdb_append_blob`. -/
@[extern "linen_duckdb_append_blob"]
opaque appendBlobRaw (appender : @& Appender) (value : @& ByteArray) : IO UInt32

def appendBlob (appender : Appender) (value : ByteArray) : IO State :=
  State.ofUInt32 <$> appendBlobRaw appender value

end Database.DuckDB.FFI.Appender
