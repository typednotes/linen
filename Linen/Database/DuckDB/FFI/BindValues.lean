/-
  Linen.Database.DuckDB.FFI.BindValues — prepared-statement parameter binding

  Mirrors Haskell's `Database.DuckDB.FFI.BindValues` (the `duckdb-ffi`
  package). Module #3 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1).

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`. This
  module is a flat, uniform family: every `bind*` function takes a
  `PreparedStatement`, a 1-indexed parameter `Idx`, and a value, and returns
  a raw `duckdb_state` — ported here decoded to this port's `State`, the
  same treatment every other bare-state-returning call in this batch gets
  (`Configuration.setConfig`, `Catalog`'s destructors' *lack* of one, …).
  Upstream's C API surfaces no further per-call error detail here (that's
  what `Database.DuckDB.FFI.ErrorData`, one layer up in `PreparedStatements`/
  `ExecutePrepared`, is for) — so, faithfully, neither does this port.

  `duckdb_bind_timestamp_tz` binds a timezone-aware `TIMESTAMPTZ` parameter,
  but is passed the *same* `duckdb_timestamp` (microseconds-since-epoch)
  representation as `duckdb_bind_timestamp` — this port's `bindTimestampTz`
  therefore takes a `Timestamp` too, exactly mirroring upstream's own
  `DuckDBTimestamp` parameter type for both.
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.BindValues

open Database.DuckDB.FFI.Types

/-! ── Boxed value / parameter lookup ── -/

/-- Raw `duckdb_bind_value`. -/
@[extern "linen_duckdb_bind_value"]
opaque bindValueRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : @& Value) : IO UInt32

/-- Bind the boxed `Value` `value` to `stmt`'s parameter at (1-indexed)
    `paramIdx`. -/
def bindValue (stmt : PreparedStatement) (paramIdx : Idx) (value : Value) : IO State :=
  State.ofUInt32 <$> bindValueRaw stmt paramIdx value

/-- Raw `duckdb_bind_parameter_index`: `(state, index?)`. -/
@[extern "linen_duckdb_bind_parameter_index"]
opaque bindParameterIndexRaw (stmt : @& PreparedStatement) (name : @& String) :
    IO (UInt32 × Option Idx)

/-- The (1-indexed) parameter index of `stmt`'s named parameter `name`
    (e.g. `$foo` for `name := "foo"`). Fails if no such named parameter
    exists. -/
def bindParameterIndex (stmt : PreparedStatement) (name : String) : IO (Except String Idx) := do
  let (rc, idxOpt) ← bindParameterIndexRaw stmt name
  match State.ofUInt32 rc, idxOpt with
  | .success, some idx => pure (.ok idx)
  | _, _ => pure (.error s!"duckdb_bind_parameter_index failed for {name}")

/-! ── Primitive scalars ── -/

/-- Raw `duckdb_bind_boolean`. -/
@[extern "linen_duckdb_bind_boolean"]
opaque bindBooleanRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : UInt8) : IO UInt32

/-- Bind a `Bool` to `stmt`'s parameter at `paramIdx`. -/
def bindBoolean (stmt : PreparedStatement) (paramIdx : Idx) (value : Bool) : IO State :=
  State.ofUInt32 <$> bindBooleanRaw stmt paramIdx (if value then 1 else 0)

/-- Bind an `Int8` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_int8"]
opaque bindInt8Raw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : Int8) : IO UInt32

def bindInt8 (stmt : PreparedStatement) (paramIdx : Idx) (value : Int8) : IO State :=
  State.ofUInt32 <$> bindInt8Raw stmt paramIdx value

/-- Bind an `Int16` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_int16"]
opaque bindInt16Raw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : Int16) : IO UInt32

def bindInt16 (stmt : PreparedStatement) (paramIdx : Idx) (value : Int16) : IO State :=
  State.ofUInt32 <$> bindInt16Raw stmt paramIdx value

/-- Bind an `Int32` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_int32"]
opaque bindInt32Raw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : Int32) : IO UInt32

def bindInt32 (stmt : PreparedStatement) (paramIdx : Idx) (value : Int32) : IO State :=
  State.ofUInt32 <$> bindInt32Raw stmt paramIdx value

/-- Bind an `Int64` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_int64"]
opaque bindInt64Raw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : Int64) : IO UInt32

def bindInt64 (stmt : PreparedStatement) (paramIdx : Idx) (value : Int64) : IO State :=
  State.ofUInt32 <$> bindInt64Raw stmt paramIdx value

/-- Raw `duckdb_bind_hugeint`, with `value`'s `lower`/`upper` fields passed as
    separate scalar arguments — `ffi/duckdb_shim.c` reassembles the
    `duckdb_hugeint` C struct from them on the other side. This sidesteps
    ever having to peek a multi-field Lean structure's packed-scalar layout
    from C: every multi-field value type in this module (`HugeInt`,
    `UHugeInt`, `Decimal`, `Interval`) is decomposed into scalar arguments
    the same way, mirroring how `Types.lean`'s own `queryProgressRaw`
    (`Database.DuckDB.FFI.OpenConnect`) returns its `QueryProgress` fields as
    a plain scalar tuple rather than a boxed struct. -/
@[extern "linen_duckdb_bind_hugeint"]
opaque bindHugeIntRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (lower : UInt64)
    (upper : Int64) : IO UInt32

/-- Bind a `HugeInt` to `stmt`'s parameter at `paramIdx`. -/
def bindHugeInt (stmt : PreparedStatement) (paramIdx : Idx) (value : HugeInt) : IO State :=
  State.ofUInt32 <$> bindHugeIntRaw stmt paramIdx value.lower value.upper

/-- Raw `duckdb_bind_uhugeint` (see `bindHugeIntRaw`'s doc comment). -/
@[extern "linen_duckdb_bind_uhugeint"]
opaque bindUHugeIntRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (lower upper : UInt64) :
    IO UInt32

/-- Bind a `UHugeInt` to `stmt`'s parameter at `paramIdx`. -/
def bindUHugeInt (stmt : PreparedStatement) (paramIdx : Idx) (value : UHugeInt) : IO State :=
  State.ofUInt32 <$> bindUHugeIntRaw stmt paramIdx value.lower value.upper

/-- Raw `duckdb_bind_decimal` (see `bindHugeIntRaw`'s doc comment). -/
@[extern "linen_duckdb_bind_decimal"]
opaque bindDecimalRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (width scale : UInt8)
    (lower : UInt64) (upper : Int64) : IO UInt32

/-- Bind a `Decimal` to `stmt`'s parameter at `paramIdx`. -/
def bindDecimal (stmt : PreparedStatement) (paramIdx : Idx) (value : Decimal) : IO State :=
  State.ofUInt32 <$>
    bindDecimalRaw stmt paramIdx value.width value.scale value.value.lower value.value.upper

/-- Bind a `UInt8` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_uint8"]
opaque bindUInt8Raw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : UInt8) : IO UInt32

def bindUInt8 (stmt : PreparedStatement) (paramIdx : Idx) (value : UInt8) : IO State :=
  State.ofUInt32 <$> bindUInt8Raw stmt paramIdx value

/-- Bind a `UInt16` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_uint16"]
opaque bindUInt16Raw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : UInt16) : IO UInt32

def bindUInt16 (stmt : PreparedStatement) (paramIdx : Idx) (value : UInt16) : IO State :=
  State.ofUInt32 <$> bindUInt16Raw stmt paramIdx value

/-- Bind a `UInt32` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_uint32"]
opaque bindUInt32Raw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : UInt32) : IO UInt32

def bindUInt32 (stmt : PreparedStatement) (paramIdx : Idx) (value : UInt32) : IO State :=
  State.ofUInt32 <$> bindUInt32Raw stmt paramIdx value

/-- Bind a `UInt64` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_uint64"]
opaque bindUInt64Raw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : UInt64) : IO UInt32

def bindUInt64 (stmt : PreparedStatement) (paramIdx : Idx) (value : UInt64) : IO State :=
  State.ofUInt32 <$> bindUInt64Raw stmt paramIdx value

/-- Bind a `Float32` to `stmt`'s parameter at `paramIdx` (mirrors upstream's
    `CFloat`; DuckDB's `FLOAT` is single-precision). -/
@[extern "linen_duckdb_bind_float"]
opaque bindFloatRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : Float32) : IO UInt32

def bindFloat (stmt : PreparedStatement) (paramIdx : Idx) (value : Float32) : IO State :=
  State.ofUInt32 <$> bindFloatRaw stmt paramIdx value

/-- Bind a `Float` to `stmt`'s parameter at `paramIdx` (DuckDB's `DOUBLE`). -/
@[extern "linen_duckdb_bind_double"]
opaque bindDoubleRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : Float) : IO UInt32

def bindDouble (stmt : PreparedStatement) (paramIdx : Idx) (value : Float) : IO State :=
  State.ofUInt32 <$> bindDoubleRaw stmt paramIdx value

/-! ── Temporal ── -/

/-- Bind a `Date` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_date"]
opaque bindDateRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : @& Date) : IO UInt32

def bindDate (stmt : PreparedStatement) (paramIdx : Idx) (value : Date) : IO State :=
  State.ofUInt32 <$> bindDateRaw stmt paramIdx value

/-- Bind a `Time` to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_time"]
opaque bindTimeRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : @& Time) : IO UInt32

def bindTime (stmt : PreparedStatement) (paramIdx : Idx) (value : Time) : IO State :=
  State.ofUInt32 <$> bindTimeRaw stmt paramIdx value

/-- Bind a `Timestamp` to `stmt`'s parameter at `paramIdx` (`TIMESTAMP`). -/
@[extern "linen_duckdb_bind_timestamp"]
opaque bindTimestampRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : @& Timestamp) :
    IO UInt32

def bindTimestamp (stmt : PreparedStatement) (paramIdx : Idx) (value : Timestamp) : IO State :=
  State.ofUInt32 <$> bindTimestampRaw stmt paramIdx value

/-- Bind a `Timestamp` to `stmt`'s parameter at `paramIdx` as `TIMESTAMPTZ`
    (see the module doc comment for why this also takes a plain
    `Timestamp`). -/
@[extern "linen_duckdb_bind_timestamp_tz"]
opaque bindTimestampTzRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : @& Timestamp) :
    IO UInt32

def bindTimestampTz (stmt : PreparedStatement) (paramIdx : Idx) (value : Timestamp) : IO State :=
  State.ofUInt32 <$> bindTimestampTzRaw stmt paramIdx value

/-- Raw `duckdb_bind_interval` (see `bindHugeIntRaw`'s doc comment). -/
@[extern "linen_duckdb_bind_interval"]
opaque bindIntervalRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (months days : Int32)
    (micros : Int64) : IO UInt32

/-- Bind an `Interval` to `stmt`'s parameter at `paramIdx`. -/
def bindInterval (stmt : PreparedStatement) (paramIdx : Idx) (value : Interval) : IO State :=
  State.ofUInt32 <$> bindIntervalRaw stmt paramIdx value.months value.days value.micros

/-! ── Strings / blobs / NULL ── -/

/-- Bind a null-terminated `String` (`VARCHAR`) to `stmt`'s parameter at
    `paramIdx`. -/
@[extern "linen_duckdb_bind_varchar"]
opaque bindVarcharRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : @& String) :
    IO UInt32

def bindVarchar (stmt : PreparedStatement) (paramIdx : Idx) (value : String) : IO State :=
  State.ofUInt32 <$> bindVarcharRaw stmt paramIdx value

/-- Bind an explicitly-length-bounded `String` (`VARCHAR`) to `stmt`'s
    parameter at `paramIdx`. Ported for API parity with upstream's
    `duckdb_bind_varchar_length` (which lets a caller bind a byte range
    shorter than the full string); this wrapper always passes the whole
    `String`'s UTF-8 byte length, since Lean's `String` has no sub-slice
    view distinct from `bindVarchar`'s plain `String` here. -/
@[extern "linen_duckdb_bind_varchar_length"]
opaque bindVarcharLengthRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : @& String)
    (length : Idx) : IO UInt32

def bindVarcharLength (stmt : PreparedStatement) (paramIdx : Idx) (value : String) : IO State :=
  State.ofUInt32 <$> bindVarcharLengthRaw stmt paramIdx value value.utf8ByteSize.toUInt64

/-- Bind a `ByteArray` (`BLOB`) to `stmt`'s parameter at `paramIdx`. -/
@[extern "linen_duckdb_bind_blob"]
opaque bindBlobRaw (stmt : @& PreparedStatement) (paramIdx : Idx) (value : @& ByteArray) :
    IO UInt32

def bindBlob (stmt : PreparedStatement) (paramIdx : Idx) (value : ByteArray) : IO State :=
  State.ofUInt32 <$> bindBlobRaw stmt paramIdx value

/-- Raw `duckdb_bind_null`. -/
@[extern "linen_duckdb_bind_null"]
opaque bindNullRaw (stmt : @& PreparedStatement) (paramIdx : Idx) : IO UInt32

/-- Bind a SQL `NULL` (of any type) to `stmt`'s parameter at `paramIdx`. -/
def bindNull (stmt : PreparedStatement) (paramIdx : Idx) : IO State :=
  State.ofUInt32 <$> bindNullRaw stmt paramIdx

end Database.DuckDB.FFI.BindValues
