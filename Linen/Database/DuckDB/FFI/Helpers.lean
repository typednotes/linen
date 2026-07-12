/-
  Linen.Database.DuckDB.FFI.Helpers — miscellaneous small helpers

  Mirrors Haskell's `Database.DuckDB.FFI.Helpers` (the `duckdb-ffi`
  package). Module #10 of `docs/imports/duckdb-ffi/dependencies.md`; depends
  only on `Database.DuckDB.FFI.Types` (module #1), which declares the
  `RawMemory` handle and the `DateStruct`/`TimeStruct`/`TimeTz`/
  `TimeTzStruct`/`TimestampStruct` value types this module decomposes
  `Date`/`Time`/`Timestamp`/`Interval` into and back.

  Every `@[extern]` declaration below is backed by `ffi/duckdb_shim.c`, and
  splits into four groups, matching upstream's own grouping:

  - **Generic**: `malloc`/`free`/`vectorSize`/`validUtf8Check`.
  - **`duckdb_string_t`**: `stringIsInlined`/`stringTLength`/`stringTData`.
    `duckdb_string_t` is a 16-byte C union with no safe direct Lean
    encoding, so this port represents it as a raw 16-byte `ByteArray` — the
    same treatment `Database.DuckDB.FFI.Appender.appendBlob` gives raw byte
    buffers. `mkInlinedStringT` builds one from a short (≤ 12-byte) `String`
    entirely in Lean, matching the union's documented little-endian,
    unpadded layout (`{ length : UInt32; inlined : UInt8[12] }`); this port
    only targets little-endian hosts (macOS/Linux on x86-64/ARM64, the same
    platforms `lakefile.lean`'s DuckDB-archive resolution supports), so no
    byte-swapping is needed.
  - **Date/time decomposition**: `fromDate`/`toDate`/`isFiniteDate`,
    `fromTime`/`createTimeTz`/`fromTimeTz`/`toTime`,
    `fromTimestamp`/`toTimestamp`/`isFiniteTimestamp`/
    `isFiniteTimestampSeconds`/`isFiniteTimestampMillis`/
    `isFiniteTimestampNanos`. Upstream's `duckdb_timestamp_s`/`_ms`/`_ns`
    are each a single-field newtype wrapping one raw `int64_t`; rather than
    add three more single-field wrapper structs to `Types.lean` purely to
    round-trip through `isFiniteTimestamp{Seconds,Millis,Nanos}`, this port
    binds those three functions directly against a plain `Int64` (the exact
    value the C ABI passes either way).
  - **Numeric conversions**: `hugeIntToDouble`/`doubleToHugeInt`,
    `uHugeIntToDouble`/`doubleToUHugeInt`, `doubleToDecimal`/
    `decimalToDouble`. Every multi-field value type here (`HugeInt`/
    `UHugeInt`/`Decimal`/`DateStruct`/`TimeStruct`/`TimeTzStruct`/
    `TimestampStruct`) crosses the FFI boundary as separate scalar
    arguments/return-tuple components, the same flattening
    `Database.DuckDB.FFI.Appender.appendHugeInt`/`appendInterval` already
    apply to their own struct-valued parameters.
-/
import Linen.Database.DuckDB.FFI.Types

namespace Database.DuckDB.FFI.Helpers

open Database.DuckDB.FFI.Types

/-! ── Generic ── -/

/-- `duckdb_malloc`: allocate `size` bytes via DuckDB's internal allocator.
    The result must eventually be freed with `free` (or let its GC finalizer
    do so). -/
@[extern "linen_duckdb_malloc"]
opaque malloc (size : UInt64) : IO RawMemory

/-- `duckdb_free`: release `memory`'s underlying allocation early.
    Idempotent. -/
@[extern "linen_duckdb_free"]
opaque free : RawMemory → IO Unit

/-- `duckdb_vector_size`: the number of tuples that fit into a data chunk
    created by `Database.DuckDB.FFI.DataChunk.createDataChunk`. -/
@[extern "linen_duckdb_vector_size"]
opaque vectorSize : IO Idx

/-- `duckdb_valid_utf8_check`: validate that `bytes` is valid UTF-8, e.g. for
    a raw byte buffer with no independent guarantee of well-formedness
    (unlike a Lean `String`, which already is). Returns `none` if `bytes` is
    valid UTF-8, or `some errorData` describing the violation otherwise; the
    returned `ErrorData` must eventually be destroyed with
    `Database.DuckDB.FFI.ErrorData.destroy` (or let its GC finalizer do
    so). -/
@[extern "linen_duckdb_valid_utf8_check"]
opaque validUtf8Check (bytes : @& ByteArray) : IO (Option Types.ErrorData)

/-! ── `duckdb_string_t` ── -/

/-- Pack a short `String` (at most 12 UTF-8 bytes) into a 16-byte
    `duckdb_string_t` image using its documented inlined layout: a 4-byte
    little-endian length prefix, followed by 12 bytes of character data
    (zero-padded). Fails if `s` exceeds 12 bytes (`duckdb_string_t` can only
    represent longer strings via the pointer variant, which needs a
    separate, independently-owned allocation this helper does not create). -/
def mkInlinedStringT (s : String) : Except String ByteArray := do
  let bytes := s.toUTF8
  if bytes.size > 12 then
    .error s!"mkInlinedStringT: {bytes.size} bytes exceeds the 12-byte inline limit"
  else
    let len := UInt32.ofNat bytes.size
    let lenBytes : ByteArray := ⟨#[
      (len &&& 0xff).toUInt8, ((len >>> 8) &&& 0xff).toUInt8,
      ((len >>> 16) &&& 0xff).toUInt8, ((len >>> 24) &&& 0xff).toUInt8]⟩
    let padded := bytes ++ ByteArray.mk (Array.replicate (12 - bytes.size) 0)
    pure (lenBytes ++ padded)

/-- Raw `duckdb_string_is_inlined`, taking a 16-byte `duckdb_string_t`
    image (see `mkInlinedStringT`). -/
@[extern "linen_duckdb_string_is_inlined"]
opaque stringIsInlined (stringT : @& ByteArray) : IO Bool

/-- Raw `duckdb_string_t_length`, taking a 16-byte `duckdb_string_t` image
    (see `mkInlinedStringT`). -/
@[extern "linen_duckdb_string_t_length"]
opaque stringTLength (stringT : @& ByteArray) : IO UInt32

/-- Raw `duckdb_string_t_data`, taking a 16-byte `duckdb_string_t` image
    (see `mkInlinedStringT`) and returning its character data as a `String`.
    Only meaningful for an *inlined* `duckdb_string_t` image (the common
    case this port constructs) — the pointer variant's `ptr` field would
    need to reference real, separately-owned memory that `mkInlinedStringT`
    never allocates. -/
@[extern "linen_duckdb_string_t_data"]
opaque stringTData (stringT : @& ByteArray) : IO String

/-! ── Date/time decomposition ── -/

/-- Raw `duckdb_from_date`: `(year, month, day)`. -/
@[extern "linen_duckdb_from_date"]
opaque fromDateRaw (date : Int32) : IO (Int32 × Int8 × Int8)

/-- Decompose `date` into a `DateStruct`. -/
def fromDate (date : Date) : IO DateStruct := do
  let (year, month, day) ← fromDateRaw date.days
  pure { year, month, day }

/-- Raw `duckdb_to_date`. -/
@[extern "linen_duckdb_to_date"]
opaque toDateRaw (year : Int32) (month : Int8) (day : Int8) : IO Int32

/-- Re-compose a `Date` from a `DateStruct`. -/
def toDate (date : DateStruct) : IO Date :=
  Date.mk <$> toDateRaw date.year date.month date.day

/-- `duckdb_is_finite_date`. -/
@[extern "linen_duckdb_is_finite_date"]
opaque isFiniteDate (date : Int32) : IO Bool

/-- Raw `duckdb_from_time`: `(hour, min, sec, micros)`. -/
@[extern "linen_duckdb_from_time"]
opaque fromTimeRaw (time : Int64) : IO (Int8 × Int8 × Int8 × Int32)

/-- Decompose `time` into a `TimeStruct`. -/
def fromTime (time : Time) : IO TimeStruct := do
  let (hour, min, sec, micros) ← fromTimeRaw time.micros
  pure { hour, min, sec, micros }

/-- `duckdb_create_time_tz`. -/
@[extern "linen_duckdb_create_time_tz"]
opaque createTimeTz (micros : Int64) (offset : Int32) : IO TimeTz

/-- Raw `duckdb_from_time_tz`: `(hour, min, sec, micros, offset)`. -/
@[extern "linen_duckdb_from_time_tz"]
opaque fromTimeTzRaw (timeTz : UInt64) : IO (Int8 × Int8 × Int8 × Int32 × Int32)

/-- Decompose `timeTz` into a `TimeTzStruct`. -/
def fromTimeTz (timeTz : TimeTz) : IO TimeTzStruct := do
  let (hour, min, sec, micros, offset) ← fromTimeTzRaw timeTz.bits
  pure { time := { hour, min, sec, micros }, offset }

/-- Raw `duckdb_to_time`. -/
@[extern "linen_duckdb_to_time"]
opaque toTimeRaw (hour : Int8) (min : Int8) (sec : Int8) (micros : Int32) : IO Int64

/-- Re-compose a `Time` from a `TimeStruct`. -/
def toTime (time : TimeStruct) : IO Time :=
  Time.mk <$> toTimeRaw time.hour time.min time.sec time.micros

/-- Raw `duckdb_from_timestamp`: `(year, month, day, hour, min, sec,
    micros)`. -/
@[extern "linen_duckdb_from_timestamp"]
opaque fromTimestampRaw (ts : Int64) : IO (Int32 × Int8 × Int8 × Int8 × Int8 × Int8 × Int32)

/-- Decompose `ts` into a `TimestampStruct`. -/
def fromTimestamp (ts : Timestamp) : IO TimestampStruct := do
  let (year, month, day, hour, min, sec, micros) ← fromTimestampRaw ts.micros
  pure { date := { year, month, day }, time := { hour, min, sec, micros } }

/-- Raw `duckdb_to_timestamp`. -/
@[extern "linen_duckdb_to_timestamp"]
opaque toTimestampRaw (year : Int32) (month : Int8) (day : Int8) (hour : Int8) (min : Int8)
    (sec : Int8) (micros : Int32) : IO Int64

/-- Re-compose a `Timestamp` from a `TimestampStruct`. -/
def toTimestamp (ts : TimestampStruct) : IO Timestamp :=
  Timestamp.mk <$>
    toTimestampRaw ts.date.year ts.date.month ts.date.day ts.time.hour ts.time.min ts.time.sec
      ts.time.micros

/-- `duckdb_is_finite_timestamp`. -/
@[extern "linen_duckdb_is_finite_timestamp"]
opaque isFiniteTimestamp (ts : Int64) : IO Bool

/-- `duckdb_is_finite_timestamp_s`, taking the raw seconds-since-epoch
    value a `duckdb_timestamp_s` wraps (see this module's doc comment). -/
@[extern "linen_duckdb_is_finite_timestamp_s"]
opaque isFiniteTimestampSeconds (seconds : Int64) : IO Bool

/-- `duckdb_is_finite_timestamp_ms`, taking the raw milliseconds-since-epoch
    value a `duckdb_timestamp_ms` wraps. -/
@[extern "linen_duckdb_is_finite_timestamp_ms"]
opaque isFiniteTimestampMillis (millis : Int64) : IO Bool

/-- `duckdb_is_finite_timestamp_ns`, taking the raw nanoseconds-since-epoch
    value a `duckdb_timestamp_ns` wraps. -/
@[extern "linen_duckdb_is_finite_timestamp_ns"]
opaque isFiniteTimestampNanos (nanos : Int64) : IO Bool

/-! ── Numeric conversions ── -/

/-- Raw `duckdb_hugeint_to_double`. -/
@[extern "linen_duckdb_hugeint_to_double"]
opaque hugeIntToDoubleRaw (lower : UInt64) (upper : Int64) : IO Float

/-- Convert a `HugeInt` to its nearest `Float` approximation. -/
def hugeIntToDouble (value : HugeInt) : IO Float :=
  hugeIntToDoubleRaw value.lower value.upper

/-- Raw `duckdb_double_to_hugeint`: `(lower, upper)`. -/
@[extern "linen_duckdb_double_to_hugeint"]
opaque doubleToHugeIntRaw (value : Float) : IO (UInt64 × Int64)

/-- Convert a `Float` to the nearest `HugeInt` (`0` if `value` is too large
    to represent). -/
def doubleToHugeInt (value : Float) : IO HugeInt := do
  let (lower, upper) ← doubleToHugeIntRaw value
  pure { lower, upper }

/-- Raw `duckdb_uhugeint_to_double`. -/
@[extern "linen_duckdb_uhugeint_to_double"]
opaque uHugeIntToDoubleRaw (lower : UInt64) (upper : UInt64) : IO Float

/-- Convert a `UHugeInt` to its nearest `Float` approximation. -/
def uHugeIntToDouble (value : UHugeInt) : IO Float :=
  uHugeIntToDoubleRaw value.lower value.upper

/-- Raw `duckdb_double_to_uhugeint`: `(lower, upper)`. -/
@[extern "linen_duckdb_double_to_uhugeint"]
opaque doubleToUHugeIntRaw (value : Float) : IO (UInt64 × UInt64)

/-- Convert a `Float` to the nearest `UHugeInt` (`0` if `value` is too large
    to represent). -/
def doubleToUHugeInt (value : Float) : IO UHugeInt := do
  let (lower, upper) ← doubleToUHugeIntRaw value
  pure { lower, upper }

/-- Raw `duckdb_double_to_decimal`: `(width, scale, lower, upper)`. -/
@[extern "linen_duckdb_double_to_decimal"]
opaque doubleToDecimalRaw (value : Float) (width : UInt8) (scale : UInt8) :
    IO (UInt8 × UInt8 × UInt64 × Int64)

/-- Convert a `Float` to the nearest `Decimal` with the given `width`/
    `scale` (`0` if `value` is too large, or `width`/`scale` are invalid). -/
def doubleToDecimal (value : Float) (width : UInt8) (scale : UInt8) : IO Decimal := do
  let (width', scale', lower, upper) ← doubleToDecimalRaw value width scale
  pure { width := width', scale := scale', value := { lower, upper } }

/-- Raw `duckdb_decimal_to_double`. -/
@[extern "linen_duckdb_decimal_to_double"]
opaque decimalToDoubleRaw (width : UInt8) (scale : UInt8) (lower : UInt64) (upper : Int64) :
    IO Float

/-- Convert a `Decimal` to its nearest `Float` approximation. -/
def decimalToDouble (value : Decimal) : IO Float :=
  decimalToDoubleRaw value.width value.scale value.value.lower value.value.upper

end Database.DuckDB.FFI.Helpers
