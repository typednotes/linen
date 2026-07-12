/-
  Linen.Database.DuckDB.Simple.Materialize — decoding a `duckdb_vector`
  column into `FieldValue`s

  Module #6 of `docs/imports/duckdb-simple/dependencies.md`, on #5
  (`Linen.Database.DuckDB.Simple.FromField`, for `FieldValue`/`BitString`),
  #2 (`…LogicalRep`, for `StructValue`/`UnionValue`/`StructField`/
  `UnionMemberType`), and the FFI layer's `DataChunk`/`Vector`/`Validity`/
  `Helpers`/`LogicalTypes`. Upstream's own module is *not* part of
  `duckdb-simple`'s public API (`FromField`'s module doc already flags this)
  — it exists purely so `FromRow` (module #8) and the top-level facade
  (module #17) have something to call when walking a real result chunk.

  ## Deviations from upstream

  - **No live chunk to decode.** Upstream calls this module directly off a
    `duckdb_data_chunk` returned by `duckdb_fetch_chunk`/inside
    `duckdb_execute_prepared_streaming`. No such "fetch a chunk from a live
    result" binding has been ported into `Linen.Database.DuckDB.FFI` yet —
    only `Database.DuckDB.FFI.DataChunk.createDataChunk` (a *fresh*,
    caller-populated chunk) exists. This module itself doesn't care where its
    `DataChunk`/`Vector` came from (it only ever calls `Vector`/`Validity`/
    `LogicalTypes` accessors, never anything result-specific), so the port
    is otherwise complete and faithful; only this batch's *tests* are
    affected, and exercise it against manually-built chunks/vectors
    populated via `Vector`'s own setters — see `Tests/…MaterializeTest.lean`.
  - **No BIGNUM/VARINT `FieldValue` constructor.** `FromField.FieldValue` (as
    already ported) has no constructor for DuckDB's `VARINT` type — upstream
    itself doesn't materialize `VARINT` into anything but a raw byte blob
    either (it's one of upstream's own least-supported types), so this port
    reports `Type_.varInt` as an explicit `IO` error rather than adding a
    speculative constructor to an already-ported, upstream-mirroring module.
  - **`BLOB`/`BIT` decoding is inline-only.** DuckDB's `duckdb_string_t` is a
    16-byte union: values of at most 12 bytes are stored inline in the
    struct itself; longer values store a pointer to separately-owned heap
    memory instead. `Linen.Database.DuckDB.FFI.Helpers.stringTData` (the
    only string-payload accessor this port has) already only supports the
    inlined case for `VARCHAR` (its own doc comment says so); the same
    constraint applies here to `BLOB`/`BIT`, which this module decodes
    directly from the raw inlined bytes with no pointer-dereferencing
    accessor of any kind ported. A `BLOB`/`BIT` value longer than 12 bytes
    is reported as an explicit `IO` error instead of silently truncating.
  - **`BitString`'s on-wire decode lands here, as planned.**
    `FromField.BitString`'s module doc explicitly deferred matching DuckDB's
    packed `BIT` byte layout (first byte = padding-bit count, remaining
    bytes = packed bits, most-significant-bit first) to whichever module
    first decodes a real `BIT` value — this module. No change to
    `FromField.BitString`'s `Array Bool` representation was needed: `BIT`'s
    packed wire bytes are unpacked into that same `Array Bool` at decode
    time (`decodeBitString` below), exactly the layering
    `FromField`'s module doc anticipated.

  ## Design

  `materializeValueFuel`/`materializeValue` mirror `LogicalRep.
  logicalTypeToRepFuel`/`logicalTypeToRep`'s fuel-parameter shape exactly,
  and for the same reason: a `LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION` column's
  nesting depth is a property of a *runtime* `duckdb_logical_type` handle,
  invisible to Lean's termination checker, and no `duckdb.h` API reports a
  type tree's maximum depth up front. `maxNestingDepth` reuses the same
  conservative bound (`64`) `LogicalRep` already documents as "far beyond
  any type tree a real schema would build" — recursing into a `UNION`
  member's own type additionally calls `LogicalRep.logicalTypeToRepFuel`
  directly with the *remaining* fuel, so one shared budget bounds both
  concerns together.

  Every scalar case reads `vector`'s raw column bytes directly (via
  `Vector.getDataBytes`, or one of `Vector`'s existing typed
  `getBool`/`getInt32`/`getInt64`/`getDouble` wrappers where the physical
  storage width already matches one of those), decoding multi-byte values
  with small private little-endian helpers (`u16LE`/`u32LE`/`u64LE` and
  their signed counterparts) — the same `ByteArray`-indexing style
  `Database.DuckDB.Simple.Types.UUID.ofBytesBE`/`toBytesBE` already use in
  this codebase, mirrored here for DuckDB's little-endian on-wire layout
  instead of `UUID`'s big-endian one. `HugeInt`/`UHugeInt`/`Decimal`/`Date`/
  `Time`/`Timestamp`/`TimeTz`/`Interval` decode directly into
  `Linen.Database.DuckDB.FFI.Types`'s existing raw structs (no
  Day/TimeOfDay-style decomposition, unlike upstream's Haskell — those
  structs already are DuckDB's own raw wire representation, so no further
  translation is useful at this layer).

  `LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION` recurse into a child `Vector`
  fetched fresh from `LogicalTypes`/`Vector`'s own accessors (rather than
  threading a `LogicalTypeRep` down by hand); `UNION`'s member-type list is
  built by calling `LogicalRep.logicalTypeToRepFuel` on each member type in
  turn, reusing that module's own conversion rather than duplicating it.

  ## Haskell source
  - `Database.DuckDB.Simple.Materialize` (`duckdb-simple` package, version
    0.1.5.1)
-/

import Linen.Database.DuckDB.Simple.FromField
import Linen.Database.DuckDB.FFI.DataChunk
import Linen.Database.DuckDB.FFI.Vector
import Linen.Database.DuckDB.FFI.Validity
import Linen.Database.DuckDB.FFI.Helpers

namespace Database.DuckDB.Simple.Materialize

open Database.DuckDB.FFI.Types (Idx LogicalType Vector DataChunk HugeInt UHugeInt Decimal
  Date Time Timestamp TimeTz Interval Type_)
open Database.DuckDB.FFI
open Database.DuckDB.Simple (FieldValue BitString UUID)
open Database.DuckDB.Simple.LogicalRep (StructValue StructField UnionValue UnionMemberType)

-- ────────────────────────────────────────────────────────────────────
-- Little-endian byte-decode helpers
-- ────────────────────────────────────────────────────────────────────

/-- The byte at `offset` in `bytes` (`0` if out of range, which shouldn't
    occur given this module's own callers, matching
    `Database.DuckDB.Simple.Types.UUID.ofBytesBE`'s `getD` pattern). -/
private def byteAt (bytes : ByteArray) (offset : Nat) : UInt8 :=
  bytes.data.getD offset 0

private def u16LE (bytes : ByteArray) (offset : Nat) : UInt16 :=
  (byteAt bytes offset).toUInt16 ||| ((byteAt bytes (offset + 1)).toUInt16 <<< 8)

private def u32LE (bytes : ByteArray) (offset : Nat) : UInt32 :=
  let byte (i : Nat) : UInt32 := (byteAt bytes (offset + i)).toUInt32
  byte 0 ||| (byte 1 <<< 8) ||| (byte 2 <<< 16) ||| (byte 3 <<< 24)

private def u64LE (bytes : ByteArray) (offset : Nat) : UInt64 :=
  let byte (i : Nat) : UInt64 := (byteAt bytes (offset + i)).toUInt64
  byte 0 ||| (byte 1 <<< 8) ||| (byte 2 <<< 16) ||| (byte 3 <<< 24) |||
  (byte 4 <<< 32) ||| (byte 5 <<< 40) ||| (byte 6 <<< 48) ||| (byte 7 <<< 56)

private def i8At (bytes : ByteArray) (offset : Nat) : Int8 := (byteAt bytes offset).toInt8
private def i16LE (bytes : ByteArray) (offset : Nat) : Int16 := (u16LE bytes offset).toInt16
private def i32LE (bytes : ByteArray) (offset : Nat) : Int32 := (u32LE bytes offset).toInt32
private def i64LE (bytes : ByteArray) (offset : Nat) : Int64 := (u64LE bytes offset).toInt64

/-- Sign-extend a value already read into an `Int64` up to a full `HugeInt`
    (used for `DECIMAL`'s `TINYINT`/`SMALLINT`/`INTEGER`/`BIGINT`-backed
    internal storage widths). -/
private def signExtendToHugeInt (v : Int64) : HugeInt :=
  { lower := v.toUInt64, upper := if v < 0 then (-1 : Int64) else 0 }

-- ────────────────────────────────────────────────────────────────────
-- BLOB / BIT (inlined `duckdb_string_t` only — see the module doc)
-- ────────────────────────────────────────────────────────────────────

/-- Copy the 16-byte `duckdb_string_t` image for row `idx` of `vector`. -/
private def readStringT (vector : Vector) (idx : Idx) : IO ByteArray :=
  Vector.getDataBytes vector (idx * 16) 16

/-- Recover the raw payload bytes of an *inlined* `duckdb_string_t`,
    throwing if `stringT` uses the pointer variant instead (see the module
    doc's "BLOB/BIT decoding is inline-only" note). -/
private def inlineStringTBytes (stringT : ByteArray) : IO ByteArray := do
  unless (← Helpers.stringIsInlined stringT) do
    throw (IO.userError
      "Materialize.materializeValue: BLOB/BIT value exceeds the 12-byte \
       duckdb_string_t inline threshold; this port has no pointer-variant \
       accessor (see the module doc)")
  let len ← Helpers.stringTLength stringT
  pure ⟨(List.range len.toNat).map (fun i => byteAt stringT (4 + i)) |>.toArray⟩

/-- Split a byte, most-significant bit first. -/
private def byteBitsMSBFirst (b : UInt8) : Array Bool :=
  #[(b &&& 0x80) != 0, (b &&& 0x40) != 0, (b &&& 0x20) != 0, (b &&& 0x10) != 0,
    (b &&& 0x08) != 0, (b &&& 0x04) != 0, (b &&& 0x02) != 0, (b &&& 0x01) != 0]

/-- Unpack DuckDB's packed on-wire `BIT` layout (first byte = padding-bit
    count; remaining bytes = the bits themselves, most-significant-bit
    first, with `padding` leading padding bits to drop) into a `BitString`
    (see the module doc's `BitString`-decode note). -/
private def decodeBitString (raw : ByteArray) : BitString :=
  if raw.size == 0 then
    { bits := #[] }
  else
    let padding := (byteAt raw 0).toNat
    let payload := (List.range (raw.size - 1)).map (fun i => byteAt raw (1 + i)) |>.toArray
    let allBits := payload.flatMap byteBitsMSBFirst
    { bits := allBits.extract padding allBits.size }

-- ────────────────────────────────────────────────────────────────────
-- Value dispatch
-- ────────────────────────────────────────────────────────────────────

/-- The maximum nesting depth `materializeValueFuel` will follow before
    reporting an error — see the module doc (shared with `LogicalRep.
    logicalTypeToRepFuel`'s own documented bound). -/
def maxNestingDepth : Nat := 64

/-- Recover a `Nat` from a `FieldValue` known to hold one of the unsigned
    integer variants (used to read a `UNION`'s selector-tag column). -/
private def fieldValueToNat? : FieldValue → Option Nat
  | .uint8 i => some i.toNat
  | .uint16 i => some i.toNat
  | .uint32 i => some i.toNat
  | .uint64 i => some i.toNat
  | _ => none

/-- Decode row `idx` of `vector` into a `FieldValue`, giving up with an `IO`
    error past `fuel` levels of `LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION`
    nesting. Structural recursion on `fuel`, exactly `LogicalRep.
    logicalTypeToRepFuel`'s shape — see the module doc. -/
def materializeValueFuel (fuel : Nat) (vector : Vector) (idx : Idx) : IO FieldValue := do
  match ← Vector.getValidity vector with
  | some validity =>
    unless (← Validity.rowIsValid validity idx) do
      return .null
  | none => pure ()
  let ty ← Vector.getColumnType vector
  try
    let tid ← LogicalTypes.getTypeId ty
    match tid with
    | .invalid | .any | .sqlNull => pure .null
    | .varInt =>
      throw (IO.userError
        "Materialize.materializeValue: VARINT/BIGNUM columns have no FieldValue \
         constructor in this port (see the module doc)")
    | .boolean => .boolean <$> Vector.getBool vector idx
    | .tinyInt => do
      let b ← Vector.getDataBytes vector idx 1
      pure (.int8 (i8At b 0))
    | .smallInt => do
      let b ← Vector.getDataBytes vector (idx * 2) 2
      pure (.int16 (i16LE b 0))
    | .integer => .int32 <$> Vector.getInt32 vector idx
    | .bigInt => .int64 <$> Vector.getInt64 vector idx
    | .uTinyInt => do
      let b ← Vector.getDataBytes vector idx 1
      pure (.uint8 (byteAt b 0))
    | .uSmallInt => do
      let b ← Vector.getDataBytes vector (idx * 2) 2
      pure (.uint16 (u16LE b 0))
    | .uInteger => do
      let b ← Vector.getDataBytes vector (idx * 4) 4
      pure (.uint32 (u32LE b 0))
    | .uBigInt => do
      let b ← Vector.getDataBytes vector (idx * 8) 8
      pure (.uint64 (u64LE b 0))
    | .hugeInt => do
      let b ← Vector.getDataBytes vector (idx * 16) 16
      pure (.hugeInt { lower := u64LE b 0, upper := i64LE b 8 })
    | .uHugeInt => do
      let b ← Vector.getDataBytes vector (idx * 16) 16
      pure (.uHugeInt { lower := u64LE b 0, upper := u64LE b 8 })
    | .float => do
      let b ← Vector.getDataBytes vector (idx * 4) 4
      pure (.float (Float32.ofBits (u32LE b 0)))
    | .double => .double <$> Vector.getDouble vector idx
    | .decimal => do
      let width ← LogicalTypes.decimalWidth ty
      let scale ← LogicalTypes.decimalScale ty
      let internalTy ← LogicalTypes.decimalInternalType ty
      let value ← match internalTy with
        | .smallInt => do
          let b ← Vector.getDataBytes vector (idx * 2) 2
          pure (signExtendToHugeInt (i16LE b 0).toInt64)
        | .integer => do
          let b ← Vector.getDataBytes vector (idx * 4) 4
          pure (signExtendToHugeInt (i32LE b 0).toInt64)
        | .bigInt => do
          let b ← Vector.getDataBytes vector (idx * 8) 8
          pure (signExtendToHugeInt (i64LE b 0))
        | .hugeInt => do
          let b ← Vector.getDataBytes vector (idx * 16) 16
          pure ({ lower := u64LE b 0, upper := i64LE b 8 } : HugeInt)
        | other =>
          throw (IO.userError
            s!"Materialize.materializeValue: unsupported DECIMAL internal type {repr other}")
      pure (.decimal { width, scale, value })
    | .varchar => do
      let st ← readStringT vector idx
      .varchar <$> Helpers.stringTData st
    | .blob => do
      let st ← readStringT vector idx
      .blob <$> inlineStringTBytes st
    | .bit => do
      let st ← readStringT vector idx
      pure (.bitString (decodeBitString (← inlineStringTBytes st)))
    | .uuid => do
      let b ← Vector.getDataBytes vector (idx * 16) 16
      let lower := u64LE b 0
      let rawUpper := u64LE b 8
      pure (.uuid { hi := rawUpper ^^^ 0x8000000000000000, lo := lower })
    | .date => .date <$> (Date.mk <$> Vector.getInt32 vector idx)
    | .time => .time <$> (Time.mk <$> Vector.getInt64 vector idx)
    | .timestamp | .timestampTz => .timestamp <$> (Timestamp.mk <$> Vector.getInt64 vector idx)
    | .timestampS => (fun secs => .timestamp { micros := secs * 1000000 }) <$>
      Vector.getInt64 vector idx
    | .timestampMs => (fun ms => .timestamp { micros := ms * 1000 }) <$> Vector.getInt64 vector idx
    | .timestampNs => (fun ns => .timestamp { micros := ns / 1000 }) <$> Vector.getInt64 vector idx
    | .timeTz => do
      let b ← Vector.getDataBytes vector (idx * 8) 8
      pure (.timeTz { bits := u64LE b 0 })
    | .interval => do
      let b ← Vector.getDataBytes vector (idx * 16) 16
      pure (.interval { months := i32LE b 0, days := i32LE b 4, micros := i64LE b 8 })
    | .enum => do
      let internalTy ← LogicalTypes.enumInternalType ty
      let index ← match internalTy with
        | .uTinyInt => do
          let b ← Vector.getDataBytes vector idx 1
          pure (byteAt b 0).toUInt64
        | .uSmallInt => do
          let b ← Vector.getDataBytes vector (idx * 2) 2
          pure (u16LE b 0).toUInt64
        | .uInteger => do
          let b ← Vector.getDataBytes vector (idx * 4) 4
          pure (u32LE b 0).toUInt64
        | other =>
          throw (IO.userError
            s!"Materialize.materializeValue: unsupported ENUM internal type {repr other}")
      let label ← LogicalTypes.enumDictionaryValue ty index
      pure (.enum index (label.getD ""))
    | .list =>
      match fuel with
      | 0 => throw (IO.userError "Materialize.materializeValue: max nesting depth exceeded")
      | fuel + 1 => do
        let entryBytes ← Vector.getDataBytes vector (idx * 16) 16
        let entryOffset := u64LE entryBytes 0
        let entryLen := u64LE entryBytes 8
        let child ← Vector.listVectorGetChild vector
        let mut elems : Array FieldValue := #[]
        for delta in [0:entryLen.toNat] do
          let v ← materializeValueFuel fuel child (entryOffset + UInt64.ofNat delta)
          elems := elems.push v
        pure (.list elems)
    | .array =>
      match fuel with
      | 0 => throw (IO.userError "Materialize.materializeValue: max nesting depth exceeded")
      | fuel + 1 => do
        let arraySize ← LogicalTypes.arrayTypeArraySize ty
        let child ← Vector.arrayVectorGetChild vector
        let baseIdx := idx * arraySize
        let mut elems : Array FieldValue := #[]
        for delta in [0:arraySize.toNat] do
          let v ← materializeValueFuel fuel child (baseIdx + UInt64.ofNat delta)
          elems := elems.push v
        pure (.list elems)
    | .map =>
      match fuel with
      | 0 => throw (IO.userError "Materialize.materializeValue: max nesting depth exceeded")
      | fuel + 1 => do
        let entryBytes ← Vector.getDataBytes vector (idx * 16) 16
        let entryOffset := u64LE entryBytes 0
        let entryLen := u64LE entryBytes 8
        let entriesChild ← Vector.listVectorGetChild vector
        let keyChild ← Vector.structVectorGetChild entriesChild 0
        let valChild ← Vector.structVectorGetChild entriesChild 1
        let mut entries : Array (FieldValue × FieldValue) := #[]
        for delta in [0:entryLen.toNat] do
          let i := entryOffset + UInt64.ofNat delta
          let k ← materializeValueFuel fuel keyChild i
          let v ← materializeValueFuel fuel valChild i
          entries := entries.push (k, v)
        pure (.map entries)
    | .struct =>
      match fuel with
      | 0 => throw (IO.userError "Materialize.materializeValue: max nesting depth exceeded")
      | fuel + 1 => do
        let n ← LogicalTypes.structTypeChildCount ty
        let mut fields : Array (StructField FieldValue) := #[]
        for i in [0:n.toNat] do
          let nameOpt ← LogicalTypes.structTypeChildName ty (UInt64.ofNat i)
          let childVec ← Vector.structVectorGetChild vector (UInt64.ofNat i)
          let v ← materializeValueFuel fuel childVec idx
          fields := fields.push { name := nameOpt.getD "", value := v }
        pure (.struct { fields })
    | .union =>
      match fuel with
      | 0 => throw (IO.userError "Materialize.materializeValue: max nesting depth exceeded")
      | fuel + 1 => do
        let n ← LogicalTypes.unionTypeMemberCount ty
        let tagChild ← Vector.structVectorGetChild vector 0
        let tagValue ← materializeValueFuel fuel tagChild idx
        let some tagIdx := fieldValueToNat? tagValue
          | throw (IO.userError "Materialize.materializeValue: invalid UNION tag value")
        if tagIdx ≥ n.toNat then
          throw (IO.userError "Materialize.materializeValue: UNION tag out of range")
        else do
          let mut members : Array UnionMemberType := #[]
          for i in [0:n.toNat] do
            let nameOpt ← LogicalTypes.unionTypeMemberName ty (UInt64.ofNat i)
            let memberTy ← LogicalTypes.unionTypeMemberType ty (UInt64.ofNat i)
            let rep ← Database.DuckDB.Simple.LogicalRep.logicalTypeToRepFuel fuel memberTy
            LogicalTypes.destroy memberTy
            members := members.push { name := nameOpt.getD "", type := rep }
          let label := (members[tagIdx]?).map (·.name) |>.getD ""
          let payloadChild ← Vector.structVectorGetChild vector (UInt64.ofNat (tagIdx + 1))
          let payload ← materializeValueFuel fuel payloadChild idx
          pure (.union { index := UInt16.ofNat tagIdx, label, payload, members })
    | .other code =>
      throw (IO.userError s!"Materialize.materializeValue: unrecognized duckdb_type code {code}")
  finally
    LogicalTypes.destroy ty

/-- Decode row `idx` of `vector` into a `FieldValue`, giving up with an `IO`
    error past `maxNestingDepth` levels of nesting. -/
def materializeValue (vector : Vector) (idx : Idx) : IO FieldValue :=
  materializeValueFuel maxNestingDepth vector idx

/-- Decode every row of `chunk`'s column at `colIdx` into an `Array
    FieldValue`, one entry per row (`chunk.getSize` rows in total).
    Structural recursion on `chunk`'s (already-fixed) row count. -/
def materializeColumn (chunk : DataChunk) (colIdx : Idx) : IO (Array FieldValue) := do
  let vector ← DataChunk.getVector chunk colIdx
  let size ← DataChunk.getSize chunk
  let mut out : Array FieldValue := #[]
  for i in [0:size.toNat] do
    let v ← materializeValue vector (UInt64.ofNat i)
    out := out.push v
  pure out

end Database.DuckDB.Simple.Materialize
