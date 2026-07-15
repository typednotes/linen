/-
  Linen.Database.DuckDB.Simple.FromField — `Field`, `FieldValue`, the
  `FromField` class, `ResultError`

  Module #5 of `docs/imports/duckdb-simple/dependencies.md`, on #2
  (`Linen.Database.DuckDB.Simple.LogicalRep`, for `StructValue`/`UnionValue`),
  #3 (`…Ok`), and #4 (`…Types`, for `UUID`).

  ## Design

  ### `FieldValue`

  Upstream decodes a DuckDB result column directly off a live
  `duckdb_vector`/`duckdb_data_chunk` (via `Database.DuckDB.Simple.Materialize`,
  module #6 — walking the chunk's raw columnar buffers is genuinely out of
  scope for this batch: no `fetch_chunk`-equivalent has been ported into
  `Linen.Database.DuckDB.FFI` yet). This module instead defines the *target*
  shape that decoding step will eventually produce: `FieldValue`, a tagged
  union with one constructor per DuckDB logical type this port recognizes.
  Numeric/temporal leaves reuse the existing by-value structs already
  ported for `Linen.Database.DuckDB.FFI.{Appender,BindValues,Helpers}`
  rather than re-declaring them (`HugeInt`/`UHugeInt` substitute for a
  bespoke "BigNum" wrapper; `Decimal`/`Interval`/`TimeTz` substitute for
  "DecimalValue"/"IntervalValue"/"TimeWithZone"), since those are already
  exact field-for-field ports of `duckdb_hugeint`/`duckdb_uhugeint`/
  `duckdb_decimal`/`duckdb_interval`/`duckdb_time_tz`. `list`/`struct`/`map`/
  `union` recurse into `FieldValue` itself (`struct`/`union` reusing
  `LogicalRep`'s generic `StructValue`/`UnionValue α`, specialized to
  `α := FieldValue`) — Lean accepts this nested-inductive shape directly
  (each recursive occurrence sits in a strictly-positive `List`/`Array`
  position), so, unlike `LogicalRep`'s handle-to-tree conversions, no fuel
  parameter or termination proof is needed anywhere in this module: every
  function here either pattern-matches structurally on an already-finite
  `FieldValue`, or returns one outright.

  `BitString` (for DuckDB's `BIT` type) has no existing port to reuse; it is
  kept here as a small `Array Bool` wrapper with a textual `'1010…'`
  rendering, deliberately *not* attempting to mirror DuckDB's packed on-wire
  `BIT` byte layout (first byte = padding count, remaining bits MSB-first) —
  since `Materialize` (the only place that would ever decode a real packed
  `BIT` value into this type) is out of scope for this batch, matching that
  layout byte-for-byte here would be untestable and premature; an
  `Array Bool` already fully captures the value `FromField`/`ResultError`
  need to describe (a documented, genuine scope simplification, not a
  proof-avoidance shortcut — see `AGENTS.md`'s termination-proof rule for
  the distinction this port is careful to keep).

  ### `ResultError`/`FromField`

  Exactly upstream's `ResultError`/`FromField`/`returnError` shape, ported
  the same way `Linen.Database.SQLite.Simple.FromField`'s module doc already
  describes for that port's identical three-constructor `ResultError` and
  `Ok`-based (rather than exception-based) failure reporting — see that
  module's doc for the full rationale, which applies here unchanged. `Field`
  additionally carries `columnLabel : Option String` (upstream's `Column`
  metadata is attached at the `duckdb_result` level, not modeled here since
  no live-result-column-name lookup has been ported into
  `Linen.Database.DuckDB.FFI` for this batch) rather than deriving a "SQL
  type name" from the field's own DuckDB type at print time (unlike
  `sqlite-simple`'s `Field.typeName`) — `FieldValue.typeName` (below) plays
  that role here.

  ## Haskell source
  - `Database.DuckDB.Simple.FromField` (`duckdb-simple` package, version
    0.1.5.1)
-/

import Linen.Database.DuckDB.Simple.LogicalRep
import Linen.Database.DuckDB.Simple.Ok
import Linen.Database.DuckDB.Simple.Types

namespace Database.DuckDB.Simple

-- The tuple-of-`FromField` instances' per-component universes are
-- independent by design, but always co-occur syntactically, so the linter
-- can't tell they need to stay free.
set_option linter.checkUnivs false

open Database.DuckDB.FFI.Types (Date Time Timestamp TimeTz Interval HugeInt UHugeInt Decimal)
open Database.DuckDB.Simple.LogicalRep (StructValue UnionValue StructField UnionMemberType)

-- ────────────────────────────────────────────────────────────────────
-- BitString
-- ────────────────────────────────────────────────────────────────────

/-- A DuckDB `BIT` value, as its individual bits in declaration order (see
    the module doc for why this doesn't mirror DuckDB's packed on-wire
    layout). -/
structure BitString where
  bits : Array Bool
deriving BEq, Repr, Inhabited

namespace BitString

/-- Render as a string of `'0'`/`'1'` characters, matching DuckDB's own
    textual `BIT` literal form (e.g. `"101010"`). -/
def render (b : BitString) : String :=
  String.ofList (b.bits.toList.map (fun bit => if bit then '1' else '0'))

instance : ToString BitString where
  toString := render

/-- Parse a string of `'0'`/`'1'` characters, `none` if it contains any
    other character. -/
def ofString? (s : String) : Option BitString := do
  let bits ← s.toList.mapM fun
    | '0' => some false
    | '1' => some true
    | _ => none
  pure { bits := bits.toArray }

end BitString

-- ────────────────────────────────────────────────────────────────────
-- FieldValue
-- ────────────────────────────────────────────────────────────────────

/-- A fully decoded DuckDB column value (see the module doc). -/
inductive FieldValue where
  /-- SQL `NULL`. -/
  | null
  | boolean (b : Bool)
  | int8 (i : Int8)
  | int16 (i : Int16)
  | int32 (i : Int32)
  | int64 (i : Int64)
  | hugeInt (i : HugeInt)
  | uint8 (i : UInt8)
  | uint16 (i : UInt16)
  | uint32 (i : UInt32)
  | uint64 (i : UInt64)
  | uHugeInt (i : UHugeInt)
  | float (f : Float32)
  | double (f : Float)
  | decimal (d : Decimal)
  | varchar (s : String)
  | blob (b : ByteArray)
  | bitString (b : BitString)
  | uuid (u : UUID)
  | date (d : Date)
  | time (t : Time)
  | timeTz (t : TimeTz)
  | timestamp (t : Timestamp)
  | interval (i : Interval)
  /-- A `LIST`/`ARRAY` value, element type given by the column's
      `LogicalTypeRep`. -/
  | list (elems : Array FieldValue)
  /-- A `STRUCT` value. -/
  | struct (fields : StructValue FieldValue)
  /-- A `MAP` value, as parallel key/value entries. -/
  | map (entries : Array (FieldValue × FieldValue))
  /-- A `UNION` value. -/
  | union (u : UnionValue FieldValue)
  /-- An `ENUM` value: its dictionary index plus resolved textual label. -/
  | enum (index : UInt64) (label : String)
deriving Inhabited

namespace FieldValue

/-- A short DuckDB-flavoured type name for `v`, used in `ResultError`
    messages in place of upstream's `Typeable`-derived SQL type name (no
    live `duckdb_logical_type` is attached to a bare `FieldValue`, so this
    reports the value's own tag rather than a column's declared type). -/
def typeName : FieldValue → String
  | .null => "NULL"
  | .boolean _ => "BOOLEAN"
  | .int8 _ => "TINYINT"
  | .int16 _ => "SMALLINT"
  | .int32 _ => "INTEGER"
  | .int64 _ => "BIGINT"
  | .hugeInt _ => "HUGEINT"
  | .uint8 _ => "UTINYINT"
  | .uint16 _ => "USMALLINT"
  | .uint32 _ => "UINTEGER"
  | .uint64 _ => "UBIGINT"
  | .uHugeInt _ => "UHUGEINT"
  | .float _ => "FLOAT"
  | .double _ => "DOUBLE"
  | .decimal _ => "DECIMAL"
  | .varchar _ => "VARCHAR"
  | .blob _ => "BLOB"
  | .bitString _ => "BIT"
  | .uuid _ => "UUID"
  | .date _ => "DATE"
  | .time _ => "TIME"
  | .timeTz _ => "TIME WITH TIME ZONE"
  | .timestamp _ => "TIMESTAMP"
  | .interval _ => "INTERVAL"
  | .list _ => "LIST"
  | .struct _ => "STRUCT"
  | .map _ => "MAP"
  | .union _ => "UNION"
  | .enum .. => "ENUM"

end FieldValue

-- ────────────────────────────────────────────────────────────────────
-- Field
-- ────────────────────────────────────────────────────────────────────

/-- A single decoded column value from a result row, together with enough
    positional metadata for a `FromField` conversion failure to name the
    offending column (see the module doc for why this doesn't carry a
    live SQL type name, unlike `sqlite-simple`'s `Field`). -/
structure Field where
  /-- The decoded column value. -/
  result : FieldValue
  /-- The zero-based index of this column within its row. -/
  column : Nat
  /-- The column's declared name, if known. -/
  columnLabel : Option String := none
deriving Inhabited

/-- Build a `Field` sharing `f`'s positional metadata but a different
    `FieldValue` — used to recurse into a `list`/`struct`/`map`/`union`
    element without losing the enclosing column's identity in error
    messages. -/
def Field.withValue (f : Field) (value : FieldValue) : Field :=
  { f with result := value }

-- ────────────────────────────────────────────────────────────────────
-- ResultError
-- ────────────────────────────────────────────────────────────────────

/-- The reason a `FromField` conversion failed (see the module doc for how
    this substitutes for upstream's exception-based `ResultError`, matching
    `Linen.Database.SQLite.Simple.FromField`'s identical substitution). -/
inductive ResultError where
  /-- The DuckDB and Lean types are not compatible. -/
  | incompatible (duckDBType leanType message : String)
  /-- A `NULL` was encountered where the Lean type did not permit one. -/
  | unexpectedNull (duckDBType leanType message : String)
  /-- The DuckDB value could not be converted to the target Lean type. -/
  | conversionFailed (duckDBType leanType message : String)
deriving Repr, Inhabited, BEq

namespace ResultError

/-- The offending value's DuckDB-flavoured type name. -/
def duckDBType : ResultError → String
  | .incompatible t _ _ | .unexpectedNull t _ _ | .conversionFailed t _ _ => t

/-- The target Lean type's name. -/
def leanType : ResultError → String
  | .incompatible _ t _ | .unexpectedNull _ t _ | .conversionFailed _ t _ => t

/-- A free-form description of the failure. -/
def message : ResultError → String
  | .incompatible _ _ m | .unexpectedNull _ _ m | .conversionFailed _ _ m => m

end ResultError

instance : ToString ResultError where
  toString
    | .incompatible dbT leanT msg =>
      s!"incompatible DuckDB type {dbT} and Lean type {leanT}: {msg}"
    | .unexpectedNull dbT leanT msg =>
      s!"unexpected NULL of DuckDB type {dbT} for non-nullable Lean type {leanT}: {msg}"
    | .conversionFailed dbT leanT msg =>
      s!"could not convert DuckDB type {dbT} to Lean type {leanT}: {msg}"

-- ────────────────────────────────────────────────────────────────────
-- The `FromField` class
-- ────────────────────────────────────────────────────────────────────

/-- A field-decoding function, from a `Field` to an error-accumulating
    result. -/
abbrev FieldParser (α : Type u) : Type u := Field → Ok α

/-- A type that may be decoded from a single DuckDB result column. -/
class FromField (α : Type u) where
  /-- Convert a decoded column (`Field`) to a Lean value. -/
  fromField : FieldParser α

export FromField (fromField)

/-- Build a failed `Ok` from one of `ResultError`'s three constructors, the
    offending `Field`, and the target Lean type's name. -/
def returnError (mk : String → String → String → ResultError) (f : Field)
    (leanType message : String) : Ok α :=
  Ok.fail (toString (mk f.result.typeName leanType message))

-- ────────────────────────────────────────────────────────────────────
-- Identity / nullable wrappers
-- ────────────────────────────────────────────────────────────────────

instance : FromField FieldValue where
  fromField f := .ok f.result

instance [FromField α] : FromField (Option α) where
  fromField f :=
    match f.result with
    | .null => .ok none
    | _ => some <$> fromField f

-- ────────────────────────────────────────────────────────────────────
-- Booleans / integers
-- ────────────────────────────────────────────────────────────────────

instance : FromField Bool where
  fromField f :=
    match f.result with
    | .boolean b => .ok b
    | _ => returnError .conversionFailed f "Bool" "expecting a BOOLEAN column"

instance : FromField Int8 where
  fromField f :=
    match f.result with
    | .int8 i => .ok i
    | _ => returnError .conversionFailed f "Int8" "expecting a TINYINT column"

instance : FromField Int16 where
  fromField f :=
    match f.result with
    | .int16 i => .ok i
    | _ => returnError .conversionFailed f "Int16" "expecting a SMALLINT column"

instance : FromField Int32 where
  fromField f :=
    match f.result with
    | .int32 i => .ok i
    | _ => returnError .conversionFailed f "Int32" "expecting an INTEGER column"

instance : FromField Int64 where
  fromField f :=
    match f.result with
    | .int64 i => .ok i
    | _ => returnError .conversionFailed f "Int64" "expecting a BIGINT column"

instance : FromField Int where
  fromField f :=
    match f.result with
    | .int64 i => .ok i.toInt
    | .int32 i => .ok i.toInt
    | .int16 i => .ok i.toInt
    | .int8 i => .ok i.toInt
    | _ => returnError .conversionFailed f "Int" "expecting a signed-integer column"

instance : FromField UInt8 where
  fromField f :=
    match f.result with
    | .uint8 i => .ok i
    | _ => returnError .conversionFailed f "UInt8" "expecting a UTINYINT column"

instance : FromField UInt16 where
  fromField f :=
    match f.result with
    | .uint16 i => .ok i
    | _ => returnError .conversionFailed f "UInt16" "expecting a USMALLINT column"

instance : FromField UInt32 where
  fromField f :=
    match f.result with
    | .uint32 i => .ok i
    | _ => returnError .conversionFailed f "UInt32" "expecting a UINTEGER column"

instance : FromField UInt64 where
  fromField f :=
    match f.result with
    | .uint64 i => .ok i
    | _ => returnError .conversionFailed f "UInt64" "expecting a UBIGINT column"

instance : FromField Nat where
  fromField f :=
    match f.result with
    | .uint64 i => .ok i.toNat
    | .uint32 i => .ok i.toNat
    | .uint16 i => .ok i.toNat
    | .uint8 i => .ok i.toNat
    | _ => returnError .conversionFailed f "Nat" "expecting an unsigned-integer column"

instance : FromField HugeInt where
  fromField f :=
    match f.result with
    | .hugeInt i => .ok i
    | _ => returnError .conversionFailed f "HugeInt" "expecting a HUGEINT column"

instance : FromField UHugeInt where
  fromField f :=
    match f.result with
    | .uHugeInt i => .ok i
    | _ => returnError .conversionFailed f "UHugeInt" "expecting a UHUGEINT column"

-- ────────────────────────────────────────────────────────────────────
-- Floating point / decimal
-- ────────────────────────────────────────────────────────────────────

instance : FromField Float32 where
  fromField f :=
    match f.result with
    | .float x => .ok x
    | _ => returnError .conversionFailed f "Float32" "expecting a FLOAT column"

instance : FromField Float where
  fromField f :=
    match f.result with
    | .double x => .ok x
    | _ => returnError .conversionFailed f "Float" "expecting a DOUBLE column"

instance : FromField Decimal where
  fromField f :=
    match f.result with
    | .decimal d => .ok d
    | _ => returnError .conversionFailed f "Decimal" "expecting a DECIMAL column"

-- ────────────────────────────────────────────────────────────────────
-- Text / blob / bit
-- ────────────────────────────────────────────────────────────────────

instance : FromField String where
  fromField f :=
    match f.result with
    | .varchar s => .ok s
    | _ => returnError .conversionFailed f "String" "expecting a VARCHAR column"

instance : FromField ByteArray where
  fromField f :=
    match f.result with
    | .blob b => .ok b
    | _ => returnError .conversionFailed f "ByteArray" "expecting a BLOB column"

instance : FromField BitString where
  fromField f :=
    match f.result with
    | .bitString b => .ok b
    | _ => returnError .conversionFailed f "BitString" "expecting a BIT column"

-- ────────────────────────────────────────────────────────────────────
-- UUID / date / time / interval
-- ────────────────────────────────────────────────────────────────────

instance : FromField UUID where
  fromField f :=
    match f.result with
    | .uuid u => .ok u
    | _ => returnError .conversionFailed f "UUID" "expecting a UUID column"

instance : FromField Date where
  fromField f :=
    match f.result with
    | .date d => .ok d
    | _ => returnError .conversionFailed f "Date" "expecting a DATE column"

instance : FromField Time where
  fromField f :=
    match f.result with
    | .time t => .ok t
    | _ => returnError .conversionFailed f "Time" "expecting a TIME column"

instance : FromField TimeTz where
  fromField f :=
    match f.result with
    | .timeTz t => .ok t
    | _ => returnError .conversionFailed f "TimeTz" "expecting a TIME WITH TIME ZONE column"

instance : FromField Timestamp where
  fromField f :=
    match f.result with
    | .timestamp t => .ok t
    | _ => returnError .conversionFailed f "Timestamp" "expecting a TIMESTAMP column"

instance : FromField Interval where
  fromField f :=
    match f.result with
    | .interval i => .ok i
    | _ => returnError .conversionFailed f "Interval" "expecting an INTERVAL column"

-- ────────────────────────────────────────────────────────────────────
-- Lists / maps
-- ────────────────────────────────────────────────────────────────────

instance [FromField α] : FromField (Array α) where
  fromField f :=
    match f.result with
    | .list elems => elems.mapM (fun v => fromField (f.withValue v))
    | _ => returnError .conversionFailed f "Array" "expecting a LIST column"

instance [FromField α] : FromField (List α) where
  fromField f := Array.toList <$> fromField f

instance [FromField k] [FromField v] : FromField (Array (k × v)) where
  fromField f :=
    match f.result with
    | .map entries =>
      entries.mapM fun (kv, vv) => do
        let k ← fromField (f.withValue kv)
        let v ← fromField (f.withValue vv)
        pure (k, v)
    | _ => returnError .conversionFailed f "Array (k × v)" "expecting a MAP column"

end Database.DuckDB.Simple
