/-
  Linen.Database.SQLite.Simple.FromField — the `FromField` class, `ResultError`

  Module #11 of `docs/imports/sqlite-simple/dependencies.md`, on module #4
  (`Linen.Database.SQLite`, for `SQLData`), module #5 (`…Types`, for
  `Null`), module #6 (`…Ok`), module #7/#8 (`…Time`, for `Day`/`UTCTime`
  parsing), and module #9 (`…Internal`, for `Field`).

  ## Design

  `FromField` decodes a single `Field` (a column value plus positional
  metadata, from module #9) into a Lean value, reporting failure as an
  `Ok.errors` message rather than throwing.

  ### `ResultError`

  Upstream's `ResultError` is a `SomeException`-wrapping exception type with
  three constructors (`Incompatible`/`UnexpectedNull`/`ConversionFailed`),
  each carrying the same three `String` fields (`errSQLType`,
  `errHaskellType`, `errMessage`). `Linen.Database.SQLite.Simple.Ok`'s module
  doc already records that this port has no open exception hierarchy to
  throw such a value into — `Ok`'s error type is `Array String`. This port
  therefore keeps `ResultError` as a plain descriptive value (still with its
  three upstream constructors and three fields, via `ResultError.sqlType`/
  `haskellType`/`message` accessors and a `ToString` instance), but
  `returnError` folds it directly into an `Ok.errors` message via `toString`
  rather than wrapping it as `SomeException` — matching how `Ok.fail` already
  substitutes for `MonadThrow`/`MonadFail` in the `Ok` port.

  Upstream's `returnError` additionally captures the *caller's* target type
  name via `Typeable`/`typeOf (undefined :: a)` — runtime type reflection
  with no general Lean counterpart. Every instance below instead passes its
  own type's name as a literal string argument, which upstream's `Typeable`
  trick would have produced automatically; the resulting messages are
  textually identical for every instance defined here.

  ### Numeric instances

  Mirrors `Linen.Database.SQLite.Simple.ToField`'s numeric type list in
  reverse: one instance per fixed-width Lean integer type that mirrors an
  upstream one, plus `Int` (substituting `Integer`) and `Nat` (substituting
  `Word`, see that module's doc for why). All decode from a `Field` holding
  `.integer (i : Int64)`, narrowing via the same truncating
  `Int64.to*`-family conversions `ToField`'s instances use in the other
  direction — **except** `Nat`: a negative `Int64` has no non-negative
  `Nat` counterpart to truncate to (unlike Haskell's `Word`, whose
  `fromIntegral` wraps around via two's complement), so this port clamps a
  negative source value to `0` (`Int64.toNatClampNeg`) instead of wrapping,
  a deliberate deviation for a case upstream's own `Word` instance leaves to
  GHC's word-size-dependent, unspecified wraparound behaviour.

  `Double` upstream is one of two floating-point instances (alongside
  `Float`, which narrows via `double2Float`); since Lean's `Float` is
  already double-precision and Lean draws no `Float`/`Double` distinction,
  a single `Float` instance covers both.

  ### Text/blob/time instances

  `String` covers upstream's `Text`/`[Char]`/`LT.Text` instances;
  `ByteArray` covers `ByteString`/`LB.ByteString`; `Day`/`UTCTime` parse
  through `Linen.Database.SQLite.Simple.Time`'s `parseDay`/`parseUTCTime`,
  matching upstream's own delegation to the same module.

  ## Haskell source
  - `Database.SQLite.Simple.FromField` (`sqlite-simple` package)
-/

import Linen.Database.SQLite
import Linen.Database.SQLite.Simple.Types
import Linen.Database.SQLite.Simple.Ok
import Linen.Database.SQLite.Simple.Time
import Linen.Database.SQLite.Simple.Internal

namespace Database.SQLite.Simple

open Database.SQLite3 (SQLData)

-- ────────────────────────────────────────────────────────────────────
-- `ResultError`
-- ────────────────────────────────────────────────────────────────────

/-- The reason a `FromField` conversion failed (see the module doc for how
    this substitutes for upstream's exception-based `ResultError`). -/
inductive ResultError where
  /-- The SQL and Lean types are not compatible. -/
  | incompatible (sqlType haskellType message : String)
  /-- A SQL `NULL` was encountered where the Lean type did not permit one. -/
  | unexpectedNull (sqlType haskellType message : String)
  /-- The SQL value could not be parsed as, or represented by, the target
      Lean type. -/
  | conversionFailed (sqlType haskellType message : String)
deriving Repr, Inhabited, BEq

namespace ResultError

/-- The offending column's declared SQL type name. -/
def sqlType : ResultError → String
  | .incompatible t _ _ | .unexpectedNull t _ _ | .conversionFailed t _ _ => t

/-- The target Lean type's name. -/
def haskellType : ResultError → String
  | .incompatible _ t _ | .unexpectedNull _ t _ | .conversionFailed _ t _ => t

/-- A free-form description of the failure. -/
def message : ResultError → String
  | .incompatible _ _ m | .unexpectedNull _ _ m | .conversionFailed _ _ m => m

end ResultError

instance : ToString ResultError where
  toString
    | .incompatible sqlT hsT msg =>
      s!"incompatible SQL type {sqlT} and Lean type {hsT}: {msg}"
    | .unexpectedNull sqlT hsT msg =>
      s!"unexpected NULL in SQL type {sqlT} for non-nullable Lean type {hsT}: {msg}"
    | .conversionFailed sqlT hsT msg =>
      s!"could not convert SQL type {sqlT} to Lean type {hsT}: {msg}"

-- ────────────────────────────────────────────────────────────────────
-- The `FromField` class
-- ────────────────────────────────────────────────────────────────────

/-- A field-decoding function, from a `Field` to an error-accumulating
    result. -/
abbrev FieldParser (α : Type u) : Type u := Field → Ok α

/-- A type that may be decoded from a single SQL result column. -/
class FromField (α : Type u) where
  /-- Convert a decoded column (`Field`) to a Lean value. -/
  fromField : FieldParser α

export FromField (fromField)

/-- Build a failed `Ok` from one of `ResultError`'s three constructors, the
    offending `Field`, and the target Lean type's name (see the module doc
    for why this must be passed explicitly rather than inferred). -/
def returnError (mk : String → String → String → ResultError) (f : Field)
    (haskellType message : String) : Ok α :=
  Ok.fail (toString (mk f.typeName haskellType message))

-- ────────────────────────────────────────────────────────────────────
-- Identity / nullable wrappers
-- ────────────────────────────────────────────────────────────────────

instance : FromField SQLData where
  fromField f := .ok f.result

instance [FromField α] : FromField (Option α) where
  fromField f :=
    if f.result == .null then .ok none else some <$> fromField f

instance : FromField Types.Null where
  fromField f :=
    if f.result == .null then .ok .null
    else returnError .conversionFailed f "Null" "data is not null"

-- ────────────────────────────────────────────────────────────────────
-- Integers (see the module doc for the type-by-type mapping)
-- ────────────────────────────────────────────────────────────────────

private def takeInt (narrow : Int64 → α) (haskellType : String) (f : Field) : Ok α :=
  match f.result with
  | .integer i => .ok (narrow i)
  | _ => returnError .conversionFailed f haskellType "need an INTEGER column"

instance : FromField Int8 where
  fromField := takeInt Int64.toInt8 "Int8"

instance : FromField Int16 where
  fromField := takeInt Int64.toInt16 "Int16"

instance : FromField Int32 where
  fromField := takeInt Int64.toInt32 "Int32"

instance : FromField Int64 where
  fromField := takeInt id "Int64"

instance : FromField Int where
  fromField := takeInt Int64.toInt "Int"

instance : FromField UInt8 where
  fromField := takeInt (fun i => i.toUInt64.toUInt8) "UInt8"

instance : FromField UInt16 where
  fromField := takeInt (fun i => i.toUInt64.toUInt16) "UInt16"

instance : FromField UInt32 where
  fromField := takeInt (fun i => i.toUInt64.toUInt32) "UInt32"

instance : FromField UInt64 where
  fromField := takeInt Int64.toUInt64 "UInt64"

instance : FromField Nat where
  fromField := takeInt Int64.toNatClampNeg "Nat"

-- ────────────────────────────────────────────────────────────────────
-- Floating point
-- ────────────────────────────────────────────────────────────────────

instance : FromField Float where
  fromField f :=
    match f.result with
    | .float x => .ok x
    | _ => returnError .conversionFailed f "Float" "expecting a FLOAT column"

-- ────────────────────────────────────────────────────────────────────
-- Boolean
-- ────────────────────────────────────────────────────────────────────

instance : FromField Bool where
  fromField f :=
    match f.result with
    | .integer 0 => .ok false
    | .integer 1 => .ok true
    | .integer b => returnError .conversionFailed f "Bool" s!"bool must be 0 or 1, got {b}"
    | _ => returnError .conversionFailed f "Bool" "expecting an INTEGER column"

-- ────────────────────────────────────────────────────────────────────
-- Text / blob
-- ────────────────────────────────────────────────────────────────────

instance : FromField String where
  fromField f :=
    match f.result with
    | .text s => .ok s
    | _ => returnError .conversionFailed f "String" "need a TEXT column"

instance : FromField ByteArray where
  fromField f :=
    match f.result with
    | .blob b => .ok b
    | _ => returnError .conversionFailed f "ByteArray" "expecting a BLOB column"

-- ────────────────────────────────────────────────────────────────────
-- Date/time
-- ────────────────────────────────────────────────────────────────────

instance : FromField Data.Time.Day where
  fromField f :=
    match f.result with
    | .text t =>
      match Database.SQLite.Simple.Time.parseDay t with
      | .ok d => .ok d
      | .error e => returnError .conversionFailed f "Day" s!"couldn't parse Day field: {e}, field contents: {t}"
    | _ => returnError .conversionFailed f "Day" "expecting a TEXT column"

instance : FromField Data.Time.UTCTime where
  fromField f :=
    match f.result with
    | .text t =>
      match Database.SQLite.Simple.Time.parseUTCTime t with
      | .ok time => .ok time
      | .error e => returnError .conversionFailed f "UTCTime" s!"couldn't parse UTCTime field: {e}, field contents: {t}"
    | _ => returnError .conversionFailed f "UTCTime" "expecting a TEXT column"

end Database.SQLite.Simple
