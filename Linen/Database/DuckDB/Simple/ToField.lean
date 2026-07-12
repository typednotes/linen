/-
  Linen.Database.DuckDB.Simple.ToField — `DuckDBColumnType`, `FieldBinding`,
  `NamedParam`, the `ToField` class

  Module #7 of `docs/imports/duckdb-simple/dependencies.md`, on #5
  (`Linen.Database.DuckDB.Simple.FromField`, for `FieldValue`/`BitString`),
  #1 (`…Internal`, for `Query`/`SQLError`), #2 (`…LogicalRep`), and #4
  (`…Types`, for `UUID`), plus `Linen.Database.DuckDB.FFI.BindValues`.

  ## Design

  `ToField` is the binding-direction counterpart to `FromField`: a class
  converting a single Lean value into a `FieldBinding` — a small closure
  pairing the `DuckDBColumnType` the value binds as (used only for
  diagnostics; no live prepared statement carries per-column type
  metadata this port can otherwise report against) with the actual
  `PreparedStatement → Idx → IO Unit` binding action, built directly from
  one of `Linen.Database.DuckDB.FFI.BindValues`'s `bind*` calls. This
  mirrors upstream's own `Action`/`Field` split (a value renders to an
  action closure, not to an immediately-executed IO effect), just avoiding
  upstream's separate `Field` newtype wrapper — `FieldBinding` already *is*
  that wrapper, one field short (its own `columnType` is metadata,
  upstream's `Field` carries none).

  `DuckDBColumnType` is not a fresh enum: it is `Linen.Database.DuckDB.FFI.
  Types.Type_`, the exact `duckdb_type` tag `Database.DuckDB.Simple.
  FromField.FieldValue` and `Materialize` already key their own dispatch on
  — reusing it here (rather than declaring a second, ToField-flavoured type
  enum) keeps a single `duckdb_type` vocabulary across the whole
  `duckdb-simple` port, matching this port's general precedence rule against
  redeclaring something already ported.

  `NamedParam` pairs a parameter name with a `FieldBinding`, for
  `duckdb_bind_parameter_index`-style named parameters (`$foo`/`:foo`).
  Upstream spells its constructor as the infix operator `(:=)`
  (`"foo" := (42 :: Int)`); Lean reserves `:=` for core declaration/`let`
  syntax, so this port names the equivalent function `named` instead
  (`named "foo" (42 : Int)`) — a mechanical renaming forced by the token
  clash, not a design change.

  ### Scope

  Every instance below binds through an existing `bind*` call.
  `UUID`, `BitString`, and the `LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION`/`ENUM`
  container shapes upstream also has `ToField` instances for have **no**
  corresponding `duckdb_bind_uuid`/`duckdb_bind_blob`-for-BIT/
  `duckdb_bind_list`-etc. entry point in `duckdb.h` at all — DuckDB's C API
  only exposes `duckdb_bind_value` (a boxed `duckdb_value`) for those, and
  building a boxed `Value` from scratch is `Database.DuckDB.FFI.
  ValueInterface`, one of the 26 modules `docs/imports/duckdb-ffi/
  dependencies.md` explicitly excludes. This is the same documented "no FFI
  entry point exists yet" scope narrowing `Materialize`'s module doc already
  records for `VARINT`/pointer-variant `duckdb_string_t`, not a silent
  omission.

  ## Haskell source
  - `Database.DuckDB.Simple.ToField` (`duckdb-simple` package, version
    0.1.5.1)
-/

import Linen.Database.DuckDB.Simple.FromField
import Linen.Database.DuckDB.FFI.BindValues

namespace Database.DuckDB.Simple

open Database.DuckDB.FFI.Types (PreparedStatement Idx State Type_ HugeInt UHugeInt Decimal
  Date Time Timestamp Interval)
open Database.DuckDB.FFI.BindValues

-- ────────────────────────────────────────────────────────────────────
-- DuckDBColumnType
-- ────────────────────────────────────────────────────────────────────

/-- The `duckdb_type` a `FieldBinding` binds as (see the module doc for why
    this is `Type_` itself, not a fresh enum). -/
abbrev DuckDBColumnType : Type := Type_

-- ────────────────────────────────────────────────────────────────────
-- FieldBinding
-- ────────────────────────────────────────────────────────────────────

/-- A single query-parameter binding: the `DuckDBColumnType` it binds as
    (diagnostics only), plus the actual DuckDB `bind_*` call, deferred until
    a `PreparedStatement` and (1-indexed) parameter index are available. -/
structure FieldBinding where
  /-- The DuckDB column type this binding targets. -/
  columnType : DuckDBColumnType
  /-- Perform the bind against `stmt`'s parameter `paramIdx`, throwing an
      `IO` exception if the underlying `duckdb_bind_*` call reports
      failure. -/
  bind : PreparedStatement → Idx → IO Unit

/-- Raise `stmt`'s bind failure at `paramIdx` for `columnType` as a plain
    `IO` exception (see `Database.DuckDB.Simple.throwSQLError`'s module doc
    for why this substitutes for an upstream typed exception). -/
private def checkBindState (columnType : DuckDBColumnType) (paramIdx : Idx) : State → IO Unit
  | .success => pure ()
  | .error =>
    throw (IO.userError
      s!"duckdb-simple: failed to bind parameter {paramIdx} as {repr columnType}")

/-- Build a `FieldBinding` from one of `BindValues`'s `bind*` calls. -/
private def mkBinding (columnType : DuckDBColumnType)
    (bindCall : PreparedStatement → Idx → IO State) : FieldBinding :=
  { columnType
    bind := fun stmt paramIdx => do checkBindState columnType paramIdx (← bindCall stmt paramIdx) }

-- ────────────────────────────────────────────────────────────────────
-- NamedParam
-- ────────────────────────────────────────────────────────────────────

/-- A named query parameter (e.g. for `$foo`/`:foo`-style prepared
    statements), built via `named` (see the module doc for why this isn't
    the infix `(:=)` upstream uses). -/
structure NamedParam where
  /-- The parameter's name, without its `$`/`:` sigil. -/
  name : String
  /-- The value's rendered binding. -/
  binding : FieldBinding

-- ────────────────────────────────────────────────────────────────────
-- The `ToField` class
-- ────────────────────────────────────────────────────────────────────

/-- A type that may be bound as a single DuckDB prepared-statement
    parameter. -/
class ToField (α : Type u) where
  /-- Render a value into its `FieldBinding`. -/
  toField : α → FieldBinding

export ToField (toField)

/-- Build a `NamedParam` binding `value` under `name` (upstream's infix
    `(:=)` — see the module doc). -/
def named [ToField α] (name : String) (value : α) : NamedParam :=
  { name, binding := toField value }

-- ────────────────────────────────────────────────────────────────────
-- Identity / nullable wrappers
-- ────────────────────────────────────────────────────────────────────

instance : ToField FieldBinding where
  toField := id

instance [ToField α] : ToField (Option α) where
  toField
    | none =>
      { columnType := .sqlNull
        bind := fun stmt paramIdx => do checkBindState .sqlNull paramIdx (← bindNull stmt paramIdx) }
    | some a => toField a

-- ────────────────────────────────────────────────────────────────────
-- Booleans / integers
-- ────────────────────────────────────────────────────────────────────

instance : ToField Bool where
  toField b := mkBinding .boolean (fun stmt idx => bindBoolean stmt idx b)

instance : ToField Int8 where
  toField n := mkBinding .tinyInt (fun stmt idx => bindInt8 stmt idx n)

instance : ToField Int16 where
  toField n := mkBinding .smallInt (fun stmt idx => bindInt16 stmt idx n)

instance : ToField Int32 where
  toField n := mkBinding .integer (fun stmt idx => bindInt32 stmt idx n)

instance : ToField Int64 where
  toField n := mkBinding .bigInt (fun stmt idx => bindInt64 stmt idx n)

/-- Narrows to `Int64` the same truncating way upstream's own
    `fromIntegral :: Integer -> Int64` does (no overflow check) — the same
    substitution `Linen.Database.SQLite.Simple.ToField`'s module doc already
    documents for its own `Int` instance. -/
instance : ToField Int where
  toField n := mkBinding .bigInt (fun stmt idx => bindInt64 stmt idx (Int64.ofInt n))

instance : ToField UInt8 where
  toField n := mkBinding .uTinyInt (fun stmt idx => bindUInt8 stmt idx n)

instance : ToField UInt16 where
  toField n := mkBinding .uSmallInt (fun stmt idx => bindUInt16 stmt idx n)

instance : ToField UInt32 where
  toField n := mkBinding .uInteger (fun stmt idx => bindUInt32 stmt idx n)

instance : ToField UInt64 where
  toField n := mkBinding .uBigInt (fun stmt idx => bindUInt64 stmt idx n)

/-- Narrows to `UInt64` the same truncating way upstream's own `Word`
    instance does. -/
instance : ToField Nat where
  toField n := mkBinding .uBigInt (fun stmt idx => bindUInt64 stmt idx (UInt64.ofNat n))

instance : ToField HugeInt where
  toField i := mkBinding .hugeInt (fun stmt idx => bindHugeInt stmt idx i)

instance : ToField UHugeInt where
  toField i := mkBinding .uHugeInt (fun stmt idx => bindUHugeInt stmt idx i)

-- ────────────────────────────────────────────────────────────────────
-- Floating point / decimal
-- ────────────────────────────────────────────────────────────────────

instance : ToField Float32 where
  toField f := mkBinding .float (fun stmt idx => bindFloat stmt idx f)

instance : ToField Float where
  toField f := mkBinding .double (fun stmt idx => bindDouble stmt idx f)

instance : ToField Decimal where
  toField d := mkBinding .decimal (fun stmt idx => bindDecimal stmt idx d)

-- ────────────────────────────────────────────────────────────────────
-- Text / blob
-- ────────────────────────────────────────────────────────────────────

instance : ToField String where
  toField s := mkBinding .varchar (fun stmt idx => bindVarchar stmt idx s)

instance : ToField ByteArray where
  toField b := mkBinding .blob (fun stmt idx => bindBlob stmt idx b)

-- ────────────────────────────────────────────────────────────────────
-- Date / time / interval
-- ────────────────────────────────────────────────────────────────────

instance : ToField Date where
  toField d := mkBinding .date (fun stmt idx => bindDate stmt idx d)

instance : ToField Time where
  toField t := mkBinding .time (fun stmt idx => bindTime stmt idx t)

instance : ToField Timestamp where
  toField t := mkBinding .timestamp (fun stmt idx => bindTimestamp stmt idx t)

instance : ToField Interval where
  toField i := mkBinding .interval (fun stmt idx => bindInterval stmt idx i)

end Database.DuckDB.Simple
