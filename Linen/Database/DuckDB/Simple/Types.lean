/-
  Linen.Database.DuckDB.Simple.Types — `FormatError`, reused `Only`/row-cons,
  and a folded-in `UUID` type

  Module #4 of `docs/imports/duckdb-simple/dependencies.md`, on #1
  (`Linen.Database.DuckDB.Simple.Internal`, for `Query`/`SQLError`).

  ## Design

  - `Connection`/`Query`/`SQLError` are upstream's own re-exports of
    `Database.DuckDB.Simple.Internal`'s definitions; this port doesn't
    re-export them under a second name since every other `duckdb-simple`
    module already refers to `Database.DuckDB.Simple.Internal.{Connection,
    Query,SQLError}` (or, once imported, their unqualified names) directly.
  - `Only`/`(:.)` (upstream's row-cons) are **reused, not re-declared**: per
    `docs/imports/duckdb-simple/dependencies.md`'s precedence note, both the
    `sqlite-simple` and `duckdb-simple` `Types` modules are meant to share
    one definition. `Linen.Database.SQLite.Simple.Types.Only`/`.Cons`
    already exist (from the `sqlite-simple` import, which per that note's
    stated ordering lands first) and are exactly upstream's shape (a
    1-field tuple wrapper; a 2-field row-cons product with a `:.` infix
    former) — this module `export`s them into `Database.DuckDB.Simple`
    rather than opening a new `Only`/`Cons` type, matching the `export
    Foo (bar)` re-exposure pattern already used across this codebase (e.g.
    `Linen.Database.SQLite.Simple.FromField`'s `export FromField
    (fromField)`). The `:.` notation itself is declared unscoped in
    `Linen.Database.SQLite.Simple.Types`, so it becomes available here
    simply by importing that module — no separate re-export is needed for
    the notation itself.
  - `Null` is *not* re-exported here: `duckdb-simple`'s own `FromField`
    (module #5) represents SQL `NULL` as a `FieldValue` constructor rather
    than reusing the `sqlite-simple` `Null` placeholder type, so this
    module has no use for it (see `FromField`'s module doc for the exact
    reasoning once that module lands).
  - `FormatError` mirrors upstream exactly, substituting this port's own
    `Query` (from `Internal`) for upstream's, and a plain `List String` for
    upstream's `[String]` parameter dump.
  - `UUID` is `duckdb-simple`'s one genuinely new type for this batch: per
    the precedence note, the upstream `uuid`/`uuid-types` Hackage packages
    are folded directly into this module rather than opening a new
    `docs/imports/uuid/` entry, since `duckdb-simple` only ever uses their
    plain 128-bit `UUID` type and byte<->`UUID` conversions (never the
    generation machinery in `Data.UUID.V1`/`V3`/`V4`/`V5`, which is out of
    scope). It is represented as a `hi`/`lo` `UInt64` pair (the value's 16
    bytes in big-endian network order, split at the midpoint — the same
    encoding DuckDB's own `duckdb_hugeint`-based on-wire `UUID`
    representation uses), with conversions to/from a 16-byte `ByteArray`
    and the canonical `8-4-4-4-12` lowercase hex string form (`Data.UUID`'s
    `toString`/`fromString`).

  ## Haskell source
  - `Database.DuckDB.Simple.Types` (`duckdb-simple` package, version
    0.1.5.1)
  - `Data.UUID`/`Data.UUID.Types` (`uuid`/`uuid-types` packages, folded in
    per the precedence note above)
-/

import Linen.Database.DuckDB.Simple.Internal
import Linen.Database.SQLite.Simple.Types

namespace Database.DuckDB.Simple

export Database.SQLite.Simple.Types (Only Cons)

-- ────────────────────────────────────────────────────────────────────
-- FormatError
-- ────────────────────────────────────────────────────────────────────

/-- Raised when parameter formatting fails before a statement is executed
    (e.g. a parameter-count mismatch between a `Query` and its arguments). -/
structure FormatError where
  /-- Human-readable description of the mismatch. -/
  message : String
  /-- Query that triggered the formatting failure. -/
  query : Query
  /-- Rendered parameter values supplied by the caller (used for
      diagnostics). -/
  params : List String
deriving Repr, Inhabited

instance : BEq FormatError where
  beq a b := a.message == b.message && a.query == b.query && a.params == b.params

instance : ToString FormatError where
  toString e := s!"duckdb-simple: format error: {e.message} (query: {e.query.fromQuery})"

/-- Throw `err` as a plain `IO` exception (Lean's `IO` has no open exception
    hierarchy to throw a bespoke error type into directly — see
    `Database.DuckDB.Simple.throwSQLError`'s module doc note for the same
    substitution). -/
def FormatError.throwIO (err : FormatError) : IO α :=
  throw (IO.userError (toString err))

-- ────────────────────────────────────────────────────────────────────
-- UUID
-- ────────────────────────────────────────────────────────────────────

/-- A 128-bit UUID value, stored as its 16 bytes split into a big-endian
    `hi`/`lo` `UInt64` pair (see the module doc). -/
structure UUID where
  hi : UInt64
  lo : UInt64
deriving BEq, Ord, Repr, Inhabited

namespace UUID

/-- Split a `UInt64` into its 8 bytes, most-significant first. -/
private def toBytesBE (w : UInt64) : Array UInt8 :=
  #[ (w >>> 56).toUInt8, (w >>> 48).toUInt8, (w >>> 40).toUInt8, (w >>> 32).toUInt8,
     (w >>> 24).toUInt8, (w >>> 16).toUInt8, (w >>> 8).toUInt8, w.toUInt8 ]

/-- Reassemble a `UInt64` from 8 big-endian bytes found at `b[offset..offset+8)`
    (missing entries, which shouldn't occur given this module's own callers,
    default to `0`). -/
private def ofBytesBE (b : Array UInt8) (offset : Nat) : UInt64 :=
  let byte (i : Nat) : UInt64 := (b.getD (offset + i) 0).toUInt64
  (byte 0 <<< 56) ||| (byte 1 <<< 48) ||| (byte 2 <<< 40) ||| (byte 3 <<< 32) |||
  (byte 4 <<< 24) ||| (byte 5 <<< 16) ||| (byte 6 <<< 8) ||| byte 7

/-- The 16 raw bytes of `u`, most-significant first (`hi`'s bytes, then
    `lo`'s) — matches `Data.UUID.toByteString`'s big-endian layout. -/
def toBytes (u : UUID) : ByteArray :=
  ⟨toBytesBE u.hi ++ toBytesBE u.lo⟩

/-- Recover a `UUID` from its 16 raw bytes, `none` if `b` is not exactly 16
    bytes long (`Data.UUID.fromByteString`). -/
def ofBytes? (b : ByteArray) : Option UUID :=
  if b.data.size == 16 then
    some { hi := ofBytesBE b.data 0, lo := ofBytesBE b.data 8 }
  else
    none

private def hexDigits : List Char :=
  ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']

private def byteToHex (b : UInt8) : List Char :=
  [hexDigits[b.toNat / 16]!, hexDigits[b.toNat % 16]!]

/-- Render `u` in the canonical `8-4-4-4-12` lowercase hex form
    (`Data.UUID.toString`), e.g. `"550e8400-e29b-41d4-a716-446655440000"`. -/
def toCanonicalString (u : UUID) : String :=
  let hex := u.toBytes.data.toList.flatMap byteToHex
  let group (start len : Nat) : String := String.ofList ((hex.drop start).take len)
  s!"{group 0 8}-{group 8 4}-{group 12 4}-{group 16 4}-{group 20 12}"

instance : ToString UUID where
  toString := toCanonicalString

/-- The hex value (`0`-`15`) of an ASCII hex digit, case-insensitive. -/
private def hexValue? (c : Char) : Option UInt8 :=
  if '0' ≤ c && c ≤ '9' then
    some (UInt8.ofNat (c.toNat - '0'.toNat))
  else if 'a' ≤ c && c ≤ 'f' then
    some (UInt8.ofNat (c.toNat - 'a'.toNat + 10))
  else if 'A' ≤ c && c ≤ 'F' then
    some (UInt8.ofNat (c.toNat - 'A'.toNat + 10))
  else
    none

/-- Consume a list of hex-digit `Char`s two at a time into bytes, `none` if
    the list has odd length or contains a non-hex character. Structural
    recursion on the (strictly shrinking-by-two) input list. -/
private def hexPairsToBytes : List Char → Option (List UInt8)
  | [] => some []
  | [_] => none
  | a :: b :: rest => do
    let hi ← hexValue? a
    let lo ← hexValue? b
    let tail ← hexPairsToBytes rest
    pure ((hi * 16 + lo) :: tail)

/-- Parse the canonical `8-4-4-4-12` (dashed or bare) hex form of a `UUID`,
    `none` if `s` is not exactly 32 hex digits once dashes are stripped
    (`Data.UUID.fromString`). -/
def ofCanonicalString? (s : String) : Option UUID := do
  let hex := s.toList.filter (· ≠ '-')
  if hex.length ≠ 32 then
    none
  else
    let bytes ← hexPairsToBytes hex
    ofBytes? ⟨bytes.toArray⟩

end UUID

end Database.DuckDB.Simple
