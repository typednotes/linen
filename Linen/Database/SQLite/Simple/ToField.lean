/-
  Linen.Database.SQLite.Simple.ToField — the `ToField` class

  Module #10 of `docs/imports/sqlite-simple/dependencies.md`, on module #4
  (`Linen.Database.SQLite`, for `SQLData`), module #5
  (`Linen.Database.SQLite.Simple.Types`, for `Null`), and module #7/#8
  (`Linen.Database.SQLite.Simple.Time`, for the `Day`/`UTCTime` textual
  rendering used by their instances).

  ## Design

  `ToField` converts a single Lean value into the untyped `SQLData` that
  `Database.SQLite3.bind` accepts as a query parameter. This mirrors
  upstream's own `ToField` exactly: one class, one method, no default
  signature.

  ### Numeric instances

  Upstream provides one instance per fixed-width Haskell integer type
  (`Int8`/`Int16`/`Int32`/`Int`/`Int64`/`Integer`, `Word8`/`Word16`/`Word32`/
  `Word`/`Word64`), all funnelling through `fromIntegral` into a single
  `SQLInteger` (64-bit) column value. This port keeps one instance per Lean
  fixed-width type that mirrors an upstream one (`Int8`/`Int16`/`Int32`/
  `Int64`, `UInt8`/`UInt16`/`UInt32`/`UInt64`), plus:

  - `Int`, substituting upstream's arbitrary-precision `Integer` (the same
    substitution `Linen.Database.SQLite.Simple.Ok`'s module doc's siblings
    use elsewhere in this port), narrowed to 64 bits the same way upstream's
    own `fromIntegral :: Integer -> Int64` narrows silently (no overflow
    check — a value outside `Int64`'s range is truncated, matching
    upstream).
  - `Nat`, substituting upstream's machine-word `Word` (there is no
    single Lean type that is simultaneously unsigned and machine-word-sized
    the way GHC's `Word` is; `Nat` is the closest "unsigned, no fixed
    ceiling" analogue, narrowed the same truncating way).

  `Float` (Lean's only floating-point type) covers both upstream's `Float`
  and `Double` instances, since Lean draws no such distinction.

  ### Text/blob instances

  `String` covers both upstream's `Text` and `[Char]`/`LT.Text` instances
  (this port has already collapsed all of those onto Lean's native `String`
  everywhere else); `ByteArray` likewise covers `ByteString`/`LB.ByteString`.

  ### Time instances

  `Day`/`UTCTime` render through `Linen.Database.SQLite.Simple.Time`'s
  `dayToString`/`utcTimeToString`, matching upstream's own `SQLText`-via-
  builder rendering. Upstream's commented-out `ZonedTime`/`LocalTime`/
  `TimeOfDay`/`*Timestamp`/`Date` instances are left disabled there too (the
  module doc marks them `-- TODO enable these`), so this port has no
  corresponding instances either — nothing to port.

  ## Haskell source
  - `Database.SQLite.Simple.ToField` (`sqlite-simple` package)
-/

import Linen.Database.SQLite
import Linen.Database.SQLite.Simple.Types
import Linen.Database.SQLite.Simple.Time

namespace Database.SQLite.Simple

open Database.SQLite3 (SQLData)

-- ────────────────────────────────────────────────────────────────────
-- The `ToField` class
-- ────────────────────────────────────────────────────────────────────

/-- A type that may be used as a single parameter to a SQL query, by
    rendering it into the untyped `SQLData` that `Database.SQLite3.bind`
    accepts. -/
class ToField (α : Type u) where
  /-- Render a value for binding as a query parameter. -/
  toField : α → SQLData

export ToField (toField)

-- ────────────────────────────────────────────────────────────────────
-- Identity / nullable wrappers
-- ────────────────────────────────────────────────────────────────────

instance : ToField SQLData where
  toField := id

instance [ToField α] : ToField (Option α) where
  toField
    | none => .null
    | some a => toField a

instance : ToField Types.Null where
  toField _ := .null

-- ────────────────────────────────────────────────────────────────────
-- Boolean
-- ────────────────────────────────────────────────────────────────────

instance : ToField Bool where
  toField b := .integer (if b then 1 else 0)

-- ────────────────────────────────────────────────────────────────────
-- Integers (see the module doc for the type-by-type mapping)
-- ────────────────────────────────────────────────────────────────────

instance : ToField Int8 where
  toField n := .integer n.toInt64

instance : ToField Int16 where
  toField n := .integer n.toInt64

instance : ToField Int32 where
  toField n := .integer n.toInt64

instance : ToField Int64 where
  toField n := .integer n

instance : ToField Int where
  toField n := .integer n.toInt64

instance : ToField UInt8 where
  toField n := .integer n.toUInt64.toInt64

instance : ToField UInt16 where
  toField n := .integer n.toUInt64.toInt64

instance : ToField UInt32 where
  toField n := .integer n.toUInt64.toInt64

instance : ToField UInt64 where
  toField n := .integer n.toInt64

instance : ToField Nat where
  toField n := .integer (Int64.ofInt (Int.ofNat n))

-- ────────────────────────────────────────────────────────────────────
-- Floating point
-- ────────────────────────────────────────────────────────────────────

instance : ToField Float where
  toField f := .float f

-- ────────────────────────────────────────────────────────────────────
-- Text / blob
-- ────────────────────────────────────────────────────────────────────

instance : ToField String where
  toField s := .text s

instance : ToField ByteArray where
  toField b := .blob b

-- ────────────────────────────────────────────────────────────────────
-- Date/time
-- ────────────────────────────────────────────────────────────────────

instance : ToField Data.Time.Day where
  toField d := .text (Database.SQLite.Simple.Time.dayToString d)

instance : ToField Data.Time.UTCTime where
  toField t := .text (Database.SQLite.Simple.Time.utcTimeToString t)

end Database.SQLite.Simple
