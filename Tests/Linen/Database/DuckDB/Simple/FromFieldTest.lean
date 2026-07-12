/-
  Tests for `Linen.Database.DuckDB.Simple.FromField`.

  `Materialize` (the module that would decode a live `duckdb_vector` into a
  `FieldValue`) is out of scope for this batch — see this module's doc — so
  these tests build `Field`s directly from hand-constructed `FieldValue`s
  (exactly the shape `Materialize` will eventually produce) and check
  `fromField`/`ResultError` against them, covering every scalar instance
  plus nested `list`/`map` decoding and the `Option`/null-handling wrapper.
-/
import Linen.Database.DuckDB.Simple.FromField

open Database.DuckDB.Simple
open Database.DuckDB.FFI.Types (Date Time Timestamp TimeTz Interval HugeInt UHugeInt Decimal)

namespace Tests.Database.DuckDB.Simple.FromField

private def field (v : FieldValue) : Field := { result := v, column := 0, columnLabel := none }

-- Booleans / integers.
#guard fromField (field (.boolean true)) == Ok.ok true
#guard fromField (field (.int8 (-5))) == (Ok.ok (-5 : Int8))
#guard fromField (field (.int16 1000)) == (Ok.ok (1000 : Int16))
#guard fromField (field (.int32 100000)) == (Ok.ok (100000 : Int32))
#guard fromField (field (.int64 (-1))) == (Ok.ok (-1 : Int64))
#guard fromField (field (.int32 7)) == (Ok.ok (7 : Int))
#guard fromField (field (.uint8 200)) == (Ok.ok (200 : UInt8))
#guard fromField (field (.uint16 40000)) == (Ok.ok (40000 : UInt16))
#guard fromField (field (.uint32 3000000000)) == (Ok.ok (3000000000 : UInt32))
#guard fromField (field (.uint64 1)) == (Ok.ok (1 : UInt64))
#guard fromField (field (.uint32 7)) == (Ok.ok (7 : Nat))
#guard fromField (field (.hugeInt { lower := 1, upper := 0 })) ==
  (Ok.ok ({ lower := 1, upper := 0 } : HugeInt))
#guard fromField (field (.uHugeInt { lower := 1, upper := 0 })) ==
  (Ok.ok ({ lower := 1, upper := 0 } : UHugeInt))

-- A type mismatch reports `.conversionFailed`, not a crash.
#guard
  match (fromField (field (.varchar "x")) : Ok Bool) with
  | .errors #[_] => true
  | _ => false

-- Floating point / decimal.
#guard fromField (field (.float 1.5)) == (Ok.ok (1.5 : Float32))
#guard fromField (field (.double 2.5)) == Ok.ok (2.5 : Float)
#guard fromField (field (.decimal { width := 10, scale := 2, value := { lower := 100, upper := 0 } })) ==
  (Ok.ok ({ width := 10, scale := 2, value := { lower := 100, upper := 0 } } : Decimal))

-- Text / blob / bit.
#guard fromField (field (.varchar "hello")) == Ok.ok "hello"
#guard fromField (field (.blob (ByteArray.mk #[1, 2, 3]))) == Ok.ok (ByteArray.mk #[1, 2, 3])
#guard
  fromField (field (.bitString { bits := #[true, false, true] })) ==
    Ok.ok ({ bits := #[true, false, true] } : BitString)
#guard toString ({ bits := #[true, false, true] } : BitString) == "101"

-- UUID / date / time / interval.
#guard fromField (field (.uuid { hi := 1, lo := 2 })) == Ok.ok ({ hi := 1, lo := 2 } : UUID)
#guard fromField (field (.date { days := 19723 })) == (Ok.ok ({ days := 19723 } : Date))
#guard fromField (field (.time { micros := 3600000000 })) ==
  (Ok.ok ({ micros := 3600000000 } : Time))
#guard fromField (field (.timestamp { micros := 0 })) == (Ok.ok ({ micros := 0 } : Timestamp))
#guard fromField (field (.interval { months := 1, days := 2, micros := 3 })) ==
  (Ok.ok ({ months := 1, days := 2, micros := 3 } : Interval))

-- `Option`: NULL decodes to `none`, a present value to `some`.
#guard (fromField (field .null) : Ok (Option Nat)) == Ok.ok none
#guard (fromField (field (.uint32 7)) : Ok (Option Nat)) == Ok.ok (some 7)

-- Nested `list`/`array` decoding.
#guard
  (fromField (field (.list #[.uint32 1, .uint32 2, .uint32 3])) : Ok (Array Nat)) ==
    Ok.ok #[1, 2, 3]
#guard
  (fromField (field (.list #[.uint32 1, .uint32 2])) : Ok (List Nat)) == Ok.ok [1, 2]

-- Nested `map` decoding.
#guard
  (fromField (field (.map #[(.varchar "a", .uint32 1), (.varchar "b", .uint32 2)])) :
      Ok (Array (String × Nat))) ==
    Ok.ok #[("a", 1), ("b", 2)]

-- `ResultError`'s `ToString`.
#guard
  toString (ResultError.incompatible "VARCHAR" "Bool" "not a bool") ==
    "incompatible DuckDB type VARCHAR and Lean type Bool: not a bool"
#guard
  toString (ResultError.unexpectedNull "INTEGER" "Nat" "no null allowed") ==
    "unexpected NULL of DuckDB type INTEGER for non-nullable Lean type Nat: no null allowed"
#guard
  toString (ResultError.conversionFailed "VARCHAR" "Nat" "not numeric") ==
    "could not convert DuckDB type VARCHAR to Lean type Nat: not numeric"

-- `FieldValue.typeName`.
#guard FieldValue.typeName .null == "NULL"
#guard FieldValue.typeName (.varchar "x") == "VARCHAR"
#guard FieldValue.typeName (.list #[]) == "LIST"

end Tests.Database.DuckDB.Simple.FromField
