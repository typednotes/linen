/-
  Tests for `Linen.Database.DuckDB.FFI.LogicalTypes`.

  Exercises primitive-type construction, `LIST`/`ARRAY`/`MAP`/`STRUCT`/
  `UNION`/`ENUM`/`DECIMAL` construction and their respective inspectors,
  and the alias getter/setter — all without needing a live connection.
-/
import Linen.Database.DuckDB.FFI.LogicalTypes

open Database.DuckDB.FFI.LogicalTypes
open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.LogicalTypes

-- Primitive type + alias.
#eval show IO Unit from do
  let intTy ← create .integer
  let id ← getTypeId intTy
  unless id == .integer do throw (IO.userError s!"expected .integer, got {repr id}")

  let aliasBefore ← getAlias intTy
  unless aliasBefore == none do throw (IO.userError "expected no alias by default")

  setAlias intTy "my_int"
  let aliasAfter ← getAlias intTy
  unless aliasAfter == some "my_int" do
    throw (IO.userError s!"expected alias 'my_int', got {aliasAfter}")

  destroy intTy

-- LIST.
#eval show IO Unit from do
  let child ← create .bigInt
  let listTy ← createListType child
  let listId ← getTypeId listTy
  unless listId == .list do throw (IO.userError s!"expected .list, got {repr listId}")

  let childBack ← listTypeChildType listTy
  let childId ← getTypeId childBack
  unless childId == .bigInt do throw (IO.userError s!"expected .bigInt child, got {repr childId}")

  destroy childBack
  destroy listTy
  destroy child

-- ARRAY.
#eval show IO Unit from do
  let child ← create .integer
  let arrTy ← createArrayType child 4
  let arrId ← getTypeId arrTy
  unless arrId == .array do throw (IO.userError s!"expected .array, got {repr arrId}")

  let size ← arrayTypeArraySize arrTy
  unless size == 4 do throw (IO.userError s!"expected array size 4, got {size}")

  destroy arrTy
  destroy child

-- MAP.
#eval show IO Unit from do
  let keyTy ← create .varchar
  let valTy ← create .integer
  let mapTy ← createMapType keyTy valTy
  let mapId ← getTypeId mapTy
  unless mapId == .map do throw (IO.userError s!"expected .map, got {repr mapId}")

  let keyBack ← mapTypeKeyType mapTy
  let keyId ← getTypeId keyBack
  unless keyId == .varchar do throw (IO.userError "expected varchar key")

  let valBack ← mapTypeValueType mapTy
  let valId ← getTypeId valBack
  unless valId == .integer do throw (IO.userError "expected integer value")

  destroy keyBack
  destroy valBack
  destroy mapTy
  destroy keyTy
  destroy valTy

-- STRUCT.
#eval show IO Unit from do
  let fieldA ← create .integer
  let fieldB ← create .varchar
  let structTy ← createStructType #[fieldA, fieldB] #["a", "b"]
  let structId ← getTypeId structTy
  unless structId == .struct do throw (IO.userError s!"expected .struct, got {repr structId}")

  let count ← structTypeChildCount structTy
  unless count == 2 do throw (IO.userError s!"expected 2 children, got {count}")

  let nameA ← structTypeChildName structTy 0
  unless nameA == some "a" do throw (IO.userError s!"expected child 0 named 'a', got {nameA}")

  let childTypeA ← structTypeChildType structTy 0
  let childIdA ← getTypeId childTypeA
  unless childIdA == .integer do throw (IO.userError "expected child 0 to be integer")

  destroy childTypeA
  destroy structTy
  destroy fieldA
  destroy fieldB

-- UNION.
#eval show IO Unit from do
  let memberA ← create .integer
  let memberB ← create .varchar
  let unionTy ← createUnionType #[memberA, memberB] #["i", "s"]
  let unionId ← getTypeId unionTy
  unless unionId == .union do throw (IO.userError s!"expected .union, got {repr unionId}")

  let count ← unionTypeMemberCount unionTy
  unless count == 2 do throw (IO.userError s!"expected 2 members, got {count}")

  let nameA ← unionTypeMemberName unionTy 0
  unless nameA == some "i" do throw (IO.userError s!"expected member 0 named 'i', got {nameA}")

  let memberTypeB ← unionTypeMemberType unionTy 1
  let memberIdB ← getTypeId memberTypeB
  unless memberIdB == .varchar do throw (IO.userError "expected member 1 to be varchar")

  destroy memberTypeB
  destroy unionTy
  destroy memberA
  destroy memberB

-- ENUM.
#eval show IO Unit from do
  let enumTy ← createEnumType #["red", "green", "blue"]
  let enumId ← getTypeId enumTy
  unless enumId == .enum do throw (IO.userError s!"expected .enum, got {repr enumId}")

  let size ← enumDictionarySize enumTy
  unless size == 3 do throw (IO.userError s!"expected dictionary size 3, got {size}")

  let value1 ← enumDictionaryValue enumTy 1
  unless value1 == some "green" do
    throw (IO.userError s!"expected dictionary value 1 to be 'green', got {value1}")

  let internalTy ← enumInternalType enumTy
  unless internalTy == .uTinyInt do
    throw (IO.userError s!"expected uint8 internal enum storage, got {repr internalTy}")

  destroy enumTy

-- DECIMAL.
#eval show IO Unit from do
  let decTy ← createDecimalType 10 2
  let decId ← getTypeId decTy
  unless decId == .decimal do throw (IO.userError s!"expected .decimal, got {repr decId}")

  let width ← decimalWidth decTy
  unless width == 10 do throw (IO.userError s!"expected width 10, got {width}")

  let scale ← decimalScale decTy
  unless scale == 2 do throw (IO.userError s!"expected scale 2, got {scale}")

  -- Width 10 exceeds DuckDB's int32 decimal-storage cutoff (width <= 9), so
  -- the internal storage type is int64 (`.bigInt`), not `.integer`.
  let internalTy ← decimalInternalType decTy
  unless internalTy == .bigInt do
    throw (IO.userError s!"expected bigInt internal decimal storage, got {repr internalTy}")

  destroy decTy

end Tests.Database.DuckDB.FFI.LogicalTypes
