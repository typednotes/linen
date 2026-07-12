/-
  Tests for `Linen.Database.DuckDB.FFI.Vector`.

  Exercises `createVector`/`destroy`, `getColumnType`, the typed
  read/write convenience wrappers (`getInt32`/`setInt32`, `getDouble`/
  `setDouble`, `getBool`/`setBool`), raw `getDataBytes`/`setDataBytes`,
  string assignment, and nested `LIST`/`STRUCT` child-vector access.
-/
import Linen.Database.DuckDB.FFI.Vector
import Linen.Database.DuckDB.FFI.LogicalTypes

open Database.DuckDB.FFI.Vector
open Database.DuckDB.FFI.LogicalTypes (create createListType createStructType getTypeId)
open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.Vector

-- Typed integer/double/bool accessors.
#eval show IO Unit from do
  let intTy ← create .integer
  let intVec ← createVector intTy 4
  setInt32 intVec 0 42
  let v0 ← getInt32 intVec 0
  unless v0 == 42 do throw (IO.userError s!"expected 42, got {v0}")
  destroy intVec

  let bigTy ← create .bigInt
  let bigVec ← createVector bigTy 4
  setInt64 bigVec 1 (-7)
  let v1 ← getInt64 bigVec 1
  unless v1 == -7 do throw (IO.userError s!"expected -7, got {v1}")
  destroy bigVec

  let dblTy ← create .double
  let dblVec ← createVector dblTy 4
  setDouble dblVec 2 3.5
  let v2 ← getDouble dblVec 2
  unless v2 == 3.5 do throw (IO.userError s!"expected 3.5, got {v2}")
  destroy dblVec

  let boolTy ← create .boolean
  let boolVec ← createVector boolTy 4
  setBool boolVec 3 true
  let v3 ← getBool boolVec 3
  unless v3 do throw (IO.userError "expected true")
  destroy boolVec

  Database.DuckDB.FFI.LogicalTypes.destroy intTy
  Database.DuckDB.FFI.LogicalTypes.destroy bigTy
  Database.DuckDB.FFI.LogicalTypes.destroy dblTy
  Database.DuckDB.FFI.LogicalTypes.destroy boolTy

-- getColumnType + raw byte access.
#eval show IO Unit from do
  let ty ← create .integer
  let vec ← createVector ty 4
  let colTy ← getColumnType vec
  let colTyId ← getTypeId colTy
  unless colTyId == .integer do throw (IO.userError "expected column type .integer")

  setDataBytes vec 0 (ByteArray.mk #[1, 0, 0, 0])
  let bytes ← getDataBytes vec 0 4
  unless bytes == ByteArray.mk #[1, 0, 0, 0] do
    throw (IO.userError s!"expected [1,0,0,0], got {bytes.toList}")

  Database.DuckDB.FFI.LogicalTypes.destroy colTy
  destroy vec
  Database.DuckDB.FFI.LogicalTypes.destroy ty

-- String assignment.
#eval show IO Unit from do
  let ty ← create .varchar
  let vec ← createVector ty 2
  assignStringElement vec 0 "hello"
  assignStringElementLen vec 1 "world".toUTF8
  destroy vec
  Database.DuckDB.FFI.LogicalTypes.destroy ty

-- LIST child vector.
#eval show IO Unit from do
  let childTy ← create .integer
  let listTy ← createListType childTy
  let listVec ← createVector listTy 4
  let listState ← listVectorSetSize listVec 2
  unless listState.isSuccess do throw (IO.userError "listVectorSetSize failed")
  let size ← listVectorGetSize listVec
  unless size == 2 do throw (IO.userError s!"expected list size 2, got {size}")
  let reserveState ← listVectorReserve listVec 10
  unless reserveState.isSuccess do throw (IO.userError "listVectorReserve failed")
  let child ← listVectorGetChild listVec
  let childTypeId ← getTypeId (← getColumnType child)
  unless childTypeId == .integer do throw (IO.userError "expected list child type .integer")

  destroy listVec
  Database.DuckDB.FFI.LogicalTypes.destroy listTy
  Database.DuckDB.FFI.LogicalTypes.destroy childTy

-- STRUCT child vector.
#eval show IO Unit from do
  let fieldTy ← create .varchar
  let structTy ← createStructType #[fieldTy] #["s"]
  let structVec ← createVector structTy 4
  let child ← structVectorGetChild structVec 0
  let childTypeId ← getTypeId (← getColumnType child)
  unless childTypeId == .varchar do throw (IO.userError "expected struct child type .varchar")

  destroy structVec
  Database.DuckDB.FFI.LogicalTypes.destroy structTy
  Database.DuckDB.FFI.LogicalTypes.destroy fieldTy

-- referenceVector.
#eval show IO Unit from do
  let ty ← create .integer
  let src ← createVector ty 4
  setInt32 src 0 99
  let dst ← createVector ty 4
  referenceVector dst src
  let v ← getInt32 dst 0
  unless v == 99 do throw (IO.userError s!"expected 99 after referenceVector, got {v}")

  destroy dst
  destroy src
  Database.DuckDB.FFI.LogicalTypes.destroy ty

end Tests.Database.DuckDB.FFI.Vector
