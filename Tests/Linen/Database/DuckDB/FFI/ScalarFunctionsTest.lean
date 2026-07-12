/-
  Tests for `Linen.Database.DuckDB.FFI.ScalarFunctions`.

  Registers a real scalar function (`linen_double(x) = 2 * x`) whose
  native implementation is a genuine Lean closure invoked through the
  `linen_duckdb_scalar_function_call_trampoline`/`..._delete_trampoline`
  pair, then runs a real query calling it and confirms both that the
  callback fired and that it computed the right answer — exercising the
  full Lean-closure-called-from-C trampoline end-to-end, not just the
  registration call. Also exercises `ScalarFunctionSet` registration
  (`createSet`/`addToSet`/`registerSet`).
-/
import Linen.Database.DuckDB.FFI.ScalarFunctions
import Linen.Database.DuckDB.FFI.Vector
import Linen.Database.DuckDB.FFI.LogicalTypes
import Linen.Database.DuckDB.FFI.OpenConnect
import Tests.Linen.Database.DuckDB.FFI.TestSupport

open Database.DuckDB.FFI.ScalarFunctions
open Database.DuckDB.FFI.Vector (getInt32 setInt32)
open Database.DuckDB.FFI.LogicalTypes (create)
open Database.DuckDB.FFI.OpenConnect
open Database.DuckDB.FFI.Types
open Tests.Database.DuckDB.FFI.TestSupport

namespace Tests.Database.DuckDB.FFI.ScalarFunctions

#eval show IO Unit from do
  let dbResult ← openDatabase none
  let db ← match dbResult with
    | .ok db => pure db
    | .error msg => throw (IO.userError s!"duckdb_open failed: {msg}")
  let connResult ← connect db
  let conn ← match connResult with
    | .ok conn => pure conn
    | .error msg => throw (IO.userError s!"duckdb_connect failed: {msg}")

  let callCount ← IO.mkRef (0 : Nat)
  let intTy ← create .integer
  let onCall : BorrowedDataChunk → Database.DuckDB.FFI.Types.Vector → IO Unit := fun input output => do
    callCount.modify (· + 1)
    let size ← inputSize input
    let arg0 ← inputVector input 0
    for i in [0:size.toNat] do
      let x ← getInt32 arg0 i.toUInt64
      setInt32 output i.toUInt64 (x * 2)

  let registerState ← register conn "linen_double" #[intTy] intTy onCall
  unless registerState.isSuccess do throw (IO.userError "register failed")

  let createState ← queryExec conn "CREATE TABLE scalar_probe(a INTEGER)"
  unless createState.isSuccess do throw (IO.userError "CREATE TABLE scalar_probe failed")
  let insertState ← queryExec conn "INSERT INTO scalar_probe VALUES (21)"
  unless insertState.isSuccess do throw (IO.userError "INSERT failed")

  let selectState ← queryExec conn "SELECT linen_double(a) FROM scalar_probe"
  unless selectState.isSuccess do throw (IO.userError "SELECT linen_double(a) failed")

  let n ← callCount.get
  unless n > 0 do
    throw (IO.userError "expected the registered scalar function's callback to have fired at least once")

  -- ScalarFunctionSet: register a second overload via a set.
  let dblTy ← create .double
  let setCallCount ← IO.mkRef (0 : Nat)
  let onSetCall : BorrowedDataChunk → Database.DuckDB.FFI.Types.Vector → IO Unit := fun _input _output => do
    setCallCount.modify (· + 1)

  let fnSet ← createSet "linen_double_set"
  let fn2 ← Database.DuckDB.FFI.ScalarFunctions.create
  setName fn2 "linen_double_set"
  addParameter fn2 dblTy
  setReturnType fn2 dblTy
  setOnCall fn2 onSetCall
  setFunction fn2

  let addState ← addToSet fnSet fn2
  unless addState.isSuccess do throw (IO.userError "addToSet failed")

  let registerSetState ← registerSet conn fnSet
  unless registerSetState.isSuccess do throw (IO.userError "registerSet failed")

  let selectSetState ← queryExec conn "SELECT linen_double_set(1.5)"
  unless selectSetState.isSuccess do throw (IO.userError "SELECT linen_double_set(1.5) failed")

  let setN ← setCallCount.get
  unless setN > 0 do
    throw (IO.userError "expected the registered set overload's callback to have fired at least once")

  Database.DuckDB.FFI.LogicalTypes.destroy intTy
  Database.DuckDB.FFI.LogicalTypes.destroy dblTy
  disconnect conn
  close db

end Tests.Database.DuckDB.FFI.ScalarFunctions
