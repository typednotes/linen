/-
  Tests for `Linen.Database.DuckDB.Simple.Materialize`.

  No `duckdb_fetch_chunk`-style binding exists to pull a live result chunk
  (see the module's own doc), so these tests build synthetic `DataChunk`s
  directly via `createDataChunk` + `LogicalTypes.create`, populate each
  column's `Vector` through `Vector`'s own setters (`setBool`/`setInt32`/
  `setInt64`/`setDouble`/`assignStringElementLen`/raw `setDataBytes`), and
  confirm `materializeValue`/`materializeColumn` decode the exact
  `FieldValue`s that were written — a real end-to-end round trip through
  DuckDB's own C vector representation, not a hand-rolled stub.

  `FieldValue` (already ported, in `FromField.lean`) has no `BEq` instance
  (it nests `StructValue`/`UnionValue`, not every field of which derives
  `BEq`), so results below are checked with explicit `match`es rather than
  `==`.
-/
import Linen.Database.DuckDB.Simple.Materialize

open Database.DuckDB.Simple
open Database.DuckDB.Simple.Materialize
open Database.DuckDB.FFI
open Database.DuckDB.FFI.Types (Type_ Idx Vector)

namespace Tests.Database.DuckDB.Simple.Materialize

/-- Build a single-column, single-row chunk of `ty`, run `setup` against its
    vector, materialize row `0`, then tear the chunk down. -/
private def withOneColumnValue (ty : Type_) (setup : Vector → IO Unit) : IO FieldValue := do
  let lt ← LogicalTypes.create ty
  let chunk ← DataChunk.createDataChunk #[lt]
  try
    DataChunk.setSize chunk 1
    let vector ← DataChunk.getVector chunk 0
    setup vector
    materializeValue vector 0
  finally
    LogicalTypes.destroy lt
    DataChunk.destroy chunk

-- Booleans / fixed-width integers, round-tripped through `Vector`'s typed
-- setters (`BOOLEAN`/`INTEGER`/`BIGINT`) or raw `setDataBytes` (everything
-- else, matching `materializeValueFuel`'s own decode widths).
#eval show IO Unit from do
  let v ← withOneColumnValue .boolean (fun vec => Vector.setBool vec 0 true)
  match v with
  | .boolean true => pure ()
  | _ => throw (IO.userError "boolean round trip failed")

  let v ← withOneColumnValue .integer (fun vec => Vector.setInt32 vec 0 (-42))
  match v with
  | .int32 (-42) => pure ()
  | _ => throw (IO.userError "int32 round trip failed")

  let v ← withOneColumnValue .bigInt (fun vec => Vector.setInt64 vec 0 12345678901)
  match v with
  | .int64 12345678901 => pure ()
  | _ => throw (IO.userError "int64 round trip failed")

  let v ← withOneColumnValue .double (fun vec => Vector.setDouble vec 0 2.5)
  match v with
  | .double d => unless d == 2.5 do throw (IO.userError "double round trip failed")
  | _ => throw (IO.userError "double round trip failed")

  -- `UINTEGER`: no typed setter, so write the raw little-endian bytes
  -- directly (mirrors `materializeValueFuel`'s own `.uInteger` decode path).
  let v ← withOneColumnValue .uInteger (fun vec =>
    Vector.setDataBytes vec 0 (ByteArray.mk #[0xD2, 0x02, 0x96, 0x49]))
  match v with
  | .uint32 1234567890 => pure ()
  | _ => throw (IO.userError "uint32 round trip failed")

-- `VARCHAR`, via `assignStringElementLen` (inlined, ≤ 12 bytes).
#eval show IO Unit from do
  let v ← withOneColumnValue .varchar (fun vec =>
    Vector.assignStringElementLen vec 0 (String.toUTF8 "hello"))
  match v with
  | .varchar "hello" => pure ()
  | _ => throw (IO.userError "varchar round trip failed")

-- `BLOB`, same inlined path as `VARCHAR`.
#eval show IO Unit from do
  let v ← withOneColumnValue .blob (fun vec =>
    Vector.assignStringElementLen vec 0 (ByteArray.mk #[1, 2, 3]))
  match v with
  | .blob b => unless b == ByteArray.mk #[1, 2, 3] do throw (IO.userError "blob round trip failed")
  | _ => throw (IO.userError "blob round trip failed")

-- `NULL`: an unwritten validity bit reports `.null`, not a decode of
-- whatever garbage bytes happen to sit in the column.
#eval show IO Unit from do
  let v ← withOneColumnValue .integer (fun vec => do
    Vector.ensureValidityWritable vec
    let some mask ← Vector.getValidity vec
      | throw (IO.userError "expected a validity mask after ensureValidityWritable")
    Validity.setRowInvalid mask 0)
  match v with
  | .null => pure ()
  | _ => throw (IO.userError "NULL round trip failed")

-- `LIST`: a two-element `INTEGER` list.
#eval show IO Unit from do
  let childTy ← LogicalTypes.create .integer
  let listTy ← LogicalTypes.createListType childTy
  let chunk ← DataChunk.createDataChunk #[listTy]
  try
    DataChunk.setSize chunk 1
    let vector ← DataChunk.getVector chunk 0
    Vector.setDataBytes vector 0 (ByteArray.mk #[0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0])
    let _ ← Vector.listVectorSetSize vector 2
    let child ← Vector.listVectorGetChild vector
    Vector.setInt32 child 0 10
    Vector.setInt32 child 1 20
    let v ← materializeValue vector 0
    match v with
    | .list #[.int32 10, .int32 20] => pure ()
    | _ => throw (IO.userError "list round trip failed")
  finally
    LogicalTypes.destroy childTy
    LogicalTypes.destroy listTy
    DataChunk.destroy chunk

-- `materializeColumn`: two rows of `BOOLEAN`.
#eval show IO Unit from do
  let lt ← LogicalTypes.create .boolean
  let chunk ← DataChunk.createDataChunk #[lt]
  try
    DataChunk.setSize chunk 2
    let vector ← DataChunk.getVector chunk 0
    Vector.setBool vector 0 true
    Vector.setBool vector 1 false
    let vs ← materializeColumn chunk 0
    match vs with
    | #[.boolean true, .boolean false] => pure ()
    | _ => throw (IO.userError "materializeColumn round trip failed")
  finally
    LogicalTypes.destroy lt
    DataChunk.destroy chunk

#guard maxNestingDepth == 64

end Tests.Database.DuckDB.Simple.Materialize
