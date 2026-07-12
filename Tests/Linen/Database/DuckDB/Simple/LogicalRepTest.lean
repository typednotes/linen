/-
  Tests for `Linen.Database.DuckDB.Simple.LogicalRep`.

  Round-trips a `LogicalTypeRep` tree through `logicalTypeFromRep` (building
  a real `duckdb_logical_type` handle) and back through `logicalTypeToRep`
  (walking that handle back into a `LogicalTypeRep`), checking the result
  matches the original — for a scalar, a `DECIMAL`, an `ENUM`, and nested
  `LIST`/`ARRAY`/`MAP`/`STRUCT`/`UNION` shapes, exercising every branch of
  both directions end-to-end against real DuckDB C API calls.
-/
import Linen.Database.DuckDB.Simple.LogicalRep

open Database.DuckDB.Simple.LogicalRep
open Database.DuckDB.FFI.Types (Type_)

namespace Tests.Database.DuckDB.Simple.LogicalRep

/-- Build `rep`'s live handle, walk it back to a `LogicalTypeRep`, destroy
    the handle, and check the round trip is faithful. -/
def roundTrips (rep : LogicalTypeRep) : IO Bool := do
  let handle ← logicalTypeFromRep rep
  let back ← logicalTypeToRep handle
  Database.DuckDB.FFI.LogicalTypes.destroy handle
  pure (back == rep)

#eval show IO Unit from do
  let cases : List LogicalTypeRep :=
    [ .scalar .integer
    , .scalar .varchar
    , .decimal 10 2
    , .enum ["red", "green", "blue"]
    , .list (.scalar .bigInt)
    , .array (.scalar .double) 4
    , .map (.scalar .varchar) (.scalar .integer)
    , .struct ["a", "b"] [.scalar .integer, .scalar .varchar]
    , .union ["i", "s"] [.scalar .integer, .scalar .varchar]
    , .list (.struct ["x", "y"] [.scalar .integer, .scalar .integer]) -- nested
    ]
  for c in cases do
    let ok ← roundTrips c
    unless ok do throw (IO.userError s!"round trip failed for {repr c}")

-- `StructValue.field?` looks fields up by name, `none` if absent.
#guard
  (StructValue.mk #[{ name := "a", value := (1 : Nat) }, { name := "b", value := 2 }]).field?
      "b" == some { name := "b", value := 2 }
#guard
  (StructValue.mk (α := Nat) #[{ name := "a", value := 1 }]).field? "missing" == none

-- The `Nat`-fuel depth cap: a type nested deeper than `maxNestingDepth`
-- reports an explicit error rather than looping or crashing.
#eval show IO Unit from do
  let deep : LogicalTypeRep :=
    (List.range (maxNestingDepth + 1)).foldl (fun rep _ => .list rep) (.scalar .integer)
  let handle ← logicalTypeFromRep deep
  let mut sawError := false
  try
    let _ ← logicalTypeToRep handle
    pure ()
  catch _ =>
    sawError := true
  Database.DuckDB.FFI.LogicalTypes.destroy handle
  unless sawError do throw (IO.userError "expected max nesting depth to be exceeded")

end Tests.Database.DuckDB.Simple.LogicalRep
