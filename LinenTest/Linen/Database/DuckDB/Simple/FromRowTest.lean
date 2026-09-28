/-
  Tests for `Linen.Database.DuckDB.Simple.FromRow`.

  Exercises the `RowParser` applicative and the `FromRow` tuple/`Only`/
  `Cons` instances (arity up to 7, the cutoff this port implements — see
  `Linen/Database/DuckDB/Simple/FromRow.lean`'s module doc) against
  hand-built `Field`s (the same style `FromFieldTest` uses, since no
  `duckdb_fetch_chunk`-style binding exists to decode a real row's `Field`s
  — see `Materialize`'s module doc for why).
-/
import Linen.Database.DuckDB.Simple.FromRow

open Database.DuckDB.Simple

namespace Tests.Database.DuckDB.Simple.FromRow

private def mkField (v : FieldValue) (col : Nat) : Field :=
  { result := v, column := col, columnLabel := none }

private def isErr : Ok α → Bool
  | .ok _ => false
  | .errors _ => true

-- `RowParser` combinators.
#guard (runFromRow (α := Only Int32) #[mkField (.int32 5) 0]) == Ok.ok ({ fromOnly := 5 } : Only Int32)

#guard (RowParser.run numFieldsRemaining #[mkField (.int32 1) 0, mkField (.int32 2) 1] 1)
  == Ok.ok (1, 1)

#guard isErr (RowParser.run (returnRowError "boom" : RowParser Int32) #[] 0)

-- running out of columns mid-parse is reported as a failure, not a crash
#guard isErr (runFromRow (α := Int32 × Int32) #[mkField (.int32 1) 0])

-- Tuple instances up to the arity-7 cutoff, against a hand-built wide row.
private def wideRow : Array Field :=
  #[mkField (.int32 1) 0, mkField (.varchar "two") 1, mkField (.double 3.0) 2,
    mkField (.int32 4) 3, mkField (.varchar "five") 4, mkField (.int32 6) 5,
    mkField (.varchar "seven") 6]

#guard runFromRow (α := Int32 × String) wideRow == Ok.ok (1, "two")
#guard runFromRow (α := Int32 × String × Float) wideRow == Ok.ok (1, "two", 3.0)
#guard runFromRow (α := Int32 × String × Float × Int32) wideRow == Ok.ok (1, "two", 3.0, 4)
#guard runFromRow (α := Int32 × String × Float × Int32 × String) wideRow ==
  Ok.ok (1, "two", 3.0, 4, "five")
#guard runFromRow (α := Int32 × String × Float × Int32 × String × Int32) wideRow ==
  Ok.ok (1, "two", 3.0, 4, "five", 6)
#guard runFromRow (α := Int32 × String × Float × Int32 × String × Int32 × String) wideRow ==
  Ok.ok (1, "two", 3.0, 4, "five", 6, "seven")

-- `Only`, consuming just the first column.
#guard runFromRow (α := Only Int32) wideRow == Ok.ok ({ fromOnly := 1 } : Only Int32)

-- `Cons`, composing two tuple-shaped `FromRow`s to reach beyond arity 7.
#guard
  runFromRow (α := Cons (Only Int32) (Only String))
    #[mkField (.int32 10) 0, mkField (.varchar "ten") 1] ==
  Ok.ok ({ car := { fromOnly := 10 }, cdr := { fromOnly := "ten" } } :
    Cons (Only Int32) (Only String))

end Tests.Database.DuckDB.Simple.FromRow
