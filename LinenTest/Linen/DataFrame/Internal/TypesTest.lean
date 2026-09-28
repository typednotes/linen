/-
  Tests for `Linen.DataFrame.Internal.Types` — core DataFrame types.
-/
import Linen.DataFrame.Internal.Types

open DataFrame

namespace Tests.DataFrameTypes

/-! ### Value -/

#guard (Value.int 5).toInt? == some 5
#guard (Value.float 1.5).toFloat? == some 1.5
#guard (Value.int 3).toFloat? == some 3.0          -- int coerces to float
#guard (Value.str "x").toStr? == some "x"
#guard (Value.bool true).toBool? == some true
#guard Value.null.isNull == true
#guard (Value.int 5).isNull == false
#guard (Value.int 5).columnType == ColumnType.int
#guard Value.null.columnType == ColumnType.mixed
#guard toString (Value.bool true) == "true"
#guard toString (Value.str "hi") == "hi"
#guard toString (Value.int 42) == "42"

/-! ### Value ordering (int < float < str < bool < null) -/

#guard compare (Value.int 1) (Value.int 2) == Ordering.lt
#guard compare (Value.int 1) (Value.str "a") == Ordering.lt   -- cross-variant
#guard compare Value.null (Value.int 0) == Ordering.gt        -- null sorts last
#guard (Value.int 5 == Value.int 5)
#guard ((Value.int 5 == Value.float 5.0) == false)            -- distinct variants

/-! ### ColumnType -/

#guard toString ColumnType.float == "float"
#guard (ColumnType.int == ColumnType.int)
#guard ((ColumnType.int == ColumnType.str) == false)

/-! ### Column -/

private def colA : Column := { name := "a", values := #[Value.int 1, Value.int 2, Value.int 3], colType := .int }

#guard colA.size == 3
#guard colA.get? 1 == some (Value.int 2)
#guard colA.get? 5 == none
#guard (colA.map (fun _ => Value.null)).values == #[Value.null, Value.null, Value.null]
#guard (colA.map (fun _ => Value.null)).colType == ColumnType.mixed
#guard (colA.filter (· != Value.int 2)).values == #[Value.int 1, Value.int 3]
#guard toString colA == "Column(a, int, n=3)"

/-! ### DataFrame: empty / fromColumns -/

private def colB : Column := { name := "b", values := #[Value.str "x", Value.str "y", Value.str "z"], colType := .str }
private def df : Option DataFrame := DataFrame.fromColumns #[colA, colB]

#guard DataFrame.empty.nRows == 0
#guard DataFrame.empty.nColumns == 0
#guard df.isSome
#guard df.map (·.nRows) == some 3
#guard df.map (·.nColumns) == some 2
#guard df.map (·.columnNames) == some ["a", "b"]
-- inconsistent column lengths ⇒ none
#guard (DataFrame.fromColumns #[colA, { name := "c", values := #[Value.int 1], colType := .int }]).isNone
#guard (DataFrame.fromColumns #[]).map (·.nColumns) == some 0

/-! ### getColumn? / getRow? -/

#guard (df.bind (·.getColumn? "b")).map (·.name) == some "b"
#guard (df.bind (·.getColumn? "zzz")).isNone
#guard df.bind (·.getRow? 0) == some #[Value.int 1, Value.str "x"]
#guard df.bind (·.getRow? 2) == some #[Value.int 3, Value.str "z"]
#guard (df.bind (·.getRow? 5)).isNone

/-! ### fromRows (padding short rows with null) -/

private def dfr : DataFrame := DataFrame.fromRows #["x", "y"] #[#[Value.int 1, Value.int 2], #[Value.int 3, Value.int 4]]
#guard dfr.nRows == 2
#guard dfr.nColumns == 2
#guard dfr.columnNames == ["x", "y"]
#guard dfr.getRow? 0 == some #[Value.int 1, Value.int 2]
#guard dfr.getRow? 1 == some #[Value.int 3, Value.int 4]
#guard (DataFrame.fromRows #["x", "y", "z"] #[#[Value.int 1]]).getRow? 0
        == some #[Value.int 1, Value.null, Value.null]

/-! ### fromNamedColumns -/

#guard (DataFrame.fromNamedColumns #[("a", #[Value.int 1]), ("b", #[Value.int 2])]).map (·.columnNames)
        == some ["a", "b"]
#guard (DataFrame.fromNamedColumns #[("a", #[Value.int 1]), ("b", #[Value.int 2, Value.int 3])]).isNone

/-! ### GroupedDataFrame -/

#guard (GroupedDataFrame.mk #[] ["k1", "k2"]).groupKeys == ["k1", "k2"]

end Tests.DataFrameTypes
