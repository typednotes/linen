/-
  Tests for `Linen.DataFrame.Internal.Column` — column-level operations.
-/
import Linen.DataFrame.Internal.Column

open DataFrame

namespace Tests.DataFrameColumn

/-! ### inferType -/

#guard Column.inferType #[Value.int 1, Value.int 2] == ColumnType.int
#guard Column.inferType #[Value.int 1, Value.str "x"] == ColumnType.mixed
#guard Column.inferType #[Value.null, Value.int 1] == ColumnType.int      -- nulls skipped
#guard Column.inferType #[] == ColumnType.mixed
#guard Column.inferType #[Value.null, Value.null] == ColumnType.mixed     -- all-null

/-! ### mk' (auto-inferred type) -/

#guard (Column.mk' "a" #[Value.int 1, Value.int 2]).colType == ColumnType.int
#guard (Column.mk' "a" #[Value.int 1, Value.int 2]).name == "a"
#guard (Column.mk' "a" #[Value.int 1, Value.bool true]).colType == ColumnType.mixed

/-! ### mapValues / reInferType -/

private def c : Column := Column.mk' "x" #[Value.int 1, Value.int 2, Value.int 3]

#guard (Column.mapValues (fun _ => Value.str "y") c).colType == ColumnType.mixed
#guard (Column.mapValues (fun _ => Value.str "y") c).values == #[Value.str "y", Value.str "y", Value.str "y"]
#guard (Column.reInferType (Column.mapValues (fun v => v) c)).colType == ColumnType.int

/-! ### filterByMask (positional; zip stops at shorter) -/

#guard (Column.filterByMask #[true, false, true] c).values == #[Value.int 1, Value.int 3]
#guard (Column.filterByMask #[true] c).values == #[Value.int 1]                       -- shorter mask
#guard (Column.filterByMask #[true, true, true, true] c).values == #[Value.int 1, Value.int 2, Value.int 3]  -- longer
#guard (Column.filterByMask #[false, false, false] c).values == #[]

/-! ### toFloats / toStrings -/

#guard Column.toFloats (Column.mk' "x" #[Value.int 2, Value.str "a", Value.float 1.5]) == #[some 2.0, none, some 1.5]
#guard Column.toStrings (Column.mk' "x" #[Value.int 1, Value.bool true, Value.null]) == #["1", "true", "null"]

/-! ### null counts -/

#guard Column.nullCount (Column.mk' "x" #[Value.int 1, Value.null, Value.null]) == 2
#guard Column.nonNullCount (Column.mk' "x" #[Value.int 1, Value.null, Value.null]) == 1
#guard Column.nullCount (Column.mk' "x" #[Value.int 1, Value.int 2]) == 0

/-! ### take / drop -/

#guard (Column.take 2 c).values == #[Value.int 1, Value.int 2]
#guard (Column.drop 1 c).values == #[Value.int 2, Value.int 3]
#guard (Column.take 10 c).values == #[Value.int 1, Value.int 2, Value.int 3]   -- take beyond length

/-! ### unique (first-occurrence order) -/

#guard Column.unique (Column.mk' "x" #[Value.int 1, Value.int 2, Value.int 1, Value.int 3, Value.int 2])
        == #[Value.int 1, Value.int 2, Value.int 3]
#guard Column.unique (Column.mk' "x" #[]) == #[]

end Tests.DataFrameColumn
