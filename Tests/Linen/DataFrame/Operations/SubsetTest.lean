/-
  Tests for `Linen.DataFrame.Operations.Subset` — column selection + row slicing/filtering.
-/
import Linen.DataFrame.Operations.Subset

open DataFrame

namespace Tests.DataFrameSubset

/-- `a`=[1,2,3,4], `b`=["w","x","y","z"], `c`=[T,F,T,F]. -/
private def df : DataFrame :=
  (DataFrame.fromNamedColumns #[("a", #[Value.int 1, Value.int 2, Value.int 3, Value.int 4]),
                                ("b", #[Value.str "w", Value.str "x", Value.str "y", Value.str "z"]),
                                ("c", #[Value.bool true, Value.bool false, Value.bool true, Value.bool false])]).getD DataFrame.empty

/-! ### select / exclude -/

#guard (df.select ["a", "c"]).columnNames == ["a", "c"]
#guard (df.select ["a", "c"]).nRows == 4
#guard (df.select ["nope"]).columnNames == []
#guard (df.exclude ["b"]).columnNames == ["a", "c"]
#guard (df.exclude ["a", "b", "c"]).columnNames == []

/-! ### take / drop / head / tail / slice -/

#guard (df.take 2).nRows == 2
#guard (df.take 2).getRow? 1 == some #[Value.int 2, Value.str "x", Value.bool false]
#guard (df.take 10).nRows == 4
#guard (df.drop 1).nRows == 3
#guard (df.drop 1).getRow? 0 == some #[Value.int 2, Value.str "x", Value.bool false]
#guard (df.head 2).nRows == 2
#guard (df.tail 2).nRows == 2
#guard (df.tail 2).getRow? 0 == some #[Value.int 3, Value.str "y", Value.bool true]
#guard (df.slice 1 3).nRows == 2
#guard (df.slice 1 3).getRow? 0 == some #[Value.int 2, Value.str "x", Value.bool false]

/-! ### filterBy / filterWhere -/

#guard (df.filterBy "a" (fun v => (v.toInt?.getD 0) > 2)).nRows == 2
#guard (df.filterBy "a" (fun v => (v.toInt?.getD 0) > 2)).getRow? 0 == some #[Value.int 3, Value.str "y", Value.bool true]
#guard (df.filterBy "nope" (fun _ => true)).nRows == 0          -- missing column ⇒ none kept
#guard (df.filterWhere (fun row => (row[2]?).getD Value.null == Value.bool true)).nRows == 2
#guard (df.filterWhere (fun row => (row[2]?).getD Value.null == Value.bool true)).getRow? 0
        == some #[Value.int 1, Value.str "w", Value.bool true]

/-! ### rename -/

#guard (df.rename [("a", "alpha")]).columnNames == ["alpha", "b", "c"]
#guard (df.rename [("a", "x"), ("b", "y")]).columnNames == ["x", "y", "c"]
#guard (df.rename [("a", "x")]).getRow? 0 == some #[Value.int 1, Value.str "w", Value.bool true]   -- data unchanged

end Tests.DataFrameSubset
