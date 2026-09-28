/-
  Tests for `Linen.DataFrame.Operations.Aggregation` — group-by + aggregate.
-/
import Linen.DataFrame.Operations.Aggregation

open DataFrame

namespace Tests.DataFrameAggregation

/-- `cat`=[a,b,a,b], `v`=[10,20,30,40]. -/
private def df : DataFrame :=
  (DataFrame.fromNamedColumns #[("cat", #[Value.str "a", Value.str "b", Value.str "a", Value.str "b"]),
                                ("v", #[Value.int 10, Value.int 20, Value.int 30, Value.int 40])]).getD DataFrame.empty

private def gdf : GroupedDataFrame := df.groupBy ["cat"]

/-! ### AggFunc -/

#guard (AggFunc.sum == AggFunc.sum)
#guard ((AggFunc.sum == AggFunc.mean) == false)
#guard toString AggFunc.mean == "mean"

/-! ### groupBy (first-occurrence key order) -/

#guard gdf.groups.size == 2
#guard gdf.groupKeys == ["cat"]
#guard (gdf.groups[0]?).map (·.1) == some #[Value.str "a"]
#guard (gdf.groups[1]?).map (·.1) == some #[Value.str "b"]
#guard (gdf.groups[0]?).map (·.2.nRows) == some 2          -- two "a" rows

/-! ### aggregate: key column first, then `col_agg` columns -/

private def agg : DataFrame := gdf.aggregate [("v", .sum)]

#guard agg.columnNames == ["cat", "v_sum"]
#guard agg.nRows == 2
#guard agg.getRow? 0 == some #[Value.str "a", Value.float 40.0]   -- 10 + 30
#guard agg.getRow? 1 == some #[Value.str "b", Value.float 60.0]   -- 20 + 40

/-! ### multiple aggregations + the other funcs -/

#guard (gdf.aggregate [("v", .sum), ("v", .count)]).columnNames == ["cat", "v_sum", "v_count"]
#guard (gdf.aggregate [("v", .count)]).getRow? 0 == some #[Value.str "a", Value.int 2]
#guard (gdf.aggregate [("v", .mean)]).getRow? 0 == some #[Value.str "a", Value.float 20.0]
#guard (gdf.aggregate [("v", .min)]).getRow? 0 == some #[Value.str "a", Value.int 10]
#guard (gdf.aggregate [("v", .max)]).getRow? 0 == some #[Value.str "a", Value.int 30]
#guard (gdf.aggregate [("v", .first)]).getRow? 0 == some #[Value.str "a", Value.int 10]
#guard (gdf.aggregate [("v", .last)]).getRow? 0 == some #[Value.str "a", Value.int 30]

/-! ### unknown column ⇒ null aggregate -/

#guard (gdf.aggregate [("nope", .sum)]).getRow? 0 == some #[Value.str "a", Value.null]

end Tests.DataFrameAggregation
