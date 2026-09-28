/-
  Tests for `Linen.DataFrame.Operations.Sort`.
-/
import Linen.DataFrame.Operations.Sort

open DataFrame

namespace Tests.DataFrameSort

/-- Fixture: `id`=[3,1,2], `name`=["c","a","b"]. -/
private def df : DataFrame :=
  (DataFrame.fromNamedColumns #[("id", #[Value.int 3, Value.int 1, Value.int 2]),
                                ("name", #[Value.str "c", Value.str "a", Value.str "b"])]).getD DataFrame.empty

/-! ### SortOrder / Value.compare -/

#guard (SortOrder.asc == SortOrder.asc)
#guard ((SortOrder.asc == SortOrder.desc) == false)
#guard DataFrame.Value.compare (Value.int 1) (Value.int 2) == Ordering.lt
#guard DataFrame.Value.compare Value.null (Value.int 0) == Ordering.gt          -- null sorts last
#guard DataFrame.Value.compare (Value.int 1) (Value.float 1.5) == Ordering.lt   -- cross-numeric

/-! ### sortBy (single column) -/

private def asc : DataFrame := df.sortBy "id"

#guard asc.getRow? 0 == some #[Value.int 1, Value.str "a"]
#guard asc.getRow? 1 == some #[Value.int 2, Value.str "b"]
#guard asc.getRow? 2 == some #[Value.int 3, Value.str "c"]

private def desc : DataFrame := df.sortBy "id" .desc

#guard desc.getRow? 0 == some #[Value.int 3, Value.str "c"]
#guard desc.getRow? 1 == some #[Value.int 2, Value.str "b"]
#guard desc.getRow? 2 == some #[Value.int 1, Value.str "a"]

-- a missing column leaves the frame unchanged
#guard (df.sortBy "nope").getRow? 0 == some #[Value.int 3, Value.str "c"]
#guard (df.sortBy "id").nRows == 3

/-! ### sortByMultiple (primary then secondary key) -/

private def df2 : DataFrame :=
  (DataFrame.fromNamedColumns #[("a", #[Value.int 1, Value.int 1, Value.int 2]),
                                ("b", #[Value.int 20, Value.int 10, Value.int 5])]).getD DataFrame.empty

private def s2 : DataFrame := df2.sortByMultiple [("a", .asc), ("b", .asc)]

#guard s2.getRow? 0 == some #[Value.int 1, Value.int 10]   -- a=1 tie broken by b asc
#guard s2.getRow? 1 == some #[Value.int 1, Value.int 20]
#guard s2.getRow? 2 == some #[Value.int 2, Value.int 5]

-- empty spec list leaves the frame unchanged
#guard (df2.sortByMultiple []).getRow? 0 == some #[Value.int 1, Value.int 20]

end Tests.DataFrameSort
