/-
  Tests for `Linen.DataFrame.Operations.Join` — table joins on a shared key.
-/
import Linen.DataFrame.Operations.Join

open DataFrame

namespace Tests.DataFrameJoin

/-- left: `id`/`name` for ids 1,2,3. -/
private def left : DataFrame :=
  (DataFrame.fromNamedColumns #[("id", #[Value.int 1, Value.int 2, Value.int 3]),
                                ("name", #[Value.str "a", Value.str "b", Value.str "c"])]).getD DataFrame.empty

/-- right: `id`/`score` for ids 1,2,4. -/
private def right : DataFrame :=
  (DataFrame.fromNamedColumns #[("id", #[Value.int 1, Value.int 2, Value.int 4]),
                                ("score", #[Value.int 10, Value.int 20, Value.int 40])]).getD DataFrame.empty

/-! ### JoinType -/

#guard (JoinType.inner == JoinType.inner)
#guard ((JoinType.inner == JoinType.left) == false)

/-! ### inner join (key columns once; right non-key columns appended) -/

private def ij : DataFrame := left.innerJoin right ["id"]

#guard ij.columnNames == ["id", "name", "score"]
#guard ij.nRows == 2
#guard ij.getRow? 0 == some #[Value.int 1, Value.str "a", Value.int 10]
#guard ij.getRow? 1 == some #[Value.int 2, Value.str "b", Value.int 20]
-- default `join` is inner
#guard (left.join right ["id"]).nRows == 2

/-! ### left join (unmatched left rows kept, right cells null) -/

private def lj : DataFrame := left.leftJoin right ["id"]

#guard lj.nRows == 3
#guard lj.getRow? 2 == some #[Value.int 3, Value.str "c", Value.null]

/-! ### right join (unmatched right rows kept, left cells null) -/

private def rj : DataFrame := left.rightJoin right ["id"]

#guard rj.nRows == 3
#guard rj.getRow? 2 == some #[Value.null, Value.null, Value.int 40]

/-! ### outer join (all rows from both sides) -/

private def oj : DataFrame := left.outerJoin right ["id"]

#guard oj.nRows == 4
#guard oj.columnNames == ["id", "name", "score"]

/-! ### no overlapping keys ⇒ empty inner join -/

private def disjoint : DataFrame :=
  (DataFrame.fromNamedColumns #[("id", #[Value.int 99]), ("score", #[Value.int 1])]).getD DataFrame.empty

#guard (left.innerJoin disjoint ["id"]).nRows == 0

end Tests.DataFrameJoin
