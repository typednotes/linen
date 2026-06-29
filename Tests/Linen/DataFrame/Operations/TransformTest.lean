/-
  Tests for `Linen.DataFrame.Operations.Transform` — column add/transform/drop/rename.
-/
import Linen.DataFrame.Operations.Transform

open DataFrame

namespace Tests.DataFrameTransform

/-- `a`=[1,2,3], `b`=[10,20,30]. -/
private def df : DataFrame :=
  (DataFrame.fromNamedColumns #[("a", #[Value.int 1, Value.int 2, Value.int 3]),
                                ("b", #[Value.int 10, Value.int 20, Value.int 30])]).getD DataFrame.empty

/-! ### addColumn -/

private def newCol : Column := { name := "c", values := #[Value.int 100, Value.int 200, Value.int 300], colType := .int }

#guard (df.addColumn newCol).map (·.columnNames) == some ["a", "b", "c"]
#guard (df.addColumn newCol).bind (·.getRow? 0) == some #[Value.int 1, Value.int 10, Value.int 100]
-- wrong length ⇒ none
#guard (df.addColumn { name := "bad", values := #[Value.int 1], colType := .int }).isNone

/-! ### derive (computed column from each row) -/

private def dd : DataFrame := df.derive "sum" .int fun _ row =>
  Value.int ((row[0]?.bind (·.toInt?)).getD 0 + (row[1]?.bind (·.toInt?)).getD 0)

#guard dd.columnNames == ["a", "b", "sum"]
#guard dd.getRow? 0 == some #[Value.int 1, Value.int 10, Value.int 11]
#guard dd.getRow? 2 == some #[Value.int 3, Value.int 30, Value.int 33]

/-! ### mapColumn -/

#guard (df.mapColumn "a" (fun v => Value.int ((v.toInt?.getD 0) * 10))).getRow? 0
        == some #[Value.int 10, Value.int 10]
#guard (df.mapColumn "a" (fun _ => Value.null)).getRow? 1 == some #[Value.null, Value.int 20]
-- unknown column ⇒ unchanged
#guard (df.mapColumn "nope" (fun _ => Value.null)).getRow? 0 == some #[Value.int 1, Value.int 10]

/-! ### dropColumn / renameColumn -/

#guard (df.dropColumn "a").columnNames == ["b"]
#guard (df.dropColumn "a").getRow? 0 == some #[Value.int 10]
#guard (df.dropColumn "nope").columnNames == ["a", "b"]
#guard (df.renameColumn "a" "alpha").columnNames == ["alpha", "b"]
#guard (df.renameColumn "a" "alpha").getRow? 0 == some #[Value.int 1, Value.int 10]   -- data unchanged

/-! ### dimensions / info -/

#guard df.dimensions == (3, 2)
#guard ((df.info).splitOn "\n").head! == "DataFrame: 3 rows × 2 columns"
#guard ((df.info).splitOn "\n").length == 3                                   -- header + 2 columns

end Tests.DataFrameTransform
