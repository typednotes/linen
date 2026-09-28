/-
  Tests for `Linen.DataFrame.Operations.Statistics`.

  Values are chosen so the float results are exact (1.0/2.5/1.25/…); `std`
  (a `sqrt`) is only checked for `isSome`/`none`.
-/
import Linen.DataFrame.Operations.Statistics

open DataFrame

namespace Tests.DataFrameStats

/-- `[1,2,3,4]`. -/
private def c : Column := { name := "x", values := #[Value.int 1, Value.int 2, Value.int 3, Value.int 4], colType := .int }

/-! ### counts / numeric extraction -/

#guard Column.Stats.count c == 4
#guard Column.Stats.countNonNull c == 4
#guard Column.Stats.countNull c == 0
#guard Column.Stats.numericValues c == #[1.0, 2.0, 3.0, 4.0]

/-! ### sum / mean / variance / std / min / max / median -/

#guard Column.Stats.sum c == some 10.0
#guard Column.Stats.mean c == some 2.5
#guard Column.Stats.variance c == some 1.25
#guard (Column.Stats.std c).isSome
#guard Column.Stats.min c == some 1.0
#guard Column.Stats.max c == some 4.0
#guard Column.Stats.median c == some 2.5                 -- even n: (2+3)/2
#guard Column.Stats.minValue c == some (Value.int 1)
#guard Column.Stats.maxValue c == some (Value.int 4)

/-! ### median, odd length -/

#guard Column.Stats.median { name := "x", values := #[Value.int 3, Value.int 1, Value.int 2], colType := .int } == some 2.0

/-! ### nulls skipped -/

private def cn : Column := { name := "x", values := #[Value.int 1, Value.null, Value.int 3], colType := .int }

#guard Column.Stats.count cn == 3
#guard Column.Stats.countNonNull cn == 2
#guard Column.Stats.countNull cn == 1
#guard Column.Stats.sum cn == some 4.0
#guard Column.Stats.mean cn == some 2.0
#guard Column.Stats.minValue cn == some (Value.int 1)
#guard Column.Stats.maxValue cn == some (Value.int 3)

/-! ### empty / non-numeric ⇒ `none` for numeric stats -/

private def ce : Column := { name := "x", values := #[], colType := .mixed }

#guard Column.Stats.sum ce == none
#guard Column.Stats.mean ce == none
#guard Column.Stats.median ce == none
#guard Column.Stats.std ce == none
#guard Column.Stats.minValue ce == none

private def cs : Column := { name := "x", values := #[Value.str "b", Value.str "a", Value.str "c"], colType := .str }

#guard Column.Stats.sum cs == none                      -- no numeric values
#guard Column.Stats.minValue cs == some (Value.str "a") -- still comparable
#guard Column.Stats.maxValue cs == some (Value.str "c")

end Tests.DataFrameStats
