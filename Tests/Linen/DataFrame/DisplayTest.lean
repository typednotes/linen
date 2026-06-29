/-
  Tests for `Linen.DataFrame.Display` — table/Markdown rendering.

  Markdown is unpadded (exact); the plain-text table is exact here because the
  single-character columns make all widths 1.
-/
import Linen.DataFrame.Display

open DataFrame

namespace Tests.DataFrameDisplay

/-- Fixture: columns `a`,`b` with rows `(1,2)` and `(3,4)`. -/
private def df : DataFrame :=
  DataFrame.fromRows #["a", "b"] #[#[Value.int 1, Value.int 2], #[Value.int 3, Value.int 4]]

/-! ### plain-text table (width-1 columns ⇒ fully determined) -/

#guard df.toString == " a | b \n---+---\n 1 | 2 \n 3 | 4 \n(2 rows x 2 columns)"
#guard ((df.toString).splitOn "\n").length == 5
#guard ((df.toString).splitOn "\n").getLast! == "(2 rows x 2 columns)"
#guard DataFrame.empty.toString == "(empty DataFrame: 0 columns, 0 rows)"

/-! ### Markdown table -/

#guard df.toMarkdown == "| a | b |\n| --- | --- |\n| 1 | 2 |\n| 3 | 4 |"
#guard DataFrame.empty.toMarkdown == "_empty DataFrame_"

/-! ### Markdown truncation footer -/

private def df3 : DataFrame :=
  DataFrame.fromRows #["x"] #[#[Value.int 1], #[Value.int 2], #[Value.int 3]]

#guard df3.toMarkdown 2 == "| x |\n| --- |\n| 1 |\n| 2 |\n\n_...and 1 more rows (3 total)_"

/-! ### instances -/

#guard ((toString df).splitOn "\n").getLast! == "(2 rows x 2 columns)"   -- ToString → df.toString
example : ToString DataFrame := inferInstance
example : Repr DataFrame := inferInstance

end Tests.DataFrameDisplay
