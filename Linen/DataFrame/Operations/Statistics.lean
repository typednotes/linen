/-
  Linen.DataFrame.Operations.Statistics — column statistics

  Column-level statistics (sum, mean, variance, std, median, min, max, counts).
  Numeric stats return `Option Float` (`none` when no numeric values); non-numeric
  cells are skipped. Pure — accumulators are `filterMap`/`foldl`, no `Id.run`.
-/

import Init.Data.List.Sort.Basic
import Linen.DataFrame.Internal.Types

namespace DataFrame
namespace Column.Stats

/-- Numeric values of a column as `Float`s (nulls / non-numeric skipped). -/
def numericValues (col : Column) : Array Float :=
  col.values.filterMap fun v => match v with
    | .int n   => some (Float.ofInt n)
    | .float f => some f
    | _        => none

/-- Count of all values (including null). -/
def count (col : Column) : Nat := col.values.size

/-- Count of non-null values. -/
def countNonNull (col : Column) : Nat :=
  col.values.foldl (fun n v => if v.isNull then n else n + 1) 0

/-- Count of null values. -/
def countNull (col : Column) : Nat :=
  col.values.foldl (fun n v => if v.isNull then n + 1 else n) 0

/-- Sum of numeric values. -/
def sum (col : Column) : Option Float :=
  let nums := numericValues col
  if nums.isEmpty then none else some (nums.foldl (· + ·) 0.0)

/-- Arithmetic mean of numeric values. -/
def mean (col : Column) : Option Float :=
  let nums := numericValues col
  if nums.isEmpty then none else some (nums.foldl (· + ·) 0.0 / nums.size.toFloat)

/-- Population variance. -/
def variance (col : Column) : Option Float :=
  let nums := numericValues col
  if nums.isEmpty then none
  else
    let μ := nums.foldl (· + ·) 0.0 / nums.size.toFloat
    some (nums.foldl (fun acc v => acc + (v - μ) * (v - μ)) 0.0 / nums.size.toFloat)

/-- Population standard deviation. -/
def std (col : Column) : Option Float := do
  some (Float.sqrt (← variance col))

/-- Minimum numeric value. -/
def min (col : Column) : Option Float :=
  let nums := numericValues col
  if nums.isEmpty then none else some (nums.foldl Min.min nums[0]!)

/-- Maximum numeric value. -/
def max (col : Column) : Option Float :=
  let nums := numericValues col
  if nums.isEmpty then none else some (nums.foldl Max.max nums[0]!)

/-- Median of numeric values. -/
def median (col : Column) : Option Float :=
  let nums := numericValues col
  if nums.isEmpty then none
  else
    let sorted := (nums.toList.mergeSort (· ≤ ·)).toArray
    let n := sorted.size
    if n % 2 == 1 then some sorted[n / 2]!
    else some ((sorted[n / 2 - 1]! + sorted[n / 2]!) / 2.0)

/-- Minimum `Value`, comparing within a single type (nulls skipped). -/
def minValue (col : Column) : Option Value :=
  if col.values.isEmpty then none
  else some (col.values.foldl (fun best v =>
    match v with
    | .null => best
    | _ =>
      if best == .null then v
      else match (v, best) with
        | (.int a, .int b) => if a < b then v else best
        | (.float a, .float b) => if a < b then v else best
        | (.str a, .str b) => if a < b then v else best
        | _ => best) col.values[0]!)

/-- Maximum `Value`, comparing within a single type (nulls skipped). -/
def maxValue (col : Column) : Option Value :=
  if col.values.isEmpty then none
  else some (col.values.foldl (fun best v =>
    match v with
    | .null => best
    | _ =>
      if best == .null then v
      else match (v, best) with
        | (.int a, .int b) => if a > b then v else best
        | (.float a, .float b) => if a > b then v else best
        | (.str a, .str b) => if a > b then v else best
        | _ => best) col.values[0]!)

end Column.Stats
end DataFrame
