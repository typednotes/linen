/-
  Linen.DataFrame.Operations.Aggregation — group-by and aggregation

  Group a DataFrame by column values into a `GroupedDataFrame`, then reduce each
  group with `sum`/`mean`/`count`/`min`/`max`/`first`/`last`/`std`/`var`. Grouping
  is a pure `foldl` (find-or-append by key) + `map`; the rectangular invariant of
  every sub-frame / result is discharged via `map_column_aligned`.
-/

import Linen.DataFrame.Internal.Types
import Linen.DataFrame.Operations.Statistics

namespace DataFrame

/-- An aggregation function applied to a column within each group. -/
inductive AggFunc where
  | sum | mean | count | min | max | first | last | std | var
deriving BEq, Repr

instance : ToString AggFunc where
  toString
    | .sum => "sum" | .mean => "mean" | .count => "count"
    | .min => "min" | .max => "max" | .first => "first"
    | .last => "last" | .std => "std" | .var => "var"

/-- Group a DataFrame by one or more columns: rows with equal key values are
    collected into sub-frames (in first-occurrence order of keys). -/
def DataFrame.groupBy (df : DataFrame) (groupCols : List String) : GroupedDataFrame :=
  let keyIndices := groupCols.filterMap fun name => df.columns.findIdx? fun c => c.name == name
  let keyOf : Nat → Array Value := fun rowIdx =>
    (keyIndices.map fun colIdx =>
      if h1 : colIdx < df.columns.size then
        let col := df.columns[colIdx]
        if h2 : rowIdx < col.values.size then col.values[rowIdx] else Value.null
      else Value.null).toArray
  -- Accumulate row indices per unique key (find-or-append).
  let groupMap : List (Array Value × Array Nat) :=
    (List.range df.nRows).foldl (fun gm rowIdx =>
      let keyVals := keyOf rowIdx
      match gm.findIdx? fun (k, _) => k == keyVals with
      | some idx => let (k, rows) := gm[idx]!; gm.set idx (k, rows.push rowIdx)
      | none => gm ++ [(keyVals, #[rowIdx])]) []
  let groups : Array (Array Value × DataFrame) :=
    groupMap.toArray.map fun (keyVals, rowIndices) =>
      let subDf : DataFrame := {
        columns := df.columns.map fun col =>
          { col with values := rowIndices.map fun idx =>
              if h : idx < col.values.size then col.values[idx] else Value.null }
        nRows := rowIndices.size
        columns_aligned := DataFrame.map_column_aligned df.columns rowIndices.size _ (fun _ => Array.size_map)
      }
      (keyVals, subDf)
  { groups, groupKeys := groupCols }

/-- Apply an aggregation function to a column. -/
private def applyAgg (aggFunc : AggFunc) (col : Column) : Value :=
  match aggFunc with
  | .count => .int col.values.size
  | .sum   => ((Column.Stats.sum col).map Value.float).getD .null
  | .mean  => ((Column.Stats.mean col).map Value.float).getD .null
  | .min   => (Column.Stats.minValue col).getD .null
  | .max   => (Column.Stats.maxValue col).getD .null
  | .first => if col.values.isEmpty then .null else col.values[0]!
  | .last  => if col.values.isEmpty then .null else col.values[col.values.size - 1]!
  | .std   => ((Column.Stats.std col).map Value.float).getD .null
  | .var   => ((Column.Stats.variance col).map Value.float).getD .null

/-- Aggregate a grouped DataFrame: each `(colName, aggFunc)` becomes one result
    column (named `colName_aggFunc`); the group-key columns come first. -/
def GroupedDataFrame.aggregate (gdf : GroupedDataFrame) (specs : List (String × AggFunc)) : DataFrame :=
  let nGroups := gdf.groups.size
  let keyArr := gdf.groupKeys.toArray
  let mkKeyCol : Nat → Column := fun idx =>
    Column.mk (if h : idx < keyArr.size then keyArr[idx] else "")
      (gdf.groups.map fun (keyVals, _) => if h : idx < keyVals.size then keyVals[idx] else Value.null) .mixed
  let mkAggCol : String × AggFunc → Column := fun spec =>
    Column.mk s!"{spec.1}_{spec.2}"
      (gdf.groups.map fun (_, subDf) =>
        match subDf.columns.find? fun c => c.name == spec.1 with
        | some col => applyAgg spec.2 col
        | none => Value.null) .mixed
  let keyCols := (Array.range keyArr.size).map mkKeyCol
  let aggCols := specs.toArray.map mkAggCol
  { columns := keyCols ++ aggCols
  , nRows := nGroups
  , columns_aligned := fun i h => by
      have hkc : keyCols.size = (Array.range keyArr.size).size := Array.size_map
      have hac : aggCols.size = specs.toArray.size := Array.size_map
      simp only [Array.size_append] at h
      simp only [Array.getElem_append]
      split
      · rename_i hlt
        exact DataFrame.map_column_aligned (Array.range keyArr.size) nGroups mkKeyCol
          (fun _ => Array.size_map) i (by rw [Array.size_map]; omega)
      · rename_i hge
        exact DataFrame.map_column_aligned specs.toArray nGroups mkAggCol
          (fun _ => Array.size_map) (i - keyCols.size)
          (by simp only [Array.size_map, Array.size_append, Array.size_range] at *; omega)
  }

end DataFrame
