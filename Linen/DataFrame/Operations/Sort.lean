/-
  Linen.DataFrame.Operations.Sort — sorting

  Sort a DataFrame by one or more columns, ascending or descending, via core
  `List.mergeSort` over a row-index permutation. Fully pure; the row reordering
  preserves the rectangular invariant (`map_column_aligned`).
-/

import Init.Data.List.Sort.Lemmas
import Linen.DataFrame.Internal.Types

namespace DataFrame

/-- Sort order for a column. -/
inductive SortOrder where
  | asc | desc
deriving BEq, Repr

/-- Compare two `Value`s (nulls sort last). Cross-numeric `int`/`float` compare
    numerically; other cross-type pairs compare by type tag. -/
def Value.compare (a b : Value) : Ordering :=
  match a, b with
  | .null, .null => .eq
  | .null, _ => .gt
  | _, .null => .lt
  | .int x, .int y => Ord.compare x y
  | .float x, .float y => if x < y then .lt else if x > y then .gt else .eq
  | .str x, .str y => Ord.compare x y
  | .bool x, .bool y => Ord.compare x.toNat y.toNat
  | .int x, .float y => let xf := Float.ofInt x; if xf < y then .lt else if xf > y then .gt else .eq
  | .float x, .int y => let yf := Float.ofInt y; if x < yf then .lt else if x > yf then .gt else .eq
  | a, b => Ord.compare (typeOrder a) (typeOrder b)
where
  typeOrder : Value → Nat
    | .bool _ => 0 | .int _ => 1 | .float _ => 2 | .str _ => 3 | .null => 4

/-- The value at `(row, colIdx)`, or `null` if out of bounds. -/
private def getValueAt (df : DataFrame) (row : Nat) (colIdx : Nat) : Value :=
  if h1 : colIdx < df.columns.size then
    let col := df.columns[colIdx]
    if h2 : row < col.values.size then col.values[row] else .null
  else .null

/-- Reorder every column by a row-index permutation (of length `nRows`). -/
private def reindexColumns (df : DataFrame) (sortedIdx : Array Nat)
    (hsize : sortedIdx.size = df.nRows) : DataFrame :=
  { columns := df.columns.map fun col =>
      { col with values := sortedIdx.map fun idx =>
          if h : idx < col.values.size then col.values[idx] else .null }
  , nRows := df.nRows
  , columns_aligned := DataFrame.map_column_aligned df.columns df.nRows _
      (fun _ => Array.size_map.trans hsize) }

/-- Sort by a single column (a missing column leaves the frame unchanged). -/
def DataFrame.sortBy (df : DataFrame) (colName : String) (order : SortOrder := .asc) : DataFrame :=
  match df.columns.findIdx? fun c => c.name == colName with
  | none => df
  | some colIdx =>
    let sorted := (Array.range df.nRows).toList.mergeSort fun i j =>
      match Value.compare (getValueAt df i colIdx) (getValueAt df j colIdx) with
      | .gt => order == .desc
      | .lt => order == .asc
      | .eq => true
    have hsize : sorted.toArray.size = df.nRows := by
      rw [List.size_toArray, List.length_mergeSort, Array.length_toList, Array.size_range]
    reindexColumns df sorted.toArray hsize

/-- Sort by multiple columns (first spec is the primary key). -/
def DataFrame.sortByMultiple (df : DataFrame) (specs : List (String × SortOrder)) : DataFrame :=
  let colSpecs := specs.filterMap fun (name, order) =>
    (df.columns.findIdx? fun c => c.name == name).map fun idx => (idx, order)
  if colSpecs.isEmpty then df
  else
    let sorted := (Array.range df.nRows).toList.mergeSort fun i j =>
      let rec cmpBy : List (Nat × SortOrder) → Bool
        | [] => true
        | (colIdx, order) :: rest =>
          match Value.compare (getValueAt df i colIdx) (getValueAt df j colIdx) with
          | .eq => cmpBy rest
          | .lt => order == .asc
          | .gt => order == .desc
      cmpBy colSpecs
    have hsize : sorted.toArray.size = df.nRows := by
      rw [List.size_toArray, List.length_mergeSort, Array.length_toList, Array.size_range]
    reindexColumns df sorted.toArray hsize

end DataFrame
