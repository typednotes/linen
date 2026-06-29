/-
  Linen.DataFrame.Operations.Subset — selection and filtering

  Select/exclude columns, slice rows (`take`/`drop`/`head`/`tail`/`slice`), and
  filter rows (`filterBy`/`filterWhere`). Every operation re-establishes the
  rectangular invariant; mask construction is pure (`filter`/`map`).
-/

import Linen.DataFrame.Internal.Types

namespace DataFrame

/-- Select columns by name (missing names skipped). -/
def DataFrame.select (df : DataFrame) (names : List String) : DataFrame :=
  { columns := df.columns.filter fun col => names.contains col.name
  , nRows := df.nRows
  , columns_aligned := fun i h => by
      have hmem : (df.columns.filter _)[i] ∈ df.columns :=
        (Array.mem_filter.mp (Array.getElem_mem h)).1
      obtain ⟨j, hj, hjv⟩ := Array.mem_iff_getElem.mp hmem
      have := df.columns_aligned j hj; rwa [hjv] at this }

/-- Exclude columns by name. -/
def DataFrame.exclude (df : DataFrame) (names : List String) : DataFrame :=
  { columns := df.columns.filter fun col => !names.contains col.name
  , nRows := df.nRows
  , columns_aligned := fun i h => by
      have hmem : (df.columns.filter _)[i] ∈ df.columns :=
        (Array.mem_filter.mp (Array.getElem_mem h)).1
      obtain ⟨j, hj, hjv⟩ := Array.mem_iff_getElem.mp hmem
      have := df.columns_aligned j hj; rwa [hjv] at this }

/-- Take the first `n` rows. -/
def DataFrame.take (df : DataFrame) (n : Nat) : DataFrame :=
  let actualN := Nat.min n df.nRows
  { columns := df.columns.map fun col => { col with values := col.values.extract 0 actualN }
  , nRows := actualN
  , columns_aligned := fun i h => by
      have h' : i < df.columns.size := by rwa [Array.size_map] at h
      have heq := @Array.getElem_map _ _ _ df.columns i h
      simp only [heq, Array.size_extract, df.columns_aligned i h']
      exact Nat.min_eq_left (Nat.min_le_right n df.nRows) }

/-- Drop the first `n` rows. -/
def DataFrame.drop (df : DataFrame) (n : Nat) : DataFrame :=
  let actualDrop := Nat.min n df.nRows
  { columns := df.columns.map fun col => { col with values := col.values.extract actualDrop col.values.size }
  , nRows := df.nRows - actualDrop
  , columns_aligned := fun i h => by
      have h' : i < df.columns.size := by rwa [Array.size_map] at h
      have heq := @Array.getElem_map _ _ _ df.columns i h
      simp only [heq, Array.size_extract, df.columns_aligned i h', Nat.min_self] }

/-- First `n` rows (default 5). -/
def DataFrame.head (df : DataFrame) (n : Nat := 5) : DataFrame := df.take n

/-- Last `n` rows (default 5). -/
def DataFrame.tail (df : DataFrame) (n : Nat := 5) : DataFrame :=
  df.drop (df.nRows - Nat.min n df.nRows)

/-- Slice rows from `start` (inclusive) to `stop` (exclusive). -/
def DataFrame.slice (df : DataFrame) (start stop : Nat) : DataFrame :=
  let s := Nat.min start df.nRows
  let e := Nat.min stop df.nRows
  { columns := df.columns.map fun col => { col with values := col.values.extract s e }
  , nRows := e - s
  , columns_aligned := fun i h => by
      have h' : i < df.columns.size := by rwa [Array.size_map] at h
      have heq := @Array.getElem_map _ _ _ df.columns i h
      simp only [heq, Array.size_extract, df.columns_aligned i h']
      have : e ≤ df.nRows := Nat.min_le_right stop df.nRows
      omega }

/-- A boolean mask from a predicate on a named column (missing column ⇒ all false). -/
private def buildMask (df : DataFrame) (colName : String) (pred : Value → Bool) : Array Bool :=
  match df.columns.find? fun c => c.name == colName with
  | none => Array.replicate df.nRows false
  | some col => col.values.map pred

/-- Keep the rows whose mask entry is true (shared across all columns). -/
private def applyMask (df : DataFrame) (mask : Array Bool) : DataFrame :=
  let keepIndices : Array Nat := (Array.range df.nRows).filter fun i => i < mask.size && mask[i]!
  { columns := df.columns.map fun col =>
      { col with values := keepIndices.map fun idx =>
          if h : idx < col.values.size then col.values[idx] else Value.null }
  , nRows := keepIndices.size
  , columns_aligned := DataFrame.map_column_aligned df.columns keepIndices.size _ (fun _ => Array.size_map) }

/-- Filter rows where a column's value satisfies a predicate. -/
def DataFrame.filterBy (df : DataFrame) (colName : String) (pred : Value → Bool) : DataFrame :=
  applyMask df (buildMask df colName pred)

/-- Filter rows by a predicate on the whole row (all column values). -/
def DataFrame.filterWhere (df : DataFrame) (pred : Array Value → Bool) : DataFrame :=
  let mask := (Array.range df.nRows).map fun rowIdx =>
    pred (df.columns.map fun col => if h : rowIdx < col.values.size then col.values[rowIdx] else Value.null)
  applyMask df mask

/-- Rename columns according to an `old → new` mapping. -/
def DataFrame.rename (df : DataFrame) (mapping : List (String × String)) : DataFrame :=
  { columns := df.columns.map fun col =>
      match mapping.find? fun (old, _) => old == col.name with
      | some (_, newName) => { col with name := newName }
      | none => col
  , nRows := df.nRows
  , columns_aligned := fun i h => by
      simp only [Array.size_map] at h
      simp only [Array.getElem_map]
      split <;> exact df.columns_aligned i h }

end DataFrame
