/-
  Linen.DataFrame.Display — display formatting for DataFrames

  Human-readable aligned plain-text tables and Markdown tables. Large frames
  are truncated to `maxRows` (default 20): the plain-text view shows bookend
  rows with an ellipsis row between them, Markdown shows the first rows plus a
  summary footer. Column widths are computed from headers and displayed cells,
  capped at `maxColWidth`.

  All rendering is pure (`.map`/`.flatMap`, no `Id.run`/`for`/`mut`).
-/

import Linen.DataFrame.Internal.Types
import Linen.DataFrame.Internal.Column

namespace DataFrame

/-! ── Helpers ── -/

/-- Right-pad to `width` with spaces; if wider, truncate and append `".."`. -/
private def padRight (s : String) (width : Nat) : String :=
  if s.length > width then (s.take (width - 2)).toString ++ ".."
  else s ++ String.ofList (List.replicate (width - s.length) ' ')

/-- Render a `Value` as a display string (`null` shows as `<null>`). -/
private def valueToDisplayString : Value → String
  | .int n   => toString n
  | .float f => toString f
  | .str s   => s
  | .bool b  => if b then "true" else "false"
  | .null    => "<null>"

/-- Maximum column display width. -/
private def maxColWidth : Nat := 30

/-- The display width for a column: capped `max` of header and cell widths. -/
private def computeColumnWidth (header : String) (cells : Array String) : Nat :=
  min maxColWidth (max header.length (cells.foldl (fun acc s => max acc s.length) 0))

namespace DataFrame

/-- Render the DataFrame as a plain-text aligned table. Frames longer than
    `maxRows` show the first/last `maxRows/2` rows with an ellipsis between. -/
def toString (df : DataFrame) (maxRows : Nat := 20) : String :=
  if df.nColumns == 0 then
    "(empty DataFrame: 0 columns, 0 rows)"
  else
    let half := maxRows / 2
    let (displayRows, truncated) :=
      if df.nRows ≤ maxRows then (List.range df.nRows, false)
      else ((List.range half) ++ (List.range (df.nRows - half)).map (· + half), true)
    let cellStrings : Array (Array String) :=
      displayRows.toArray.map fun idx =>
        df.columns.map fun col =>
          valueToDisplayString (if idx < col.values.size then col.values[idx]! else Value.null)
    let colWidths : Array Nat :=
      (Array.range df.columns.size).map fun j =>
        computeColumnWidth (df.columns[j]!).name (cellStrings.map fun row => if j < row.size then row[j]! else "")
    let headerParts := (Array.range df.columns.size).map fun j =>
      padRight (df.columns[j]!).name (if j < colWidths.size then colWidths[j]! else 10)
    let headerLine := " " ++ String.intercalate " | " headerParts.toList ++ " "
    let sepParts := colWidths.map fun w => String.ofList (List.replicate w '-')
    let sepLine := "-" ++ String.intercalate "-+-" sepParts.toList ++ "-"
    let dataLines : List String :=
      (List.range cellStrings.size).flatMap fun i =>
        let row := cellStrings[i]!
        let rowParts := (Array.range df.columns.size).map fun j =>
          padRight (if j < row.size then row[j]! else "") (if j < colWidths.size then colWidths[j]! else 10)
        let rowLine := " " ++ String.intercalate " | " rowParts.toList ++ " "
        if truncated && i == half then
          let ellipsis := " " ++ String.intercalate " | " (colWidths.map fun w => padRight "..." w).toList ++ " "
          [ellipsis, rowLine]
        else [rowLine]
    let table := String.intercalate "\n" (headerLine :: sepLine :: dataLines)
    if truncated then table ++ s!"\n[{df.nRows} rows x {df.nColumns} columns]"
    else table ++ s!"\n({df.nRows} rows x {df.nColumns} columns)"

/-- Render the DataFrame as a Markdown table (truncated to `maxRows`). -/
def toMarkdown (df : DataFrame) (maxRows : Nat := 20) : String :=
  if df.nColumns == 0 then
    "_empty DataFrame_"
  else
    let displayCount := min maxRows df.nRows
    let headerLine := "| " ++ String.intercalate " | " (df.columns.toList.map (·.name)) ++ " |"
    let sepLine := "| " ++ String.intercalate " | " (df.columns.toList.map fun _ => "---") ++ " |"
    let dataLines : List String :=
      (List.range displayCount).map fun i =>
        "| " ++ String.intercalate " | " (df.columns.toList.map fun col =>
          valueToDisplayString (if i < col.values.size then col.values[i]! else Value.null)) ++ " |"
    let table := String.intercalate "\n" (headerLine :: sepLine :: dataLines)
    if df.nRows > maxRows then table ++ s!"\n\n_...and {df.nRows - maxRows} more rows ({df.nRows} total)_"
    else table

end DataFrame

/-! ── Instances ── -/

instance : ToString DataFrame where
  toString df := df.toString

instance : Repr DataFrame where
  reprPrec df _ :=
    let header := s!"DataFrame({df.nRows} rows x {df.nColumns} columns)"
    let colInfo := df.columns.toList.map fun c => s!"  {c.name} : {c.colType}"
    Std.Format.text (String.intercalate "\n" (header :: colInfo))

end DataFrame
