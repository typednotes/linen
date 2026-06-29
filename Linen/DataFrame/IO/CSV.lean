/-
  Linen.DataFrame.IO.CSV — CSV read/write

  RFC 4180-compliant CSV parsing and writing: quoted fields, escaped (doubled)
  quotes, CRLF/LF line endings, with `Value` type inference.

  `parseCsvRaw` is a `for`-loop state machine over the finite character list
  (`List.forIn` is structural — no `partial`/`while`/fuel); the float parser is
  pure structural code over `List.span`.
-/

import Linen.DataFrame.Internal.Types

namespace DataFrame

/-- CSV parsing/writing options. -/
structure CsvOptions where
  /-- Field delimiter character. -/
  delimiter : Char := ','
  /-- Whether the first row is a header row. -/
  hasHeader : Bool := true
  /-- Quote character for fields containing delimiters/newlines. -/
  quoteChar : Char := '"'
deriving Repr

/-- State of the CSV parsing state machine. -/
private inductive CsvState where
  | fieldStart     -- beginning of a field
  | unquotedField  -- inside an unquoted field
  | quotedField    -- inside a quoted field
  | quotedQuote    -- just saw a quote inside a quoted field (escape or end)
deriving BEq

/-- Parse a CSV string into rows of raw field strings (a finite `for`-loop
    state machine over the characters). -/
def parseCsvRaw (content : String) (opts : CsvOptions := {}) : Array (Array String) := Id.run do
  let mut rows : Array (Array String) := #[]
  let mut currentRow : Array String := #[]
  let mut currentField : String := ""
  let mut state := CsvState.fieldStart
  for c in content.toList do
    match state with
    | .fieldStart =>
      if c == opts.quoteChar then
        state := .quotedField
      else if c == opts.delimiter then
        currentRow := currentRow.push currentField; currentField := ""
      else if c == '\n' then
        currentRow := currentRow.push currentField
        rows := rows.push currentRow; currentRow := #[]; currentField := ""
      else if c == '\r' then
        pure ()
      else
        currentField := currentField.push c; state := .unquotedField
    | .unquotedField =>
      if c == opts.delimiter then
        currentRow := currentRow.push currentField; currentField := ""; state := .fieldStart
      else if c == '\n' then
        currentRow := currentRow.push currentField
        rows := rows.push currentRow; currentRow := #[]; currentField := ""; state := .fieldStart
      else if c == '\r' then
        pure ()
      else
        currentField := currentField.push c
    | .quotedField =>
      if c == opts.quoteChar then state := .quotedQuote
      else currentField := currentField.push c
    | .quotedQuote =>
      if c == opts.quoteChar then
        currentField := currentField.push opts.quoteChar; state := .quotedField
      else if c == opts.delimiter then
        currentRow := currentRow.push currentField; currentField := ""; state := .fieldStart
      else if c == '\n' then
        currentRow := currentRow.push currentField
        rows := rows.push currentRow; currentRow := #[]; currentField := ""; state := .fieldStart
      else if c == '\r' then
        currentRow := currentRow.push currentField; state := .fieldStart
      else
        currentField := currentField.push c; state := .unquotedField
  -- Flush a trailing field/row when the content doesn't end in a newline.
  if !currentField.isEmpty || state != .fieldStart || !currentRow.isEmpty then
    currentRow := currentRow.push currentField
    rows := rows.push currentRow
  rows

/-- A digit character's numeric value. -/
private def charToDigit (c : Char) : Nat := c.toNat - '0'.toNat

/-- Parse a string as a `Float` (optional sign, integer/fractional parts,
    optional `e`/`E` exponent), or `none` if it is not a valid float literal.
    Pure structural code over `List.span`. -/
private def parseFloat? (s : String) : Option Float :=
  let chars := s.trimAscii.toString.toList
  if chars.isEmpty then none
  else
    let (negative, rest) := match chars with
      | '-' :: cs => (true, cs)
      | '+' :: cs => (false, cs)
      | cs => (false, cs)
    if rest.isEmpty then none
    else
      let (intDigits, afterInt) := rest.span Char.isDigit
      let (fracDigits, afterFrac) := match afterInt with
        | '.' :: cs => cs.span Char.isDigit
        | cs => ([], cs)
      if intDigits.isEmpty && fracDigits.isEmpty then none
      else if fracDigits.isEmpty && afterInt.head? != some '.' && !afterFrac.any (fun c => c == 'e' || c == 'E') then none
      else
        let parseExp := match afterFrac with
          | 'e' :: rest' | 'E' :: rest' =>
            let (en, ed) := match rest' with
              | '-' :: ds => (true, ds)
              | '+' :: ds => (false, ds)
              | ds => (false, ds)
            let (expDigits, trailing) := ed.span Char.isDigit
            if !trailing.isEmpty || expDigits.isEmpty then none
            else some (en, expDigits.foldl (fun acc c => acc * 10 + charToDigit c) 0)
          | [] => some (false, 0)
          | _ => none
        match parseExp with
        | none => none
        | some (expNeg, ev) =>
          let mantissa := (intDigits ++ fracDigits).foldl (fun acc c => acc * 10 + charToDigit c) 0
          let netExp : Int := (if expNeg then -(ev : Int) else (ev : Int)) - (fracDigits.length : Int)
          let f := if netExp >= 0 then Float.ofScientific mantissa false netExp.toNat
                   else Float.ofScientific mantissa true (-netExp).toNat
          some (if negative then -f else f)

/-- Infer a `Value` from a CSV field string. -/
private def inferValue (s : String) : Value :=
  if s.isEmpty || s == "NA" || s == "null" || s == "NULL" then .null
  else if s == "true" || s == "True" || s == "TRUE" then .bool true
  else if s == "false" || s == "False" || s == "FALSE" then .bool false
  else match s.toInt? with
    | some n => .int n
    | none => match parseFloat? s with
      | some f => .float f
      | none => .str s

/-- Infer a `ColumnType` from a column's values (nulls skipped; mixed if conflicting). -/
private def inferColumnType (vals : Array Value) : ColumnType := Id.run do
  let mut seenType : Option ColumnType := none
  for v in vals do
    let vt := match v with
      | .int _ => some ColumnType.int
      | .float _ => some ColumnType.float
      | .str _ => some ColumnType.str
      | .bool _ => some ColumnType.bool
      | .null => none
    match vt with
    | none => pure ()
    | some t =>
      match seenType with
      | none => seenType := some t
      | some prev => if prev != t then return ColumnType.mixed
  seenType.getD .mixed

/-- Parse a CSV string into a `DataFrame` (with `Value` type inference). -/
def parseCsv (content : String) (opts : CsvOptions := {}) : DataFrame :=
  let rawRows := parseCsvRaw content opts
  if rawRows.isEmpty then DataFrame.empty
  else
    let (colNames, dataRows) :=
      if opts.hasHeader then
        (rawRows[0]!, rawRows.extract 1 rawRows.size)
      else
        ((Array.range rawRows[0]!.size).map fun i => s!"col{i}", rawRows)
    let nRows := dataRows.size
    let nCols := colNames.size
    let columns := (Array.range nCols).map fun colIdx =>
      let vals := dataRows.map fun row => if h : colIdx < row.size then inferValue row[colIdx] else Value.null
      let ct := inferColumnType vals
      Column.mk (if h : colIdx < colNames.size then colNames[colIdx] else s!"col{colIdx}") vals ct
    { columns
    , nRows
    , columns_aligned := fun i h =>
        DataFrame.map_column_aligned (Array.range nCols) nRows _ (fun _ => Array.size_map) i h }

/-- Read a CSV file into a `DataFrame`. -/
def readCsv (path : System.FilePath) (opts : CsvOptions := {}) : IO DataFrame := do
  return parseCsv (← IO.FS.readFile path) opts

/-- Does a field need quoting? -/
private def needsQuoting (s : String) (opts : CsvOptions) : Bool :=
  s.any fun c => c == opts.delimiter || c == opts.quoteChar || c == '\n' || c == '\r'

/-- Quote a field for CSV output (doubling embedded quotes). -/
private def quoteField (s : String) (opts : CsvOptions) : String :=
  if needsQuoting s opts then
    let escaped := s.toList.map fun c =>
      if c == opts.quoteChar then s!"{opts.quoteChar}{opts.quoteChar}" else c.toString
    s!"{opts.quoteChar}{"".intercalate escaped}{opts.quoteChar}"
  else s

/-- Render a `Value` as a CSV field. -/
private def valueToField : Value → String
  | .int n   => toString n
  | .float f => toString f
  | .str s   => s
  | .bool b  => if b then "true" else "false"
  | .null    => ""

/-- Render a `DataFrame` to a CSV string. -/
def DataFrame.toCsv (df : DataFrame) (opts : CsvOptions := {}) : String := Id.run do
  let delim := opts.delimiter.toString
  let mut lines : Array String := #[]
  if opts.hasHeader then
    lines := lines.push (delim.intercalate (df.columns.toList.map fun c => quoteField c.name opts))
  for rowIdx in [:df.nRows] do
    let fields := df.columns.toList.map fun col =>
      quoteField (valueToField (if h : rowIdx < col.values.size then col.values[rowIdx] else .null)) opts
    lines := lines.push (delim.intercalate fields)
  "\n".intercalate lines.toList

/-- Write a `DataFrame` to a CSV file. -/
def writeCsv (df : DataFrame) (path : System.FilePath) (opts : CsvOptions := {}) : IO Unit :=
  IO.FS.writeFile path (df.toCsv opts)

/-- Read a TSV (tab-separated) file. -/
def readTsv (path : System.FilePath) : IO DataFrame :=
  readCsv path { delimiter := '\t' }

end DataFrame
