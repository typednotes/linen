/-
  Linen.DataFrame.Internal.Types — core DataFrame types

  A strongly-typed tabular structure with a proven rectangular invariant:
  `DataFrame.columns_aligned` certifies that every column has exactly `nRows`
  elements. The proof is a (runtime-erased) field; all smart constructors
  discharge it, so column/row access within bounds is always safe.

  Runtime representation is `Array Column × Nat`; all proofs are erased.
-/

namespace DataFrame

/-! ── ColumnType ── -/

/-- Runtime type tag for column values. -/
inductive ColumnType where
  | int
  | float
  | str
  | bool
  | mixed
deriving BEq, DecidableEq, Repr, Inhabited

instance : ToString ColumnType where
  toString
    | .int   => "int"
    | .float => "float"
    | .str   => "str"
    | .bool  => "bool"
    | .mixed => "mixed"

/-! ── Value ── -/

/-- A heterogeneous cell value. -/
inductive Value where
  | int   : Int    → Value
  | float : Float  → Value
  | str   : String → Value
  | bool  : Bool   → Value
  | null  : Value
deriving BEq, Repr, Inhabited

instance : ToString Value where
  toString
    | .int n   => toString n
    | .float f => toString f
    | .str s   => s
    | .bool b  => if b then "true" else "false"
    | .null    => "null"

/-- A numeric tag for cross-variant ordering: int < float < str < bool < null. -/
private def Value.variantOrd : Value → Nat
  | .int _   => 0
  | .float _ => 1
  | .str _   => 2
  | .bool _  => 3
  | .null    => 4

/-- Ordering on `Value`: nulls last; same-variant uses the natural order;
    cross-variant uses `int < float < str < bool < null`. -/
instance : Ord Value where
  compare a b := match a, b with
    | .int x,   .int y   => compare x y
    | .float x, .float y => if x < y then .lt else if x == y then .eq else .gt
    | .str x,   .str y   => compare x y
    | .bool x,  .bool y  => compare (if x then 1 else 0 : Nat) (if y then 1 else 0)
    | x,        y        => compare x.variantOrd y.variantOrd

namespace Value

/-- Extract a `Float` from a numeric value (`int`/`float`), else `none`. -/
def toFloat? : Value → Option Float
  | .int n   => some (Float.ofInt n)
  | .float f => some f
  | _        => none

/-- The `ColumnType` tag for a single value (`null` is type-agnostic → `mixed`). -/
def columnType : Value → ColumnType
  | .int _   => .int
  | .float _ => .float
  | .str _   => .str
  | .bool _  => .bool
  | .null    => .mixed

/-- Extract an `Int`, else `none`. -/
def toInt? : Value → Option Int
  | .int n => some n
  | _      => none

/-- Extract a `String`, else `none`. -/
def toStr? : Value → Option String
  | .str s => some s
  | _      => none

/-- Extract a `Bool`, else `none`. -/
def toBool? : Value → Option Bool
  | .bool b => some b
  | _       => none

/-- Is this value null? -/
def isNull : Value → Bool
  | .null => true
  | _     => false

end Value

/-! ── Column ── -/

/-- A named column with a tracked element type. -/
structure Column where
  /-- Human-readable column name. -/
  name : String
  /-- The column data, stored as a flat array. -/
  values : Array Value
  /-- The predominant element type. -/
  colType : ColumnType
deriving Repr, Inhabited

namespace Column

/-- Number of elements in this column. -/
def size (c : Column) : Nat := c.values.size

/-- The value at index `i`, if in bounds. -/
def get? (c : Column) (i : Nat) : Option Value :=
  if h : i < c.values.size then some c.values[i] else none

/-- Map a function over every value (result type becomes `mixed`). -/
def map (f : Value → Value) (c : Column) : Column :=
  { name := c.name, values := c.values.map f, colType := .mixed }

/-- Keep only values satisfying `p`. -/
def filter (p : Value → Bool) (c : Column) : Column :=
  { name := c.name, values := c.values.filter p, colType := c.colType }

instance : ToString Column where
  toString c := s!"Column({c.name}, {c.colType}, n={c.size})"

end Column

/-! ── DataFrame ── -/

/-- A tabular structure with a proven rectangular invariant: every column has
    exactly `nRows` elements (the `columns_aligned` field, erased at runtime). -/
structure DataFrame where
  /-- The named columns. -/
  columns : Array Column
  /-- Number of rows (shared by all columns). -/
  nRows : Nat
  /-- Every column has exactly `nRows` elements. -/
  columns_aligned : ∀ (i : Nat) (h : i < columns.size), columns[i].values.size = nRows

namespace DataFrame

/-- If every column produced by `f` has `nRows` values, the mapped array is aligned. -/
protected theorem map_column_aligned {α : Type} (src : Array α) (nRows : Nat)
    (f : α → Column) (hf : ∀ a, (f a).values.size = nRows)
    (i : Nat) (h : i < (src.map f).size) :
    (src.map f)[i].values.size = nRows := by
  rw [Array.getElem_map]; exact hf _

/-- The empty DataFrame: zero columns, zero rows. -/
def empty : DataFrame where
  columns := #[]
  nRows := 0
  columns_aligned := fun _ h => absurd h (Nat.not_lt_zero _)

/-- Number of columns. -/
def nColumns (df : DataFrame) : Nat := df.columns.size

/-- Column names, in order. -/
def columnNames (df : DataFrame) : List String :=
  df.columns.toList.map Column.name

/-- Look up a column by name (first match). -/
def getColumn? (df : DataFrame) (name : String) : Option Column :=
  df.columns.find? (·.name == name)

/-- Collect the values of row `i` across all columns, using the alignment proof
    for safe in-bounds access. Well-founded on `columns.size - j`. -/
private def rowAux (df : DataFrame) (i : Nat) (hi : i < df.nRows) (j : Nat) (acc : Array Value) :
    Array Value :=
  if hj : j < df.columns.size then
    rowAux df i hi (j + 1) (acc.push (df.columns[j].values[i]'(by rw [df.columns_aligned j hj]; exact hi)))
  else acc
termination_by df.columns.size - j
decreasing_by omega

/-- Retrieve a full row as an array of values, if the index is in bounds. -/
def getRow? (df : DataFrame) (i : Nat) : Option (Array Value) :=
  if hi : i < df.nRows then some (rowAux df i hi 0 #[]) else none

/-- Construct a `DataFrame` from columns, or `none` if their lengths differ. -/
def fromColumns (cols : Array Column) : Option DataFrame :=
  if h0 : cols.size = 0 then
    some empty
  else
    let col0 : Column := cols[0]'(by omega)
    let nRows := col0.values.size
    let allMatch := cols.all (·.values.size == nRows)
    if hallMatch : allMatch then
      some {
        columns := cols
        nRows := nRows
        columns_aligned := fun i hi => eq_of_beq ((Array.all_eq_true.mp hallMatch) i hi)
      }
    else none

/-- Construct from row-oriented data: short rows are padded with `null`, extra
    cells dropped. Always succeeds. -/
def fromRows (header : Array String) (rows : Array (Array Value)) : DataFrame :=
  let nRows := rows.size
  {
    columns := (Array.range header.size).map fun j =>
      let name := if h : j < header.size then header[j] else ""
      let vals := rows.map fun row => if h : j < row.size then row[j] else Value.null
      { name := name, values := vals, colType := ColumnType.mixed : Column }
    nRows := nRows
    columns_aligned := fun i hi => by
      simp only [Array.size_map, Array.size_range] at hi
      simp only [Array.getElem_map, Array.getElem_range, Array.size_map]
      rfl
  }

/-- Construct from named column pairs, or `none` if lengths differ. -/
def fromNamedColumns (pairs : Array (String × Array Value)) : Option DataFrame :=
  fromColumns (pairs.map fun (name, vals) =>
    { name := name, values := vals, colType := ColumnType.mixed : Column })

end DataFrame

/-! ── GroupedDataFrame ── -/

/-- A DataFrame split into groups by one or more key columns: each entry is a
    (key-value tuple, sub-frame) pair; `groupKeys` records the grouping columns. -/
structure GroupedDataFrame where
  /-- Each entry is (key-value tuple, sub-frame). -/
  groups : Array (Array Value × DataFrame)
  /-- Names of the grouping columns. -/
  groupKeys : List String

end DataFrame
