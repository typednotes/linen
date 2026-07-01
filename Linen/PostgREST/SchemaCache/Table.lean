/-
  PostgREST.SchemaCache.Table — Table and column metadata

  Types representing PostgreSQL table/view metadata as discovered from
  the system catalogs.  Tables carry their columns, primary keys, and
  access permissions.

  ## Haskell source
  - `PostgREST.SchemaCache.Table` (postgrest package)

  ## Typing
  - `Table.pk_subset` proves that primary key columns reference existing columns
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.SchemaCache

open PostgREST.SchemaCache.Identifiers

-- ────────────────────────────────────────────────────────────────────
-- Column
-- ────────────────────────────────────────────────────────────────────

/-- A PostgreSQL column descriptor.
    $$\text{Column} = \{ \text{name}, \text{type}, \text{nullable}, \ldots \}$$ -/
structure Column where
  /-- The table this column belongs to. -/
  colTable : QualifiedIdentifier
  /-- Column name. -/
  colName : FieldName
  /-- Human-readable description (from `COMMENT ON COLUMN`). -/
  colDescription : Option String := none
  /-- Whether the column accepts NULL values. -/
  colNullable : Bool
  /-- PostgreSQL type name (e.g., `"text"`, `"integer"`, `"uuid"`). -/
  colType : String
  /-- Maximum character length (for `varchar(n)`). -/
  colMaxLen : Option Nat := none
  /-- Default value expression (e.g., `"nextval('seq')"`). -/
  colDefault : Option String := none
  /-- Enum values if this is an enum type. -/
  colEnum : List String := []
  /-- Whether this column is part of the primary key. -/
  colIsPrimaryKey : Bool := false
  deriving BEq, Repr

instance : ToString Column where
  toString c := s!"{c.colTable}.{c.colName} : {c.colType}"

-- ────────────────────────────────────────────────────────────────────
-- Table
-- ────────────────────────────────────────────────────────────────────

/-- A PostgreSQL table or view descriptor.
    $$\text{Table} = \{ \text{schema}, \text{name}, \text{columns}, \text{pk}, \ldots \}$$ -/
structure Table where
  /-- Schema name. -/
  tableSchema : Schema
  /-- Table/view name. -/
  tableName : TableName
  /-- Human-readable description (from `COMMENT ON TABLE`). -/
  tableDescription : Option String := none
  /-- Whether the table supports INSERT. -/
  tableInsertable : Bool := true
  /-- Whether the table supports UPDATE. -/
  tableUpdatable : Bool := true
  /-- Whether the table supports DELETE. -/
  tableDeletable : Bool := true
  /-- Whether this is a view (not a base table). -/
  tableIsView : Bool := false
  /-- All columns of the table. -/
  tableColumns : Array Column := #[]
  /-- Primary key columns (subset of `tableColumns`). -/
  tablePrimaryKey : Array Column := #[]
  /-- Proof that every primary key column name appears in `tableColumns`.
      This ensures the PK is a genuine subset of the table's columns.
      $$\forall c \in \text{tablePrimaryKey},\;
        c.\text{colName} \in \text{tableColumns}.\text{map}(\cdot.\text{colName})$$ -/
  pk_subset : ∀ c, c ∈ tablePrimaryKey.toList →
    c.colName ∈ (tableColumns.map (·.colName)).toList := by
    intro c hc; simp_all
  deriving Repr

/-- Get the qualified identifier for a table. -/
def Table.toQi (t : Table) : QualifiedIdentifier :=
  { qiSchema := t.tableSchema, qiName := t.tableName }

/-- Look up a column by name. -/
def Table.findColumn (t : Table) (name : FieldName) : Option Column :=
  t.tableColumns.find? (·.colName == name)

/-- Get all column names. -/
def Table.columnNames (t : Table) : Array FieldName :=
  t.tableColumns.map (·.colName)

/-- Get the primary key column names. -/
def Table.pkColumnNames (t : Table) : Array FieldName :=
  t.tablePrimaryKey.map (·.colName)

/-- Whether the table has a primary key. -/
def Table.hasPrimaryKey (t : Table) : Bool :=
  !t.tablePrimaryKey.isEmpty

instance : ToString Table where
  toString t := s!"{t.tableSchema}.{t.tableName} ({t.tableColumns.size} cols)"

end PostgREST.SchemaCache
