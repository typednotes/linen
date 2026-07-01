/-
  PostgREST.SchemaCache.Relationship — Table relationships

  Represents foreign key relationships between tables, discovered from
  `pg_catalog.pg_constraint`.  PostgREST uses these to resolve resource
  embedding (e.g., `/users?select=*,posts(*)` joins users to posts).

  ## Haskell source
  - `PostgREST.SchemaCache.Relationship` (postgrest package)
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.SchemaCache

open PostgREST.SchemaCache.Identifiers

-- ────────────────────────────────────────────────────────────────────
-- Cardinality
-- ────────────────────────────────────────────────────────────────────

/-- The cardinality of a relationship.
    $$\text{Cardinality} \in \{\text{O2M}, \text{M2O}, \text{O2O}, \text{M2M}\}$$ -/
inductive Cardinality where
  /-- One-to-many: the local table is the "one" side. -/
  | o2m
  /-- Many-to-one: the local table is the "many" side. -/
  | m2o
  /-- One-to-one. -/
  | o2o
  /-- Many-to-many via a junction table. -/
  | m2m (junction : QualifiedIdentifier)
    (junctionCols1 : Array (FieldName × FieldName))
    (junctionCols2 : Array (FieldName × FieldName))
  deriving Repr

instance : BEq Cardinality where
  beq a b := match a, b with
    | .o2m, .o2m => true
    | .m2o, .m2o => true
    | .o2o, .o2o => true
    | .m2m j1 _ _, .m2m j2 _ _ => j1 == j2
    | _, _ => false

instance : ToString Cardinality where
  toString
    | .o2m => "O2M"
    | .m2o => "M2O"
    | .o2o => "O2O"
    | .m2m jt _ _ => s!"M2M({jt})"

-- ────────────────────────────────────────────────────────────────────
-- Relationship
-- ────────────────────────────────────────────────────────────────────

/-- A foreign key relationship between two tables.
    $$\text{Relationship} = \{ \text{table}, \text{foreignTable},
      \text{cardinality}, \text{columns} \}$$ -/
structure Relationship where
  /-- The "local" table. -/
  relTable : QualifiedIdentifier
  /-- The "foreign" (referenced) table. -/
  relForeignTable : QualifiedIdentifier
  /-- Relationship cardinality. -/
  relCardinality : Cardinality
  /-- Column pairs: (local_col, foreign_col). -/
  relColumns : Array (FieldName × FieldName)
  /-- Constraint name (if from a named FK constraint). -/
  relConstraint : Option ConstraintName := none
  /-- Whether this is a self-referencing relationship. -/
  relIsSelf : Bool := false
  /-- Whether this relationship was computed (e.g., M2M from two FK constraints). -/
  relIsComputed : Bool := false
  deriving Repr

instance : BEq Relationship where
  beq a b :=
    a.relTable == b.relTable &&
    a.relForeignTable == b.relForeignTable &&
    a.relCardinality == b.relCardinality &&
    a.relConstraint == b.relConstraint

instance : ToString Relationship where
  toString r :=
    s!"{r.relTable} {r.relCardinality} {r.relForeignTable}"

/-- Get the relationship column names on the local side. -/
def Relationship.localColumns (r : Relationship) : Array FieldName :=
  r.relColumns.map Prod.fst

/-- Get the relationship column names on the foreign side. -/
def Relationship.foreignColumns (r : Relationship) : Array FieldName :=
  r.relColumns.map Prod.snd

end PostgREST.SchemaCache
