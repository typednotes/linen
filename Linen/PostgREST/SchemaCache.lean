/-
  PostgREST.SchemaCache — Schema introspection cache

  Queries PostgreSQL system catalogs to discover tables, views, columns,
  relationships, functions, and type representations.  The result is
  cached in memory and refreshed on LISTEN/NOTIFY events.

  ## Haskell source
  - `PostgREST.SchemaCache` (postgrest package)

  ## Design
  The `SchemaCache` is a snapshot of the database schema at a point in
  time.  It is loaded once at startup and reloaded when PostgREST
  receives a `NOTIFY pgrst` event from PostgreSQL.
-/

import Linen.PostgREST.SchemaCache.Identifiers
import Linen.PostgREST.SchemaCache.Table
import Linen.PostgREST.SchemaCache.Relationship
import Linen.PostgREST.SchemaCache.Routine
import Linen.PostgREST.SchemaCache.Representations

namespace PostgREST.SchemaCache

open Identifiers

-- ────────────────────────────────────────────────────────────────────
-- Schema cache
-- ────────────────────────────────────────────────────────────────────

/-- The complete schema cache, loaded from PostgreSQL system catalogs.
    $$\text{SchemaCache} = \{ \text{tables}, \text{relationships},
      \text{routines}, \text{representations}, \text{timezones} \}$$ -/
structure SchemaCache where
  /-- All tables and views, indexed by qualified identifier. -/
  dbTables : List (QualifiedIdentifier × Table)
  /-- All relationships, indexed by source table. -/
  dbRelationships : List (QualifiedIdentifier × Array Relationship)
  /-- All routines (functions/procedures), indexed by qualified identifier. -/
  dbRoutines : List (QualifiedIdentifier × Array Routine)
  /-- Type representations (output casts). -/
  dbRepresentations : List (QualifiedIdentifier × Array Representation)
  /-- Media handlers for custom content types. -/
  dbMediaHandlers : List MediaHandler := []
  /-- Known timezones. -/
  dbTimezones : List String := []
  /-- PostgreSQL version. -/
  dbPgVersion : Option Nat := none
  deriving Repr

namespace SchemaCache

/-- An empty schema cache. -/
def empty : SchemaCache :=
  { dbTables := []
    dbRelationships := []
    dbRoutines := []
    dbRepresentations := [] }

/-- Look up a table by qualified identifier. -/
def findTable (sc : SchemaCache) (qi : QualifiedIdentifier) : Option Table :=
  sc.dbTables.lookup qi

/-- Look up relationships for a table. -/
def findRelationships (sc : SchemaCache) (qi : QualifiedIdentifier) : Array Relationship :=
  match sc.dbRelationships.lookup qi with
  | some rels => rels
  | none => #[]

/-- Look up routines by qualified identifier. -/
def findRoutines (sc : SchemaCache) (qi : QualifiedIdentifier) : Array Routine :=
  match sc.dbRoutines.lookup qi with
  | some rs => rs
  | none => #[]

/-- Get all table names in the given schemas. -/
def tablesInSchemas (sc : SchemaCache) (schemas : List Schema) : List Table :=
  sc.dbTables.filterMap fun (qi, t) =>
    if schemas.elem qi.qiSchema then some t else none

-- ────────────────────────────────────────────────────────────────────
-- SQL queries for schema introspection
-- ────────────────────────────────────────────────────────────────────

/-- SQL query to load all tables and views from the given schemas. -/
def tablesSql (schemas : List Schema) : String :=
  let schemaList := ", ".intercalate (schemas.map (s!"'{·}'"))
  s!"SELECT
    n.nspname AS table_schema,
    c.relname AS table_name,
    d.description AS table_description,
    c.relkind IN ('r', 'p') AS insertable,
    c.relkind IN ('r', 'p') AS updatable,
    c.relkind IN ('r', 'p') AS deletable,
    c.relkind IN ('v', 'm') AS is_view
  FROM pg_catalog.pg_class c
  JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
  LEFT JOIN pg_catalog.pg_description d ON d.objoid = c.oid AND d.objsubid = 0
  WHERE n.nspname IN ({schemaList})
    AND c.relkind IN ('r', 'v', 'm', 'f', 'p')
  ORDER BY n.nspname, c.relname"

/-- SQL query to load all columns. -/
def columnsSql (schemas : List Schema) : String :=
  let schemaList := ", ".intercalate (schemas.map (s!"'{·}'"))
  s!"SELECT
    n.nspname AS table_schema,
    c.relname AS table_name,
    a.attname AS column_name,
    d.description,
    NOT a.attnotnull AS is_nullable,
    pg_catalog.format_type(a.atttypid, a.atttypmod) AS data_type,
    CASE WHEN a.atttypmod > 0 AND pg_catalog.format_type(a.atttypid, a.atttypmod) LIKE 'character%'
         THEN a.atttypmod - 4 END AS max_length,
    pg_catalog.pg_get_expr(ad.adbin, ad.adrelid) AS column_default,
    COALESCE(
      (SELECT array_agg(e.enumlabel ORDER BY e.enumsortorder)
       FROM pg_catalog.pg_enum e WHERE e.enumtypid = a.atttypid), ARRAY[]::text[]
    ) AS enum_values,
    EXISTS (
      SELECT 1 FROM pg_catalog.pg_index i
      WHERE i.indrelid = c.oid AND i.indisprimary AND a.attnum = ANY(i.indkey)
    ) AS is_pk
  FROM pg_catalog.pg_attribute a
  JOIN pg_catalog.pg_class c ON c.oid = a.attrelid
  JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
  LEFT JOIN pg_catalog.pg_description d ON d.objoid = c.oid AND d.objsubid = a.attnum
  LEFT JOIN pg_catalog.pg_attrdef ad ON ad.adrelid = c.oid AND ad.adnum = a.attnum
  WHERE n.nspname IN ({schemaList})
    AND a.attnum > 0
    AND NOT a.attisdropped
    AND c.relkind IN ('r', 'v', 'm', 'f', 'p')
  ORDER BY n.nspname, c.relname, a.attnum"

/-- SQL query to load foreign key relationships. -/
def relationshipsSql (schemas : List Schema) : String :=
  let schemaList := ", ".intercalate (schemas.map (s!"'{·}'"))
  s!"SELECT
    ns1.nspname AS table_schema,
    tab1.relname AS table_name,
    ns2.nspname AS foreign_table_schema,
    tab2.relname AS foreign_table_name,
    con.conname AS constraint_name,
    (SELECT array_agg(a.attname ORDER BY u.ord)
     FROM unnest(con.conkey) WITH ORDINALITY u(attnum, ord)
     JOIN pg_catalog.pg_attribute a ON a.attrelid = con.conrelid AND a.attnum = u.attnum
    ) AS columns,
    (SELECT array_agg(a.attname ORDER BY u.ord)
     FROM unnest(con.confkey) WITH ORDINALITY u(attnum, ord)
     JOIN pg_catalog.pg_attribute a ON a.attrelid = con.confrelid AND a.attnum = u.attnum
    ) AS foreign_columns,
    tab1.oid = tab2.oid AS is_self
  FROM pg_catalog.pg_constraint con
  JOIN pg_catalog.pg_class tab1 ON tab1.oid = con.conrelid
  JOIN pg_catalog.pg_namespace ns1 ON ns1.oid = tab1.relnamespace
  JOIN pg_catalog.pg_class tab2 ON tab2.oid = con.confrelid
  JOIN pg_catalog.pg_namespace ns2 ON ns2.oid = tab2.relnamespace
  WHERE con.contype = 'f'
    AND (ns1.nspname IN ({schemaList}) OR ns2.nspname IN ({schemaList}))
  ORDER BY ns1.nspname, tab1.relname, con.conname"

/-- SQL query to load routines (functions). -/
def routinesSql (schemas : List Schema) : String :=
  let schemaList := ", ".intercalate (schemas.map (s!"'{·}'"))
  s!"SELECT
    n.nspname AS routine_schema,
    p.proname AS routine_name,
    d.description,
    pg_catalog.pg_get_function_result(p.oid) AS return_type,
    p.provolatile AS volatility,
    p.proretset AS returns_set,
    COALESCE(p.proargnames, ARRAY[]::text[]) AS arg_names,
    COALESCE(
      array_agg(pg_catalog.format_type(unnested.type_oid, NULL) ORDER BY unnested.ord),
      ARRAY[]::text[]
    ) AS arg_types,
    p.pronargdefaults AS n_defaults
  FROM pg_catalog.pg_proc p
  JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
  LEFT JOIN pg_catalog.pg_description d ON d.objoid = p.oid
  LEFT JOIN LATERAL unnest(COALESCE(p.proargtypes::oid[], ARRAY[]::oid[]))
    WITH ORDINALITY AS unnested(type_oid, ord) ON true
  WHERE n.nspname IN ({schemaList})
  GROUP BY n.nspname, p.proname, p.oid, d.description, p.provolatile, p.proretset,
           p.proargnames, p.pronargdefaults
  ORDER BY n.nspname, p.proname"

/-- SQL to get the PostgreSQL version number. -/
def versionSql : String :=
  "SELECT current_setting('server_version_num')::integer"

end SchemaCache
end PostgREST.SchemaCache
