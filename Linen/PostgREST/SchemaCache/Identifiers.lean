/-
  PostgREST.SchemaCache.Identifiers — Schema-qualified identifiers

  Core identifier types used throughout PostgREST: schema names, table names,
  field names, and qualified identifiers (schema.name pairs). SQL escaping
  functions prevent injection by construction.

  ## Haskell source
  - `PostgREST.SchemaCache.Identifiers` (postgrest package)

  ## Design
  - `Schema`, `TableName`, `FieldName` are type aliases over `String`
    (matching Haskell's `Text` aliases)
  - `QualifiedIdentifier` pairs a schema with a name
  - `escapeIdent` doubles internal quotes, preventing SQL injection
  - `quoteQi` produces a fully qualified `"schema"."name"` string
-/

namespace PostgREST.SchemaCache.Identifiers

-- ────────────────────────────────────────────────────────────────────
-- Type aliases for clarity
-- ────────────────────────────────────────────────────────────────────

/-- A PostgreSQL schema name (e.g., `"public"`). -/
abbrev Schema := String

/-- A PostgreSQL table or view name. -/
abbrev TableName := String

/-- A PostgreSQL column/field name. -/
abbrev FieldName := String

/-- A PostgreSQL function name. -/
abbrev FunctionName := String

/-- A PostgreSQL constraint name. -/
abbrev ConstraintName := String

-- ────────────────────────────────────────────────────────────────────
-- Qualified identifier
-- ────────────────────────────────────────────────────────────────────

/-- A schema-qualified identifier: `schema.name`.
    $$\text{QualifiedIdentifier} = \text{Schema} \times \text{String}$$

    Used for tables, views, functions, etc. The `schema` is the
    PostgreSQL schema (e.g., `"public"`) and `name` is the object name. -/
structure QualifiedIdentifier where
  qiSchema : Schema
  qiName : String
  deriving Repr, Inhabited

instance : BEq QualifiedIdentifier where
  beq a b := a.qiSchema == b.qiSchema && a.qiName == b.qiName

instance : Hashable QualifiedIdentifier where
  hash qi := mixHash (hash qi.qiSchema) (hash qi.qiName)

instance : Ord QualifiedIdentifier where
  compare a b :=
    match compare a.qiSchema b.qiSchema with
    | .eq => compare a.qiName b.qiName
    | ord => ord

instance : ToString QualifiedIdentifier where
  toString qi :=
    if qi.qiSchema.isEmpty then qi.qiName
    else s!"{qi.qiSchema}.{qi.qiName}"

-- ────────────────────────────────────────────────────────────────────
-- SQL escaping
-- ────────────────────────────────────────────────────────────────────

/-- Escape a SQL identifier by doubling internal double-quotes.
    $$\text{escapeIdent}(s) = s[\texttt{"} \mapsto \texttt{""}]$$
    This prevents SQL injection in identifier contexts. -/
def escapeIdent (s : String) : String :=
  s.replace "\"" "\"\""

/-- Quote a single SQL identifier: `"name"` with internal quotes escaped.
    $$\text{quoteIdent}(s) = \texttt{"}\ \text{escapeIdent}(s)\ \texttt{"}$$ -/
def quoteIdent (s : String) : String :=
  "\"" ++ escapeIdent s ++ "\""

/-- Produce a fully qualified SQL identifier: `"schema"."name"`.
    $$\text{quoteQi}(qi) = \text{quoteIdent}(qi.\text{schema}) \cdot \texttt{.} \cdot
      \text{quoteIdent}(qi.\text{name})$$ -/
def quoteQi (qi : QualifiedIdentifier) : String :=
  quoteIdent qi.qiSchema ++ "." ++ quoteIdent qi.qiName

-- ────────────────────────────────────────────────────────────────────
-- Parsing
-- ────────────────────────────────────────────────────────────────────

/-- Parse a dotted identifier string into a `QualifiedIdentifier`.
    If no dot is present, uses `"public"` as the default schema.
    $$\text{toQi}(\texttt{public.users}) = \langle \texttt{public}, \texttt{users} \rangle$$ -/
def toQi (s : String) : QualifiedIdentifier :=
  match s.splitOn "." with
  | [schema, name] => { qiSchema := schema, qiName := name }
  | _ => { qiSchema := "public", qiName := s }

-- ────────────────────────────────────────────────────────────────────
-- Special identifiers
-- ────────────────────────────────────────────────────────────────────

/-- The wildcard identifier representing "any element" in a path. -/
def anyElement : QualifiedIdentifier :=
  { qiSchema := "", qiName := "" }

/-- Test whether a qualified identifier is the "any element" wildcard. -/
def QualifiedIdentifier.isAnyElement (qi : QualifiedIdentifier) : Bool :=
  qi.qiSchema.isEmpty && qi.qiName.isEmpty

-- ────────────────────────────────────────────────────────────────────
-- Relation identifiers
-- ────────────────────────────────────────────────────────────────────

/-- A relation identifier: either a qualified name or the "any element" wildcard. -/
inductive RelIdentifier where
  | relId (qi : QualifiedIdentifier)
  | relAnyElement
  deriving BEq, Repr

-- ────────────────────────────────────────────────────────────────────
-- SQL quoting correctness theorems
-- ────────────────────────────────────────────────────────────────────

/-- `quoteIdent` always produces a string that starts with `"`.
    $$\forall s,\; (\text{quoteIdent}(s)).\text{startsWith}(\texttt{"}) = \text{true}$$ -/
theorem quoteIdent_startsWith_quote (s : String) :
    (quoteIdent s).startsWith "\"" = true := by
  simp [quoteIdent]

/-- Escaping an empty string yields the empty string.
    $$\text{escapeIdent}(\varepsilon) = \varepsilon$$ -/
theorem escapeIdent_empty : escapeIdent "" = "" := by native_decide

/-- Quoting an empty identifier yields `""` (two double-quote characters).
    $$\text{quoteIdent}(\varepsilon) = \texttt{""}$$ -/
theorem quoteIdent_empty : quoteIdent "" = "\"\"" := by native_decide

/-- Escaping a string with an internal double-quote doubles it.
    $$\text{escapeIdent}(\texttt{a"b}) = \texttt{a""b}$$ -/
theorem escapeIdent_doubles_quotes : escapeIdent "a\"b" = "a\"\"b" := by native_decide

/-- `quoteIdent` on a string with internal quotes correctly doubles them.
    $$\text{quoteIdent}(\texttt{a"b}) = \texttt{"a""b"}$$ -/
theorem quoteIdent_doubles_internal : quoteIdent "a\"b" = "\"a\"\"b\"" := by native_decide

end PostgREST.SchemaCache.Identifiers
