/-
  PostgREST.Query.SqlFragment -- SQL fragment generation

  The core SQL builder for PostgREST.  Provides functions to generate
  safe SQL fragments from plan types: identifier quoting, literal escaping,
  field references with JSON paths, filter expressions, logic trees,
  ORDER BY terms, and JSON aggregation wrappers.

  ## Haskell source
  - `PostgREST.Query.SqlFragment` (postgrest package)

  ## Design
  - SQL identifier quoting (`pgFmtIdent`) doubles internal double-quotes,
    preventing SQL injection by construction:
    $$\text{pgFmtIdent}(s) = \texttt{"}\ s[\texttt{"} \mapsto \texttt{""}]\ \texttt{"}$$
  - Literal quoting (`pgFmtLit`) doubles internal single-quotes:
    $$\text{pgFmtLit}(s) = \texttt{'}\ s[\texttt{'} \mapsto \texttt{''}]\ \texttt{'}$$
  - Field formatting composes the table reference, JSON path operators,
    and optional cast into a single SQL expression
  - Filter formatting maps PostgREST operators to their SQL equivalents
  - Logic tree formatting produces nested boolean expressions with AND/OR/NOT
-/

import Linen.PostgREST.SchemaCache.Identifiers
import Linen.PostgREST.Plan.Types

namespace PostgREST.Query

open PostgREST.SchemaCache.Identifiers
open PostgREST.Plan

-- ────────────────────────────────────────────────────────────────────
-- SQL identifier and literal quoting
-- ────────────────────────────────────────────────────────────────────

/-- Quote a SQL identifier by wrapping in double-quotes and escaping
    internal double-quotes.
    $$\text{pgFmtIdent}(s) = \texttt{"}\ s[\texttt{"} \mapsto \texttt{""}]\ \texttt{"}$$
    This prevents SQL injection in identifier contexts. -/
def pgFmtIdent (s : String) : String :=
  "\"" ++ s.replace "\"" "\"\"" ++ "\""

/-- Format a schema-qualified identifier as `"schema"."name"`.
    $$\text{pgFmtQi}(qi) = \text{pgFmtIdent}(qi.\text{schema})
      \cdot \texttt{.} \cdot \text{pgFmtIdent}(qi.\text{name})$$ -/
def pgFmtQi (qi : QualifiedIdentifier) : String :=
  pgFmtIdent qi.qiSchema ++ "." ++ pgFmtIdent qi.qiName

/-- Quote a SQL literal by wrapping in single-quotes and escaping
    internal single-quotes.
    $$\text{pgFmtLit}(s) = \texttt{'}\ s[\texttt{'} \mapsto \texttt{''}]\ \texttt{'}$$
    Use for known-safe values only, NOT arbitrary user input (use
    parameterized queries for user input). -/
def pgFmtLit (s : String) : String :=
  "'" ++ s.replace "'" "''" ++ "'"

-- ────────────────────────────────────────────────────────────────────
-- Column and field references
-- ────────────────────────────────────────────────────────────────────

/-- Format a column reference: `"schema"."table"."column"`.
    $$\text{pgFmtColumn}(t, c) = \text{pgFmtQi}(t) \cdot \texttt{.}
      \cdot \text{pgFmtIdent}(c)$$ -/
def pgFmtColumn (table : QualifiedIdentifier) (col : FieldName) : String :=
  pgFmtQi table ++ "." ++ pgFmtIdent col

/-- Format a coercible field reference with optional JSON path traversal
    and type cast.
    $$\text{pgFmtField}(cf, t?) = \text{base}
      \circ \text{jsonOps} \circ \text{cast}$$
    where base is `"table"."col"` or just `"col"`, jsonOps chains
    `->` and `->>` operators, and cast appends `::type`. -/
def pgFmtField (cf : CoercibleField) (table : Option QualifiedIdentifier) : String :=
  let base := match table with
    | some qi => pgFmtQi qi ++ "." ++ pgFmtIdent cf.cfName
    | none => pgFmtIdent cf.cfName
  let withJson := cf.cfJsonPath.foldl (fun acc op => match op with
    | .arrowRight k => s!"{acc}->'{k}'"
    | .arrowRightRight k => s!"{acc}->>'{k}'"
  ) base
  match cf.cfTransform with
  | some cast => s!"({withJson})::{cast}"
  | none => withJson

-- ────────────────────────────────────────────────────────────────────
-- Operator mapping
-- ────────────────────────────────────────────────────────────────────

/-- Map a PostgREST simple operator name to its SQL equivalent.
    $$\text{simpleOpToSql} : \text{String} \to \text{String}$$ -/
def simpleOpToSql : String -> String
  | "eq"         => "="
  | "neq"        => "<>"
  | "gt"         => ">"
  | "gte"        => ">="
  | "lt"         => "<"
  | "lte"        => "<="
  | "like"       => "LIKE"
  | "ilike"      => "ILIKE"
  | "in"         => "IN"
  | "is"         => "IS"
  | "isdistinct" => "IS DISTINCT FROM"
  | "cs"         => "@>"
  | "cd"         => "<@"
  | "ov"         => "&&"
  | "sl"         => "<<"
  | "sr"         => ">>"
  | "nxl"        => "&<"
  | "nxr"        => "&>"
  | "adj"        => "-|-"
  | "match"      => "~"
  | "imatch"     => "~*"
  | op           => op

/-- Map a PostgREST full-text search operator to its SQL equivalent.
    All FTS variants use the `@@` operator; the difference is in
    how the query term is constructed. -/
def ftsOpToSql : String -> String
  | "fts"   => "@@"
  | "plfts" => "@@"
  | "phfts" => "@@"
  | "wfts"  => "@@"
  | op      => op

-- ────────────────────────────────────────────────────────────────────
-- Filter expression formatting
-- ────────────────────────────────────────────────────────────────────

/-- Format a filter expression as a SQL WHERE clause fragment.
    Uses `$?` as placeholder for parameterized values.
    $$\text{pgFmtFilter}(f, t?) = \text{field}\ \text{op}\ \$?$$ -/
def pgFmtFilter (f : CoercibleFilter) (table : Option QualifiedIdentifier) : String :=
  let field := pgFmtField f.cfField table
  match f.cfOperator with
  | .simple op => s!"{field} {simpleOpToSql op} $?"
  | .fts op => s!"{field} {ftsOpToSql op} $?"
  | .quantified quant op => s!"{field} {simpleOpToSql op} {quant}($?)"

-- ────────────────────────────────────────────────────────────────────
-- Logic tree formatting
-- ────────────────────────────────────────────────────────────────────

/-- Format a logic tree as a SQL boolean expression.
    $$\text{pgFmtLogicTree}(t, \text{tbl}?) = \begin{cases}
      \text{pgFmtFilter}(f, \text{tbl}?) & \text{if } t = \text{stmnt}(f) \\
      [\lnot]\ (\text{child}_1\ \text{op}\ \text{child}_2\ \ldots)
        & \text{if } t = \text{expr}(\neg?, \text{op}, \text{children})
    \end{cases}$$ -/
def pgFmtLogicTree (lt : CoercibleLogicTree) (table : Option QualifiedIdentifier) : String :=
  match lt with
  | .stmnt f => pgFmtFilter f table
  | .expr neg op children =>
    let opStr := match op with | .and_ => " AND " | .or_ => " OR "
    let inner := (children.map (pgFmtLogicTree · table)).toList
    let joined := String.intercalate opStr inner
    let wrapped := s!"({joined})"
    if neg then s!"NOT {wrapped}" else wrapped

-- ────────────────────────────────────────────────────────────────────
-- ORDER BY formatting
-- ────────────────────────────────────────────────────────────────────

/-- Format an ORDER BY term as a SQL fragment.
    $$\text{pgFmtOrderTerm}(t, \text{tbl}?) = \text{field}\ \text{dir}\
      [\text{NULLS FIRST|LAST}]$$ -/
def pgFmtOrderTerm (t : CoercibleOrderTerm) (table : Option QualifiedIdentifier) : String :=
  let field := pgFmtField t.cotField table
  let dir := match t.cotDirection with | .asc => "ASC" | .desc => "DESC"
  let nulls := match t.cotNulls with
    | some .nullsFirst => " NULLS FIRST"
    | some .nullsLast => " NULLS LAST"
    | none => ""
  s!"{field} {dir}{nulls}"

-- ────────────────────────────────────────────────────────────────────
-- JSON aggregation wrappers
-- ────────────────────────────────────────────────────────────────────

/-- Wrap a SQL expression in `json_agg` with a `coalesce` fallback to `'[]'`.
    $$\text{asJsonF}(sql) = \texttt{coalesce(json\_agg(}sql\texttt{), '[]'::json)}$$ -/
def asJsonF (sql : String) : String :=
  s!"coalesce(json_agg({sql}), '[]'::json)"

/-- Wrap a SQL expression in `json_agg` and extract the first element,
    with a `coalesce` fallback to `'null'`.
    $$\text{asJsonSingleF}(sql) =
      \texttt{coalesce((json\_agg(}sql\texttt{))->0, 'null'::json)}$$ -/
def asJsonSingleF (sql : String) : String :=
  s!"coalesce((json_agg({sql}))->0, 'null'::json)"

-- ────────────────────────────────────────────────────────────────────
-- GUC variable setting
-- ────────────────────────────────────────────────────────────────────

/-- Generate a `SET LOCAL` statement for a GUC variable.
    The key is quoted as an identifier and the value as a literal.
    $$\text{setConfigLocal}(k, v) =
      \texttt{SET LOCAL}\ \text{pgFmtIdent}(k)\ \texttt{=}\ \text{pgFmtLit}(v)$$ -/
def setConfigLocal (key : String) (value : String) : String :=
  s!"SET LOCAL {pgFmtIdent key} = {pgFmtLit value}"

/-- Generate a `SET LOCAL` statement with a constant (unescaped) key name.
    Used for PostgREST internal settings where the key is a known constant.
    $$\text{setConfigWithConstantName}(k, v) =
      \texttt{SET LOCAL "}k\texttt{" =}\ \text{pgFmtLit}(v)$$ -/
def setConfigWithConstantName (key : String) (value : String) : String :=
  s!"SET LOCAL \"{key}\" = {pgFmtLit value}"

-- ────────────────────────────────────────────────────────────────────
-- Convenience: format multiple order terms
-- ────────────────────────────────────────────────────────────────────

/-- Format an array of ORDER BY terms as a comma-separated SQL fragment.
    Returns `none` if the array is empty. -/
def pgFmtOrderClause (terms : Array CoercibleOrderTerm) (table : Option QualifiedIdentifier) : Option String :=
  if terms.isEmpty then none
  else
    let formatted := terms.map (pgFmtOrderTerm · table)
    some (String.intercalate ", " formatted.toList)

-- ────────────────────────────────────────────────────────────────────
-- Convenience: format multiple filters as AND-connected WHERE clause
-- ────────────────────────────────────────────────────────────────────

/-- Format an array of filters as an AND-connected WHERE clause fragment.
    Returns `none` if the array is empty. -/
def pgFmtWhereClause (filters : Array CoercibleFilter) (table : Option QualifiedIdentifier) : Option String :=
  if filters.isEmpty then none
  else
    let formatted := filters.map (pgFmtFilter · table)
    some (String.intercalate " AND " formatted.toList)

-- ────────────────────────────────────────────────────────────────────
-- SQL quoting correctness theorems
-- ────────────────────────────────────────────────────────────────────

/-- `pgFmtIdent` always produces a string starting with `"`.
    $$\forall s,\; \text{pgFmtIdent}(s).\text{startsWith}(\texttt{"}) = \text{true}$$ -/
theorem pgFmtIdent_startsWith_dquote (s : String) :
    (pgFmtIdent s).startsWith "\"" = true := by
  simp [pgFmtIdent]

/-- `pgFmtLit` always produces a string starting with `'`.
    $$\forall s,\; \text{pgFmtLit}(s).\text{startsWith}(\texttt{'}) = \text{true}$$ -/
theorem pgFmtLit_startsWith_squote (s : String) :
    (pgFmtLit s).startsWith "'" = true := by
  simp [pgFmtLit]

/-- Quoting and then identity-quoting an empty string:
    `pgFmtIdent "" = "\"\""`.
    $$\text{pgFmtIdent}(\varepsilon) = \texttt{""}$$ -/
theorem pgFmtIdent_empty : pgFmtIdent "" = "\"\"" := by native_decide

/-- Quoting an empty literal yields `''` (two single-quote characters).
    $$\text{pgFmtLit}(\varepsilon) = \texttt{''}$$ -/
theorem pgFmtLit_empty : pgFmtLit "" = "''" := by native_decide

end PostgREST.Query
