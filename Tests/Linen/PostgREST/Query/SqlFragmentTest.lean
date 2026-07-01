/-
  Tests for `Linen.PostgREST.Query.SqlFragment`.
-/
import Linen.PostgREST.Query.SqlFragment

open PostgREST.Query
open PostgREST.Plan
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.Query.SqlFragment

/-! ### `pgFmtIdent` / `pgFmtQi` / `pgFmtLit` -/

#guard pgFmtIdent "users" == "\"users\""
#guard pgFmtIdent "a\"b" == "\"a\"\"b\""
#guard pgFmtQi { qiSchema := "public", qiName := "users" } == "\"public\".\"users\""
#guard pgFmtLit "hello" == "'hello'"
#guard pgFmtLit "it's" == "'it''s'"

/-! ### `pgFmtColumn` -/

#guard pgFmtColumn { qiSchema := "public", qiName := "users" } "id" == "\"public\".\"users\".\"id\""

/-! ### `pgFmtField` -/

#guard pgFmtField { cfName := "age" } none == "\"age\""
#guard pgFmtField { cfName := "age" } (some { qiSchema := "public", qiName := "users" }) ==
  "\"public\".\"users\".\"age\""
#guard pgFmtField { cfName := "data", cfJsonPath := [.arrowRight "a", .arrowRightRight "b"] } none ==
  "\"data\"->'a'->>'b'"
#guard pgFmtField { cfName := "age", cfTransform := some "integer" } none == "(\"age\")::integer"

/-! ### `simpleOpToSql` / `ftsOpToSql` -/

#guard simpleOpToSql "eq" == "="
#guard simpleOpToSql "gte" == ">="
#guard simpleOpToSql "isdistinct" == "IS DISTINCT FROM"
#guard simpleOpToSql "unknown" == "unknown"

#guard ftsOpToSql "fts" == "@@"
#guard ftsOpToSql "plfts" == "@@"
#guard ftsOpToSql "unknown" == "unknown"

/-! ### `pgFmtFilter` -/

#guard pgFmtFilter { cfField := { cfName := "age" }, cfOperator := .simple "gt", cfValue := "18" } none ==
  "\"age\" > $?"
#guard pgFmtFilter { cfField := { cfName := "body" }, cfOperator := .fts "fts", cfValue := "cat" } none ==
  "\"body\" @@ $?"
#guard pgFmtFilter { cfField := { cfName := "tags" }, cfOperator := .quantified "any" "eq", cfValue := "a" } none ==
  "\"tags\" = any($?)"

/-! ### `pgFmtLogicTree` -/

#guard pgFmtLogicTree (.stmnt { cfField := { cfName := "age" }, cfOperator := .simple "gt", cfValue := "18" }) none ==
  "\"age\" > $?"
#guard pgFmtLogicTree (.expr false .and_ #[
    .stmnt { cfField := { cfName := "age" }, cfOperator := .simple "gt", cfValue := "18" },
    .stmnt { cfField := { cfName := "name" }, cfOperator := .simple "eq", cfValue := "bob" }
  ]) none == "(\"age\" > $? AND \"name\" = $?)"
#guard pgFmtLogicTree (.expr true .or_ #[
    .stmnt { cfField := { cfName := "age" }, cfOperator := .simple "gt", cfValue := "18" }
  ]) none == "NOT (\"age\" > $?)"

/-! ### `pgFmtOrderTerm` -/

#guard pgFmtOrderTerm { cotField := { cfName := "age" } } none == "\"age\" ASC"
#guard pgFmtOrderTerm { cotField := { cfName := "age" }, cotDirection := .desc, cotNulls := some .nullsLast } none ==
  "\"age\" DESC NULLS LAST"
#guard pgFmtOrderTerm { cotField := { cfName := "age" }, cotNulls := some .nullsFirst } none ==
  "\"age\" ASC NULLS FIRST"

/-! ### `asJsonF` / `asJsonSingleF` -/

#guard asJsonF "x" == "coalesce(json_agg(x), '[]'::json)"
#guard asJsonSingleF "x" == "coalesce((json_agg(x))->0, 'null'::json)"

/-! ### `setConfigLocal` / `setConfigWithConstantName` -/

#guard setConfigLocal "role" "webuser" == "SET LOCAL \"role\" = 'webuser'"
#guard setConfigWithConstantName "request.method" "GET" == "SET LOCAL \"request.method\" = 'GET'"

/-! ### `pgFmtOrderClause` -/

#guard pgFmtOrderClause #[] none == none
#guard pgFmtOrderClause #[{ cotField := { cfName := "age" } }, { cotField := { cfName := "name" }, cotDirection := .desc }] none ==
  some "\"age\" ASC, \"name\" DESC"

/-! ### `pgFmtWhereClause` -/

#guard pgFmtWhereClause #[] none == none
#guard pgFmtWhereClause #[
    { cfField := { cfName := "age" }, cfOperator := .simple "gt", cfValue := "18" },
    { cfField := { cfName := "name" }, cfOperator := .simple "eq", cfValue := "bob" }
  ] none == some "\"age\" > $? AND \"name\" = $?"

/-! ### Correctness theorems -/

example : ∀ s : String, (pgFmtIdent s).startsWith "\"" = true := pgFmtIdent_startsWith_dquote
example : ∀ s : String, (pgFmtLit s).startsWith "'" = true := pgFmtLit_startsWith_squote

end Tests.PostgREST.Query.SqlFragment
