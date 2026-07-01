/-
  PostgREST.MainTx — Main transaction wrapper

  Wraps every PostgREST request in a PostgreSQL transaction, setting
  the search_path, role, GUC variables, and executing pre-request
  functions.

  ## Haskell source
  - `PostgREST.MainTx` (postgrest package)

  ## Transaction flow
  1. `BEGIN` (or `BEGIN READ ONLY` for safe methods)
  2. `SET LOCAL search_path TO "schema1","schema2",...`
  3. `SET LOCAL role TO "authenticated_role"`
  4. `SET LOCAL request.jwt.claims TO '...'` (JSON claims)
  5. `SET LOCAL request.method TO 'GET'`
  6. `SET LOCAL request.path TO '/table'`
  7. `SET LOCAL request.headers TO '...'` (JSON headers)
  8. Execute pre-request function (if configured)
  9. Execute the actual query
  10. `COMMIT` (or `ROLLBACK` if configured)
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.MainTx

open PostgREST.SchemaCache.Identifiers

-- ────────────────────────────────────────────────────────────────────
-- SQL generation for transaction setup
-- ────────────────────────────────────────────────────────────────────

/-- Escape a string for use as a SQL literal value (single-quote escaping). -/
def sqlLit (s : String) : String :=
  "'" ++ s.replace "'" "''" ++ "'"

/-- Generate the SET LOCAL search_path statement. -/
def setSearchPath (schemas : List Schema) : String :=
  let quoted := schemas.map (fun s => "\"" ++ escapeIdent s ++ "\"")
  s!"SET LOCAL search_path TO {", ".intercalate quoted}"

/-- Generate the SET LOCAL role statement. -/
def setRole (role : String) : String :=
  s!"SET LOCAL role TO {sqlLit role}"

/-- Generate SET LOCAL statements for request context (JWT claims, method, path, headers). -/
def setRequestContext (method : String) (path : String) (role : String)
    (claims : List (String × String)) (headers : List (String × String))
    : List String :=
  let claimsJson := "{" ++ ", ".intercalate (claims.map fun (k, v) =>
    s!"\"{k}\": \"{v}\"") ++ "}"
  let headersJson := "{" ++ ", ".intercalate (headers.map fun (k, v) =>
    s!"\"{k}\": \"{v}\"") ++ "}"
  [ s!"SET LOCAL role TO {sqlLit role}"
  , s!"SET LOCAL request.jwt.claims TO {sqlLit claimsJson}"
  , s!"SET LOCAL request.method TO {sqlLit method}"
  , s!"SET LOCAL request.path TO {sqlLit path}"
  , s!"SET LOCAL request.headers TO {sqlLit headersJson}" ]

/-- Generate the pre-request function call SQL. -/
def preRequestSql (preReq : QualifiedIdentifier) : String :=
  s!"SELECT {quoteQi preReq}()"

end PostgREST.MainTx
