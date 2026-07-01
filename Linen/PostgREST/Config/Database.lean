/-
  PostgREST.Config.Database -- Database connection config helpers

  Utility functions for working with PostgreSQL connection URIs and
  database configuration settings.  Provides URI parsing, search path
  construction, and connection string helpers.

  ## Haskell source
  - `PostgREST.Config` (postgrest package, database-related helpers)

  ## Design
  - `DbUriParts` decomposes a PostgreSQL connection URI into its
    constituent parts:
    $$\text{DbUriParts} = \{ \text{host},\; \text{port},\; \text{dbname},\;
      \text{user},\; \text{password}? \}$$
  - Search path construction produces the `SET search_path TO ...`
    statement combining exposed schemas and extra search path schemas
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.Config

open PostgREST.SchemaCache.Identifiers

-- ────────────────────────────────────────────────────────────────────
-- Connection URI parts
-- ────────────────────────────────────────────────────────────────────

/-- Decomposed PostgreSQL connection URI.
    $$\text{DbUriParts} = \{ \text{host},\; \text{port},\; \text{dbname},\;
      \text{user},\; \text{password}? \}$$ -/
structure DbUriParts where
  /-- Database host. -/
  dbHost : String := "localhost"
  /-- Database port. -/
  dbPort : Nat := 5432
  /-- Database name. -/
  dbName : String := "postgres"
  /-- Database user. -/
  dbUser : String := "postgres"
  /-- Database password (none for peer/trust authentication). -/
  dbPassword : Option String := none
  deriving Repr, Inhabited

/-- Reconstruct a connection URI from parts.
    $$\text{toUri}(p) = \texttt{postgresql://}[\text{user}[:\text{pass}]@]
      \text{host}:\text{port}/\text{dbname}$$ -/
def DbUriParts.toUri (p : DbUriParts) : String :=
  let auth := match p.dbPassword with
    | some pass => s!"{p.dbUser}:{pass}@"
    | none => s!"{p.dbUser}@"
  s!"postgresql://{auth}{p.dbHost}:{p.dbPort}/{p.dbName}"

-- ────────────────────────────────────────────────────────────────────
-- Search path
-- ────────────────────────────────────────────────────────────────────

/-- Build the `search_path` setting from exposed schemas and extra
    search path schemas.  Schemas are quoted to prevent injection.
    $$\text{searchPathSql}(\text{schemas}, \text{extra}) =
      \texttt{SET search\_path TO}\ \text{quoted}(\text{schemas} \cup \text{extra})$$ -/
def searchPathSql (schemas : List Schema) (extraSearchPath : List Schema) : String :=
  let allSchemas := schemas ++ extraSearchPath
  let quoted := allSchemas.map (fun s => "\"" ++ s.replace "\"" "\"\"" ++ "\"")
  s!"SET search_path TO {String.intercalate ", " quoted}"

/-- Build a search path using comma-separated schema names (unquoted).
    For display purposes only, not for SQL injection-safe contexts. -/
def searchPathDisplay (schemas : List Schema) (extraSearchPath : List Schema) : String :=
  String.intercalate ", " (schemas ++ extraSearchPath)

-- ────────────────────────────────────────────────────────────────────
-- Role setting
-- ────────────────────────────────────────────────────────────────────

/-- Generate the SQL to switch to a given database role.
    $$\text{setRoleSql}(r) = \texttt{SET LOCAL ROLE}\ \text{quoted}(r)$$ -/
def setRoleSql (role : String) : String :=
  let quoted := "\"" ++ role.replace "\"" "\"\"" ++ "\""
  s!"SET LOCAL ROLE {quoted}"

/-- Generate the SQL to reset the role to the default.
    $$\text{resetRoleSql} = \texttt{RESET ROLE}$$ -/
def resetRoleSql : String :=
  "RESET ROLE"

-- ────────────────────────────────────────────────────────────────────
-- Transaction mode
-- ────────────────────────────────────────────────────────────────────

/-- Transaction access mode. -/
inductive TxMode where
  /-- Read-only transaction (for GET/HEAD requests). -/
  | readOnly
  /-- Read-write transaction (for POST/PUT/PATCH/DELETE). -/
  | readWrite
  deriving BEq, Repr

/-- Generate the SQL to set the transaction access mode.
    $$\text{setTxModeSql}(\text{readOnly}) =
      \texttt{SET TRANSACTION READ ONLY}$$ -/
def setTxModeSql : TxMode -> String
  | .readOnly  => "SET TRANSACTION READ ONLY"
  | .readWrite => "SET TRANSACTION READ WRITE"

/-- Transaction end action. -/
inductive TxEnd where
  /-- Commit the transaction. -/
  | commit
  /-- Rollback the transaction. -/
  | rollback
  deriving BEq, Repr

end PostgREST.Config
