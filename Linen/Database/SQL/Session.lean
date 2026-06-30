/-
  Linen.Database.SQL.Session — Database session monad

  A `Session` is a computation that runs against a PostgreSQL connection,
  producing either a result or a `SessionError`.  Sessions compose via
  standard monadic operations and can execute `Statement` values.

  ## Haskell source
  - `Hasql.Session` (hasql package)

  ## Design
  `Session` is a thin alias for the standard transformer stack
  `ReaderT Connection (ExceptT SessionError IO)`, so its `Monad`,
  `MonadLift IO`, `MonadReader`, and `MonadExcept` instances — and the monad
  laws — all come from the Lean standard library rather than a bespoke copy:
  - access to the connection via `ReaderT` (`read`/`getConnection`),
  - typed error handling via `ExceptT` (`throw`/`try … catch`),
  - `IO` for actual database calls (auto-lifted).
-/

import Linen.Database.SQL.Connection
import Linen.Database.PostgreSQL.LibPQ

namespace Database.SQL.Session

open Database.PostgreSQL.LibPQ
open Database.SQL.Connection

-- ────────────────────────────────────────────────────────────────────
-- Session errors
-- ────────────────────────────────────────────────────────────────────

/-- Errors that can occur during a database session. -/
inductive SessionError where
  /-- Query returned an error from PostgreSQL. -/
  | queryError (status : ExecStatus) (message : String)
  /-- A result decoding error (wrong type, unexpected null, etc.). -/
  | resultError (message : String)
  /-- The connection was lost. -/
  | connectionError (message : String)
  /-- A client-side error (e.g. bad parameters). -/
  | clientError (message : String)
  deriving BEq, Repr

instance : ToString SessionError where
  toString
    | .queryError st msg => s!"QueryError({repr st}): {msg}"
    | .resultError msg => s!"ResultError: {msg}"
    | .connectionError msg => s!"ConnectionError: {msg}"
    | .clientError msg => s!"ClientError: {msg}"

-- ────────────────────────────────────────────────────────────────────
-- Session monad
-- ────────────────────────────────────────────────────────────────────

/-- A database session: a computation with access to a `Connection`
    that may fail with `SessionError`.
    $$\text{Session}\ \alpha := \text{ReaderT Connection}\ (\text{ExceptT SessionError IO})\ \alpha$$

    Defined as an `abbrev` so the standard library supplies `Monad`,
    `MonadLift IO`, `MonadReader Connection`, and `MonadExcept SessionError`. -/
abbrev Session (α : Type) := ReaderT Connection (ExceptT SessionError IO) α

namespace Session

/-- Get the managed connection from the session. -/
def getConnection : Session Connection := read

/-- Get the raw libpq `PgConn` from the session. -/
def getRawConnection : Session PgConn := return (← read).raw

-- ────────────────────────────────────────────────────────────────────
-- SQL execution within a session
-- ────────────────────────────────────────────────────────────────────

/-- Execute a simple SQL statement (no parameters, no result decoding). -/
def sql (query : String) : Session Unit := do
  let conn ← read
  let result ← exec conn.raw query
  let st ← resultStatus result
  if st.isOk then
    return ()
  else
    let msg ← resultErrorMessage result
    throw (.queryError st msg)

/-- Execute a parameterized SQL query, returning the raw `PgResult`.
    This is the low-level escape hatch; prefer `Statement.run` for
    type-safe parameter encoding and result decoding. -/
def query (queryStr : String) (params : Array (Option String)) : Session PgResult := do
  let conn ← read
  let result ← execParams conn.raw queryStr params
  let st ← resultStatus result
  if st.isOk then
    return result
  else
    let msg ← resultErrorMessage result
    throw (.queryError st msg)

-- ────────────────────────────────────────────────────────────────────
-- Transaction helpers
-- ────────────────────────────────────────────────────────────────────

/-- Run a session inside a transaction.  Rolls back on `SessionError`. -/
def transaction (action : Session α) : Session α := do
  let conn ← read
  let _ ← exec conn.raw "BEGIN"
  try
    let a ← action
    let _ ← exec conn.raw "COMMIT"
    return a
  catch e =>
    let _ ← exec conn.raw "ROLLBACK"
    throw e

-- ────────────────────────────────────────────────────────────────────
-- Running a session
-- ────────────────────────────────────────────────────────────────────

/-- Run a session against a connection.
    $$\text{run} : \text{Session}\ \alpha \to \text{Connection}
      \to \text{IO}\ (\text{Except SessionError}\ \alpha)$$ -/
def run (session : Session α) (conn : Connection) : IO (Except SessionError α) :=
  ExceptT.run (ReaderT.run session conn)

end Session
end Database.SQL.Session
