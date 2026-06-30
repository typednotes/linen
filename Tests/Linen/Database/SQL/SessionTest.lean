/-
  Tests for `Linen.Database.SQL.Session`.

  `SessionError` is a pure data type and is checked with `#guard`. Running a
  `Session` requires a live `Connection` (an FFI-only `PgConn`), so the monad
  operations are exercised at the type level: the `example`s confirm that the
  standard `ReaderT`/`ExceptT` stack supplies `Monad`, auto-lifting of `IO`,
  `read`, `throw`, and `try … catch`, and that the SQL combinators compose.
-/
import Linen.Database.SQL.Session

open Database.PostgreSQL.LibPQ
open Database.SQL.Connection
open Database.SQL.Session

namespace Tests.Database.SQL.Session

/-! ### SessionError -/

#guard (SessionError.resultError "x") == SessionError.resultError "x"
#guard (SessionError.queryError .fatalError "boom") == SessionError.queryError .fatalError "boom"
#guard ((SessionError.queryError .fatalError "boom")
          == SessionError.queryError .tuplesOk "boom") == false
#guard ((SessionError.resultError "a") == SessionError.clientError "a") == false
#guard toString (SessionError.resultError "boom") == "ResultError: boom"
#guard toString (SessionError.connectionError "lost") == "ConnectionError: lost"
#guard toString (SessionError.clientError "bad") == "ClientError: bad"
-- queryError embeds the `repr` of the status, whose exact text we don't pin down.
#guard (toString (SessionError.queryError .fatalError "x")).startsWith "QueryError("

/-! ### Session combinators — signatures (running needs a live connection) -/

example : String → Session Unit := Session.sql
example : String → Array (Option String) → Session PgResult := Session.query
example : Session Connection := Session.getConnection
example : Session PgConn := Session.getRawConnection
example {α} : Session α → Session α := Session.transaction
example {α} : Session α → Connection → IO (Except SessionError α) := Session.run

/-! ### The standard monad stack is available -/

-- Monadic sequencing, `pure`, and auto-lifted `IO` all typecheck.
example : Session Nat := do
  Session.sql "CREATE TEMP TABLE t (x int)"
  let _ ← Session.query "INSERT INTO t VALUES ($1)" #[some "1"]
  pure 42

-- `throw` comes from `MonadExcept SessionError Session`.
example : Session Nat := throw (.clientError "bad params")

-- `try … catch` comes from the same instance.
example : Session Unit :=
  try Session.sql "SELECT 1"
  catch _ => pure ()

-- `IO` actions lift automatically into a session.
example : Session Unit := do
  (IO.println "side effect" : IO Unit)
  pure ()

end Tests.Database.SQL.Session
