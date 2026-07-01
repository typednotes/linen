/-
  Tests for `Linen.Database.SQL.Connection`.

  The `Settings` builders are pure, so they are checked with `#guard`. Most of
  `acquire`/`release`/`withConnection` are live libpq calls in `IO`, so they
  are only pinned down at the type level (no running server here) — except
  for the rejected-connection regression test below, which needs no server:
  connecting to an unreachable port fails immediately and deterministically.
-/
import Linen.Database.SQL.Connection

open Database.SQL.Connection

namespace Tests.Database.SQL.Connection

/-! ### Settings.components — pure connection-string assembly -/

#guard (Settings.components).connString == "host=localhost port=5432"
#guard (Settings.components (host := "")).connString == "port=5432"
#guard (Settings.components (database := "mydb")).connString
        == "host=localhost port=5432 dbname=mydb"
#guard (Settings.components (host := "db.example.com") (port := 6543)
          (user := "alice") (password := "secret") (database := "prod")).connString
        == "host=db.example.com port=6543 user=alice password=secret dbname=prod"

/-! ### Settings.uri / default / BEq / Coe -/

#guard (Settings.uri "postgresql://localhost/db").connString == "postgresql://localhost/db"
#guard (default : Settings).connString == "host=localhost"
#guard (Settings.uri "a") == (Settings.uri "a")
#guard ((Settings.uri "a") == (Settings.uri "b")) == false
#guard ((Settings.components) == Settings.uri "host=localhost port=5432")

-- The proof field guarantees a non-empty connection string.
example : (Settings.uri "x").connString.length > 0 := (Settings.uri "x").nonEmpty

-- `Coe Settings String` projects the connection string.
example : String := Settings.components

/-! ### ConnectionError -/

#guard (ConnectionError.cantConnect "boom") == ConnectionError.cantConnect "boom"
#guard ((ConnectionError.cantConnect "a") == ConnectionError.cantConnect "b") == false
#guard toString (ConnectionError.cantConnect "boom") == "ConnectionError: boom"

/-! ### Acquire / release / withConnection — signatures only (need a server) -/

example : Settings → IO (Except ConnectionError Connection) := acquire
example : Connection → IO Unit := release
example : Settings → (Connection → IO Nat) → IO (Except ConnectionError Nat) := withConnection

/-! ### A rejected connection surfaces as `Except.error`, not a thrown exception

    Regression test for a bug in `ffi/postgres.c`'s `linen_pg_connect`: it used
    to special-case a bad `PQstatus` by throwing the IO error itself and
    discarding the connection, so `acquire`'s own status check (the documented
    `Except`-returning contract) was unreachable dead code — a failed
    connection surfaced as a raw uncaught exception instead. Connecting to an
    unreachable port fails immediately without needing a real Postgres server,
    so this is safe to run unconditionally. -/

#eval show IO Unit from do
  let unreachable := Settings.components (host := "localhost") (port := 1)
  match ← acquire unreachable with
  | .error (.cantConnect _) => pure ()
  | .ok conn =>
    release conn
    throw (IO.userError "expected acquire to fail against an unreachable port, but it succeeded")

#eval show IO Unit from do
  let unreachable := Settings.components (host := "localhost") (port := 1)
  match ← withConnection unreachable (fun _ => pure (0 : Nat)) with
  | .error (.cantConnect _) => pure ()
  | .ok _ =>
    throw (IO.userError "expected withConnection to fail against an unreachable port, but it succeeded")

end Tests.Database.SQL.Connection
