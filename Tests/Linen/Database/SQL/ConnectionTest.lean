/-
  Tests for `Linen.Database.SQL.Connection`.

  The `Settings` builders are pure, so they are checked with `#guard`. The
  `acquire`/`release`/`withConnection` operations are live libpq calls in `IO`,
  so they are only pinned down at the type level (no running server here).
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

end Tests.Database.SQL.Connection
