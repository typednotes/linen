/-
  Tests for `Linen.Database.SQL.Pool`.

  `PoolSettings` (with its proof fields) and `PoolError` are pure, so they are
  checked with `#guard`. An offline regression exercises checkout accounting
  when libpq cannot open a connection.
-/
import Linen.Database.SQL.Pool

open Database.SQL.Connection
open Database.SQL.Session
open Database.SQL.Pool

namespace Tests.Database.SQL.Pool

/-! ### PoolSettings — defaults and proof fields -/

-- Defaults: maxSize = 10, idleTimeout = 300; the proofs fill in via `by omega`.
#guard ({ connSettings := Settings.uri "host=localhost" } : PoolSettings).maxSize == 10
#guard ({ connSettings := Settings.uri "host=localhost" } : PoolSettings).idleTimeout == 300
#guard ({ maxSize := 20, connSettings := Settings.uri "x" } : PoolSettings).maxSize == 20
#guard ({ idleTimeout := 600, connSettings := Settings.uri "x" } : PoolSettings).idleTimeout == 600
#guard ({ connSettings := Settings.uri "host=db port=5432" } : PoolSettings).connSettings.connString
        == "host=db port=5432"

-- The proof fields really constrain the values.
example : ({ connSettings := Settings.uri "x" } : PoolSettings).maxSize > 0 :=
  ({ connSettings := Settings.uri "x" } : PoolSettings).maxSize_pos
example : ({ connSettings := Settings.uri "x" } : PoolSettings).idleTimeout ≤ 86400 :=
  ({ connSettings := Settings.uri "x" } : PoolSettings).idleTimeout_bounded

-- Repr renders the headline fields.
#guard (reprStr ({ connSettings := Settings.uri "host=localhost" } : PoolSettings)).startsWith
        "PoolSettings(maxSize=10,"

/-! ### PoolError -/

#guard toString PoolError.poolExhausted == "PoolError: pool exhausted"
#guard toString (PoolError.connectionError (ConnectionError.cantConnect "boom"))
        == "PoolError: ConnectionError: boom"
#guard toString (PoolError.sessionError (SessionError.clientError "bad"))
        == "PoolError: ClientError: bad"

/-! ### Pool operations — signatures (running needs live connections) -/

example : PoolSettings → IO Pool := Pool.create
example {α} : Pool → Session α → IO (Except PoolError α) := Pool.use
example : Pool → IO Unit := Pool.destroy
example : Pool → IO (Nat × Nat × Nat) := Pool.stats

-- S2: a failed open restores capacity and does not count as a creation.
#eval show IO Unit from do
  let settings := Settings.uri "host=/dev/null connect_timeout=2"
  let pool ← Pool.create { maxSize := 1, connSettings := settings }
  try
    for _ in [:2] do
      match ← pool.use (pure () : Session Unit) with
      | .error (.connectionError _) => pure ()
      | .error .poolExhausted => throw (IO.userError "failed checkout kept its reservation")
      | _ => throw (IO.userError "expected to fail opening the PostgreSQL connection")
      unless (← pool.stats) == (0, 0, 0) do
        throw (IO.userError "failed checkout changed pool statistics")
  finally
    pool.destroy

end Tests.Database.SQL.Pool
