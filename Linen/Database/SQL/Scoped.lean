import Linen.Control.Resource
import Linen.Database.SQL.Statement
import Linen.Database.SQL.Pool

/-! # Scoped PostgreSQL resources
Execute typed statements without passing the connection or pool to callers.
Acquisition, statement, and pool errors stay in their existing result types.
-/

namespace Database.SQL
open Control.Resource

private opaque connectionAccess : {p : Prop // p} := ⟨True, trivial⟩
private opaque poolAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- An action over a private managed PostgreSQL connection. -/
abbrev ConnectionM := Owned connectionAccess.val Connection.Connection

/-- An action over a private PostgreSQL connection pool. -/
abbrev PoolM := Owned poolAccess.val Pool.Pool

namespace ConnectionM

/-- Run a PostgreSQL program after successfully acquiring its connection. -/
def run (settings : Connection.Settings) (action : ConnectionM .open_ post α) :
    IO (Except Connection.ConnectionError α) := do
  match ← Connection.acquire settings with
  | .error error => return .error error
  | .ok conn =>
    .ok <$> Owned.run connectionAccess.property (pure conn) Connection.release action

/-- Execute SQL, returning its typed session error if it fails. -/
def sql (query : String) : ConnectionM .open_ .open_ (Except Session.SessionError Unit) :=
  Owned.use connectionAccess.property (Session.Session.run (Session.Session.sql query))

/-- Execute a typed statement and decode its result. -/
def statement (stmt : Statement.Statement params result) (args : params) :
    ConnectionM .open_ .open_ (Except Session.SessionError result) :=
  Owned.use connectionAccess.property (Session.Session.run (stmt.run args))

/-- Release the connection early. -/
def close : ConnectionM .open_ .closed Unit :=
  Owned.close connectionAccess.property Connection.release

end ConnectionM

namespace PoolM

/-- Create a pool and drain its idle connections when the program returns.
All uses through this program finish before the next step begins. -/
def run (settings : Pool.PoolSettings) (action : PoolM .open_ post α) : IO α :=
  Owned.run poolAccess.property (Pool.Pool.create settings) Pool.Pool.destroy action

/-- Run a typed statement on a borrowed connection. The pool takes the
connection back before the next action can run. -/
def statement (stmt : Statement.Statement params result) (args : params) :
    PoolM .open_ .open_ (Except Pool.PoolError result) :=
  Owned.use poolAccess.property (·.use (stmt.run args))

/-- Inspect idle, checked out, and total connection counts. -/
def stats : PoolM .open_ .open_ (Nat × Nat × Nat) :=
  Owned.use poolAccess.property Pool.Pool.stats

/-- Destroy the pool before the program returns. -/
def close : PoolM .open_ .closed Unit :=
  Owned.close poolAccess.property Pool.Pool.destroy

end PoolM
end Database.SQL
