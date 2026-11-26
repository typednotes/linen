import Linen.Control.Resource
import Linen.Database.SQLite.Simple

/-! # Scoped SQLite connections and statements
Own a connection for a program and nest statements inside it. A step's
result determines whether the continuation can read a row.
-/

namespace Database.SQLite.Simple
open Control.Resource
open Database.SQLite.Simple.Types (Query)

private opaque connectionAccess : {p : Prop // p} := ⟨True, trivial⟩
private opaque statementAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- Program over a privately owned SQLite connection. -/
abbrev ConnectionM := Owned connectionAccess.val Connection

/-- States of a statement cursor. Bind parameters before the first step or
after resetting. Inspect column data only when there is a current row. -/
inductive CursorState where
  | ready
  | row
  | done
  | closed
  deriving DecidableEq

/-- Cursor state after a successful SQLite step. -/
def stepState : Database.SQLite3.Bindings.Types.StepResult → CursorState
  | .row => .row
  | .done => .done

/-- Statement action with a fixed final cursor state. -/
abbrev StatementM :=
  Action statementAccess.val (fun _ : CursorState => Statement)

/-- Statement action with a result-dependent final cursor state. -/
abbrev StatementProgram :=
  Program statementAccess.val (fun _ : CursorState => Statement)

namespace StatementM

private def atCursor (f : Statement → IO α) : StatementM state state α :=
  Program.step statementAccess.property fun stmt => do return ⟨← f stmt, stmt⟩

/-- Bind positional parameters before stepping. -/
def bindParams (params : Array Database.SQLite3.SQLData) : StatementM .ready .ready Unit :=
  atCursor fun stmt => Database.SQLite3.bind stmt.statementHandle params

/-- Advance a new statement or move past its current row. -/
def step (_ : state = .ready ∨ state = .row := by decide) :
    StatementProgram state Database.SQLite3.Bindings.Types.StepResult stepState :=
  Program.step statementAccess.property fun stmt => do
    return ⟨← Database.SQLite3.step stmt.statementHandle, stmt⟩

/-- Decode all columns from the current row. -/
def columns : StatementM .row .row (Array Database.SQLite3.SQLData) :=
  atCursor fun stmt => Database.SQLite3.columns stmt.statementHandle

/-- Reset execution to the start, retaining SQLite's current bindings. -/
def reset (_ : state ≠ .closed := by decide) : StatementM state .ready Unit :=
  Program.step statementAccess.property fun stmt => do
    Database.SQLite3.reset stmt.statementHandle
    return ⟨(), stmt⟩

/-- Finalize the statement in any live state. -/
def finalize (_ : state ≠ .closed := by decide) : StatementM state .closed Unit :=
  Program.step statementAccess.property fun stmt => do
    closeStatement stmt
    return ⟨(), stmt⟩

end StatementM

namespace ConnectionM

/-- Open SQLite for the duration of the program. For a private database,
pass `":memory:"`. -/
def run (path : String) (action : ConnectionM .open_ post α) : IO α :=
  Owned.run connectionAccess.property (openConnection path) closeConnection action

/-- Execute a statement with positional parameters. -/
def execute [ToRow q] (sql : Query) (params : q) : ConnectionM .open_ .open_ Unit :=
  Owned.use connectionAccess.property fun conn => Simple.execute conn sql params

/-- Execute a statement with no parameters. -/
def execute_ (sql : Query) : ConnectionM .open_ .open_ Unit :=
  Owned.use connectionAccess.property (Simple.execute_ · sql)

/-- Execute a parameterized query and decode its rows. -/
def query [ToRow q] [FromRow r] (sql : Query) (params : q) :
    ConnectionM .open_ .open_ (Array r) :=
  Owned.use connectionAccess.property fun conn => Simple.query conn sql params

/-- Execute and decode a query with no parameters. -/
def query_ [FromRow r] (sql : Query) : ConnectionM .open_ .open_ (Array r) :=
  Owned.use connectionAccess.property (Simple.query_ · sql)

/-- Run a statement program while the connection remains open. The native
finalizer call is idempotent and also runs if a statement operation throws. -/
def withStatement (sql : String) (action : StatementM .ready .closed α) :
    ConnectionM .open_ .open_ α :=
  Owned.use connectionAccess.property fun conn => do
    let stmt ← openStatement conn sql
    try (·.1) <$> Program.run statementAccess.property action stmt
    finally closeStatement stmt

/-- Begin a transaction, commit on success and roll back on an exception.
The inner program leaves the connection open. -/
def transaction (action : ConnectionM .open_ .open_ α) : ConnectionM .open_ .open_ α :=
  Program.step connectionAccess.property fun cell => do
    let some conn ← cell.get | throw (IO.userError "resource owner is empty")
    let result ← withTransaction conn ((·.1) <$> Program.run connectionAccess.property action cell)
    return ⟨result, cell⟩

/-- Close the connection before the scope returns. -/
def close : ConnectionM .open_ .closed Unit :=
  Owned.close connectionAccess.property closeConnection

end ConnectionM
end Database.SQLite.Simple
