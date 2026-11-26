import Linen.Control.Resource
import Linen.Database.DuckDB.Simple
import Linen.Database.DuckDB.Simple.Copy
import Linen.Database.DuckDB.Simple.FileSystem

/-! # Scoped DuckDB resources
A connection owns its appender and filesystem scopes. Their native handles
remain private to their respective interpreters.
-/

namespace Database.DuckDB.Simple
open Control.Resource
open Database.DuckDB.FFI.Types (Appender FileHandle FileFlag)

private opaque connectionAccess : {p : Prop // p} := ⟨True, trivial⟩
private opaque appenderAccess : {p : Prop // p} := ⟨True, trivial⟩
private opaque fileAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- Program over an owned database connection. -/
abbrev ConnectionM := Owned connectionAccess.val Connection

/-- Program over a bulk appender. Each append takes a complete row. -/
abbrev AppenderM := Owned appenderAccess.val Appender

/-- Program over a nested DuckDB file handle. -/
abbrev FileM := Action fileAccess.val (fun _ : Unit => FileHandle) () ()

namespace AppenderM

private def release (app : Appender) : IO Unit := do
  try
    unless (← FFI.Appender.close app).isSuccess do
      throwSQLError (registrationError "close appender")
  finally
    FFI.Appender.destroy app

/-- Append a complete row. A row shape or value error aborts the program. -/
def appendRow (row : Array FieldValue) : AppenderM .open_ .open_ Unit :=
  Owned.use appenderAccess.property (Copy.appendRow · row)

/-- Flush the appender and release it. Errors abort the program. -/
def close : AppenderM .open_ .closed Unit :=
  Owned.close appenderAccess.property release

end AppenderM

namespace FileM

private def use (f : FileHandle → IO α) : FileM α :=
  Program.step fileAccess.property fun file => do return ⟨← f file, file⟩

/-- Read a chunk, up to the requested size. -/
def read (size : Int64) : FileM ByteArray :=
  use (FileSystem.readFileHandleChunk · size)

/-- Write a complete byte array. -/
def write (data : ByteArray) : FileM Unit :=
  use (FileSystem.writeFileHandleBytes · data)

/-- Move the file cursor to the given position. -/
def seek (position : Int64) : FileM Unit :=
  use (FileSystem.fileHandleSeek · position)

/-- Flush outstanding writes. -/
def sync : FileM Unit :=
  use FileSystem.fileHandleSync

end FileM

namespace ConnectionM

/-- Open and own a connection. Pass `none` for an in-memory database. -/
def run (path : Option String) (action : ConnectionM .open_ post α) : IO α :=
  Owned.run connectionAccess.property (openConnection path) closeConnection action

/-- Execute a parameterized statement. -/
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

/-- Run a bulk appender inside its connection's lifetime. On exit, DuckDB
flushes buffered rows. Use a transaction for all-or-nothing insertion. -/
def withAppender (table : String) (action : AppenderM .open_ post α)
    (schema : Option String := none) : ConnectionM .open_ .open_ α :=
  Owned.use connectionAccess.property fun conn =>
    Owned.run appenderAccess.property (Copy.createAppender conn schema table) AppenderM.release action

/-- Open a file through this connection's filesystem for a nested program. -/
def withFile (path : String) (flags : Array FileFlag) (action : FileM α) :
    ConnectionM .open_ .open_ α :=
  Owned.use connectionAccess.property fun conn =>
    FileSystem.withFileHandle conn path flags fun file =>
      (·.1) <$> Program.run fileAccess.property action file

/-- Run a transaction on this connection, committing on success and rolling
back on an exception. The body leaves the connection open. -/
def transaction (action : ConnectionM .open_ .open_ α) : ConnectionM .open_ .open_ α :=
  Program.step connectionAccess.property fun cell => do
    let some conn ← cell.get | throw (IO.userError "resource owner is empty")
    let result ← withTransaction conn ((·.1) <$> Program.run connectionAccess.property action cell)
    return ⟨result, cell⟩

/-- Close the database connection before returning from the scope. -/
def close : ConnectionM .open_ .closed Unit :=
  Owned.close connectionAccess.property closeConnection

end ConnectionM
end Database.DuckDB.Simple
