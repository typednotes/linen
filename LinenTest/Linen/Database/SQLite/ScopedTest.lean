import Linen.Database.SQLite.Scoped

open Database.SQLite.Simple Control.Resource

example : True := by
  fail_if_success
    have _ : StatementM .ready .ready (Array Database.SQLite3.SQLData) := StatementM.columns
  fail_if_success
    have _ : StatementM .closed .ready Unit := StatementM.reset
  fail_if_success
    have _ : ConnectionM .open_ .closed Unit :=
      Program.ibind ConnectionM.close fun () => ConnectionM.execute_ "SELECT 1"
  trivial

-- Step produces a row that can be read, then DONE that can only be reset
-- or finalized. The following statement can still use the connection.
#eval show IO Unit from do
  let result ← ConnectionM.run ":memory:" <|
    Program.ibind (ConnectionM.withStatement "SELECT ?" <|
      Program.ibind (StatementM.bindParams #[.integer 42]) fun () =>
      Program.bind StatementM.step fun status =>
        match status with
        | .done => Program.map (fun () => #[]) StatementM.finalize
        | .row =>
          Program.ibind StatementM.columns fun row =>
          Program.bind StatementM.step fun next =>
            match next with
            | .row => Program.map (fun () => #[]) StatementM.finalize
            | .done => Program.map (fun () => row) StatementM.finalize) fun row =>
    Program.ibind (ConnectionM.execute_ "CREATE TABLE test (x INTEGER)") fun () =>
    Program.map (fun () => row) ConnectionM.close
  unless result == #[.integer 42] do throw (IO.userError "unexpected scoped SQLite row")
