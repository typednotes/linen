import Linen.Database.DuckDB.Scoped

open Database.DuckDB.Simple Control.Resource

example : True := by
  fail_if_success
    have _ : AppenderM .open_ .closed Unit :=
      Program.ibind AppenderM.close fun () => AppenderM.appendRow #[.int32 1]
  trivial

#eval show IO Unit from do
  let rows : Array (Only Int32) ← ConnectionM.run none do
    ConnectionM.execute_ "CREATE TABLE test (i INTEGER)"
    ConnectionM.transaction <|
      ConnectionM.withAppender "test" <|
        Program.ibind (AppenderM.appendRow #[.int32 42]) fun () => AppenderM.close
    ConnectionM.query_ "SELECT i FROM test"
  unless rows.map (·.fromOnly) == #[42] do throw (IO.userError "scoped appender lost a row")
