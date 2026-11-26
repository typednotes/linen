import Linen.Database.SQL.Scoped

open Database.SQL Control.Resource

example : True := by
  fail_if_success
    have _ : ConnectionM .open_ .closed (Except Session.SessionError Unit) :=
      Program.ibind ConnectionM.close fun () => ConnectionM.sql "SELECT 1"
  fail_if_success
    have _ : PoolM .open_ .closed (Nat × Nat × Nat) :=
      Program.ibind PoolM.close fun () => PoolM.stats
  trivial

#eval show IO Unit from do
  let settings := Connection.Settings.uri "host=/dev/null connect_timeout=2"
  let acquisition ← ConnectionM.run settings ConnectionM.close
  match acquisition with
  | .error (.cantConnect _) => pure ()
  | .ok _ => throw (IO.userError "unexpected PostgreSQL connection")
  let stats ← PoolM.run { connSettings := settings } <|
    Program.ibind PoolM.stats fun counts => Program.map (fun () => counts) PoolM.close
  unless stats == (0, 0, 0) do throw (IO.userError "new pool was not empty")
