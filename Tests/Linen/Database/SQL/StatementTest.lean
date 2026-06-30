/-
  Tests for `Linen.Database.SQL.Statement`.

  A `Statement` bundles a SQL string, an `Encoders.Params`, and a
  `Decoders.Result`.  The SQL string, the `prepared` flag, and the encoder
  (which is pure) are checked with `#guard`; `run` and the decoders require a
  live `PgResult`/`Connection`, so they are pinned at the type level.
-/
import Linen.Database.SQL.Statement

open Database.PostgreSQL.LibPQ
open Database.SQL.Session
open Database.SQL.Encoders
open Database.SQL.Statement

namespace Tests.Database.SQL.Statement

/-! ### sql_ — no params, no result -/

#guard (Statement.sql_ "SELECT 1").sql == "SELECT 1"
#guard (Statement.sql_ "SELECT 1").prepared == true
#guard (Statement.sql_ "SELECT 1").encode.encode () == #[]
#guard (Statement.sql_ "SELECT 1").encode.width == 0

/-! ### command — params, no result -/

#guard (Statement.command "INSERT INTO t VALUES ($1)" Params.text).sql
        == "INSERT INTO t VALUES ($1)"
#guard (Statement.command "q" Params.text).encode.encode "hi" == #[some "hi"]
#guard (Statement.command "q" Params.text).encode.width == 1
#guard (Statement.command "q" Params.text).prepared == true
#guard (Statement.command "q" (Params.pair Params.text Params.int)).encode.encode ("a", 5)
        == #[some "a", some "5"]
#guard (Statement.command "q" (Params.pair Params.text Params.int)).encode.width == 2

/-! ### contramapParams — rewrites the encoder, preserves sql/width -/

#guard (Statement.contramapParams String.length (Statement.command "q" Params.nat)).encode.encode "abc"
        == #[some "3"]
#guard (Statement.contramapParams String.length (Statement.command "q" Params.nat)).encode.width == 1
#guard (Statement.contramapParams String.length (Statement.command "q" Params.nat)).sql == "q"

/-! ### mapResult — preserves sql / encoder (decode needs a live PgResult) -/

#guard (Statement.mapResult (fun _ => 42) (Statement.command "q" Params.text)).sql == "q"
#guard (Statement.mapResult (fun _ => 42) (Statement.command "q" Params.text)).encode.width == 1
#guard (Statement.mapResult (fun _ => 42) (Statement.command "q" Params.text)).prepared == true

/-! ### Combinators — signatures (running needs a live session/result) -/

example {p r} : Statement p r → p → Session r := Statement.run
example {p} : String → Params p → Statement p Unit := Statement.command
example : String → Statement Unit Unit := Statement.sql_
example {p a b} : (a → b) → Statement p a → Statement p b := Statement.mapResult
example {a b r} : (b → a) → Statement a r → Statement b r := Statement.contramapParams

end Tests.Database.SQL.Statement
