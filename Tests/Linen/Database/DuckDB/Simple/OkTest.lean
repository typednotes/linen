/-
  Tests for `Linen.Database.DuckDB.Simple.Ok`.

  Illustrates `Ok`'s error-accumulating `Applicative`/`Alternative`/`Monad`
  behaviour and the `toExcept`/`ofExcept` conversion, mirroring
  `Linen.Database.SQLite.Simple.Ok`'s own test module (they share the exact
  same shape — see this module's doc for why it isn't reused directly).
-/
import Linen.Database.DuckDB.Simple.Ok

open Database.DuckDB.Simple

namespace Tests.Database.DuckDB.Simple.Ok

#guard (Ok.ok 1 : Ok Nat) == .ok 1
#guard (Ok.ok 1 : Ok Nat) != .ok 2
#guard (Ok.errors #["a"] : Ok Nat) == .errors #["b"] -- coarse `BEq`, see the module doc

-- `Functor`.
#guard ((· + 1) <$> Ok.ok 1 : Ok Nat) == .ok 2
#guard ((· + 1) <$> (Ok.errors #["boom"] : Ok Nat)) == .errors #["boom"]

-- `Applicative`: short-circuits on the first failing argument.
#guard (Ok.ok (· + 1) <*> Ok.ok 1 : Ok Nat) == .ok 2
#guard (Ok.errors #["f"] <*> Ok.ok 1 : Ok Nat) == .errors #["f"]
#guard (Ok.ok (· + 1) <*> (Ok.errors #["x"] : Ok Nat)) == .errors #["x"]
#guard ((Ok.errors #["f"] : Ok (Nat → Nat)) <*> (Ok.errors #["x"] : Ok Nat)) ==
  .errors #["f", "x"]

-- `Alternative`: prefers the first success; accumulates errors otherwise.
#guard ((Ok.ok 1 : Ok Nat) <|> Ok.ok 2) == .ok 1
#guard ((Ok.errors #["a"] : Ok Nat) <|> Ok.ok 2) == .ok 2
#guard ((Ok.errors #["a"] : Ok Nat) <|> Ok.errors #["b"]) == .errors #["a", "b"]

-- `Monad`.
#guard ((Ok.ok 1 : Ok Nat) >>= fun n => .ok (n + 1)) == .ok 2
#guard ((Ok.errors #["boom"] : Ok Nat) >>= fun n => .ok (n + 1)) == .errors #["boom"]

-- `fail`.
#guard (Ok.fail "bad value" : Ok Nat) == .errors #["bad value"]

-- `toExcept`/`ofExcept`. (`Except` itself has no `BEq` instance, so these
-- match on the result explicitly rather than comparing with `==`.)
#guard match (Ok.ok 1 : Ok Nat).toExcept with | .ok 1 => true | _ => false
#guard match (Ok.errors #["boom"] : Ok Nat).toExcept with | .error #["boom"] => true | _ => false
#guard (Ok.ofExcept (.ok 1 : Except (Array String) Nat)) == .ok 1
#guard (Ok.ofExcept (.error #["boom"] : Except (Array String) Nat)) == .errors #["boom"]

example (e : Except (Array String) Nat) : (Ok.ofExcept e).toExcept = e :=
  Ok.toExcept_ofExcept e

end Tests.Database.DuckDB.Simple.Ok
