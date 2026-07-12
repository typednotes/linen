/-
  Tests for `Linen.Database.SQLite.Simple.Ok`.
-/
import Linen.Database.SQLite.Simple.Ok

open Database.SQLite.Simple

namespace Tests.Database.SQLite.Simple.Ok

#guard (Functor.map (· + 1) (Ok.ok 1 : Ok Nat)) == Ok.ok 2
#guard (Functor.map (· + 1) (Ok.errors #["boom"] : Ok Nat)) == Ok.errors #["boom"]

#guard (Ok.fail "boom" : Ok Nat) == Ok.errors #["boom"]

-- `Applicative`: short-circuits on the first failing argument.
#guard ((Ok.ok (· + 1) : Ok (Nat → Nat)) <*> Ok.ok 41) == Ok.ok 42
#guard ((Ok.errors #["f"] : Ok (Nat → Nat)) <*> Ok.errors #["x"]) == Ok.errors #["f"]
#guard ((Ok.ok (· + 1) : Ok (Nat → Nat)) <*> Ok.errors #["x"]) == Ok.errors #["x"]

-- `Alternative`: prefers the first success, concatenates errors on double failure.
#guard ((Ok.ok 1 : Ok Nat) <|> Ok.ok 2) == Ok.ok 1
#guard ((Ok.errors #["a"] : Ok Nat) <|> Ok.ok 2) == Ok.ok 2
#guard ((Ok.errors #["a"] : Ok Nat) <|> Ok.errors #["b"]) == Ok.errors #["a", "b"]

-- `Monad`.
#guard ((Ok.ok 1 : Ok Nat) >>= fun n => Ok.ok (n + 1)) == Ok.ok 2
#guard ((Ok.errors #["e"] : Ok Nat) >>= fun n => Ok.ok (n + 1)) == Ok.errors #["e"]

#guard match (Ok.ok 1 : Ok Nat).toExcept with | .ok 1 => true | _ => false
#guard match (Ok.errors #["e"] : Ok Nat).toExcept with | .error #["e"] => true | _ => false
#guard Ok.ofExcept (Except.ok 1 : Except (Array String) Nat) == Ok.ok 1
#guard Ok.ofExcept (Except.error #["e"] : Except (Array String) Nat) == Ok.errors #["e"]

example (e : Except (Array String) Nat) : (Ok.ofExcept e).toExcept = e :=
  Ok.toExcept_ofExcept e

end Tests.Database.SQLite.Simple.Ok
