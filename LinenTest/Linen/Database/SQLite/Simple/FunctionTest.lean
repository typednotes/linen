/-
  Tests for `Linen.Database.SQLite.Simple.Function`.

  Exercises `createFunction0`/`createFunction1`/`createFunction2`/
  `createFunction3` (the 0..3 arity cutoff documented in
  `Linen/Database/SQLite/Simple/Function.lean`'s module doc) and
  `deleteFunction`, calling each registered function from real SQL against a
  `:memory:` database.
-/
import Linen.Database.SQLite.Simple
import Linen.Database.SQLite.Simple.Function

open Database.SQLite.Simple
open Database.SQLite.Simple.Types (Only)

namespace Tests.Database.SQLite.Simple.Function

#eval show IO Unit from do
  withConnection ":memory:" fun conn => do
    -- arity 0
    createFunction0 conn "answer" true (pure (42 : Int))
    let r0 : Array (Only Int) ← query_ conn "SELECT answer()"
    if r0[0]!.fromOnly != 42 then throw (IO.userError "createFunction0: unexpected result")

    -- arity 1
    createFunction1 conn "double" true (fun (n : Int) => pure (n * 2))
    let r1 : Array (Only Int) ← query conn "SELECT double(?)" (Only.mk (21 : Int))
    if r1[0]!.fromOnly != 42 then throw (IO.userError "createFunction1: unexpected result")

    -- arity 2
    createFunction2 conn "addTwo" true (fun (a b : Int) => pure (a + b))
    let r2 : Array (Only Int) ← query conn "SELECT addTwo(?, ?)" ((19 : Int), (23 : Int))
    if r2[0]!.fromOnly != 42 then throw (IO.userError "createFunction2: unexpected result")

    -- arity 3
    createFunction3 conn "addThree" true (fun (a b c : Int) => pure (a + b + c))
    let r3 : Array (Only Int) ← query conn "SELECT addThree(?, ?, ?)" ((10 : Int), (10 : Int), (22 : Int))
    if r3[0]!.fromOnly != 42 then throw (IO.userError "createFunction3: unexpected result")

    -- a `String`-valued function, to exercise `ToField`/`FromField` beyond `Int`
    createFunction1 conn "shout" true (fun (s : String) => pure (s ++ "!"))
    let r4 : Array (Only String) ← query conn "SELECT shout(?)" (Only.mk "hi")
    if r4[0]!.fromOnly != "hi!" then throw (IO.userError "createFunction1 (String): unexpected result")

    -- `deleteFunction` removes the registration: SQLite reports an error for
    -- the now-unknown function rather than succeeding
    deleteFunction conn "double" 1
    let failed ← try
      let _ : Array (Only Int) ← query conn "SELECT double(?)" (Only.mk (1 : Int))
      pure false
    catch _ =>
      pure true
    if !failed then throw (IO.userError "expected calling a deleted function to fail")

end Tests.Database.SQLite.Simple.Function
