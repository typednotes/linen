/-
  Tests for `Linen.Control.Monad.Except`.

  Covers the Haskell `mtl` names built on Lean's `ExceptT`/`Except`:
  `throwError`, `catchError`, `liftEither`, `mapExceptT`, `withExceptT`,
  `runExceptT`.
-/
import Linen.Control.Monad.Except

open Control.Monad.Except

namespace Tests.Control.Monad.Except

-- Lean's `Except` has no `BEq` instance, so compare via `Except.toOption`
-- (fine here since no test distinguishes two different `.error` payloads
-- from one another).
def isErr [BEq α] (ea : Except ε α) : Bool := ea.toOption == none
def isOk [BEq α] (ea : Except ε α) (a : α) : Bool := ea.toOption == some a

-- `throwError` produces an `.error` when run.
#guard isErr (Id.run (runExceptT (throwError "boom" : ExceptT String Id Nat)))

-- `catchError` recovers from an error via the handler.
#guard isOk (Id.run (runExceptT (catchError (throwError "boom") (fun _ => pure 42) : ExceptT String Id Nat))) 42

-- `catchError` leaves a successful computation untouched.
#guard isOk (Id.run (runExceptT (catchError (pure 7) (fun _ => pure 0) : ExceptT String Id Nat))) 7

-- `liftEither` lifts both branches of a pure `Except`.
#guard isOk (Id.run (runExceptT (liftEither (.ok 5) : ExceptT String Id Nat))) 5
#guard isErr (Id.run (runExceptT (liftEither (.error "bad") : ExceptT String Id Nat)))

-- `withExceptT` maps the error type, leaving success alone.
#guard isErr (Id.run (runExceptT (withExceptT String.length (throwError "boom") : ExceptT Nat Id Unit)))
#guard isOk (Id.run (runExceptT (withExceptT String.length (pure () : ExceptT String Id Unit) : ExceptT Nat Id Unit))) ()

-- `mapExceptT` transforms the underlying computation.
#guard isOk (Id.run (runExceptT (mapExceptT
    (fun (ea : Id (Except String Nat)) => (Except.map (· + 1) ea.run : Id (Except String Nat)))
    (pure 1 : ExceptT String Id Nat)))) 2

-- Reduction laws (checked at compile time).
example (a : Nat) : runExceptT (liftEither (.ok a) : ExceptT String Id Nat) = pure (.ok a) :=
  runExceptT_liftEither_ok a
example (e : String) : runExceptT (liftEither (.error e) : ExceptT String Id Nat) = pure (.error e) :=
  runExceptT_liftEither_error e

end Tests.Control.Monad.Except
