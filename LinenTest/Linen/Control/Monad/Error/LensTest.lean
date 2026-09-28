/-
  Tests for `Linen.Control.Monad.Error.Lens`.

  Lean core ships no generic `DecidableEq (Except ε α)` instance, so every
  equality below is between concrete `Except MyError _` terms and is checked
  with `example ... := rfl` (definitional equality) rather than `#guard`.
-/
import Linen.Control.Monad.Error.Lens
import Linen.Control.Lens.Prism

open Control.Lens Control.Monad.Error.Lens

namespace Tests.Linen.Control.Monad.Error.Lens

/-- A small sum-of-errors type, standing in for a real application's error
    type: two constructors, one of which (`notFound`) carries a payload. -/
inductive MyError
  | notFound (id : Nat)
  | other (msg : String)
  deriving DecidableEq, Repr

/-- `_NotFound :: Prism' MyError Nat`: focus on the `notFound` case. -/
def _NotFound : Prism' MyError Nat :=
  prism' MyError.notFound (fun e => match e with
    | .notFound n => some n
    | .other _ => none)

-- `throwing`/`throwing_` build an error value from the prism's constructor
-- and throw it via `MonadExcept.throw`, here specialized to `Except`.

example : (throwing _NotFound 5 : Except MyError Nat) = Except.error (MyError.notFound 5) := rfl

def _Other : Prism' MyError Unit :=
  prism' (fun _ => MyError.other "") (fun e => match e with
    | .other _ => some ()
    | .notFound _ => none)

example : (throwing_ _Other : Except MyError Nat) = Except.error (MyError.other "") := rfl

-- `catching`: a recognised error is handed to the handler; unrecognised
-- errors (and successes) pass through unchanged.

example : catching _NotFound (throw (MyError.notFound 5) : Except MyError Nat)
    (fun n => pure (n + 1)) = Except.ok 6 := rfl

example : catching _NotFound (throw (MyError.other "boom") : Except MyError Nat)
    (fun n => pure (n + 1)) = Except.error (MyError.other "boom") := rfl

example : catching _NotFound (pure 10 : Except MyError Nat) (fun n => pure (n + 1)) = Except.ok 10 := rfl

-- `catching_`: like `catching`, ignoring the recovered payload.

example : catching_ _NotFound (throw (MyError.notFound 5) : Except MyError Nat) (pure 0) = Except.ok 0 := rfl

-- `handling`/`handling_`: `catching`/`catching_` with the handler and action
-- arguments flipped.

example : handling _NotFound (fun n : Nat => pure (n + 1)) (throw (MyError.notFound 5) : Except MyError Nat)
    = Except.ok 6 := rfl

example : handling_ _NotFound (pure 0) (throw (MyError.notFound 5) : Except MyError Nat) = Except.ok 0 := rfl

-- `trying`: recognised errors become `Except.error`, successes become
-- `Except.ok`, wrapped in an outer `Except.ok` (the action itself does not
-- throw once caught).

example : trying _NotFound (throw (MyError.notFound 5) : Except MyError Nat) = Except.ok (Except.error 5) := rfl
example : trying _NotFound (pure 10 : Except MyError Nat) = Except.ok (Except.ok 10) := rfl

end Tests.Linen.Control.Monad.Error.Lens
