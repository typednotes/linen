/-
  Tests for the `Data.Fold` facade (module #33).

  Exercises the re-exported `Fold` type and combinators through the clean
  top-level `Linen.Data.Fold` import.
-/
import Linen.Data.Fold

open Data.Fold

namespace Tests.Data.Fold.Facade

/-- Feed a list through a fold, returning the final result. -/
def run [Monad m] (fld : Fold m a b) (xs : List a) : m b := do
  match ← fld.initial with
  | .Done b => pure b
  | .Partial s0 => go s0 xs
where
  go (s : fld.s) : List a → m b
    | [] => fld.final s
    | x :: rest => do
      match ← fld.step s x with
      | .Done b => pure b
      | .Partial s' => go s' rest

-- `foldl'` and `toList` reached via the facade.
#guard Id.run (run (foldl' (· + ·) 0) [1, 2, 3, 4]) == 10
#guard Id.run (run toList [1, 2, 3]) == [1, 2, 3]

end Tests.Data.Fold.Facade
