/-
  Tests for the `Data.Scanl` facade (module #34).

  Exercises the re-exported `Scanl` type and combinators through the clean
  top-level `Linen.Data.Scanl` import.
-/
import Linen.Data.Scanl

open Data.Scanl

namespace Tests.Data.Scanl.Facade

/-- Feed a list through a scan, returning the final result. -/
def run [Monad m] (sc : Scanl m a b) (xs : List a) : m b := do
  match ← sc.initial with
  | .Done b => pure b
  | .Partial s0 => go s0 xs
where
  go (s : sc.s) : List a → m b
    | [] => sc.final s
    | x :: rest => do
      match ← sc.step s x with
      | .Done b => pure b
      | .Partial s' => go s' rest

-- `mkScanl` and `mkScanlM` reached via the facade.
#guard Id.run (run (mkScanl (· + ·) 0) [1, 2, 3, 4]) == 10
#guard Id.run (run (mkScanlM (m := Id) (fun s a => pure (s * a)) (pure 1)) [1, 2, 3, 4]) == 24

end Tests.Data.Scanl.Facade
