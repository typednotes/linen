/-
  Tests for `Data.Scanl.Type` — driving scans over `Id` to their final result,
  honouring early `Done`.
-/
import Linen.Data.Scanl.Type

open Data.Scanl

namespace Tests.Data.Scanl

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

-- `mkScanl` is a running left fold.
#guard Id.run (run (mkScanl (· + ·) 0) [1, 2, 3, 4]) == 10

-- `mkScanlM` with a monadic step (over `Id`).
#guard Id.run (run (mkScanlM (m := Id) (fun s a => pure (s * a)) (pure 1)) [1, 2, 3, 4]) == 24

-- `Functor` maps the output.
#guard Id.run (run ((· + 1) <$> mkScanl (· + ·) 0) [1, 2, 3]) == 7

-- `mkScanl1` uses the first element as the seed; empty ⇒ `none`.
#guard Id.run (run (mkScanl1 (m := Id) (· + ·)) [10, 20, 30]) == some 60
#guard Id.run (run (mkScanl1 (m := Id) (· + ·)) ([] : List Nat)) == none

-- `mkScanr` folds from the right with a seed.
#guard Id.run (run (mkScanr (· + ·) 100) [1, 2, 3]) == 106

-- `rmapM` maps the output monadically.
#guard Id.run (run (rmapM (m := Id) (fun x => pure (x * 2)) (mkScanl (· + ·) 0)) [1, 2, 3]) == 12

-- `lmap` maps the input.
#guard Id.run (run (lmap (· * 2) (mkScanl (· + ·) 0)) [1, 2, 3]) == 12

-- `filter` drops elements failing the predicate.
#guard Id.run (run (filter (· % 2 == 0) (mkScanl (· + ·) 0)) [1, 2, 3, 4]) == 6

-- `take 2` folds at most two inputs.
#guard Id.run (run (take 2 (mkScanl (· + ·) 0)) [1, 2, 3, 4]) == 3

-- `postscanl` chains two scans (running sum, then running sum of that).
#guard Id.run (run (postscanl (mkScanl (· + ·) 0) (mkScanl (· + ·) 0)) [1, 2, 3]) == 10

-- `drain` discards everything.
#guard Id.run (run drain [1, 2, 3]) == ⟨⟩

end Tests.Data.Scanl
