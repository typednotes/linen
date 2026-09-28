/-
  Tests for `Data.Refold.Type` — driving `Refold`s over `Id` with a small
  list-feeding runner that honours early `Done` termination.
-/
import Linen.Data.Refold.Type

open Data.Refold

namespace Tests.Data.Refold

/-- Feed a seed and a list of inputs through a `Refold`, stopping early on
    `Done`. -/
def runR [Monad m] (r : Refold m c a b) (seed : c) (xs : List a) : m b := do
  match ← r.inject seed with
  | .Done b => pure b
  | .Partial s0 => go s0 xs
where
  go (s : r.s) : List a → m b
    | [] => r.extract s
    | x :: rest => do
      match ← r.step s x with
      | .Done b => pure b
      | .Partial s' => go s' rest

-- `foldl'` is a pure left fold seeded by the supplied value.
#guard Id.run (runR (foldl' (· + ·)) 100 [1, 2, 3]) == 106

-- `lmapM` maps a (pure, over `Id`) function on the input.
#guard Id.run (runR (lmapM (m := Id) (fun x => pure (x * 2)) (foldl' (· + ·))) 0 [1, 2, 3]) == 12

-- `rmapM` maps a function on the output.
#guard Id.run (runR (rmapM (m := Id) (fun x => pure (x + 1)) (foldl' (· + ·))) 0 [1, 2, 3]) == 7

-- `sconcat` appends inputs to the seed (over `List` with `Append`).
#guard Id.run (runR sconcat [0] [[1], [2], [3]]) == [0, 1, 2, 3]

-- `take 2` folds at most two inputs then is `Done`.
#guard Id.run (runR (take 2 (foldl' (· + ·))) 0 [10, 20, 30, 40]) == 30

-- `take 0` immediately extracts the seed.
#guard Id.run (runR (take 0 (foldl' (· + ·))) 7 [10, 20]) == 7

-- `append`: first refold sums two inputs (via `take 2`), feeding the result as
-- the seed of a second summing refold.
#guard Id.run (runR (append (take 2 (foldl' (· + ·))) (foldl' (· + ·))) 0 [1, 2, 100, 200]) == 303

end Tests.Data.Refold
