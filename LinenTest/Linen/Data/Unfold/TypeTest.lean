/-
  Tests for `Data.Unfold.Type` — running unfolds over `Id` with a fuel-bounded
  executor (structural on the fuel, so total; the bound also caps otherwise
  unbounded unfolds).
-/
import Linen.Data.Unfold.Type

open Data.Unfold

namespace Tests.Data.Unfold

/-- Run an unfold from a seed, collecting up to `fuel` steps. -/
def run [Monad m] (u : Unfold m a b) (seed : a) (fuel : Nat) : m (List b) := do
  go (← u.inject seed) fuel
where
  go (s : u.s) : Nat → m (List b)
    | 0 => pure []
    | n + 1 => do
      match ← u.step s with
      | .Yield x s' => (x :: ·) <$> go s' n
      | .Skip s' => go s' n
      | .Stop => pure []

-- `fromList` generates the list.
#guard Id.run (run fromList [1, 2, 3] 100) == [1, 2, 3]

-- `unfoldr` (finite).
#guard Id.run (run (unfoldr (fun n => if n > 3 then none else some (n, n + 1))) 1 100) == [1, 2, 3]

-- `function` / `identity` generate singletons.
#guard Id.run (run (function (· * 2)) 5 100) == [10]
#guard Id.run (run identity 7 100) == [7]

-- `fromPure` ignores the seed.
#guard Id.run (run (fromPure 99) 0 100) == [99]

-- `fromTuple`.
#guard Id.run (run fromTuple (1, 2) 100) == [1, 2]

-- `map` on output, `lmap` on input.
#guard Id.run (run (map (· + 100) fromList) [1, 2] 100) == [101, 102]
#guard Id.run (run (lmap (fun n => List.range n) fromList) 3 100) == [0, 1, 2]

-- `mapM` (over `Id`).
#guard Id.run (run (mapM (m := Id) (fun x => pure (x * 10)) fromList) [1, 2] 100) == [10, 20]

-- `takeWhile`.
#guard Id.run (run (takeWhile (· < 3) fromList) [1, 2, 3, 4] 100) == [1, 2]

-- `cross` product of two unfolds sharing the same seed.
#guard Id.run (run (cross (lmap Prod.fst fromList) (lmap Prod.snd fromList)) ([1, 2], [10, 20]) 100)
        == [(1, 10), (1, 20), (2, 10), (2, 20)]

end Tests.Data.Unfold
