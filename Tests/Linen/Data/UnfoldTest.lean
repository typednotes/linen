/-
  Tests for the `Data.Unfold` facade (module #35).

  Exercises the re-exported `Unfold` type together with a `Type` combinator
  (`fromList`) and an `Enumeration` combinator (`enumerateFromToIntegral`),
  reached through the clean top-level `Linen.Data.Unfold` import.
-/
import Linen.Data.Unfold

open Data.Unfold

namespace Tests.Data.Unfold.Facade

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

-- `fromList` (from `Unfold.Type`, #15).
#guard Id.run (run fromList [1, 2, 3] 100) == [1, 2, 3]

-- `enumerateFromToIntegral` (from `Unfold.Enumeration`, #16).
#guard Id.run (run (enumerateFromToIntegral (a := Nat)) (1, 5) 100) == [1, 2, 3, 4, 5]

end Tests.Data.Unfold.Facade
