/-
  Tests for `Data.Unfold.Enumeration` — running enumerators over `Id` with a
  fuel-bounded executor (unbounded enumerators are capped by the fuel).
-/
import Linen.Data.Unfold.Enumeration

open Data.Unfold

namespace Tests.Data.Unfold.Enumeration

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

-- `enumerateFromToIntegral` is inclusive and bounded.
#guard Id.run (run (enumerateFromToIntegral (a := Int)) (1, 5) 100) == [1, 2, 3, 4, 5]
#guard Id.run (run (enumerateFromToIntegral (a := Int)) (5, 1) 100) == []

-- `enumerateFromThenToIntegral` uses stride `next - from`, ascending & descending.
#guard Id.run (run (enumerateFromThenToIntegral (a := Int)) (1, 3, 9) 100) == [1, 3, 5, 7, 9]
#guard Id.run (run (enumerateFromThenToIntegral (a := Int)) (9, 7, 1) 100) == [9, 7, 5, 3, 1]

-- `enumerateFromStepIntegral` (unbounded) — take a prefix via the fuel.
#guard Id.run (run (enumerateFromStepIntegral (a := Int)) (0, 2) 4) == [0, 2, 4, 6]

-- `enumerateFromIntegral` (stride 1, unbounded) — prefix.
#guard Id.run (run (enumerateFromIntegral (a := Int)) 10 3) == [10, 11, 12]

-- `enumerateFromThenIntegral` (stride from `next - from`) — prefix.
#guard Id.run (run (enumerateFromThenIntegral (a := Int)) (0, 5) 3) == [0, 5, 10]

-- `enumerateFromStepNum` yields `from + i*stride` — prefix.
#guard Id.run (run (enumerateFromStepNum (a := Int)) (100, 10) 3) == [100, 110, 120]

-- `enumerateFromNum` (stride 1) — prefix.
#guard Id.run (run (enumerateFromNum (a := Int)) 7 3) == [7, 8, 9]

end Tests.Data.Unfold.Enumeration
