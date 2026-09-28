/-
  Tests for `Data.Producer` — the `Producer`→`Unfold` bridge.
-/
import Linen.Data.Producer
import Linen.Data.Unfold.Type

open Data.Producer
open Data.Unfold (Unfold)

namespace Tests.Data.Producer.Facade

/-- Run an unfold from a seed, collecting up to `fuel` steps. -/
def runU [Monad m] (u : Unfold m a b) (seed : a) (fuel : Nat) : m (List b) := do
  go (← u.inject seed) fuel
where
  go (s : u.s) : Nat → m (List b)
    | 0 => pure []
    | n + 1 => do
      match ← u.step s with
      | .Yield x s' => (x :: ·) <$> go s' n
      | .Skip s' => go s' n
      | .Stop => pure []

-- `simplify` forgets the seed-extract, giving an equivalent `Unfold`.
#guard Id.run (runU (simplify (Data.Producer.fromList : Producer Id (List Nat) Nat)) [1, 2, 3] 100) == [1, 2, 3]

end Tests.Data.Producer.Facade
