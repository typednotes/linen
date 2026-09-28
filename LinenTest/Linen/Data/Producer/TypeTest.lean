/-
  Tests for `Data.Producer.Type` — running producers over `Id` with a
  fuel-bounded executor.
-/
import Linen.Data.Producer.Type

open Data.Producer

namespace Tests.Data.Producer

/-- Run a producer from a seed, collecting up to `fuel` steps. -/
def run [Monad m] (p : Producer m a b) (seed : a) (fuel : Nat) : m (List b) := do
  go (← p.inject seed) fuel
where
  go (s : p.s) : Nat → m (List b)
    | 0 => pure []
    | n + 1 => do
      match ← p.step s with
      | .Yield x s' => (x :: ·) <$> go s' n
      | .Skip s' => go s' n
      | .Stop => pure []

-- `fromList` generates the list.
#guard Id.run (run fromList [1, 2, 3] 100) == [1, 2, 3]

-- `nil` is empty.
#guard Id.run (run (nil : Producer Id Nat Nat) 0 100) == []

-- `unfoldrM` (finite, over `Id`).
#guard Id.run (run (unfoldrM (m := Id) (fun n => pure (if n > 3 then none else some (n, n + 1)))) 1 100) == [1, 2, 3]

-- `map` on output.
#guard Id.run (run (map (· + 10) fromList) [1, 2] 100) == [11, 12]

-- `lmap` / `translate` on the seed.
#guard Id.run (run (lmap (fun xs => 0 :: xs) fromList) [1, 2] 100) == [0, 1, 2]
#guard Id.run (run (translate (fun (n : Nat) => List.range n) (·.length) fromList) 3 100) == [0, 1, 2]

-- `concat` flattens a nested loop: outer produces lists, inner produces elements.
#guard Id.run (run (concat fromList fromList) (.outerLoop [[1, 2], [3], [4, 5]]) 100) == [1, 2, 3, 4, 5]

-- `extract` recovers the residual seed of `fromList` (the unconsumed tail).
#guard Id.run (do
  let p : Producer Id (List Nat) Nat := fromList
  let s0 ← p.inject [1, 2, 3]
  match ← p.step s0 with
  | .Yield _ s1 => p.extract s1
  | _ => pure []) == [2, 3]

end Tests.Data.Producer
