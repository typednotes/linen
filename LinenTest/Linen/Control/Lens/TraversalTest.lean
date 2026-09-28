/-
  Tests for `Linen.Control.Lens.Traversal`.
-/
import Linen.Control.Lens.Traversal
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Setter

open Control.Lens

namespace Tests.Linen.Control.Lens.Traversal

-- `traverseOf` / `forOf` / `sequenceAOf`, run at `Option` (the "may fail"
-- applicative).
#guard traverseOf traversed (fun n => if n > 0 then some (n + 1) else none) [1, 2, 3]
  = some [2, 3, 4]
#guard traverseOf traversed (fun n => if n > 0 then some (n + 1) else none) [1, 0, 3] = none
#guard forOf traversed [1, 2, 3] (fun n => some (n + 1)) = some [2, 3, 4]
#guard sequenceAOf traversed ([some 1, some 2, some 3] : List (Option Nat)) = some [1, 2, 3]
#guard sequenceAOf traversed ([some 1, none, some 3] : List (Option Nat)) = none

-- `mapAccumLOf`: running sum, left to right.
#guard mapAccumLOf traversed (fun acc a => (acc + a, acc + a)) 0 [1, 2, 3] = (6, [1, 3, 6])

-- `scanl1Of`: running max.
#guard scanl1Of traversed max [3, 1, 4, 1, 5] = [3, 3, 4, 4, 5]

-- `failover`: `Option` as the target `Alternative`.
#guard (failover traversed (· + 1) [1, 2, 3] : Option (List Nat)) = some [2, 3, 4]
#guard (failover traversed (· + 1) ([] : List Nat) : Option (List Nat)) = none

-- `both`: over `Prod`, run at `Option` (the "may fail" applicative).
#guard traverseOf both (fun n => if n > 0 then some (n + 1) else none) ((1, 2) : Nat × Nat)
  = some (2, 3)
#guard traverseOf both (fun n => if n > 0 then some (n + 1) else none) ((0, 2) : Nat × Nat) = none

-- `traversed` on `Option`.
#guard traverseOf traversed (fun n => some (n + 1)) (some 1 : Option Nat) = some (some 2)

-- `ignored`: touches nothing.
#guard over ignored (· + (1 : Nat)) (5 : Nat) = 5
#guard toListOf ignored (5 : Nat) = ([] : List Nat)

end Tests.Linen.Control.Lens.Traversal
