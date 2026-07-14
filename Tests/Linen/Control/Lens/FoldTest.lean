/-
  Tests for `Linen.Control.Lens.Fold`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Setter

open Control.Lens

namespace Tests.Linen.Control.Lens.Fold

structure Point where
  x : Nat
  y : Nat
deriving Repr, BEq

/-- `each`-style `Fold` over a `List`, built with `folding id` (i.e. `folded`). -/
def eachL : Fold (List Nat) Nat := folded

#guard toListOf eachL [1, 2, 3] = [1, 2, 3]
#guard ([1, 2, 3] : List Nat) ^.. eachL = [1, 2, 3]

-- `folding` builds a `Fold` from an arbitrary `Foldable`-producing function.
def digits : Fold Nat Nat := folding (fun n => [n / 10, n % 10])

#guard toListOf digits 42 = [4, 2]

-- `preview` / `(^?)` recover the first focused value, if any.
#guard preview eachL [1, 2, 3] = some 1
#guard preview eachL ([] : List Nat) = none
#guard (([1, 2, 3] : List Nat) ^? eachL) = some 1

-- `has` / `hasn't`.
#guard has eachL [1, 2, 3] = true
#guard has eachL ([] : List Nat) = false
#guard hasn't eachL ([] : List Nat) = true
#guard nullOf eachL ([] : List Nat) = true
#guard notNullOf eachL [1] = true

-- `foldOf` on a single-focus `Getting` is `view`.
def xG : Getting Nat Point Nat := to Point.x
#guard foldOf xG ⟨3, 4⟩ = 3

-- `foldrOf` / `foldlOf`.
#guard foldrOf eachL (· :: ·) [] [1, 2, 3] = [1, 2, 3]
#guard foldlOf eachL (fun acc a => a :: acc) [] [1, 2, 3] = [3, 2, 1]

-- `anyOf` / `allOf` / `andOf` / `orOf`.
def eachB : Fold (List Bool) Bool := folded

#guard anyOf eachL (· > 2) [1, 2, 3] = true
#guard allOf eachL (· > 0) [1, 2, 3] = true
#guard andOf eachB [true, true, true] = true
#guard andOf eachB [true, false] = false
#guard orOf eachB [false, false, true] = true

-- `elemOf` / `lengthOf`.
#guard elemOf eachL 2 [1, 2, 3] = true
#guard elemOf eachL 5 [1, 2, 3] = false
#guard lengthOf eachL [1, 2, 3] = 3

-- `firstOf` / `lastOf`.
#guard firstOf eachL [1, 2, 3] = some 1
#guard lastOf eachL [1, 2, 3] = some 3
#guard firstOf eachL ([] : List Nat) = none

-- `sumOf` / `productOf`.
#guard sumOf eachL [1, 2, 3] = 6
#guard productOf eachL [1, 2, 3] = 6

-- `minimumOf` / `maximumOf`.
#guard minimumOf eachL [3, 1, 2] = some 1
#guard maximumOf eachL [3, 1, 2] = some 3

-- `findOf`.
#guard findOf eachL (· > 1) [1, 2, 3] = some 2
#guard findOf eachL (· > 5) [1, 2, 3] = none

-- `foldByOf` / `foldMapByOf`.
#guard foldByOf eachL (· + ·) 0 [1, 2, 3] = 6
#guard foldMapByOf eachL (· ++ ·) "" toString [1, 2, 3] = "123"

-- `filtered`: an affine `Traversal'` that only touches values matching a predicate.
#guard toListOf (eachL ∘ filtered (· % 2 == 0)) ([1, 2, 3, 4] : List Nat) = [2, 4]
#guard over (filtered (· % 2 == 0)) (· + 100) (4 : Nat) = 104
#guard over (filtered (· % 2 == 0)) (· + 100) (3 : Nat) = 3

end Tests.Linen.Control.Lens.Fold
