/-
  Tests for `Linen.Data.Traversable.WithIndex`.

  `traverseWithIndex` across the `List`, `Option`, `Prod`, `Std.HashMap`,
  `Data.Map`, `Data.IntMap`, and `List.NonEmpty` instances, using `Option`
  and `Id` as the traversed-into applicative.
-/
import Linen.Data.Traversable.WithIndex

open Data
open Data.Traversable.WithIndex
open Data.List (NonEmpty)

namespace Tests.Data.Traversable.WithIndex

/-! ### List (index : Nat) -/

example : traverseWithIndex (G := Id) (fun i a => i + a) [10, 20, 30] = [10, 21, 32] := rfl
#guard traverseWithIndex (fun i a => if a > i then some a else none) [1, 2, 3] == some [1, 2, 3]
#guard traverseWithIndex (fun i a => if a > i then some a else none) [1, 0, 3] == none

/-! ### Option (index : Unit) -/

example : traverseWithIndex (G := Id) (fun () a => a + 1) (some 4) = some 5 := rfl
example : traverseWithIndex (G := Id) (fun () a => a + 1) (none : Option Nat) = none := rfl

/-! ### Prod (index : fixed first component) -/

example : traverseWithIndex (G := Id) (fun k a => k ++ a) (("x", "y") : String × String)
        = ("x", "xy") := rfl

/-! ### Std.HashMap (index : keys) -/

#guard Std.HashMap.get?
          (traverseWithIndex (G := Id) (t := fun v => Std.HashMap Nat v) (fun k v => k + v)
            (Std.HashMap.ofList [(1, 10)])) 1
        == some 11

/-! ### Data.Map (index : keys, ascending) -/

#guard Map.toList' (traverseWithIndex (G := Id) (t := fun v => Map Nat v) (fun k v => k + v)
          (Map.fromList [(1, 10), (2, 20), (3, 30)]))
        == [(1, 11), (2, 22), (3, 33)]

/-! ### Data.IntMap (index : keys) — the `Std.HashMap Nat` case, not a separate instance -/

#guard IntMap.lookup 2 (traverseWithIndex (G := Id) (fun k v => k + v)
          (IntMap.fromList [(1, 10), (2, 20)]))
        == some 22

/-! ### List.NonEmpty (index : Nat, head at 0) -/

example : traverseWithIndex (G := Id) (fun i a => (i, a)) (⟨"a", ["b", "c"]⟩ : NonEmpty String)
        = ⟨(0, "a"), [(1, "b"), (2, "c")]⟩ := rfl

end Tests.Data.Traversable.WithIndex
