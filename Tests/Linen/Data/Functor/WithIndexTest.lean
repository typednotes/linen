/-
  Tests for `Linen.Data.Functor.WithIndex`.

  `mapWithIndex` across the `List`, `Option`, `Prod`, `Std.HashMap`,
  `Data.Map`, `Data.IntMap`, and `List.NonEmpty` instances.
-/
import Linen.Data.Functor.WithIndex

open Data
open Data.Functor.WithIndex
open Data.List (NonEmpty)

namespace Tests.Data.Functor.WithIndex

/-! ### List (index : Nat) -/

#guard mapWithIndex (fun i a => (i, a)) ["a", "b", "c"]
        == [(0, "a"), (1, "b"), (2, "c")]
#guard mapWithIndex (fun i a => i + a) ([] : List Nat) == []

/-! ### Option (index : Unit) -/

#guard mapWithIndex (fun () a => a + 1) (some 4) == some 5
#guard mapWithIndex (fun () a => a + 1) (none : Option Nat) == none

/-! ### Prod (index : fixed first component) -/

#guard mapWithIndex (fun k a => k ++ a) (("x", "y") : String × String) == ("x", "xy")

/-! ### Std.HashMap (index : keys) -/

#guard (Std.HashMap.toList (mapWithIndex (f := fun v => Std.HashMap Nat v) (fun k v => k + v)
          (Std.HashMap.ofList [(1, 10), (2, 20)]))).length == 2
#guard Std.HashMap.get? (mapWithIndex (f := fun v => Std.HashMap Nat v) (fun k v => k + v)
          (Std.HashMap.ofList [(1, 10)])) 1 == some 11

/-! ### Data.Map (index : keys, ascending) -/

#guard Map.toList' (mapWithIndex (f := fun v => Map Nat v) (fun k v => k + v)
          (Map.fromList [(1, 10), (2, 20), (3, 30)]))
        == [(1, 11), (2, 22), (3, 33)]

/-! ### Data.IntMap (index : keys) — the `Std.HashMap Nat` case, not a separate instance -/

#guard IntMap.lookup 2 (mapWithIndex (fun k v => k + v) (IntMap.fromList [(1, 10), (2, 20)]))
        == some 22

/-! ### List.NonEmpty (index : Nat, head at 0) -/

#guard mapWithIndex (fun i a => (i, a)) (⟨"a", ["b", "c"]⟩ : NonEmpty String)
        == ⟨(0, "a"), [(1, "b"), (2, "c")]⟩

end Tests.Data.Functor.WithIndex
