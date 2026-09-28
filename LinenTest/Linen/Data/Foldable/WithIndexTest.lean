/-
  Tests for `Linen.Data.Foldable.WithIndex`.

  `foldrWithIndex` and the derived `foldMapWithIndex`, across the `List`,
  `Option`, `Prod`, `Std.HashMap`, `Data.Map`, `Data.IntMap`, and
  `List.NonEmpty` instances.
-/
import Linen.Data.Foldable.WithIndex

open Data
open Data.Foldable.WithIndex
open Data.List (NonEmpty)

namespace Tests.Data.Foldable.WithIndex

/-! ### List (index : Nat) -/

#guard foldrWithIndex (fun i a acc => (i, a) :: acc) [] ["a", "b", "c"]
        == [(0, "a"), (1, "b"), (2, "c")]
#guard foldMapWithIndex (fun i a => [(i, a)]) ["a", "b"] == [(0, "a"), (1, "b")]

/-! ### Option (index : Unit) -/

#guard foldrWithIndex (fun () a acc => a + acc) 0 (some 4) == 4
#guard foldrWithIndex (fun () a acc => a + acc) 0 (none : Option Nat) == 0

/-! ### Prod (index : fixed first component) -/

#guard foldrWithIndex (fun k a acc => k ++ a ++ acc) "" (("x", "y") : String × String) == "xy"

/-! ### Std.HashMap (index : keys; order unspecified, so sum/length are order-independent) -/

#guard (foldMapWithIndex (f := fun v => Std.HashMap Nat v) (fun k v => [k + v])
          (Std.HashMap.ofList [(1, 10), (2, 20)]) |>.foldl (· + ·) 0) == 33

/-! ### Data.Map (index : keys, ascending) -/

#guard foldrWithIndex (f := fun v => Map Nat v) (fun k _ acc => k :: acc) []
          (Map.fromList [(1, 10), (2, 20), (3, 30)]) == [1, 2, 3]
#guard foldMapWithIndex (f := fun v => Map Nat v) (fun k v => [k + v])
          (Map.fromList [(1, 10), (2, 20)]) == [11, 22]

/-! ### Data.IntMap (index : keys) — the `Std.HashMap Nat` case, not a separate instance -/

#guard (foldMapWithIndex (fun k v => [k + v]) (IntMap.fromList [(1, 10), (2, 20)])
        |>.foldl (· + ·) 0) == 33

/-! ### List.NonEmpty (index : Nat, head at 0) -/

#guard foldrWithIndex (fun i a acc => (i, a) :: acc) [] (⟨"a", ["b", "c"]⟩ : NonEmpty String)
        == [(0, "a"), (1, "b"), (2, "c")]
#guard foldMapWithIndex (fun i a => [(i, a)]) (⟨"a", ["b"]⟩ : NonEmpty String)
        == [(0, "a"), (1, "b")]

end Tests.Data.Foldable.WithIndex
