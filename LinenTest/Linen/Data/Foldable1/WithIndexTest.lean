/-
  Tests for `Linen.Data.Foldable1.WithIndex`.

  `foldMap1WithIndex` on `List.NonEmpty` — the one container in `linen`
  statically known to be non-empty.
-/
import Linen.Data.Foldable1.WithIndex

open Data
open Data.Foldable1.WithIndex
open Data.List (NonEmpty)

namespace Tests.Data.Foldable1.WithIndex

/-! ### List.NonEmpty (index : Nat, head at 0) -/

#guard foldMap1WithIndex (fun i a => [(i, a)]) (⟨"a", ["b", "c"]⟩ : NonEmpty String)
        == [(0, "a"), (1, "b"), (2, "c")]

-- A singleton non-empty list: the fold is just the head, no `Inhabited`/identity needed.
#guard foldMap1WithIndex (fun i a => [(i, a)]) (⟨"only", []⟩ : NonEmpty String) == [(0, "only")]

end Tests.Data.Foldable1.WithIndex
