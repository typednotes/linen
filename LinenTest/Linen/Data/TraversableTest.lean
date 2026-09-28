/-
  Tests for `Linen.Data.Traversable` — `traverse`/`sequence` over `List`,
  `Option`, and `List.NonEmpty`, with the identity law on concrete values.
-/
import Linen.Data.Traversable

open Data

namespace Tests.Data.Traversable

/-- An `Option`-valued effect: keep positives, fail on non-positives. -/
private def pos? (n : Int) : Option Int := if n > 0 then some n else none

/-! ### traverse over List (Option effect) -/

#guard Traversable.traverse pos? [1, 2, 3] == some [1, 2, 3]
#guard Traversable.traverse pos? [1, -2, 3] == none
#guard Traversable.traverse pos? ([] : List Int) == some []

/-! ### traverse over Option -/

#guard Traversable.traverse pos? (some 5) == some (some 5)
#guard Traversable.traverse pos? (some (-5)) == none
#guard Traversable.traverse pos? (none : Option Int) == some none

/-! ### sequence (= traverse id) -/

#guard Traversable.sequence [some 1, some 2, some 3] == some [1, 2, 3]
#guard Traversable.sequence [some 1, none, some 3] == none
#guard (Traversable.sequence (some (some 7)) : Option (Option Nat)) == some (some 7)

/-! ### traverse over List.NonEmpty -/

#guard Traversable.traverse pos? (⟨1, [2, 3]⟩ : List.NonEmpty Int) == some ⟨1, [2, 3]⟩
#guard (Traversable.traverse pos? (⟨1, [-2, 3]⟩ : List.NonEmpty Int)).isNone

/-! ### identity law, `traverse pure = pure`, on concrete values -/

example : Traversable.traverse (G := Id) (pure : Nat → Id Nat) [1, 2, 3] = [1, 2, 3] := rfl
example : Traversable.traverse (G := Id) (pure : Nat → Id Nat) (some 9) = some 9 := rfl
-- and the general law for `Option` via the lawful instance
example (o : Option Nat) :
    Traversable.traverse (G := Id) (pure : Nat → Id Nat) o = pure o :=
  LawfulTraversable.traverse_identity o

end Tests.Data.Traversable
