/-
  Tests for `Linen.Control.Profunctor.Mapping`.

  `Mapping` over `Control.Fun`: `roam` and `map'`.
-/
import Linen.Control.Profunctor.Mapping

open Control Control.Profunctor

namespace Tests.Control.Profunctor.Mapping

def inc : Fun Nat Nat := ⟨(· + 1)⟩

#guard (Mapping.roam (fun (ab : Nat → Nat) (l : List Nat) => l.map ab) inc).apply [1, 2, 3] ==
  [2, 3, 4]
#guard (Mapping.map' (F := List) inc).apply [1, 2, 3] == [2, 3, 4]
#guard (Mapping.map' (F := Option) inc).apply (some 5) == some 6

end Tests.Control.Profunctor.Mapping
