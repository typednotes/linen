/-
  Tests for `Linen.Control.Profunctor` (the facade).

  A smoke test confirming the facade re-exports `Profunctor`/`Strong`/
  `Choice`/`Closed`/`Mapping` and their concrete `Types` instances all at
  once, without needing to import any sub-module directly.
-/
import Linen.Control.Profunctor

open Control Control.Profunctor

namespace Tests.Control.Profunctor

def inc : Fun Nat Nat := ⟨(· + 1)⟩

#guard (Profunctor.rmap (· + 1) inc).apply 5 == 7
#guard (Strong.first' inc).apply (5, "x") == (6, "x")
#guard (match (Choice.left' inc).apply (Sum.inl 5 : Nat ⊕ String) with
        | .inl n => n == 6 | .inr _ => false)
#guard (Closed.closed (X := Bool) inc).apply (fun _ => 5) true == 6
#guard (Mapping.map' (F := List) inc).apply [1, 2, 3] == [2, 3, 4]

end Tests.Control.Profunctor
