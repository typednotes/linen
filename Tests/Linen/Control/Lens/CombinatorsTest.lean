/-
  Tests for `Linen.Control.Lens.Combinators` (the facade re-exporting every
  already-ported `Control.Lens.*` module).

  Since the facade's entire content is its `import` list, these `#guard`s
  just check a small, representative handful of names from across the whole
  re-exported family resolve through a bare `import Linen.Control.Lens.
  Combinators` alone, with no explicit import of the underlying module. -/
import Linen.Control.Lens.Combinators

open Control.Lens

namespace Tests.Linen.Control.Lens.Combinators

-- `Getter.view`, via `Lens.lens`.
def fstL : Lens' (Nat × Nat) Nat := lens Prod.fst (fun s v => (v, s.2))
#guard view fstL (3, 4) == 3

-- `Setter.set`/`over`.
#guard set fstL 9 (3, 4) == (9, 4)
#guard over fstL (· + 1) (3, 4) == (4, 4)

-- `Prism._Just`, via `Prism.withPrism`/`review`.
#guard withPrism (_Just (A := Nat) (B := Nat)) (fun bt _ => bt 5) == some (5 : Nat)
#guard review (_Just (A := Nat) (B := Nat)) 5 == some (5 : Nat)

-- `Each.each`, via `Fold.toListOf`.
#guard toListOf each ([1, 2, 3] : List Nat) = [1, 2, 3]

-- `Iso.iso`/`withIso` — confirming both resolve through the same bare
-- facade import.
def notIso : Iso' Bool Bool := iso not not
#guard withIso notIso (fun sa _ => sa true) == false

end Tests.Linen.Control.Lens.Combinators
