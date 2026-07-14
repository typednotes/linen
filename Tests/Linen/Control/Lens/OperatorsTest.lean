/-
  Tests for `Linen.Control.Lens.Operators` (the operator-only facade).

  As with `CombinatorsTest`, these `#guard`s just exercise a representative
  handful of the infix operators this facade is nominally "for" (see its own
  doc comment), confirming they resolve through a bare `import Linen.Control.
  Lens.Operators` with no explicit import of the module each operator is
  actually declared in. -/
import Linen.Control.Lens.Operators

open Control.Lens

namespace Tests.Linen.Control.Lens.Operators

def fstL : Lens' (Nat × Nat) Nat := lens Prod.fst (fun s v => (v, s.2))

-- `(^.)` — `Getter`.
#guard (3, 4) ^. fstL == 3

-- `(.~)`/`(%~)` — `Setter`.
#guard ((3, 4) |> fstL .~ 9) == (9, 4)
#guard ((3, 4) |> fstL %~ (· + 1)) == (4, 4)

-- `(^..)` — `Fold`, via `folded`.
def eachL : Fold (List Nat) Nat := folded
#guard ([1, 2, 3] : List Nat) ^.. eachL = [1, 2, 3]

-- `(^?)` — `Fold`, via `At.ix`.
#guard (some 5 : Option Nat) ^? ix () == some (5 : Nat)

-- `(#)` — `Review`.
#guard ((_Just (A := Nat)) # 7) = some (7 : Nat)

end Tests.Linen.Control.Lens.Operators
