/-
  Tests for `Linen.Control.Lens.Internal.Magma`.

  `Magma`/`Molten`: `run`/`mapLeaves`/`foldrWithIndex`/`toListWithIndex`/
  `traverseWithIndex`, plus `These`/`Swap`/`Assoc`.
-/
import Linen.Control.Lens.Internal.Magma

open Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Magma

/-- `[1, 2, 3]`'s shape as a `Magma`, tagged with a list index. -/
def tree : Magma Nat Nat Nat Nat :=
  .ap (.ap (.fmap (· + · + ·) (.leaf 0 1)) (.leaf 1 2)) (.leaf 2 3)

#guard Magma.run tree == 6

#guard Magma.toListWithIndex tree == [(0, 1), (1, 2), (2, 3)]

#guard Magma.run (Magma.mapLeaves (· * 10) tree) == 60

#guard Magma.foldrWithIndex (fun _ a acc => a + acc) tree 0 == 6

#guard (Magma.traverseWithIndex (F := Option)
  (fun _ a => some (ULift.up (a + 100))) tree).map Magma.run == some 306

def m : Molten Nat Nat Nat Nat := Molten.mk tree
#guard Magma.run (Molten.runMolten m) == 6

/-! ### These / Swap / Assoc -/

#guard (Swap.swap (5, "x") : String × Nat) == ("x", 5)
example : (Swap.swap (These.this 5 : These Nat String)) = .that 5 := rfl
example : (Swap.swap (These.both 5 "x" : These Nat String)) = .both "x" 5 := rfl
#guard (Assoc.assoc ((5, "x"), true) : Nat × (String × Bool)) == (5, ("x", true))

end Tests.Control.Lens.Internal.Magma
