/-
  Tests for `Linen.Control.Lens.Internal.Level`.

  `Level`: `size`/`lappend`/`toListWithIndex`/`map`/`foldrWithIndex`/
  `traverseWithIndex`, plus the `Functor` instance.
-/
import Linen.Control.Lens.Internal.Level

open Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Level

def l0 : Level Nat String := .zero
def l1 : Level Nat String := .one 0 "a"
def l2 : Level Nat String := .one 1 "b"
def l3 : Level Nat String := Level.lappend l1 l2

#guard l0.size == 0
#guard l1.size == 1
#guard l3.size == 2

#guard l3.toListWithIndex == [(0, "a"), (1, "b")]

/-! ### `lappend` with `zero` on either side is the identity -/

example : Level.lappend (.zero) l1 = l1 := rfl
example : Level.lappend l1 (.zero) = l1 := rfl

/-! ### `map` -/

#guard (l3.map String.length).toListWithIndex == [(0, 1), (1, 1)]

/-! ### `foldrWithIndex` -/

#guard l3.foldrWithIndex (fun i a acc => (i, a.length) :: acc) [] == [(0, 1), (1, 1)]

/-! ### `traverseWithIndex` -/

#guard (l3.traverseWithIndex (F := Option) (fun _ a => some (a ++ "!"))).map
  Level.toListWithIndex == some [(0, "a!"), (1, "b!")]

#guard (l3.traverseWithIndex (F := Option)
  (fun i _ => if i == 0 then none else some "x")) == none

/-! ### `Functor` instance -/

#guard (String.length <$> l3 : Level Nat Nat).toListWithIndex == [(0, 1), (1, 1)]

/-! ### a bigger, nested level built via repeated `lappend` -/

def big : Level Nat Nat :=
  (List.range 5).foldl (fun acc i => Level.lappend acc (.one i i)) .zero

#guard big.toListWithIndex == [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]
#guard big.size == 5

end Tests.Control.Lens.Internal.Level
