/-
  Tests for `Linen.Control.Lens.Internal.Deque`.

  `Deque`: `empty`/`singleton`/`null`/`size`/`cons`/`snoc`/`uncons`/`unsnoc`/
  `fromList`/`toList`, plus the `Functor` instance.
-/
import Linen.Control.Lens.Internal.Deque

open Control.Lens.Internal

namespace Tests.Control.Lens.Internal.Deque

#guard (Deque.empty : Deque Nat).null == true
#guard (Deque.empty : Deque Nat).size == 0
#guard (Deque.empty : Deque Nat).toList == []

#guard (Deque.singleton 1).toList == [1]
#guard (Deque.singleton 1).size == 1
#guard (Deque.singleton 1).null == false

/-! ### `cons` / `snoc` -/

def d : Deque Nat := Deque.empty |> Deque.cons 1 |> Deque.cons 2 |> (Deque.snoc · 3) |> (Deque.snoc · 4)

#guard d.toList == [2, 1, 3, 4]
#guard d.size == 4

/-! ### `uncons` / `unsnoc` -/

#guard (d.uncons.map (fun (a, d') => (a, d'.toList))) == some (2, [1, 3, 4])
#guard (d.unsnoc.map (fun (d', a) => (d'.toList, a))) == some ([2, 1, 3], 4)

#guard (Deque.empty : Deque Nat).uncons == none
#guard (Deque.empty : Deque Nat).unsnoc == none

/-! ### rebalancing survives many operations -/

def big : Deque Nat := Deque.fromList (List.range 50)

#guard big.toList == List.range 50
#guard big.size == 50

/-- `uncons` recovers exactly the first element and leaves the rest of the
    order untouched, even after several rebalancing splits have happened
    inside `fromList`. -/
def unconsed : List Nat :=
  match big.uncons with
  | some (a, dq) => a :: dq.toList
  | none => []

#guard unconsed == List.range 50

/-- `unsnoc` is the symmetric fact at the other end. -/
def unsnocced : List Nat :=
  match big.unsnoc with
  | some (dq, a) => dq.toList ++ [a]
  | none => []

#guard unsnocced == List.range 50

/-! ### `fromList` / `Functor` -/

#guard (Deque.fromList [1, 2, 3] : Deque Nat).toList == [1, 2, 3]
#guard ((· + 1) <$> Deque.fromList [1, 2, 3] : Deque Nat).toList == [2, 3, 4]

end Tests.Control.Lens.Internal.Deque
