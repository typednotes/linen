/-
  Tests for `Linen.Data.List.NonEmpty`.

  Construction, total `head`/`last`, folds, conversions, and the `Functor`/`Monad`
  instances over `NonEmpty`.
-/
import Linen.Data.List.NonEmpty

open Data.List

namespace Tests.Data.List.NonEmpty

def ne123 : NonEmpty Nat := ⟨1, [2, 3]⟩

/-! ### Construction / conversion -/

#guard (NonEmpty.singleton 7).toList == [7]
#guard (NonEmpty.cons 0 ne123).toList == [0, 1, 2, 3]
#guard ne123.toList == [1, 2, 3]
#guard NonEmpty.fromList? ([] : List Nat) == none
#guard (NonEmpty.fromList? [4, 5]).map (·.toList) == some [4, 5]
#guard (NonEmpty.fromList [4, 5] (by simp)).toList == [4, 5]

/-! ### Total accessors -/

#guard ne123.head == 1
#guard ne123.last == 3
#guard (NonEmpty.singleton 9).last == 9
#guard ne123.length.val == 3

/-! ### Combinators -/

#guard (ne123 ++ NonEmpty.singleton 4).toList == [1, 2, 3, 4]
#guard (ne123.map (· * 10)).toList == [10, 20, 30]
#guard ne123.reverse.toList == [3, 2, 1]
#guard ne123.foldr (· + ·) 0 == 6
#guard ne123.foldr1 (· + ·) == 6
#guard ne123.foldl1 (· + ·) == 6                       -- ((1+2)+3)

/-! ### Functor / Monad -/

#guard ((· + 1) <$> ne123).toList == [2, 3, 4]
#guard (pure 5 : NonEmpty Nat).toList == [5]
#guard (ne123 >>= fun x => ⟨x, [x * 10]⟩).toList == [1, 10, 2, 20, 3, 30]

#guard toString ne123 == "[1, 2, 3]"

/-! ### Proofs (compile-time) -/

example (ne : NonEmpty Nat) : ne.toList ≠ [] := NonEmpty.toList_ne_nil ne
example (ne : NonEmpty Nat) : ne.reverse.toList.length = ne.toList.length :=
  NonEmpty.reverse_length ne

end Tests.Data.List.NonEmpty
