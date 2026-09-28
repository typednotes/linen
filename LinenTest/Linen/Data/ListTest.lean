/-
  Tests for `Linen.Data.List`.

  The `Data.List'` operations that core lacks. (Core's `eraseDups`/`splitBy`/
  `scanr`/`intercalate`/`eraseP`/`union`/`inter` cover the rest and aren't
  re-ported; `unfoldr` is intentionally absent — fuel/partial are banned.)
-/
import Linen.Data.List

open Data.List'

namespace Tests.Data.List'

/-! ### transpose -/

#guard transpose [[1, 2, 3], [4, 5, 6]] == [[1, 4], [2, 5], [3, 6]]
#guard transpose ([] : List (List Nat)) == []
#guard transpose [[1], [2], [3]] == [[1, 2, 3]]

/-! ### tails / inits / subsequences / permutations -/

#guard (tails [1, 2, 3]).toList == [[1, 2, 3], [2, 3], [3], []]
#guard (inits [1, 2, 3]).toList == [[], [1], [1, 2], [1, 2, 3]]
#guard subsequences [1, 2] == [[], [2], [1], [1, 2]]
#guard (permutations [1, 2, 3]).length == 6
#guard permutations [1, 2] == [[1, 2], [2, 1]]

/-! ### mapAccumL / mapAccumR -/

#guard mapAccumL (fun s x => (s + x, s)) 0 [1, 2, 3] == (6, [0, 1, 3])      -- running prefix sums (exclusive)
#guard mapAccumR (fun s x => (s + x, s)) 0 [1, 2, 3] == (6, [5, 3, 0])      -- suffix sums (exclusive)

/-! ### sortOn / maximumBy / minimumBy -/

#guard sortOn (fun s => s.length) ["ccc", "a", "bb"] == ["a", "bb", "ccc"]
#guard maximumBy compare [3, 1, 4, 1, 5] == some 5
#guard minimumBy compare [3, 1, 4, 1, 5] == some 1
#guard maximumBy compare ([] : List Nat) == none

/-! ### unionBy / intersectBy / insertBy -/

#guard unionBy (· == ·) [1, 2, 3] [2, 3, 4] == [1, 2, 3, 4]
#guard intersectBy (· == ·) [1, 2, 3, 4] [2, 4, 6] == [2, 4]
#guard insertBy compare 3 [1, 2, 4, 5] == [1, 2, 3, 4, 5]

/-! ### Proofs (compile-time) -/

example (l : List Nat) : (tails l).toList.length = l.length + 1 := tails_length l
example (l : List Nat) : (inits l).toList.length = l.length + 1 := inits_length l

end Tests.Data.List'
