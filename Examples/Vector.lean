/-
  Examples.Vector — the `Data.Vector`-derived `Array` combinators end-to-end.

  `Linen.Data.Vector` doesn't introduce a wrapper type (Lean's `Array` already
  *is* Haskell's `Data.Vector`); it just adds the handful of combinators
  `Array` was missing. This demo exercises each of them in turn:
  `generate`, `ifilter`, `foldl1'`/`foldr1`, `ifoldl'`/`ifoldr`, `and`/`or`,
  `product`, `notElem`, `backpermute`, `slice`.

  Args: (none) -- runs every check below and exits non-zero on any mismatch
-/
import Linen.Data.Vector

namespace Examples.Vector

def demoConstructionAndFilter : IO Bool := do
  IO.println "── generate / ifilter ──"
  let squares := Array.generate 6 (fun i => i * i)
  let evenIndexed := squares.ifilter (fun i _ => i % 2 == 0)
  IO.println s!"  generate 6 (i*i) = {squares}"
  IO.println s!"  ifilter (even index) = {evenIndexed}"
  pure (squares == #[0, 1, 4, 9, 16, 25] && evenIndexed == #[0, 4, 16])

def demoFolds : IO Bool := do
  IO.println "── foldl1' / foldr1 / ifoldl' / ifoldr ──"
  let xs := #[1, 2, 3, 4]
  let maxLeft := xs.foldl1' max
  let minusRight := xs.foldr1 (· - ·)
  let weightedSum := xs.ifoldl' (fun acc i x => acc + i * x) 0
  let indexedList := xs.ifoldr (fun i x acc => (i, x) :: acc) []
  IO.println s!"  foldl1' max = {maxLeft}, foldr1 (-) = {minusRight}"
  IO.println s!"  ifoldl' (+ i*x) = {weightedSum}, ifoldr pair-up = {indexedList}"
  -- `-` on `Nat` truncates at 0: foldr1 nests as `1 - (2 - (3 - 4))`, and each
  -- inner subtraction underflows to 0 before the next one is applied.
  pure (maxLeft == some 4 && minusRight == some 0 &&
        weightedSum == 20 && indexedList == [(0, 1), (1, 2), (2, 3), (3, 4)])

def demoReductionsAndSearch : IO Bool := do
  IO.println "── and / or / product / notElem ──"
  let allTrue := #[true, true, true]
  let mixed := #[true, false, true]
  let nums := #[2, 3, 4]
  IO.println s!"  and allTrue = {Array.and allTrue}, and mixed = {Array.and mixed}"
  IO.println s!"  or mixed = {Array.or mixed}, product nums = {Array.product nums}"
  IO.println s!"  notElem 5 nums = {Array.notElem 5 nums}, notElem 3 nums = {Array.notElem 3 nums}"
  pure (Array.and allTrue && !Array.and mixed && Array.or mixed &&
        Array.product nums == 24 && Array.notElem 5 nums && !Array.notElem 3 nums)

def demoReorderingAndSlicing : IO Bool := do
  IO.println "── backpermute / slice ──"
  let letters := #['a', 'b', 'c', 'd', 'e']
  let reordered := letters.backpermute #[4, 0, 2]
  let middle := letters.slice 1 3
  IO.println s!"  backpermute [4,0,2] = {reordered}"
  IO.println s!"  slice 1 3 = {middle}"
  pure (reordered == #['e', 'a', 'c'] && middle == #['b', 'c', 'd'])

def run (_args : List String) : IO Unit := do
  let okConstruct ← demoConstructionAndFilter
  IO.println ""
  let okFolds ← demoFolds
  IO.println ""
  let okReduce ← demoReductionsAndSearch
  IO.println ""
  let okReorder ← demoReorderingAndSlicing
  if okConstruct && okFolds && okReduce && okReorder then
    IO.println "\nvector demo done · all checks passed"
  else
    throw (IO.userError "vector demo done · some checks failed")

end Examples.Vector
