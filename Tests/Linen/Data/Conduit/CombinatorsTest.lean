/-
  Tests for `Data.Conduit.Combinators`.

  `ConduitT` is `unsafe` (see `Data.Conduit`'s module docstring), so every
  check runs inside `#eval show IO Unit from do ...` rather than `#guard`.
-/
import Linen.Data.Conduit.Combinators

open Data.Conduit
open Data.Conduit.Combinators

namespace Tests.Data.Conduit.Combinators

private unsafe def runList {r : Type} (c : ConduitT PEmpty PEmpty Id r) : r :=
  runConduitPure c

#eval show IO Unit from do
  -- sourceList / mapC / sinkList, chained with `.|`
  unless runList (sourceList [1, 2, 3, 4, 5] .| mapC (· * 2) .| sinkList) == [2, 4, 6, 8, 10] do
    throw (IO.userError "sourceList .| mapC .| sinkList failed")

  -- sourceArray / sinkArray
  unless runList (sourceArray #[1, 2, 3] .| sinkArray) == #[1, 2, 3] do
    throw (IO.userError "sourceArray .| sinkArray failed")

  -- filterC keeps only matching elements
  unless runList (sourceList [1, 2, 3, 4, 5, 6] .| filterC (· % 2 == 0) .| sinkList) == [2, 4, 6] do
    throw (IO.userError "filterC failed")

  -- takeC / dropC bound the stream
  unless runList (sourceList [1, 2, 3, 4, 5] .| takeC 3 .| sinkList) == [1, 2, 3] do
    throw (IO.userError "takeC failed")
  unless runList (sourceList [1, 2, 3, 4, 5] .| dropC 3 .| sinkList) == [4, 5] do
    throw (IO.userError "dropC failed")

  -- takeWhileC / dropWhileC split on a predicate
  unless runList (sourceList [1, 2, 3, 10, 1] .| takeWhileC (· < 5) .| sinkList) == [1, 2, 3] do
    throw (IO.userError "takeWhileC failed")
  unless runList (sourceList [1, 2, 3, 10, 1] .| dropWhileC (· < 5) .| sinkList) == [10, 1] do
    throw (IO.userError "dropWhileC failed")

  -- folds: sum / length / lengthC
  unless runList (sourceList [1, 2, 3, 4] .| sumC) == 10 do
    throw (IO.userError "sumC failed")
  unless runList (sourceList [1, 2, 3, 4] .| lengthC) == 4 do
    throw (IO.userError "lengthC failed")
  unless runList (sourceList ([] : List Nat) .| lengthC) == 0 do
    throw (IO.userError "lengthC on empty stream failed")

  -- predicates: allC / anyC / elemC / findC
  unless runList (sourceList [2, 4, 6] .| allC (· % 2 == 0)) == true do
    throw (IO.userError "allC failed")
  unless runList (sourceList [2, 4, 5] .| allC (· % 2 == 0)) == false do
    throw (IO.userError "allC (negative) failed")
  unless runList (sourceList [1, 3, 4] .| anyC (· % 2 == 0)) == true do
    throw (IO.userError "anyC failed")
  unless runList (sourceList [1, 2, 3] .| elemC 2) == true do
    throw (IO.userError "elemC failed")
  unless runList (sourceList [1, 2, 3] .| findC (· > 1)) == some 2 do
    throw (IO.userError "findC failed")

  -- maximumC / minimumC
  unless runList (sourceList [3, 1, 4, 1, 5] .| maximumC) == some 5 do
    throw (IO.userError "maximumC failed")
  unless runList (sourceList [3, 1, 4, 1, 5] .| minimumC) == some 1 do
    throw (IO.userError "minimumC failed")
  unless runList (sourceList ([] : List Nat) .| maximumC) == none do
    throw (IO.userError "maximumC on empty stream failed")

  -- concatMapC / mapMaybeC
  unless runList (sourceList [1, 2, 3] .| concatMapC (fun n => [n, n]) .| sinkList) == [1, 1, 2, 2, 3, 3] do
    throw (IO.userError "concatMapC failed")
  unless runList (sourceList [1, 2, 3, 4] .| mapMaybeC (fun n => if n % 2 == 0 then some n else none) .| sinkList)
      == [2, 4] do
    throw (IO.userError "mapMaybeC failed")

  -- scanlC emits the running accumulation, seeded with `init`
  unless runList (sourceList [1, 2, 3] .| scanlC (· + ·) 0 .| sinkList) == [0, 1, 3, 6] do
    throw (IO.userError "scanlC failed")

  -- intersperseC inserts a separator between elements
  unless runList (sourceList [1, 2, 3] .| intersperseC 0 .| sinkList) == [1, 0, 2, 0, 3] do
    throw (IO.userError "intersperseC failed")

  -- concatC flattens a stream of lists
  unless runList (sourceList [[1, 2], [3], ([] : List Nat), [4]] .| concatC .| sinkList) == [1, 2, 3, 4] do
    throw (IO.userError "concatC failed")

  -- chunksOfC groups into fixed-size chunks, keeping the final partial chunk
  unless runList (sourceList [1, 2, 3, 4, 5] .| chunksOfC 2 .| sinkList) == [[1, 2], [3, 4], [5]] do
    throw (IO.userError "chunksOfC failed")

  -- replicateC / enumFromToC
  unless runList (replicateC 3 (9 : Nat) .| sinkList) == [9, 9, 9] do
    throw (IO.userError "replicateC failed")
  unless runList (enumFromToC 1 4 .| sinkList) == [1, 2, 3, 4] do
    throw (IO.userError "enumFromToC failed")

  -- nullC reports emptiness, and leftover-pushes a peeked element back
  unless runList (sourceList ([] : List Nat) .| nullC) == true do
    throw (IO.userError "nullC on empty stream failed")
  unless runList (sourceList [1, 2] .| nullC) == false do
    throw (IO.userError "nullC on non-empty stream failed")

-- Monadic combinators (`mapMC`, `foldMC`, `mapM_C`) run their effects in `IO`.
#eval show IO Unit from do
  let log ← IO.mkRef (#[] : Array Nat)
  let result ← runConduit (sourceList [1, 2, 3] .| mapMC (fun n => pure (n * 10)) .| sinkList
    : ConduitT PEmpty PEmpty IO (List Nat))
  unless result == [10, 20, 30] do throw (IO.userError s!"mapMC failed, got {result}")

  let total ← runConduit (sourceList [1, 2, 3, 4] .| foldMC (fun acc n => pure (acc + n)) 0
    : ConduitT PEmpty PEmpty IO Nat)
  unless total == 10 do throw (IO.userError s!"foldMC failed, got {total}")

  runConduit (sourceList [1, 2, 3] .| mapM_C (fun n => log.modify (·.push n))
    : ConduitT PEmpty PEmpty IO Unit)
  unless (← log.get) == #[1, 2, 3] do
    throw (IO.userError s!"mapM_C failed, got {← log.get}")

end Tests.Data.Conduit.Combinators
