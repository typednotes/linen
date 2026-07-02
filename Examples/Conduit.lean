/-
  Examples.Conduit — `Data.Conduit` / `Data.Conduit.Combinators` end-to-end.

  `ConduitT` is `unsafe` (see `Data.Conduit`'s module docstring: `awaitForever`
  and friends recurse on a runtime-exhausted source with no structural or
  well-founded measure), so this whole module, and every example that
  transitively calls into it, is `unsafe` as well.

  * `demoPureCombinatorPipeline` — a pure `Id`-based pipeline fused with `.|`:
    `sourceList .| filterC .| mapC .| takeC .| sinkList`, run with
    `runConduitPure`;
  * `demoEffectfulCombinators` — `mapMC`/`foldMC`/`mapM_C` run their effects in
    `IO`, run with `runConduit`;
  * `demoResourceSafeStreaming` — `bracketP` acquires a real scratch file
    (via `IO.FS.createTempFile`) inside a conduit, streams lines out of it
    with `Combinators`, and `runConduitRes` (built on
    `Control.Monad.Trans.Resource.runResourceT`) guarantees the file is
    deleted once the pipeline finishes — tying `Recv`/`ResourceT`'s sibling
    feature into `Conduit`.

  Args: (none) -- runs every check below and exits non-zero on any mismatch
-/
import Linen.Data.Conduit.Combinators

open Data.Conduit
open Data.Conduit.Combinators
open Control.Monad.Trans.Resource

namespace Examples.Conduit

unsafe def demoPureCombinatorPipeline : IO Bool := do
  IO.println "── pure combinator pipeline: filter → map → take, over Id ──"
  let result :=
    runConduitPure
      (sourceList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        .| filterC (· % 2 == 0)
        .| mapC (· * 10)
        .| takeC 3
        .| sinkList)
  IO.println s!"  evens, ×10, first 3: {result}"
  pure (result == [20, 40, 60])

unsafe def demoEffectfulCombinators : IO Bool := do
  IO.println "── effectful combinators: mapMC / foldMC / mapM_C, over IO ──"
  let log ← IO.mkRef (#[] : Array Nat)
  let doubled ←
    runConduit
      (sourceList [1, 2, 3] .| mapMC (fun n => pure (n * 2)) .| sinkList
        : ConduitT PEmpty PEmpty IO (List Nat))
  let total ←
    runConduit
      (sourceList [1, 2, 3, 4] .| foldMC (fun acc n => pure (acc + n)) 0
        : ConduitT PEmpty PEmpty IO Nat)
  runConduit
    (sourceList [1, 2, 3] .| mapM_C (fun n => log.modify (·.push n))
      : ConduitT PEmpty PEmpty IO Unit)
  let logged ← log.get
  IO.println s!"  mapMC doubled: {doubled}, foldMC total: {total}, mapM_C log: {logged}"
  pure (doubled == [2, 4, 6] && total == 10 && logged == #[1, 2, 3])

unsafe def demoResourceSafeStreaming : IO Bool := do
  IO.println "── bracketP + runConduitRes: stream a scratch file, then it's gone ──"
  let pathRef ← IO.mkRef (none : Option System.FilePath)
  let lines ←
    runConduitRes
      (bracketP
        (do
          let (handle, path) ← IO.FS.createTempFile
          handle.putStr "alpha\nbeta\ngamma\n"
          handle.flush
          pathRef.set (some path)
          pure path)
        (fun path => IO.FS.removeFile path)
        (fun path => do
          let contents ← liftConduit (IO.FS.readFile path)
          sourceList ((contents.splitOn "\n").filter (· != "")) .| sinkList)
        : ConduitT PEmpty PEmpty (ResourceT IO) (List String))
  let some path ← pathRef.get | throw (IO.userError "scratch file was never allocated")
  let stillExists ← System.FilePath.pathExists path
  IO.println s!"  lines read: {lines}, scratch file still on disk: {stillExists}"
  pure (lines == ["alpha", "beta", "gamma"] && !stillExists)

unsafe def run (_args : List String) : IO Unit := do
  let okPure ← demoPureCombinatorPipeline
  IO.println ""
  let okEffectful ← demoEffectfulCombinators
  IO.println ""
  let okResource ← demoResourceSafeStreaming
  IO.println ""
  if okPure && okEffectful && okResource then
    IO.println "conduit demo done · all checks passed"
  else
    throw (IO.userError "conduit demo done · some checks failed")

end Examples.Conduit
