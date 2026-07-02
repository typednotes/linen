/-
  Examples.ResourceT — `Control.Monad.Trans.Resource` end-to-end.

  Ports of `Control.Monad.Trans.Resource` (Hackage `resourcet`) give
  deterministic, exception-safe, LIFO resource cleanup. This demo uses a real
  resource — scratch files created with `IO.FS.createTempFile` — rather than a
  simulated one, so "released" means the file is actually gone from disk:

  * `demoLifoCleanup` — three scratch files are `allocate`d inside one
    `runResourceT` block; the demo checks all three exist while the block is
    running, then that all three are gone, in last-acquired-first-released
    order, once it returns;
  * `demoCleanupOnException` — a scratch file is `allocate`d and the block
    then throws; the demo checks the exception still propagates *and* the
    file is still cleaned up;
  * `demoEarlyRelease` — `release` is called explicitly inside the block,
    before `runResourceT` returns; the demo checks the file is gone
    immediately, and that the implicit cleanup at the end doesn't error on
    the already-released key.

  Args: (none) -- runs every check below and exits non-zero on any mismatch
-/
import Linen.Control.Monad.Trans.Resource

open Control.Monad.Trans.Resource
open System (FilePath)

namespace Examples.ResourceT

/-- Acquire a fresh scratch file and write `tag` into it, releasing by
deleting the file. Logs `"acquire " ++ tag` / `"release " ++ tag"` to `log`, so
callers can inspect ordering. -/
def scratchFile (log : IO.Ref (Array String)) (tag : String) :
    Control.Monad.Trans.Resource.ResourceT IO (ReleaseKey × FilePath) :=
  allocate
    (do
      let (handle, path) ← IO.FS.createTempFile
      handle.putStr tag
      handle.flush
      log.modify (·.push s!"acquire {tag}")
      pure path)
    (fun path => do
      log.modify (·.push s!"release {tag}")
      IO.FS.removeFile path)

def demoLifoCleanup : IO Bool := do
  IO.println "── LIFO cleanup across three scratch files ──"
  let log ← IO.mkRef (#[] : Array String)
  let paths ← IO.mkRef (#[] : Array FilePath)
  runResourceT do
    for tag in ["first", "second", "third"] do
      let (_, path) ← scratchFile log tag
      liftM (paths.modify (·.push path) : IO Unit)
    let allExist ← liftM (do (← paths.get).allM (System.FilePath.pathExists ·) : IO Bool)
    unless allExist do throw (IO.userError "a scratch file went missing before cleanup")
  let stillExist ← (← paths.get).anyM (System.FilePath.pathExists ·)
  let trace ← log.get
  IO.println s!"  order: {trace}"
  pure (trace == #["acquire first", "acquire second", "acquire third",
                    "release third", "release second", "release first"]
        && !stillExist)

def demoCleanupOnException : IO Bool := do
  IO.println "── cleanup still runs when the block throws ──"
  let log ← IO.mkRef (#[] : Array String)
  let pathRef ← IO.mkRef (none : Option FilePath)
  let threw ←
    try
      runResourceT do
        let (_, path) ← scratchFile log "doomed"
        liftM (pathRef.set (some path) : IO Unit)
        liftM (throw (IO.userError "boom") : IO Unit)
      pure false
    catch _ => pure true
  let some path ← pathRef.get | throw (IO.userError "scratch file was never allocated")
  let stillExists ← System.FilePath.pathExists path
  IO.println s!"  exception propagated: {threw}, file still on disk: {stillExists}"
  pure (threw && !stillExists)

def demoEarlyRelease : IO Bool := do
  IO.println "── an explicit early `release` runs immediately ──"
  let log ← IO.mkRef (#[] : Array String)
  let pathRef ← IO.mkRef (none : Option FilePath)
  runResourceT do
    let (key, path) ← scratchFile log "early"
    liftM (pathRef.set (some path) : IO Unit)
    let existsBefore ← liftM (System.FilePath.pathExists path : IO Bool)
    let goneBefore := !existsBefore
    if goneBefore then throw (IO.userError "file should still exist before release")
    release key
    release key  -- second release is a documented no-op
  let some path ← pathRef.get | throw (IO.userError "scratch file was never allocated")
  let goneAfter := !(← System.FilePath.pathExists path)
  let trace ← log.get
  IO.println s!"  order: {trace}, released before runResourceT returned: {goneAfter}"
  pure (trace == #["acquire early", "release early"] && goneAfter)

def run (_args : List String) : IO Unit := do
  let okLifo ← demoLifoCleanup
  IO.println ""
  let okException ← demoCleanupOnException
  IO.println ""
  let okEarly ← demoEarlyRelease
  IO.println ""
  if okLifo && okException && okEarly then
    IO.println "resourcet demo done · all checks passed"
  else
    throw (IO.userError "resourcet demo done · some checks failed")

end Examples.ResourceT
