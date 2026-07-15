/-
  `Linen.Text.Pandoc.Class.PandocPure` — the pure `PandocMonad` instance.

  ## Haskell source

  Ported from `Text.Pandoc.Class.PandocPure` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Class/PandocPure.hs`).

  Provides `PureState` (a pure environment: files, env vars, clock, RNG),
  `FileTree`/`FileInfo`, the `PandocPure` monad, its `PandocMonad` instance
  (all capabilities served without real IO), and `runPure`.

  Only the pure instance is in scope; `Class.PandocIO`/`Class.IO`/
  `Class.Sandbox` (the `IO`-backed monad) are deferred per
  `docs/imports/pandoc/dependencies.md`.

  ### Deviations from upstream

  * `PandocPure` is a plain `abbrev` for the transformer stack
    `ExceptT PandocError (StateT CommonState (StateM PureState))` (upstream is a
    `newtype` with `GeneralizedNewtypeDeriving`).
  * `stWord8Store` and the reference-doc archives
    (`stReferenceDocx`/`Pptx`/`ODT`) are dropped (tied to deferred binary
    formats); the infinite `stUniqStore` becomes a `Nat` counter
    (`stNextUnique`).
  * `glob` is simplified to exact-name / `"*"`-all matching (the `Glob`
    pattern engine is deferred).
-/

import Linen.Text.Pandoc.Class.PandocMonad

namespace Linen.Text.Pandoc

open Data (Map)

-- ── File tree ─────────────────────────────────────────────────────────

/-- Metadata and contents of a stored file. -/
structure FileInfo where
  /-- The file's modification time. -/
  infoFileMTime : UTCTime := 0
  /-- The file's contents. -/
  infoFileContents : ByteArray := ByteArray.empty
  deriving Inhabited

/-- A map from (canonicalised) paths to file info. -/
structure FileTree where
  /-- The underlying path → info map. -/
  unFileTree : Map String FileInfo
  deriving Inhabited

namespace FileTree

/-- The empty file tree. -/
def empty : FileTree := ⟨Data.Map.empty⟩

/-- Look up a file's info by (canonicalised) path. -/
def getFileInfo (fp : String) (tree : FileTree) : Option FileInfo :=
  tree.unFileTree.lookup (Shared.collapseFilePath fp)

/-- Insert a file into the tree. -/
def insertInFileTree (fp : String) (info : FileInfo) (tree : FileTree) : FileTree :=
  ⟨tree.unFileTree.insert' (Shared.collapseFilePath fp) info⟩

end FileTree

instance : EmptyCollection FileTree := ⟨FileTree.empty⟩

-- ── Pure state ────────────────────────────────────────────────────────

/-- The pure environment backing `PandocPure`. -/
structure PureState where
  /-- The pseudo-random generator. -/
  stStdGen : StdGen := mkStdGen 1848
  /-- Counter yielding fresh unique hashes. -/
  stNextUnique : Nat := 1
  /-- Environment variables. -/
  stEnv : List (String × String) := [("USER", "pandoc-user")]
  /-- The (frozen) current time, as POSIX seconds. -/
  stTime : UTCTime := 0
  /-- The (frozen) current time zone, as minutes east of UTC. -/
  stTimeZone : TimeZone := 0
  /-- The available files. -/
  stFiles : FileTree := FileTree.empty
  /-- The contents of standard input. -/
  stStdin : ByteArray := ByteArray.empty
  /-- Files under the user data directory. -/
  stUserDataFiles : FileTree := FileTree.empty
  /-- Files under the cabal data directory. -/
  stCabalDataFiles : FileTree := FileTree.empty
  deriving Inhabited

/-- The default pure state. -/
def defaultPureState : PureState := {}

-- ── The PandocPure monad ──────────────────────────────────────────────

/-- The pure `PandocMonad`: an error layer over `CommonState` over
    `PureState`, with no real IO. -/
abbrev PandocPure := ExceptT PandocError (StateT CommonState (StateM PureState))

namespace PandocPure

/-- Get the pure state. -/
def getsPure {α : Type} (f : PureState → α) : PandocPure α := f <$> getThe PureState

/-- Modify the pure state. -/
def modifyPure (f : PureState → PureState) : PandocPure Unit := modifyThe PureState f

end PandocPure

open PandocPure in
instance : PandocMonad PandocPure where
  throwError e := throw e
  catchError x h := tryCatch x h
  lookupEnv k := getsPure (fun s => s.stEnv.lookup k)
  getCurrentTime := getsPure (·.stTime)
  getCurrentTimeZone := getsPure (·.stTimeZone)
  newStdGen := do
    let g ← getsPure (·.stStdGen)
    let (g1, g2) := g.split
    modifyPure (fun s => { s with stStdGen := g2 })
    pure g1
  newUniqueHash := do
    let n ← getsPure (·.stNextUnique)
    modifyPure (fun s => { s with stNextUnique := s.stNextUnique + 1 })
    pure (Int.ofNat n)
  openURL u := throw (.PandocResourceNotFound u)
  readFileLazy fp := do
    match (← getsPure (·.stFiles)).getFileInfo fp with
    | some info => pure info.infoFileContents
    | none => throw (.PandocResourceNotFound fp)
  readFileStrict fp := do
    match (← getsPure (·.stFiles)).getFileInfo fp with
    | some info => pure info.infoFileContents
    | none => throw (.PandocResourceNotFound fp)
  readStdinStrict := getsPure (·.stStdin)
  glob s := do
    let tree ← getsPure (·.stFiles)
    let ks := tree.unFileTree.keys
    if s == "*" then pure ks
    else pure (ks.filter (· == Shared.collapseFilePath s))
  fileExists fp := do
    match (← getsPure (·.stFiles)).getFileInfo fp with
    | some _ => pure true
    | none => pure false
  getDataFileName fp := pure ("data/" ++ fp)
  getModificationTime fp := do
    match (← getsPure (·.stFiles)).getFileInfo fp with
    | some info => pure info.infoFileMTime
    | none => throw (.PandocIOError fp "Can't get modification time")
  getCommonState := getThe CommonState
  putCommonState st := modifyThe CommonState (fun _ => st)
  logOutput _ := pure ()
  trace _ := pure ()

/-- Run a pure pandoc computation, returning its result or a `PandocError`. -/
def runPure {α : Type} (x : PandocPure α) : Except PandocError α :=
  let withCommon : StateM PureState (Except PandocError α) :=
    StateT.run' (ExceptT.run x) defaultCommonState
  Id.run (StateT.run' withCommon defaultPureState)

end Linen.Text.Pandoc
