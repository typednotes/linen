/-
  Integration test of `System.GitFn`, end to end: `lake exe gitfn-integration`.

  It creates a local git repository holding a small Lean project that is
  partly hostile — a lakefile that would leave a marker file if it were ever
  run, a module that runs code at compile time, a function whose type has no
  JSON instances — then builds and calls functions from it with the secure
  pipeline, over stdio and HTTP, and vendors one statically. It needs `git`
  and the pinned toolchain, and runs nested Lake builds, so it is an
  executable run by CI rather than a `#guard`.
-/
import Linen.System.GitFn

open System.GitFn Lean

namespace Integration.GitFn

deriving instance BEq for Except

/-- `s` contains `sub`. -/
def has (s sub : String) : Bool := (s.splitOn sub).length > 1

/-- The fixture project. -/
def fixture : List (String × String) := [
  ("lean/lakefile.lean", "import Lake\nopen Lake DSL\n" ++
    "#eval IO.FS.writeFile \"LAKEFILE_WAS_RUN\" \"\"\npackage demo\nlean_lib Demo\n"),
  ("lean/Demo/Basic.lean", "import Lean.Data.Json\n\nnamespace Demo\n\n" ++
    "structure Point where\n  x : Nat\n  y : Nat\n  deriving Lean.ToJson, Lean.FromJson, Repr\n\n" ++
    "/-- Move a point right. -/\ndef shift (p : Point) (d : Nat) : Point := { p with x := p.x + d }\n\n" ++
    "def add (a b : Nat) : Nat := a + b\n\n" ++
    "def greet (name : String) : IO String := pure s!\"hello {name}\"\n\n" ++
    "def applyZero (f : Nat → Nat) : Nat := f 0\n\nend Demo\n"),
  ("lean/Demo/Evil.lean", "import Demo.Basic\n\n" ++
    "#eval IO.FS.writeFile \"EVIL_WAS_RUN\" \"\"\n\ndef Demo.evil : Nat := 42\n")]

/-- Run a command, failing loudly. -/
def sh (cmd : String) (args : Array String) (cwd : System.FilePath) : IO String := do
  let out ← IO.Process.output { cmd, args, cwd }
  unless out.exitCode == 0 do throw (IO.userError s!"{cmd} {args}: {out.stderr}")
  pure out.stdout

/-- Create the fixture repository; its path and head commit. -/
def makeRepo (root : System.FilePath) : IO (System.FilePath × String) := do
  let repo := root / "repo"
  for (path, content) in fixture do
    let file := repo / path
    if let some parent := file.parent then IO.FS.createDirAll parent
    IO.FS.writeFile file content
  let git (args : Array String) := sh "git" (#["-c", "user.name=t", "-c", "user.email=t@t"] ++ args) repo
  discard <| git #["init", "-q"]
  discard <| git #["add", "."]
  discard <| git #["commit", "-q", "-m", "fixture"]
  pure (repo, (← git #["rev-parse", "HEAD"]).trimAscii.toString)

/-- A check: its name, and whether it held. -/
abbrev Check := String × Bool

def report (checks : List Check) : IO UInt32 := do
  for (name, ok) in checks do
    IO.println s!"{if ok then "PASS" else "FAIL"}  {name}"
  let failed := checks.filter (!·.2)
  IO.println s!"{checks.length - failed.length}/{checks.length} passed"
  pure (if failed.isEmpty then 0 else 1)

def main : IO UInt32 := do
  let root := System.FilePath.mk s!"/tmp/linen-gitfn-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  let (repo, head) ← makeRepo root
  let some sha := CommitSha.ofString? head | throw (IO.userError s!"bad head {head}")
  let cfg : Config := { cache := root / "cache" }
  let desc (name : Name) (type : String) : GitFn :=
    { repo := repo.toString, commit := sha, project := "lean", name, type }
  let mut checks : List Check := []

  -- Resolving a revision gives the commit.
  checks := checks ++ [("resolve HEAD", (← resolve repo.toString "HEAD") == .ok sha)]

  -- A pure function over a record, built once, called over stdio and HTTP.
  match ← build cfg (desc `Demo.shift "Demo.Point → Nat → Demo.Point") with
  | .error e => checks := checks ++ [(s!"build Demo.shift: {e}", false)]
  | .ok built =>
    checks := checks ++ [("Demo.Evil is excluded by the policy",
      built.report.excluded.any (·.1 == `Demo.Evil))]
    let w ← built.spawn
    let r ← w.call [Json.mkObj [("x", 1), ("y", 2)], (10 : Nat)]
    checks := checks ++ [("stdio call", r == .ok (Json.mkObj [("x", 11), ("y", 2)]))]
    let bad ← w.call [(1 : Nat)]
    checks := checks ++ [("stdio call with a bad argument is an error", bad matches .error _)]
    let n ← w.stop
    checks := checks ++ [("stdio worker stops", n == 0)]
    match ← built.serve with
    | .error e => checks := checks ++ [(s!"http serve: {e}", false)]
    | .ok h =>
      let r ← h.call [Json.mkObj [("x", 5), ("y", 0)], (3 : Nat)]
      checks := checks ++ [("http call", r == .ok (Json.mkObj [("x", 8), ("y", 0)]))]
      h.stop

  -- An effectful function: its effect is in its type.
  match ← build cfg (desc `Demo.greet "String → IO String") with
  | .error e => checks := checks ++ [(s!"build Demo.greet: {e}", false)]
  | .ok built =>
    let w ← built.spawn
    let r : Except String String ← w.invoke ["world"]
    checks := checks ++ [("IO function", r == .ok "hello world")]
    discard <| w.stop

  -- A declared type that is not the function's: rejected by the semantic check.
  match ← build cfg (desc `Demo.add "Nat → Nat") with
  | .ok _ => checks := checks ++ [("wrong declared type is rejected", false)]
  | .error e => checks := checks ++
    [("wrong declared type is rejected", has e "but the descriptor declares")]

  -- A function over a type without JSON instances: it does not build.
  match ← build cfg (desc `Demo.applyZero "(Nat → Nat) → Nat") with
  | .ok _ => checks := checks ++ [("non-JSON argument type is rejected", false)]
  | .error e => checks := checks ++
    [("non-JSON argument type is rejected", has e "LinenGitFnCall")]

  -- A function the policy excluded (from `Demo.Evil`): not found.
  match ← build cfg (desc `Demo.evil "Nat") with
  | .ok _ => checks := checks ++ [("function of an excluded module is rejected", false)]
  | .error e => checks := checks ++
    [("function of an excluded module is rejected", has e "is not defined by the admitted modules")]

  -- Static mode: vendor the checked sources.
  match ← vendor cfg (desc `Demo.add "Nat → Nat → Nat") (root / "vendored") with
  | .error e => checks := checks ++ [(s!"vendor: {e}", false)]
  | .ok v =>
    checks := checks ++ [("vendored package builds", ← (root / "vendored" / ".lake").pathExists)]
    checks := checks ++ [("vendored definition", v.definition `myAdd == "def myAdd : (Nat → Nat → Nat) := @Demo.add")]
    checks := checks ++ [("vendored package excludes Demo.Evil",
      !(← (root / "vendored" / "remote" / "Demo" / "Evil.lean").pathExists))]

  -- Nothing from the repository ever ran.
  let ran ← [repo / "lean", cfg.cache / "src" / sha.hex / "lean", root / "vendored"].anyM fun d => do
    pure ((← (d / "LAKEFILE_WAS_RUN").pathExists) || (← (d / "EVIL_WAS_RUN").pathExists))
  let ranElsewhere ← (System.FilePath.mk "LAKEFILE_WAS_RUN").pathExists
  checks := checks ++ [("no remote code ran (lakefile, #eval)", !ran && !ranElsewhere)]

  let code ← report checks
  -- Keep the fixture for inspection when something failed.
  if code == 0 then IO.FS.removeDirAll root else IO.println s!"fixture kept in {root}"
  pure code

end Integration.GitFn

def main : IO UInt32 := Integration.GitFn.main
