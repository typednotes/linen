/-
  Tests for `Linen.System.GitFn.Build`: the generated package, check and
  worker sources, cache keys and configuration. Fetching and building are
  exercised end to end by `lake exe gitfn-integration`.
-/
import Linen.System.GitFn.Build

open System.GitFn Lean

namespace Tests.System.GitFn.Build

/-- `s` contains `sub`. -/
def has (s sub : String) : Bool := (s.splitOn sub).length > 1

def sha : CommitSha := (CommitSha.ofString? "0123456789abcdef0123456789abcdef01234567").get!
def fn : GitFn := { repo := "r", commit := sha, name := `Geo.shift, type := "Geo.Point → Nat → Geo.Point" }
def mods : List Name := [`Geo.Basic, .str .anonymous "my-mod"]

-- Names are written escaped where needed.
#guard nameLiteral `Geo.Basic == "`Geo.Basic"
#guard nameSource (.str .anonymous "my-mod") == "«my-mod»"

-- The lakefile: the remote library, the selected libraries by path, and — for
-- a worker — the check and the executable as default targets.
def lf := lakefileSource "linen-gitfn" [Library.linen "/opt/linen"] mods true
#guard has lf "package «linen-gitfn»"
#guard has lf "require «linen» from \"/opt/linen\""
#guard has lf "srcDir := \"remote\"\n  roots := #[`Geo.Basic, `«my-mod»]"
#guard has lf "@[default_target] lean_lib LinenGitFnCheck"
#guard has lf "lean_exe «linen-gitfn-worker»"
#guard !has (lakefileSource "p" [] mods false) "lean_exe"

-- The check imports the admitted modules and checks the descriptor's claim.
def ck := checkModuleSource [fn, { fn with name := `Geo.origin, type := "Geo.Point" }] mods
#guard has ck "import Geo.Basic\nimport «my-mod»\n"
#guard has ck "abbrev LinenGitFnExpected0 := (Geo.Point → Nat → Geo.Point)\n#eval linenGitFnCheck `Geo.shift ``LinenGitFnExpected0"
#guard has ck "abbrev LinenGitFnExpected1 := (Geo.Point)\n#eval linenGitFnCheck `Geo.origin ``LinenGitFnExpected1"
#guard has ck "Meta.isDefEq info.type e.value!"
#guard has ck "collectAxioms target"
#guard has ck "Lean.hasInitAttr env n"

-- A vendored function is bound at its declared type in the importing project.
#guard fn.definition `shift == "def shift : (Geo.Point → Nat → Geo.Point) := @Geo.shift"

-- The worker binds the function at its declared type and serves both ways.
def wk := workerSource fn mods
#guard has wk "def linenGitFnEntry : (Geo.Point → Nat → Geo.Point) := @Geo.shift"
#guard has wk "| [\"--http\", port] =>"
#guard has wk "import Lean.Data.JsonRpc"
#guard has wk "| .ok (.request id \"call\" params) =>"
#guard has wk "| .ok (Message.notification \"exit\" _) => break"
#guard has wk "req.line.method == Std.Http.Method.post && path == \"/call\""

-- Cache keys depend on what determines the build, and only on that.
def cfg : Config := { cache := "/tmp/c" }
#guard buildKey cfg fn "worker" == buildKey cfg fn "worker"
#guard buildKey cfg fn "worker" != buildKey cfg { fn with type := "Nat" } "worker"
#guard buildKey cfg fn "worker" != buildKey { cfg with libraries := [.linen "/l"] } fn "worker"
#guard (buildKey cfg fn "worker").startsWith "0123456789ab-"

-- The selected libraries' prefixes join the policy.
#guard ({ cfg with libraries := [.linen "/l"] } : Config).effectivePolicy.imports.contains `Linen
#guard !cfg.effectivePolicy.imports.contains `Linen
#guard cfg.toolchain == s!"leanprover/lean4:v{Lean.versionString}"
-- A nested build must not inherit the host's Lake environment.
#guard cleanEnv.any (·.1 == "LEAN_PATH") && cleanEnv.all (·.2.isNone)

-- Every step has a deadline, an hour unless configured.
#guard cfg.timeoutMs == 3600 * 1000

-- A step past its deadline is killed, and the build fails saying so.
#eval show IO Unit from do
  match ← run "sleep" #["30"] "." 200 with
  | .error e => unless e.startsWith "`sleep 30` was killed after" do throw (IO.userError e)
  | .ok _ => throw (IO.userError "sleep should have been killed")

-- A failing step reports its exit code and output; a missing command, why.
#eval show IO Unit from do
  match ← run "sh" #["-c", "echo boom; exit 2"] "." with
  | .error e => unless e == "`sh -c echo boom; exit 2` failed (2):\nboom\n" do throw (IO.userError e)
  | .ok _ => throw (IO.userError "sh should have failed")
  match ← run "linen-no-such-command" #[] "." with
  | .error e => unless e.startsWith "`linen-no-such-command ` " do throw (IO.userError e)
  | .ok _ => throw (IO.userError "a missing command should fail")

end Tests.System.GitFn.Build
