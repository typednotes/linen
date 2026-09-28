/-
  Integration test of `System.GitFn` against a real, private GitHub
  repository: `lake exe gitfn-remote`.

  `gitfn-integration` builds its fixture locally; this test fetches one over
  the network — `typednotes/test`, private, at a pinned commit — the way a
  real descriptor names code: authenticated `git fetch` of a SHA. The
  repository carries its own expectations in `gitfn.json`: which modules the
  secure policy must exclude and why, which functions must build and what
  their calls answer (over stdio, and over HTTP for some), which descriptors
  must be rejected and why, and the markers its hostile lakefile and `#eval`
  would leave if they ever ran. So cases are added in that repository, and
  picked up here by moving the pinned commit.

  Authentication is `git`'s, not this program's: CI fetches the repository
  over SSH with its read-only deploy key (the `GITFN_DEPLOY_KEY` secret),
  through a git `insteadOf` scoped to its URL, so the descriptor keeps its
  HTTPS URL (see `.github/workflows/lean_action_ci.yml`); locally, whatever
  credential `git` already has for github.com works.

  - `GITFN_TEST_REPO` overrides the repository (e.g. a local checkout);
  - `GITFN_TEST_COMMIT` overrides the commit (a full SHA).
-/
import Linen.System.GitFn

open System.GitFn Lean

namespace Integration.GitFnRemote

deriving instance BEq for Except

/-- The repository tested by default. -/
def defaultRepo : String := "https://github.com/typednotes/test"

/-- The commit tested by default: move it to pick up new cases. -/
def defaultCommit : String := "cfead8738fbbc27197f36774eb646a8fa8859be3"

/-- `s` contains `sub` (every string contains `""`). -/
def has (s sub : String) : Bool := sub.isEmpty || (s.splitOn sub).length > 1

-- ── The repository's expectations (`gitfn.json`) ────────────────────────────

/-- One call and what it must answer: a result, or an error containing a
    substring. -/
structure Call where
  args : List Json
  expected : Except String Json

/-- One descriptor: calls it must answer, or the reason it must be rejected
    (a substring of the build's error). -/
structure Case where
  name : Name
  type : String
  http : Bool
  calls : List Call
  rejected : Option String

/-- Everything `gitfn.json` expects. -/
structure Manifest where
  project : String
  markers : List String
  excluded : List (Name × String)
  cases : List Case

private def field (α : Type) [FromJson α] (j : Json) (k : String) (ctx : String) : Except String α :=
  (j.getObjValAs? α k).mapError (s!"{ctx}.{k}: " ++ ·)

def Call.parse (j : Json) (ctx : String) : Except String Call := do
  let args ← field (List Json) j "args" ctx
  match j.getObjValAs? String "error", j.getObjVal? "result" with
  | .ok e, _ => pure ⟨args, .error e⟩
  | _, .ok r => pure ⟨args, .ok r⟩
  | _, _ => throw s!"{ctx}: a call needs a `result` or an `error`"

def Case.parse (j : Json) (ctx : String) : Except String Case := do
  let name ← Data.Name.parse (← field String j "name" ctx)
  let calls ← match j.getObjValAs? (Array Json) "calls" with
    | .ok cs => cs.toList.zipIdx.mapM fun (c, i) => Call.parse c s!"{ctx}.calls[{i}]"
    | .error _ => pure []
  pure { name, type := ← field String j "type" ctx
         http := (j.getObjValAs? Bool "http").toOption.getD false, calls
         rejected := (j.getObjValAs? String "rejected").toOption }

def Manifest.parse (text : String) : Except String Manifest := do
  let j ← Json.parse text
  let excluded ← match j.getObjVal? "excluded" with
    | .ok (.obj kvs) => kvs.toList.mapM fun (k, v) => do
        pure (← Data.Name.parse k, ← (fromJson? v : Except String String))
    | _ => pure []
  let cases ← (← field (Array Json) j "functions" "gitfn.json").toList.zipIdx.mapM fun (c, i) =>
    Case.parse c s!"functions[{i}]"
  pure { project := (j.getObjValAs? String "project").toOption.getD "."
         markers := (j.getObjValAs? (List String) "markers").toOption.getD []
         excluded, cases }

-- ── Checks ──────────────────────────────────────────────────────────────────

/-- A check: its name, and whether it held (with what was seen if not). -/
abbrev Check := String × Option String

def pass (name : String) : Check := (name, none)
def failWith (name why : String) : Check := (name, some why)
def expect (name : String) (ok : Bool) (why : String := "") : Check :=
  if ok then pass name else failWith name why

def report (checks : Array Check) : IO UInt32 := do
  for (name, why) in checks do
    match why with
    | none => IO.println s!"PASS  {name}"
    | some w => IO.println s!"FAIL  {name}{if w.isEmpty then "" else s!"\n      {w}"}"
  let failed := checks.filter (·.2.isSome)
  IO.println s!"{checks.size - failed.size}/{checks.size} passed"
  pure (if failed.isEmpty then 0 else 1)

/-- Whether an answer is the one a call expects. -/
def answers (expected : Except String Json) (got : Except String Json) : Bool :=
  match expected, got with
  | .ok e, .ok g => e == g
  | .error sub, .error g => has g sub
  | _, _ => false

/-- A call, described. -/
def Call.describe (fn : Name) (c : Call) : String :=
  s!"{fn} {(Json.arr c.args.toArray).compress} ↦ " ++ match c.expected with
    | .ok r => r.compress
    | .error e => s!"error ∋ \"{e}\""

/-- An answer, described. -/
def describeAnswer (r : Except String Json) : String :=
  match r with
  | .ok j => s!"got {j.compress}"
  | .error e => s!"got the error: {e}"

def main : IO UInt32 := do
  let repo := (← IO.getEnv "GITFN_TEST_REPO").getD defaultRepo
  let commitHex := (← IO.getEnv "GITFN_TEST_COMMIT").getD defaultCommit
  let some commit := CommitSha.ofString? commitHex
    | IO.eprintln s!"`{commitHex}` is not a full commit SHA"; return 1
  let root := System.FilePath.mk s!"/tmp/linen-gitfn-remote-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  let cfg : Config := { cache := root / "cache" }
  IO.println s!"{repo} at {commit}"
  let mut checks : Array Check := #[]

  -- Fetch, and read the repository's expectations.
  let probe : GitFn := { repo, commit, name := `probe, type := "Nat" }
  let src ← match ← fetch cfg probe with
    | .ok d => pure d
    | .error e => discard <| report #[failWith "fetch the commit" e]; return 1
  checks := checks.push (pass "fetch the commit")
  let m ← match Manifest.parse (← IO.FS.readFile (src / "gitfn.json")) with
    | .ok m => pure m
    | .error e => discard <| report #[failWith "read gitfn.json" e]; return 1
  let desc (c : Case) : GitFn := { repo, commit, project := m.project, name := c.name, type := c.type }

  -- The policy: exactly the expected modules are excluded, each for its reason.
  let checkedFirst ← fetchAndCheck cfg { probe with project := m.project }
  -- Every later build checks the project again; it must reuse these.
  let envsAfterFirst ← libraryEnvironmentsLoaded
  match checkedFirst with
  | .error e => checks := checks.push (failWith "check the project" e)
  | .ok checked =>
    let excluded := checked.report.excluded
    for (mod, reason) in m.excluded do
      let problems := (excluded.lookup mod).getD []
      checks := checks.push <| expect s!"{mod} is excluded: {reason}"
        (problems.any (has · reason)) (if problems.isEmpty then "it is admitted" else s!"{problems}")
    let unexpected := excluded.filter fun (mod, _) => (m.excluded.lookup mod).isNone
    checks := checks.push <| expect "no other module is excluded" unexpected.isEmpty s!"{unexpected}"
    let modules := checked.report.files.toList.map (·.module)
    let shouldAdmit := modules.filter fun mod => (m.excluded.lookup mod).isNone
    checks := checks.push <| expect s!"the other {shouldAdmit.length} modules are admitted"
      (shouldAdmit.all checked.report.admitted.contains) s!"admitted: {checked.report.admitted}"

  -- Every descriptor: built and called, or rejected for its reason.
  for c in m.cases do
    match c.rejected, ← build cfg (desc c) with
    | some reason, .error e =>
      checks := checks.push <| expect s!"{c.name} : {c.type} is rejected: {reason}" (has e reason)
        (e.take 2000).toString
    | some reason, .ok _ =>
      checks := checks.push (failWith s!"{c.name} : {c.type} is rejected: {reason}" "it built")
    | none, .error e =>
      checks := checks.push (failWith s!"build {c.name} : {c.type}" (e.take 4000).toString)
    | none, .ok built =>
      checks := checks.push (pass s!"build {c.name} : {c.type}")
      let w ← built.spawn
      for call in c.calls do
        let r ← w.call call.args
        checks := checks.push <| expect s!"stdio: {call.describe c.name}" (answers call.expected r)
          (describeAnswer r)
      checks := checks.push <| expect s!"{c.name}: the stdio worker stops" ((← w.stop) == 0)
      if c.http then
        match ← built.serve with
        | .error e => checks := checks.push (failWith s!"{c.name}: serve over HTTP" e)
        | .ok h =>
          for call in c.calls do
            let r ← h.call call.args
            checks := checks.push <| expect s!"http: {call.describe c.name}" (answers call.expected r)
              (describeAnswer r)
          h.stop

  -- Static mode: every accepted function, vendored into one package.
  let accepted := (m.cases.filter (·.rejected.isNone)).map desc
  match ← vendor cfg accepted (root / "vendored") with
  | .error e => checks := checks.push (failWith s!"vendor the {accepted.length} accepted functions" e)
  | .ok v =>
    checks := checks.push (pass s!"vendor the {accepted.length} accepted functions")
    let leaked := v.modules.filter fun mod => (m.excluded.lookup mod).isSome
    checks := checks.push <| expect "no excluded module is vendored" leaked.isEmpty s!"{leaked}"

  -- Nothing from the repository ever ran.
  let dirs := [src, src / m.project, root / "vendored", ".", root]
  let found ← dirs.flatMap (fun d => m.markers.map (d / ·)) |>.filterM (·.pathExists)
  checks := checks.push <| expect s!"no remote code ran ({", ".intercalate m.markers})" found.isEmpty
    s!"{found}"

  -- An import is never released, so a per-build import leaked ~860 MB per
  -- build: this test's builds outgrew a 16 GB runner (see
  -- `System.GitFn.libraryEnvironments`).
  let envs ← libraryEnvironmentsLoaded
  checks := checks.push <| expect s!"library environments are loaded once per process ({envs})"
    (envsAfterFirst > 0 && envs == envsAfterFirst) s!"{envsAfterFirst} after the first check"

  let code ← report checks
  -- Keep everything for inspection when something failed.
  if code == 0 then IO.FS.removeDirAll root else IO.println s!"kept in {root}"
  pure code

end Integration.GitFnRemote

def main : IO UInt32 := Integration.GitFnRemote.main
