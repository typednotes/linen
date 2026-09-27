/-
  `System.GitFn.Descriptor` — a function identified by where it is defined

  A `GitFn` names a Lean function universally, by five things:

  - `repo` — the git repository (typically a GitHub URL);
  - `commit` — the commit, as a full SHA (`CommitSha`): branches and tags move,
    a SHA does not. `resolve` turns a branch, tag or `HEAD` into one, once;
  - `project` — the directory of the Lean project (its lakefile) inside the
    repository, relative;
  - `name` — the function's fully qualified name;
  - `type` — its declared type, as Lean syntax read in that project.

  Descriptors are plain data: they compare, and they serialise to JSON (Lean
  core's `Lean.Json`). Nothing here fetches, compiles or runs anything; that
  is `System.GitFn.Build`, in secure mode only.

  Descriptors come from outside, so `validate` rejects the shapes that could
  be used to smuggle options to `git` (a repository starting with `-`) or to
  escape the checkout (an absolute project path, or one with `..`).
-/
import Lean.Data.Json
import Linen.Data.Name

namespace System.GitFn

open Lean (Json ToJson FromJson)

-- ── Commits ─────────────────────────────────────────────────────────────────

/-- A full commit SHA: 40 (SHA-1) or 64 (SHA-256) lowercase hexadecimal
    digits. The constructor is private: a `CommitSha` is always valid. -/
structure CommitSha where
  private mk ::
  /-- The hexadecimal digits. -/
  hex : String
  deriving Repr, DecidableEq, Hashable

/-- Whether `s` is a full lowercase hexadecimal commit SHA. -/
def CommitSha.isValid (s : String) : Bool :=
  (s.length == 40 || s.length == 64) && s.all fun c => c.isDigit || ('a' ≤ c && c ≤ 'f')

/-- A commit SHA, if `s` is one. -/
def CommitSha.ofString? (s : String) : Option CommitSha :=
  if CommitSha.isValid s then some ⟨s⟩ else none

instance : ToString CommitSha := ⟨CommitSha.hex⟩

/-- The all-zero SHA: well formed, naming no commit. -/
instance : Inhabited CommitSha := ⟨⟨String.ofList (List.replicate 40 '0')⟩⟩

-- ── Descriptors ─────────────────────────────────────────────────────────────

/-- A Lean function, identified by where it is defined. -/
structure GitFn where
  /-- The git repository. -/
  repo : String
  /-- The commit. -/
  commit : CommitSha
  /-- The directory of the Lean project inside the repository. -/
  project : String := "."
  /-- The function's fully qualified name. -/
  name : Lean.Name
  /-- Its declared type, as Lean syntax read in that project. -/
  type : String
  deriving Repr, DecidableEq

/-- The components of a relative path, rejecting absolute paths, `..`, empty
    components and backslashes. `"."` is the repository root. -/
def projectComponents (p : String) : Except String (List String) := do
  if p.startsWith "/" then throw s!"project path `{p}` must be relative"
  if p.any (· == '\\') then throw s!"project path `{p}` must use `/`"
  let comps := (p.splitOn "/").filter (· != ".")
  if comps.any (· == "..") then throw s!"project path `{p}` must not contain `..`"
  if comps.any (· == "") then throw s!"project path `{p}` has an empty component"
  pure comps

/-- Check a descriptor from outside before anything uses it. -/
def GitFn.validate (f : GitFn) : Except String Unit := do
  if f.repo.isEmpty then throw "the repository is empty"
  if f.repo.startsWith "-" then throw s!"repository `{f.repo}` must not start with `-`"
  if f.repo.any (fun c => c == '\n' || c == '\r' || c == '\x00') then
    throw "the repository contains a control character"
  discard <| projectComponents f.project
  if f.name.isAnonymous then throw "the function name is empty"
  unless Data.Name.roundTrips f.name do
    throw s!"the function name `{f.name}` has no dotted form"
  if f.type.trimAscii.isEmpty then throw "the declared type is empty"

instance : ToJson GitFn where
  toJson f := Json.mkObj [
    ("repo", f.repo), ("commit", f.commit.hex), ("project", f.project),
    ("name", f.name.toString), ("type", f.type)]

instance : FromJson GitFn where
  fromJson? j := do
    let hex ← j.getObjValAs? String "commit"
    let some commit := CommitSha.ofString? hex
      | throw s!"`{hex}` is not a full commit SHA"
    let name ← Data.Name.parse (← j.getObjValAs? String "name")
    let f : GitFn := {
      repo := ← j.getObjValAs? String "repo", commit
      project := (j.getObjValAs? String "project").toOption.getD "."
      name, type := ← j.getObjValAs? String "type" }
    f.validate
    pure f

-- ── Resolving a revision ────────────────────────────────────────────────────

/-- The commit a revision (branch, tag, `HEAD`, or already a SHA) of `repo`
    points to now, via `git ls-remote`. Resolve once and keep the SHA: the
    descriptor then names the same code forever. -/
def resolve (repo rev : String) (git : String := "git") : IO (Except String CommitSha) := do
  if let some sha := CommitSha.ofString? rev then return .ok sha
  if repo.startsWith "-" || rev.startsWith "-" then
    return .error "neither the repository nor the revision may start with `-`"
  let out ← IO.Process.output { cmd := git, args := #["ls-remote", "--", repo, rev] }
  if out.exitCode != 0 then return .error s!"git ls-remote failed: {out.stderr.trimAscii}"
  match out.stdout.splitOn "\n" |>.head? |>.bind (fun l => (l.splitOn "\t").head?) with
  | some hex => match CommitSha.ofString? hex with
    | some sha => return .ok sha
    | none => return .error s!"`{rev}` does not name a commit of `{repo}`"
  | none => return .error s!"`{rev}` does not name a commit of `{repo}`"

end System.GitFn
