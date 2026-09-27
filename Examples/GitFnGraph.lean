/-
  Examples.GitFnGraph — a reactive graph built from functions that live in a
  git repository, imported and compiled like any other Lean function.

  The functions of an order-pricing pipeline are defined in a separate Lean
  project, in its own git repository. `System.GitFn` identifies each by
  (repository, commit, project, name, declared type); `vendor` fetches the
  project, checks its sources with the secure policy, and turns the admitted
  modules into a local package, built once to run the semantic check of every
  function against its declared type. An ordinary program then `require`s that
  package, imports `Pricing`, and uses `Pricing.lineTotal` & co. as the
  functions of a `Control.Reactive` graph, next to local operators:

      price ─┐
             ├─ lineTotal ─ withTax ─ debounceTime 50 ─ label ─→ "43.20 EUR"
      qty ───┘

  Nothing is remote here: the functions are compiled into the program. (The
  same descriptors could instead run in workers — `build` + `Reactive.remote`,
  see `System.GitFn.Reactive` — when they must be isolated or deployed apart;
  the graph would not change.)

  To stay self-contained, the example creates the pricing repository in a
  scratch directory, and writes the importing program (`app/`) there; for a
  real repository, put its URL and a commit (or `System.GitFn.resolve url
  "main"`) in the descriptors. The program runs the graph on a few timestamped
  events, checks every stream, and writes an offline HTML page drawing the
  graph with its run (`Control.Reactive.Graphviz` → `Graphics.Graphviz.Html`).

  Args: [--out FILE]  -- where to write the HTML page (default: scratch dir)

  Needs `git` and the pinned toolchain: the vendored package and the program
  are built with nested Lake builds (a minute or two the first time).
-/
import Linen.System.GitFn

namespace Examples.GitFnGraph

open System.GitFn

-- ── The pricing repository ──────────────────────────────────────────────────

/-- The pricing library, as its own Lean project. Its lakefile is never run
    (secure mode builds the checked sources itself). -/
def pricingProject : List (String × String) := [
  ("lakefile.toml", "name = \"pricing\"\ndefaultTargets = [\"Pricing\"]\n\n[[lean_lib]]\nname = \"Pricing\"\n"),
  ("Pricing.lean", "namespace Pricing\n\n" ++
    "/-- The total of an order line, in cents. -/\n" ++
    "def lineTotal (unitCents qty : Nat) : Nat := unitCents * qty\n\n" ++
    "/-- A price with 20% VAT, in cents. -/\n" ++
    "def withTax (cents : Nat) : Nat := cents + cents * 20 / 100\n\n" ++
    "/-- A price for display. -/\n" ++
    "def label (cents : Nat) : String :=\n" ++
    "  let c := cents % 100\n" ++
    "  s!\"{cents / 100}.{if c < 10 then \"0\" else \"\"}{c} EUR\"\n\n" ++
    "end Pricing\n")]

/-- Run a command outside of Lake's environment; its output, failing loudly. -/
def sh (cmd : String) (args : Array String) (cwd : System.FilePath)
    (env : Array (String × Option String) := #[]) : IO String := do
  let out ← IO.Process.output { cmd, args, cwd, env := cleanEnv ++ env }
  unless out.exitCode == 0 do
    throw (IO.userError s!"`{cmd} {" ".intercalate args.toList}` failed:\n{out.stdout}{out.stderr}")
  pure out.stdout

/-- Create the pricing repository; its head commit. The commit is dated and
    signed deterministically, so its id — and every cache keyed by it — is the
    same from one run to the next. -/
def makeRepo (repo : System.FilePath) : IO CommitSha := do
  if ← repo.pathExists then IO.FS.removeDirAll repo
  IO.FS.createDirAll repo
  for (path, content) in pricingProject do IO.FS.writeFile (repo / path) content
  let when := "2026-01-01T00:00:00Z"
  let git (args : Array String) :=
    sh "git" (#["-c", "user.name=example", "-c", "user.email=example@example.com"] ++ args) repo
      #[("GIT_AUTHOR_DATE", some when), ("GIT_COMMITTER_DATE", some when)]
  discard <| git #["init", "-q", "-b", "main"]
  discard <| git #["add", "."]
  discard <| git #["commit", "-q", "-m", "pricing"]
  let head := (← git #["rev-parse", "HEAD"]).trimAscii.toString
  let some sha := CommitSha.ofString? head | throw (IO.userError s!"unexpected commit id {head}")
  pure sha

-- ── The importing program ───────────────────────────────────────────────────

/-- The program that uses the pricing functions: it imports them, binds them
    at their declared types (`definitions`, from `GitFn.definition`), and
    builds, runs, checks and draws the graph. -/
def appMain (definitions : List String) : String := s!"import Linen.Control.Reactive
import Linen.Control.Reactive.Graphviz
import Linen.Graphics.Graphviz.Html
import Pricing

open Control.Reactive Lean

-- The functions from git, at their declared types: compiling these lines
-- re-checks the types, so a changed upstream signature fails this build.
{"\n".intercalate definitions}

/-- The graph's inputs and streams. -/
structure Pipeline where
  price : Subject Nat
  qty : Subject Nat
  total : Observable Nat
  taxed : Observable Nat
  settled : Observable Nat
  shown : Observable String

/-- The pricing pipeline: functions from git and local operators, one graph. -/
def pricing : Reactive Id Json Pipeline := do
  node price ← subject Nat
  node qty ← subject Nat
  node lineTotalFn ← fn lineTotal
  node withTaxFn ← fn withTax
  node labelFn ← fn label
  node total ← lineTotalFn price qty        -- recomputed when either changes
  node taxed ← total.mapFn withTaxFn
  node settled ← taxed.debounceTime 50      -- wait for edits to settle
  node shown ← settled.mapFn labelFn
  pure \{ price, qty, total, taxed, settled, shown }

def main (args : List String) : IO UInt32 := do
  let b := pricing
  let g := b.graph!
  let p := b.result
  -- An order edited over time: 10.00 EUR × 2, then × 3 (quickly), then the
  -- unit price changes to 12.00 EUR.
  let tr := g.run [.next p.price 0 (1000 : Nat), .next p.qty 10 (2 : Nat),
    .next p.qty 20 (3 : Nat), .next p.price 200 (1200 : Nat)]
  for (l, evs) in tr.toList do
    IO.println s!\"  \{displayLabel l}: \{evs.filterMap fun
      | (t, .next v) => some s!\"\{t}→\{v.compress}\"
      | _ => none}\"
  let checks : List (String × Bool) := [
    (\"lineTotal recomputes on either input\",
      tr.values p.total == [(10, 2000), (20, 3000), (200, 3600)]),
    (\"withTax adds 20%\", tr.values p.taxed == [(10, 2400), (20, 3600), (200, 4320)]),
    (\"debounceTime 50 keeps only settled values\", tr.values p.settled == [(70, 3600), (250, 4320)]),
    (\"label formats them\", tr.values p.shown == [(70, \"36.00 EUR\"), (250, \"43.20 EUR\")]),
    (\"no scheduler fault\", tr.faults.isEmpty)]
  for (what, ok) in checks do IO.println s!\"\{if ok then \"PASS\" else \"FAIL\"}  \{what}\"
  if let [out] := args then
    Graphics.Graphviz.Html.writePage out (g.toDotWithTrace tr (·.compress) \"pricing\") \"Pricing, from git\"
    IO.println s!\"the graph and its run: \{out}\"
  pure (if checks.all (·.2) then 0 else 1)
"

/-- The program's lakefile: `linen`, and the vendored package, by path. -/
def appLakefile (linen : System.FilePath) (vendored : Vendored) : String :=
  s!"import Lake\nopen Lake DSL\n\npackage app\n\nrequire linen from {repr linen.toString}\n" ++
  s!"{vendored.requireLean}\n\n@[default_target] lean_exe app where\n  root := `Main\n"

/-- This checkout of `linen` (the example binary is `.lake/build/bin/examples`). -/
def linenDir : IO System.FilePath := do
  let exe ← IO.appPath
  for dir in [exe.parent.bind (·.parent) |>.bind (·.parent) |>.bind (·.parent), some (← IO.currentDir)] do
    if let some d := dir then
      if ← (d / "Linen.lean").pathExists then return ← IO.FS.realPath d
  throw (IO.userError "cannot find the linen checkout (run with `lake exe examples gitfn`)")

-- ── Running it ──────────────────────────────────────────────────────────────

def run (args : List String) : IO Unit := do
  let root := System.FilePath.mk s!"/tmp/linen-example-gitfn"
  let out := match args.dropWhile (· != "--out") with
    | _ :: file :: _ => System.FilePath.mk file
    | _ => root / "pricing.html"

  let repo := root / "pricing"
  let sha ← makeRepo repo
  let desc (name : Lean.Name) (type : String) : GitFn := { repo := repo.toString, commit := sha, name, type }
  let fns := [desc `Pricing.lineTotal "Nat → Nat → Nat", desc `Pricing.withTax "Nat → Nat",
    desc `Pricing.label "Nat → String"]

  IO.println s!"vendoring 3 functions of {repo} at {sha} (secure mode) …"
  let vendored ← match ← vendor { cache := root / "cache" } fns (root / "vendored") with
    | .ok v => pure v
    | .error e => throw (IO.userError e)
  IO.println s!"  package {vendored.package}, modules {vendored.modules}"

  let app := root / "app"
  IO.FS.createDirAll app
  if ← (app / "lake-manifest.json").pathExists then IO.FS.removeFile (app / "lake-manifest.json")
  IO.FS.writeFile (app / "lean-toolchain") (({ cache := root } : Config).toolchain ++ "\n")
  IO.FS.writeFile (app / "lakefile.lean") (appLakefile (← linenDir) vendored)
  IO.FS.writeFile (app / "Main.lean")
    (appMain (fns.map fun fn => fn.definition (.mkSimple fn.name.getString!)))
  IO.println s!"building the program that imports them ({app}) …"
  discard <| sh "lake" #["build"] app

  IO.println "running it:"
  let child ← IO.Process.spawn
    { cmd := (app / ".lake" / "build" / "bin" / "app").toString, args := #[out.toString],
      env := cleanEnv }
  let code ← child.wait
  unless code == 0 do throw (IO.userError s!"the program's checks failed ({code})")

end Examples.GitFnGraph
