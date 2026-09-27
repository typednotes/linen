/-
  `System.GitFn.Build` — fetching, checking and compiling a `GitFn`, securely

  Secure mode is the only mode:

  1. **Fetch** the repository at the descriptor's commit — `git fetch --depth 1`
     of that SHA into a cache directory, with hooks disabled and no
     submodules — and check that `HEAD` is that commit. Nothing from the
     repository runs: not its lakefile, not its build scripts, not its
     toolchain.
  2. **Check** the project's sources with `System.GitFn.Policy`, before
     compiling anything, and keep the admitted modules only.
  3. **Compile** them with the **host's** toolchain (the Lean version this code
     was compiled with, pinned in a generated `lean-toolchain`) in a package
     this module generates, whose only dependencies are the libraries you
     select (e.g. linen, required by path). Its build runs a generated
     **semantic check** of the compiled result: the declared type is
     definitionally equal to the constant's (no coercion); no remote constant
     the function depends on is `unsafe`, `partial`, `extern`, or
     `implemented_by`, or refers to `panic`/`dbgTrace`; no remote constant runs
     at load time (`[init]`); the function's axioms are only `propext`,
     `Classical.choice` and `Quot.sound`.
  4. **Serve** it: a **worker** executable, speaking JSON-RPC 2.0 (Lean core's
     `Lean.JsonRpc`) over stdio, or over HTTP (`POST /call`, `GET /health`,
     core `Std.Http`). Arguments and results are JSON (Lean core's
     `ToJson`/`FromJson`): a function whose types lack them does not build.

  `vendor` is the static alternative: the admitted, checked sources in a
  generated package your project `require`s by path — the remote lakefile
  still never runs.

  Timeouts and OS sandboxing are not provided here: a trusted elaborator
  that runs forever makes `build` wait forever.
-/
import Linen.System.GitFn.Descriptor
import Linen.System.GitFn.Policy

namespace System.GitFn

open Lean (Name)

-- ── Configuration ───────────────────────────────────────────────────────────

/-- A library remote code may use: a Lake package, required by path, and the
    module prefixes it provides. -/
structure Library where
  /-- Its Lake package name (e.g. `linen`). -/
  package : Name
  /-- Its directory. -/
  path : System.FilePath
  /-- The module prefixes it provides (e.g. `Linen`). -/
  prefixes : List Name
  deriving Repr

/-- linen itself, at `path`, as a selectable library. -/
def Library.linen (path : System.FilePath) : Library := ⟨`linen, path, [`Linen]⟩

/-- Where and how to build. -/
structure Config where
  /-- Where checkouts and generated packages are kept. -/
  cache : System.FilePath
  /-- What secure mode admits (the libraries' prefixes are added to it). -/
  policy : Policy := {}
  /-- The libraries remote code may use. -/
  libraries : List Library := []
  /-- The `git` executable. -/
  git : String := "git"
  /-- The `lake` executable (it follows the generated `lean-toolchain`). -/
  lake : String := "lake"
  /-- Where the libraries' compiled modules are, for parsing their syntax
      (typically `LEAN_PATH`'s entries). -/
  searchPath : List System.FilePath := []
  /-- The toolchain the generated packages pin: the host's. -/
  toolchain : String := s!"leanprover/lean4:v{Lean.versionString}"

/-- The policy in force: the configured one, allowing the libraries' prefixes. -/
def Config.effectivePolicy (cfg : Config) : Policy :=
  cfg.policy.allowing (cfg.libraries.flatMap (·.prefixes))

-- ── Processes ───────────────────────────────────────────────────────────────

/-- Environment variables a host run under Lake would leak into a nested build. -/
def cleanEnv : Array (String × Option String) :=
  #[("LEAN_PATH", none), ("LEAN_SRC_PATH", none), ("LAKE", none), ("LAKE_HOME", none),
    ("LEAN_SYSROOT", none), ("LEAN_GITHASH", none), ("LAKE_PKG_URL_MAP", none),
    ("ELAN_TOOLCHAIN", none)]

/-- Run a command; its output, or why it failed. -/
def run (cmd : String) (args : Array String) (cwd : System.FilePath) : IO (Except String String) := do
  let out ← IO.Process.output { cmd, args, cwd, env := cleanEnv }
  if out.exitCode == 0 then return .ok out.stdout
  return .error s!"`{cmd} {" ".intercalate args.toList}` failed ({out.exitCode}):\n{out.stdout}{out.stderr}"

-- ── Fetching ────────────────────────────────────────────────────────────────

/-- Git options that keep a checkout inert: no hooks, no submodules. -/
def gitSafety : Array String := #["-c", "core.hooksPath=/dev/null", "-c", "advice.detachedHead=false"]

/-- The repository at the descriptor's commit, in the cache (reused if present). -/
def fetch (cfg : Config) (fn : GitFn) : IO (Except String System.FilePath) := do
  let dir := cfg.cache / "src" / fn.commit.hex
  if ← (dir / ".git").pathExists then
    if let .ok head := ← run cfg.git (gitSafety ++ #["rev-parse", "HEAD"]) dir then
      if head.trimAscii.toString == fn.commit.hex then return .ok dir
  if ← dir.pathExists then IO.FS.removeDirAll dir
  IO.FS.createDirAll dir
  let steps : List (Array String) :=
    [#["init", "-q"],
     gitSafety ++ #["fetch", "-q", "--depth", "1", "--no-recurse-submodules", fn.repo, fn.commit.hex],
     gitSafety ++ #["checkout", "-q", "--detach", "FETCH_HEAD"]]
  for args in steps do
    if let .error e := ← run cfg.git args dir then return .error e
  match ← run cfg.git (gitSafety ++ #["rev-parse", "HEAD"]) dir with
  | .ok head =>
    if head.trimAscii.toString == fn.commit.hex then return .ok dir
    return .error s!"the checkout is at {head.trimAscii}, not {fn.commit}"
  | .error e => return .error e

-- ── Generated sources ───────────────────────────────────────────────────────

/-- A name as Lean source (`Name.toString` escapes what needs escaping). -/
def nameSource (n : Name) : String := n.toString

/-- A name as a Lean name literal. -/
def nameLiteral (n : Name) : String := "`" ++ nameSource n

/-- The generated lakefile: the admitted modules as a library, the selected
    libraries as path dependencies, and — for a worker — the check library and
    the worker executable as default targets. -/
def lakefileSource (pkg : String) (libraries : List Library) (modules : List Name)
    (worker : Bool) : String :=
  let requires := libraries.map fun l =>
    s!"require «{l.package}» from {repr l.path.toString}\n"
  let roots := ", ".intercalate (modules.map nameLiteral)
  s!"import Lake\nopen Lake DSL\n\npackage «{pkg}»\n\n" ++ String.join requires ++
  s!"\n@[default_target] lean_lib Remote where\n  srcDir := \"remote\"\n  roots := #[{roots}]\n\n" ++
  "@[default_target] lean_lib LinenGitFnCheck where\n  roots := #[`LinenGitFnCheck]\n" ++
  (if worker then
    "\n@[default_target] lean_exe «linen-gitfn-worker» where\n  root := `LinenGitFnWorker\n"
  else "")

/-- The generated semantic check, run when the package is built: each
    function is defined by the admitted modules at its declared type, and
    everything it reaches there is safe (no `unsafe`, `extern`,
    `implemented_by`, `partial`, or side-effecting primitives), no admitted
    module runs code when loaded, and it uses only the standard axioms. -/
def checkModuleSource (fns : List GitFn) (modules : List Name) : String :=
  let imports := String.join (modules.map (s!"import {nameSource ·}\n"))
  let remote := ", ".intercalate (modules.map nameLiteral)
  let claims := String.join <| fns.zipIdx.map fun (fn, i) =>
    s!"\nabbrev LinenGitFnExpected{i} := ({fn.type})\n" ++
    s!"#eval linenGitFnCheck {nameLiteral fn.name} ``LinenGitFnExpected{i}\n"
  s!"import Lean\n{imports}open Lean Meta Elab Command\n\n" ++
  "/-- Check that `target` is defined by the admitted modules at the type\n" ++
  "    `expected` abbreviates, and is safe. -/\n" ++
  "def linenGitFnCheck (target expected : Name) : CommandElabM Unit := do\n" ++
  "  let env ← getEnv\n" ++
  s!"  let remoteModules : List Name := [{remote}]\n" ++
  "  let some info := env.find? target\n" ++
  "    | throwError \"`{target}` is not defined by the admitted modules\"\n" ++
  "  let some e := env.find? expected | throwError \"internal: no expected type\"\n" ++
  "  unless ← liftTermElabM (Meta.isDefEq info.type e.value!) do\n" ++
  "    throwError m!\"`{target}` has type{indentExpr info.type}\\nbut the descriptor declares{indentExpr e.value!}\"\n" ++
  "  let isRemote (n : Name) : Bool := match env.getModuleIdxFor? n with\n" ++
  "    | some i => remoteModules.contains (env.header.moduleNames[i.toNat]!)\n" ++
  "    | none => false\n" ++
  "  let sideEffects : List Name := [``panicCore, ``dbgTrace, ``dbgTraceIfShared, ``dbgSleep]\n" ++
  "  let mut seen : NameSet := {}\n" ++
  "  let mut todo : List Name := [target]\n" ++
  "  while !todo.isEmpty do\n" ++
  "    let n := todo.head!\n" ++
  "    todo := todo.tail!\n" ++
  "    if seen.contains n then continue\n" ++
  "    seen := seen.insert n\n" ++
  "    let some ci := env.find? n | continue\n" ++
  "    if isRemote n then\n" ++
  "      if ci.isUnsafe then throwError \"`{n}` is unsafe\"\n" ++
  "      if Lean.isExtern env n then throwError \"`{n}` is extern\"\n" ++
  "      if (Lean.Compiler.implementedByAttr.getParam? env n).isSome then\n" ++
  "        throwError \"`{n}` has an implementation other than its definition\"\n" ++
  "      if let .defnInfo v := ci then\n" ++
  "        if v.safety == .partial then throwError \"`{n}` is partial\"\n" ++
  "      let used := ci.type.getUsedConstants ++ (ci.value?.map (·.getUsedConstants)).getD #[]\n" ++
  "      for s in sideEffects do\n" ++
  "        if used.contains s then throwError \"`{n}` uses `{s}`\"\n" ++
  "      todo := used.toList ++ todo\n" ++
  "  for (n, _) in env.constants.map₁.toList do\n" ++
  "    if isRemote n && (Lean.isIOUnitInitFn env n || Lean.hasInitAttr env n) then\n" ++
  "      throwError \"`{n}` runs when its module is loaded\"\n" ++
  "  let axioms ← liftCoreM (collectAxioms target)\n" ++
  "  let bad := axioms.filter (!#[``propext, ``Classical.choice, ``Quot.sound].contains ·)\n" ++
  "  unless bad.isEmpty do throwError \"`{target}` depends on the axioms {bad.toList}\"\n" ++
  claims

/-- The generated worker: JSON-RPC 2.0 requests `call` in, the function
    applied, responses out — over stdio (one message per line; the
    notification `exit` ends it), or over HTTP with `--http PORT`. -/
def workerSource (fn : GitFn) (modules : List Name) : String :=
  let imports := String.join (modules.map (s!"import {nameSource ·}\n"))
  s!"import Lean.Data.JsonRpc\nimport Std.Http\n{imports}open Lean JsonRpc\n\n" ++
  "/-- A function over JSON-convertible types, called with JSON arguments.\n" ++
  "    Arguments need `FromJson`; the result needs `ToJson`. -/\n" ++
  "class LinenGitFnCall (F : Type) where\n" ++
  "  call : F → List Json → IO (Except (ErrorCode × String) Json)\n\n" ++
  "def linenGitFnArity {α : Type} : Except (ErrorCode × String) α :=\n" ++
  "  .error (.invalidParams, \"wrong number of arguments\")\n\n" ++
  "instance (priority := low) {β : Type} [ToJson β] : LinenGitFnCall β where\n" ++
  "  call b args := pure (if args.isEmpty then .ok (toJson b) else linenGitFnArity)\n" ++
  "instance {β : Type} [ToJson β] : LinenGitFnCall (Except String β) where\n" ++
  "  call r args := pure <| if !args.isEmpty then linenGitFnArity else match r with\n" ++
  "    | .ok b => .ok (toJson b)\n" ++
  "    | .error e => .error (.internalError, e)\n" ++
  "instance {β : Type} [ToJson β] : LinenGitFnCall (IO β) where\n" ++
  "  call act args := if !args.isEmpty then pure linenGitFnArity else\n" ++
  "    try pure (.ok (toJson (← act))) catch e => pure (.error (.internalError, toString e))\n" ++
  "instance {α F : Type} [FromJson α] [LinenGitFnCall F] : LinenGitFnCall (α → F) where\n" ++
  "  call f args := match args with\n" ++
  "    | a :: rest => match fromJson? a with\n" ++
  "      | .ok x => LinenGitFnCall.call (f x) rest\n" ++
  "      | .error e => pure (.error (.invalidParams, e))\n" ++
  "    | [] => pure linenGitFnArity\n\n" ++
  s!"def linenGitFnEntry : ({fn.type}) := @{nameSource fn.name}\n\n" ++
  "/-- Answer one message: a response, or nothing (a notification). -/\n" ++
  "def linenGitFnHandle (line : String) : IO (Option Message) := do\n" ++
  "  match (Json.parse line >>= fromJson? : Except String Message) with\n" ++
  "  | .error e => pure (some (.responseError .null .parseError e none))\n" ++
  "  | .ok (.request id \"call\" params) =>\n" ++
  "    let args := match params with\n" ++
  "      | some (.arr a) => a.toList\n" ++
  "      | _ => []\n" ++
  "    match ← LinenGitFnCall.call linenGitFnEntry args with\n" ++
  "    | .ok v => pure (some (.response id v))\n" ++
  "    | .error (code, e) => pure (some (.responseError id code e none))\n" ++
  "  | .ok (.request id m _) =>\n" ++
  "    pure (some (.responseError id .methodNotFound s!\"no method `{m}`\" none))\n" ++
  "  | .ok _ => pure none\n\n" ++
  "open Std Async Http Server in\nstructure LinenGitFnServer\n\n" ++
  "open Std Async Http Server in\ninstance : Handler LinenGitFnServer where\n" ++
  "  onRequest _ req := do\n" ++
  "    let path := toString req.line.uri\n" ++
  "    if req.line.method == Std.Http.Method.post && path == \"/call\" then\n" ++
  "      let body : String ← req.body.readAll (maximumSize := some (64 * 1024 * 1024))\n" ++
  "      match ← linenGitFnHandle body with\n" ++
  "      | some reply => Response.ok |>.json (toJson reply).compress\n" ++
  "      | none => Response.new |>.status .noContent |>.text \"\"\n" ++
  "    else if path == \"/health\" then\n" ++
  "      Response.ok |>.json \"{\\\"ok\\\":true}\"\n" ++
  "    else\n" ++
  "      Response.new |>.status .notFound |>.text \"not found\"\n\n" ++
  "def main (args : List String) : IO Unit := do\n" ++
  "  match args with\n" ++
  "  | [\"--http\", port] =>\n" ++
  "    Std.Async.Async.block do\n" ++
  "      let addr : Std.Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, (port.toNat?.getD 0).toUInt16⟩\n" ++
  "      let server ← Std.Http.Server.serve addr LinenGitFnServer.mk\n" ++
  "      let actual := match server.localAddr with\n" ++
  "        | some (.v4 a) => a.port.toNat\n" ++
  "        | _ => 0\n" ++
  "      IO.println (Json.mkObj [(\"listening\", toJson actual)]).compress\n" ++
  "      (← IO.getStdout).flush\n" ++
  "      server.waitShutdown\n" ++
  "  | _ =>\n" ++
  "    let stdin ← IO.getStdin\n" ++
  "    let stdout ← IO.getStdout\n" ++
  "    repeat\n" ++
  "      let line ← stdin.getLine\n" ++
  "      if line.isEmpty then break                 -- end of input\n" ++
  "      match (Json.parse line >>= fromJson? : Except String Message) with\n" ++
  "      | .ok (Message.notification \"exit\" _) => break\n" ++
  "      | _ =>\n" ++
  "        if let some reply ← linenGitFnHandle line.trimAscii.toString then\n" ++
  "          stdout.putStrLn (toJson reply).compress\n" ++
  "          stdout.flush\n"

-- ── Staging and building ────────────────────────────────────────────────────

/-- A checked checkout: where the project is, its report, its admitted files. -/
structure Checked where
  /-- The project directory inside the checkout. -/
  project : System.FilePath
  /-- The policy's report on it. -/
  report : ProjectReport
  /-- The admitted modules and their files. -/
  admitted : List (Name × System.FilePath)

/-- Fetch and check a descriptor's project. -/
def fetchAndCheck (cfg : Config) (fn : GitFn) : IO (Except String Checked) := do
  if let .error e := fn.validate then return .error e
  let src ← match ← fetch cfg fn with
    | .ok d => pure d
    | .error e => return .error e
  let comps ← match projectComponents fn.project with
    | .ok c => pure c
    | .error e => return .error e
  let project := comps.foldl (· / ·) src
  unless ← project.isDir do return .error s!"project `{fn.project}` is not a directory of {fn.repo}"
  let report ← checkProject cfg.effectivePolicy cfg.searchPath project
  if report.admitted.isEmpty then
    return .error s!"no module of {fn.repo} passes the secure policy:\n{reprStr report.excluded}"
  let sources ← projectSources project
  pure (.ok ⟨project, report, sources.toList.filter (report.admitted.contains ·.1)⟩)

/-- Write a generated package: the admitted sources under `remote/`, the
    lakefile, the toolchain, the check and (for a worker) the worker. -/
def writePackage (cfg : Config) (dir : System.FilePath) (pkg : String) (fns : List GitFn)
    (checked : Checked) (worker : Option GitFn) : IO Unit := do
  if ← dir.pathExists then IO.FS.removeDirAll dir
  IO.FS.createDirAll (dir / "remote")
  let rootComps := checked.project.normalize.components
  for (_, file) in checked.admitted do
    let rel := file.normalize.components.drop rootComps.length
    let target := rel.foldl (· / ·) (dir / "remote")
    if let some parent := target.parent then IO.FS.createDirAll parent
    IO.FS.writeFile target (← IO.FS.readFile file)
  let modules := checked.admitted.map (·.1)
  IO.FS.writeFile (dir / "lean-toolchain") (cfg.toolchain ++ "\n")
  IO.FS.writeFile (dir / "lakefile.lean") (lakefileSource pkg cfg.libraries modules worker.isSome)
  IO.FS.writeFile (dir / "LinenGitFnCheck.lean") (checkModuleSource fns modules)
  if let some fn := worker then
    IO.FS.writeFile (dir / "LinenGitFnWorker.lean") (workerSource fn modules)

/-- A cache key for a build: everything that determines it. -/
def buildKey (cfg : Config) (fn : GitFn) (what : String) : String :=
  -- `what` includes the generated sources, so a change to the worker's
  -- protocol never reuses a worker built from an older generator.
  let h := hash (reprStr fn ++ what ++ cfg.toolchain ++ reprStr cfg.effectivePolicy ++
    reprStr cfg.libraries)
  s!"{fn.commit.hex.take 12}-{h.toNat}"

/-- A built worker. -/
structure Built where
  /-- The function. -/
  fn : GitFn
  /-- The generated package. -/
  dir : System.FilePath
  /-- The worker executable. -/
  exe : System.FilePath
  /-- The policy's report on the project. -/
  report : ProjectReport

/-- Fetch, check and compile a descriptor into a worker executable (cached by
    everything that determines it). Fails with the reason: a malformed
    descriptor, a failed fetch, a policy rejection, or a failed build — whose
    log includes the semantic check's verdict or a missing JSON instance. -/
def build (cfg : Config) (fn : GitFn) : IO (Except String Built) := do
  let checked ← match ← fetchAndCheck cfg fn with
    | .ok c => pure c
    | .error e => return .error e
  let modules := checked.admitted.map (·.1)
  let dir := cfg.cache / "build" / buildKey cfg fn ("worker" ++ workerSource fn modules ++
    checkModuleSource [fn] modules ++ lakefileSource "linen-gitfn" cfg.libraries modules true)
  let exe := dir / ".lake" / "build" / "bin" / "linen-gitfn-worker"
  unless ← exe.pathExists do
    writePackage cfg dir "linen-gitfn" [fn] checked (some fn)
    if let .error e := ← run cfg.lake #["build"] dir then
      return .error s!"{e}\nexcluded modules: {reprStr checked.report.excluded}"
  pure (.ok ⟨fn, dir, exe, checked.report⟩)

/-- A definition binding a function under `localName` at its declared type,
    for a project that imports it (`vendor`): compiling it re-checks the type
    there, so a changed upstream signature fails at your build. -/
def GitFn.definition (fn : GitFn) (localName : Name) : String :=
  s!"def {nameSource localName} : ({fn.type}) := @{nameSource fn.name}"

/-- A vendored package: checked sources your project can `require`. -/
structure Vendored where
  /-- Its directory. -/
  dir : System.FilePath
  /-- Its package name. -/
  package : String
  /-- The line to add to your `lakefile.lean`. -/
  requireLean : String
  /-- The `[[require]]` table to add to your `lakefile.toml`. -/
  requireToml : String
  /-- The modules to import. -/
  modules : List Name

/-- Static mode: vendor the admitted, checked sources of a project into a
    package at `into`, and build it there, running the semantic check of each
    function (`fns` all name the same repository, commit and project). Your
    project then `require`s it by path and imports its functions as ordinary
    Lean definitions, compiled into your program; the remote lakefile never
    runs. -/
def vendor (cfg : Config) (fns : List GitFn) (into : System.FilePath) :
    IO (Except String Vendored) := do
  let some fn := fns.head? | return .error "nothing to vendor"
  if let some other := fns.find? (fun g => g.repo != fn.repo || g.commit != fn.commit ||
      g.project != fn.project) then
    return .error s!"`{other.name}` is not from {fn.repo} at {fn.commit}, project `{fn.project}`"
  for g in fns do
    if let .error e := g.validate then return .error e
  let checked ← match ← fetchAndCheck cfg fn with
    | .ok c => pure c
    | .error e => return .error e
  let pkg := s!"gitfn-{fn.commit.hex.take 12}"
  writePackage cfg into pkg fns checked none
  if let .error e := ← run cfg.lake #["build"] into then
    return .error s!"{e}\nexcluded modules: {reprStr checked.report.excluded}"
  let dir ← IO.FS.realPath into
  pure <| .ok {
    dir, package := pkg, modules := checked.admitted.map (·.1)
    requireLean := s!"require «{pkg}» from {repr dir.toString}"
    requireToml := s!"[[require]]\nname = {repr pkg}\npath = {repr dir.toString}" }

end System.GitFn
