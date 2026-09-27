/-
  `System.GitFn.Policy` — secure mode's check, before anything is compiled

  Compiling Lean runs code: elaboration executes macros, elaborators,
  tactics and `#eval`. So remote sources are checked **before** they reach a
  compiler, by parsing them with the host's own parser (parsing runs no code
  from the files parsed) and admitting only a small, safe subset of Lean:

  - **imports**: the project's own modules, and module prefixes the policy
    allows (by default `Init`, `Std` and `Lean.Data.Json`; add the libraries
    you select, e.g. `Linen`);
  - **commands**: plain declarations (`def`, `theorem`, `abbrev`, `instance`,
    `structure`, `inductive`, `class`, `example`), `namespace`/`section`/
    `end`, `open`, `variable`, `universe`, `mutual`, `deriving instance`,
    module docs, and `set_option`/`attribute` restricted to allowlists —
    anything else, notably `#eval`, `run_cmd`, `macro`, `syntax`, `elab`,
    `notation` and `initialize`, is rejected;
  - **nowhere**: `unsafe`, `partial` (its compiled body is not kernel-checked),
    `opaque`, `axiom`, `sorry`, `native_decide` and `decide +native`,
    `run_tac`, `by_elab`, `include_str`, `panic!`/`assert!`/`unreachable!`/
    `dbg_trace`, the attributes `extern`, `implemented_by`, `export`, `init`,
    and identifiers naming side effects outside `IO` (`panic`, `dbgTrace…`,
    `unsafeBaseIO`, `unsafeCast`, `ptrAddrUnsafe`, `ofReduceBool`, …).

  With this, the only code that runs while the remote sources are compiled
  is the trusted elaborators of the allowed libraries, and the remote code
  itself can only affect the world through a monad like `IO` visible in its
  type. `System.GitFn.Build` then checks the compiled result again
  (unsafe/partial/extern/implemented_by constants, axioms, initializers, the
  declared type) — defense in depth.

  What this cannot rule out is a bug in a trusted elaborator, or a slow one
  (a `simp` that takes an hour); the build step is where to add a timeout or
  an OS sandbox.
-/
import Lean

namespace System.GitFn

open Lean

-- ── The policy ──────────────────────────────────────────────────────────────

/-- What secure mode admits. -/
structure Policy where
  /-- Module prefixes that may be imported besides the project's own modules. -/
  imports : List Name := [`Init, `Std, `Lean.Data.Json]
  /-- Attributes that may be used. -/
  attributes : List String :=
    ["simp", "inline", "noinline", "specialize", "reducible", "irreducible", "semireducible",
     "instance", "default_instance", "match_pattern", "macro_inline", "ext", "local", "scoped",
     "csimp", "refl", "symm", "trans", "coe"]
  /-- Options (or option prefixes, ending in `.`) that `set_option` may set. -/
  options : List String :=
    ["maxHeartbeats", "maxRecDepth", "autoImplicit", "relaxedAutoImplicit",
     "synthInstance.maxHeartbeats", "synthInstance.maxSize", "linter.", "pp."]
  deriving Repr, Inhabited

/-- The policy with more allowed import prefixes (the selected libraries). -/
def Policy.allowing (p : Policy) (prefixes : List Name) : Policy :=
  { p with imports := p.imports ++ prefixes }

/-- The commands admitted, by syntax kind. -/
def allowedCommands : List Name :=
  [``Parser.Command.declaration, ``Parser.Command.namespace, ``Parser.Command.section,
   ``Parser.Command.end, ``Parser.Command.open, ``Parser.Command.variable,
   ``Parser.Command.universe, ``Parser.Command.mutual, ``Parser.Command.moduleDoc,
   ``Parser.Command.deriving, ``Parser.Command.set_option, ``Parser.Command.attribute,
   ``Parser.Command.eoi]

/-- The declarations admitted, by syntax kind. -/
def allowedDeclarations : List Name :=
  [``Parser.Command.definition, ``Parser.Command.theorem, ``Parser.Command.abbrev,
   ``Parser.Command.instance, ``Parser.Command.structure, ``Parser.Command.inductive,
   ``Parser.Command.classInductive, ``Parser.Command.example]

/-- Keywords and tactic names rejected wherever they appear. -/
def forbiddenAtoms : List String :=
  ["unsafe", "partial", "opaque", "axiom", "sorry", "admit", "native_decide", "run_tac",
   "by_elab", "include_str", "panic!", "assert!", "unreachable!", "dbg_trace",
   "implemented_by", "extern", "export", "init", "builtin_init", "initialize",
   "builtin_initialize"]

/-- Identifiers (last component) rejected wherever they appear: side effects
    outside `IO`, and escape hatches from the kernel. -/
def forbiddenIdents : List String :=
  ["panic", "panicCore", "panicWithPos", "panicWithPosWithDecl", "dbgTrace", "dbgTraceIfShared",
   "dbgTraceVal", "dbgSleep", "dbgStackTrace", "unsafeBaseIO", "unsafeIO", "unsafeEIO",
   "unsafeCast", "ptrAddrUnsafe", "ptrEq", "withPtrEq", "ofReduceBool", "reduceBool", "sorryAx"]

-- ── Checking syntax ─────────────────────────────────────────────────────────

/-- The first atom or identifier in `stx`, as text. -/
def firstToken : Syntax → Option String
  | .atom _ v => some v
  | .ident _ _ n _ => some n.toString
  | .node _ _ args => args.attach.toList.findSome? fun ⟨a, _⟩ => firstToken a
  | .missing => none

/-- What is wrong with one syntax node (not its children). -/
def nodeProblems (p : Policy) (kind : Name) (args : Array Syntax) : List String :=
  if kind == ``Parser.Term.attrInstance then
    match (args[1]?).bind firstToken with
    | some a => if p.attributes.contains a then [] else [s!"attribute `{a}` is not allowed"]
    | none => []
  else if kind.toString.endsWith "set_option" then
    match (args[1]?).bind firstToken with
    | some o =>
      if p.options.any (fun q => if q.endsWith "." then o.startsWith q else o == q) then []
      else [s!"option `{o}` is not allowed"]
    | none => []
  else if kind.toString.endsWith "ConfigItem" then
    if args.any (fun a => firstToken a == some "native") then ["`+native` is not allowed"] else []
  else []

/-- Everything wrong with a syntax tree. -/
def syntaxProblems (p : Policy) : Syntax → List String
  | .node _ kind args =>
    nodeProblems p kind args ++ args.attach.toList.flatMap fun ⟨a, _⟩ => syntaxProblems p a
  | .atom _ v => if forbiddenAtoms.contains v then [s!"`{v}` is not allowed"] else []
  | .ident _ _ n _ =>
    match n with
    | .str _ s => if forbiddenIdents.contains s then [s!"`{n}` is not allowed"] else []
    | _ => []
  | .missing => []

/-- What is wrong with one command. `c₁ in c₂` (e.g. `set_option … in def …`)
    is checked as its two commands. -/
def commandProblems (p : Policy) (cmd : Syntax) : List String :=
  let kind := cmd.getKind
  if kind == ``Parser.Command.in then
    match cmd with
    | .node _ _ args =>
      args.attach.toList.flatMap fun ⟨a, _⟩ =>
        if a.isAtom then [] else commandProblems p a
    | _ => []
  else if !allowedCommands.contains kind then
    [s!"command `{(firstToken cmd).getD kind.toString}` is not allowed"]
  else
    let decl : List String :=
      if kind == ``Parser.Command.declaration then
        let k := cmd[1].getKind
        if allowedDeclarations.contains k then []
        else [s!"declaration `{(firstToken cmd[1]).getD k.toString}` is not allowed"]
      else []
    decl ++ syntaxProblems p cmd
termination_by cmd

-- ── Checking a file ─────────────────────────────────────────────────────────

/-- Parse every command of a file (after its header). Each command must
    advance the parser; the recursion is on the input left. -/
def parseCommands (ictx : Parser.InputContext) (pmctx : Parser.ParserModuleContext)
    (st : Parser.ModuleParserState) (msgs : MessageLog) (acc : Array Syntax) :
    Array Syntax × MessageLog :=
  let (cmd, st', msgs') := Parser.parseCommand ictx pmctx st msgs
  if cmd.isOfKind ``Parser.Command.eoi then (acc, msgs')
  else if _h : st.pos.byteIdx < st'.pos.byteIdx ∧ st'.pos.byteIdx ≤ ictx.inputString.utf8ByteSize then
    parseCommands ictx pmctx st' msgs' (acc.push cmd)
  else (acc.push cmd, msgs')
termination_by ictx.inputString.utf8ByteSize + 1 - st.pos.byteIdx
decreasing_by omega

/-- The check of one file. -/
structure FileReport where
  /-- The module the file is. -/
  module : Name
  /-- Its imports. -/
  imports : Array Name
  /-- What is wrong with it (empty: admitted, if its imports are). -/
  problems : List String
  deriving Repr, Inhabited

/-- Whether `m` may be imported: a project module, or under an allowed prefix. -/
def importAllowed (p : Policy) (project : List Name) (m : Name) : Bool :=
  project.contains m || p.imports.any (·.isPrefixOf m)

/-- Check one file's source. `envFor` gives the environment to parse with,
    from the file's external imports (for their syntax); project modules add
    none, since they may not define syntax. -/
def checkSource (p : Policy) (project : List Name) (envFor : Array Name → IO Environment)
    (module : Name) (src : String) : IO FileReport := do
  let ictx := Parser.mkInputContext src module.toString
  let (hdr, st, msgs) ← Parser.parseHeader ictx
  let imports := (Elab.headerToImports hdr).map (·.module) |>.toList.eraseDups.toArray
  let badImports := imports.toList.filter (!importAllowed p project ·)
  if !badImports.isEmpty then
    return ⟨module, imports, badImports.map (s!"import `{·}` is not allowed")⟩
  let env ← envFor (imports.filter (!project.contains ·))
  let (cmds, msgs) := parseCommands ictx { env, options := {} } st msgs #[]
  let parseErrors ← msgs.toList.filterMapM fun m => do
    if m.severity == .error then return some s!"does not parse: {← m.data.toString}" else return none
  pure ⟨module, imports, parseErrors ++ cmds.toList.flatMap (commandProblems p)⟩

-- ── Checking a project ──────────────────────────────────────────────────────

/-- The check of a project: every file's report, and the modules admitted. -/
structure ProjectReport where
  /-- Every file checked. -/
  files : Array FileReport
  /-- The modules admitted: problem-free, importing only admitted project
      modules and allowed libraries. -/
  admitted : List Name
  deriving Repr

/-- The modules not admitted, each with why. -/
def ProjectReport.excluded (r : ProjectReport) : List (Name × List String) :=
  r.files.toList.filterMap fun f =>
    if r.admitted.contains f.module then none
    else some (f.module, if f.problems.isEmpty then ["imports a module that is not admitted"]
      else f.problems)

/-- Admit the problem-free files whose project imports are all admitted: a
    greatest fixpoint, reached in at most as many rounds as there are files. -/
def admit (project : List Name) (files : Array FileReport) : List Name :=
  let start := files.toList.filterMap fun f => if f.problems.isEmpty then some f.module else none
  (List.range files.size).foldl (fun ok _ =>
    ok.filter fun m => match files.find? (·.module == m) with
      | some f => f.imports.all fun i => !project.contains i || ok.contains i
      | none => false) start

/-- The module a source file is, from its path relative to the project root
    (`A/B.lean` ↦ `A.B`), if it is a Lean source outside `.lake`. -/
def moduleOfPath (rel : List String) : Option Name :=
  match rel.getLast? with
  | some file =>
    if !file.endsWith ".lean" || rel.any (·.startsWith ".") || rel == ["lakefile.lean"] then none
    else
      let comps := rel.dropLast ++ [(file.dropEnd 5).toString]
      if comps.any (·.isEmpty) then none
      else some (comps.foldl Name.str .anonymous)
  | none => none

/-- Every Lean source of the project rooted at `root`, with its module. -/
def projectSources (root : System.FilePath) : IO (Array (Name × System.FilePath)) := do
  let files ← root.walkDir (enter := fun p => pure !(p.fileName.getD "").startsWith ".")
  let rootComps := root.normalize.components
  pure <| files.filterMap fun f =>
    let rel := f.normalize.components.drop rootComps.length
    (moduleOfPath rel).map (·, f)

/-- Loading an environment with its syntax extensions runs the imported
    modules' initializers, which Lean guards with the `unsafe`
    `enableInitializersExecution`. -/
private unsafe def libraryEnvironmentImpl (searchPath : List System.FilePath)
    (imports : Array Name) : IO Environment := do
  initSearchPath (← findSysroot) searchPath
  enableInitializersExecution
  importModules (loadExts := true) (imports.map ({ module := · })) {}

/-- Load the environment imports need, for parsing (their syntax extensions).
    Only **trusted library** modules are ever imported here — the imports a
    file may have besides project modules, which the policy allows — never a
    project module, so no remote code runs. (Implemented with Lean's `unsafe`
    `enableInitializersExecution`, as Lean's own frontend is.) -/
@[implemented_by libraryEnvironmentImpl]
opaque libraryEnvironment (searchPath : List System.FilePath) (imports : Array Name) :
    IO Environment

/-- Check every source of the project rooted at `root`. -/
def checkProject (p : Policy) (searchPath : List System.FilePath) (root : System.FilePath) :
    IO ProjectReport := do
  let sources ← projectSources root
  let project := sources.toList.map (·.1)
  let cache ← IO.mkRef (∅ : Std.HashMap (List Name) Environment)
  let envFor (imports : Array Name) : IO Environment := do
    let key := imports.toList
    if let some env := (← cache.get)[key]? then return env
    let env ← libraryEnvironment searchPath imports
    cache.modify (·.insert key env)
    pure env
  let files ← sources.mapM fun (m, f) => do checkSource p project envFor m (← IO.FS.readFile f)
  pure ⟨files, admit project files⟩

end System.GitFn
