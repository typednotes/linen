/-
  Tests for `Linen.System.GitFn.Policy`: secure mode's source check.

  Each forbidden construct is shown rejected, with its message, and a
  realistic safe file admitted; then imports, module paths and the admission
  fixpoint. Sources are parsed with this file's environment (`checkProject`,
  which loads library environments itself, is exercised by the integration
  test).
-/
import Linen.System.GitFn.Policy

open Lean Elab Command System.GitFn

namespace Tests.System.GitFn.Policy

/-- The problems `src` has as module `M.A` of a project `[M.A, M.B]`. -/
def problemsOf (src : String) (p : Policy := {}) : CommandElabM (List String) := do
  let env ← getEnv
  let r ← checkSource p [`M.A, `M.B] (fun _ => pure env) `M.A src
  pure r.problems

/-- Assert the problems of `src`. -/
def expect (src : String) (expected : List String) (p : Policy := {}) : CommandElabM Unit := do
  let got ← problemsOf src p
  unless got == expected do throwError "for {repr src}:\n  expected {expected}\n  got {got}"

-- A safe file is admitted.
#eval expect "import Std.Data.HashMap
import M.B

namespace M
/-- Twice. -/
@[simp] def twice (n : Nat) : Nat := n + n
theorem twice_zero : twice 0 = 0 := by decide
structure P where
  x : Nat
  deriving Repr
instance : Inhabited P := ⟨⟨0⟩⟩
def g : IO Unit := IO.println \"effects are fine in IO\"
set_option maxHeartbeats 1000 in
example : 1 + 1 = 2 := rfl
end M
" []

-- Commands that run code at compile time are rejected.
#eval expect "#eval IO.println \"hi\"" ["command `#eval` is not allowed"]
#eval expect "macro \"m\" : term => `(1)" ["command `macro` is not allowed"]
#eval expect "initialize r : IO.Ref Nat ← IO.mkRef 0" ["command `initialize` is not allowed"]
#eval expect "run_cmd pure ()" ["command `run_cmd` is not allowed"]
-- Declarations that are not kernel-checked, or add axioms.
#eval expect "unsafe def u : Nat := 0" ["`unsafe` is not allowed"]
#eval expect "partial def loop (n : Nat) : Nat := loop n" ["`partial` is not allowed"]
#eval expect "axiom bad : False" ["declaration `axiom` is not allowed", "`axiom` is not allowed"]
#eval expect "opaque o : Nat" ["declaration `opaque` is not allowed", "`opaque` is not allowed"]
#eval expect "theorem t : 1 = 1 := sorry" ["`sorry` is not allowed"]
#eval expect "theorem t : 2 + 2 = 4 := by native_decide" ["`native_decide` is not allowed"]
#eval expect "theorem t : 2 + 2 = 4 := by decide +native" ["`+native` is not allowed"]
-- Escapes to native code or to the file system.
#eval expect "@[extern \"system\"] def s : Nat := 0" ["attribute `extern` is not allowed", "`extern` is not allowed"]
#eval expect "@[implemented_by f] def s : Nat := 0" ["attribute `implemented_by` is not allowed"]
-- `c₁ in c₂` is checked as both commands.
#eval expect "set_option maxHeartbeats 10 in\n#eval 1" ["command `#eval` is not allowed"]
#eval expect "def secret : String := include_str \"/etc/passwd\"" ["`include_str` is not allowed"]
-- Side effects outside `IO`.
#eval expect "def p : Nat := panic! \"x\"" ["`panic!` is not allowed"]
#eval expect "def p : Nat := dbgTrace \"x\" fun _ => 0" ["`dbgTrace` is not allowed"]
#eval expect "def p : Nat := unsafeBaseIO (pure 0)" ["`unsafeBaseIO` is not allowed"]
-- Options and attributes are allowlisted.
#eval expect "set_option debug.skipKernelTC true" ["option `debug.skipKernelTC` is not allowed"]
#eval expect "@[export my_sym] def e : Nat := 0" ["attribute `export` is not allowed", "`export` is not allowed"]
-- Imports: the project's modules and allowed libraries only.
#eval expect "import Lean.Elab.Command" ["import `Lean.Elab.Command` is not allowed"]
#eval expect "import M.C" ["import `M.C` is not allowed"]
#eval expect "import Lean.Elab.Command" [] ({ : Policy }.allowing [`Lean.Elab])
-- A file that does not parse is rejected.
#eval do
  let ps ← problemsOf "def f : Nat := )"
  unless ps.any (·.startsWith "does not parse") do throwError "expected a parse error, got {ps}"

-- ── Paths and admission ─────────────────────────────────────────────────────

#guard moduleOfPath ["A", "B.lean"] == some `A.B
#guard moduleOfPath ["Main.lean"] == some `Main
#guard moduleOfPath ["lakefile.lean"] == none
#guard moduleOfPath [".lake", "x", "Y.lean"] == none
#guard moduleOfPath ["README.md"] == none

def rep (m : Name) (imports : List Name) (problems : List String := []) : FileReport :=
  ⟨m, imports.toArray, problems⟩

-- `C` has a problem; `B` imports it, so it is excluded too; `A` stands.
def files : Array FileReport :=
  #[rep `A [`Init], rep `B [`C], rep `C [] ["`unsafe` is not allowed"], rep `D [`B]]
#guard admit [`A, `B, `C, `D] files == [`A]
#guard ({ files, admitted := admit [`A, `B, `C, `D] files } : ProjectReport).excluded ==
  [(`B, ["imports a module that is not admitted"]), (`C, ["`unsafe` is not allowed"]),
   (`D, ["imports a module that is not admitted"])]

-- ── The library-environment cache key ───────────────────────────────────────

-- Imports are a set: order and repetition do not make a new key.
#guard canonicalImports #[`Std.Data.HashMap, `Init] == #[`Init, `Std.Data.HashMap]
#guard canonicalImports #[`Init, `Std.Data.HashMap] == canonicalImports #[`Std.Data.HashMap, `Init]
#guard canonicalImports #[`Init, `Lean.Data.Json, `Init, `Lean.Data.Json] ==
  #[`Init, `Lean.Data.Json]
#guard canonicalImports #[] == #[]

-- Paths, lexically: `.`, empty components and `..` go.
#guard lexicalNormalize "/a/./b/../c" == "/a/c"
#guard lexicalNormalize "/a//b/" == "/a/b"
#guard lexicalNormalize "/../a" == "/a"
#guard lexicalNormalize "/" == "/"
#guard lexicalNormalize "a/../../b" == "../b"
#guard lexicalNormalize "a/.." == "."
#guard lexicalNormalize "../../a" == "../../a"

-- A search path: absolute, resolved, deduplicated — and still in order, since
-- the first entry holding a module wins.
#eval show IO Unit from do
  -- Resolved, as `canonicalSearchPath` resolves (`/tmp` is `/private/tmp` on macOS).
  let dir ← IO.FS.realPath (← IO.FS.createTempDir)
  IO.FS.createDirAll (dir / "a" / "b")
  try
    let a := dir / "a"
    let got ← canonicalSearchPath [a / "b" / "..", dir / ".", a, dir, dir / "missing" / ".." / "a"]
    unless got == [a, dir] do throw (IO.userError s!"expected {[a, dir]}, got {got}")
    -- The order is kept, not sorted.
    let got ← canonicalSearchPath [dir, a]
    unless got == [dir, a] do throw (IO.userError s!"expected {[dir, a]}, got {got}")
    -- An entry that does not exist is normalised lexically.
    let got ← canonicalSearchPath [dir / "nope" / "." / "x" / ".."]
    unless got == [dir / "nope"] do throw (IO.userError s!"expected {dir / "nope"}, got {got}")
  finally
    IO.FS.removeDirAll dir

end Tests.System.GitFn.Policy
