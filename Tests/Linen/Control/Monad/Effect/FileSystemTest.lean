/-
  Tests for `Linen.Control.Monad.Effect.FileSystem`.

  Covers both halves of the capability system:

  - **Permissions** — which operations are allowed. A withheld permission makes
    the call fail to elaborate (asserted with `#guard_msgs`, since instance
    synthesis failures are capturable).
  - **Path scope** — which arguments those operations may be called on. A path
    outside the capability's roots makes the obligation `cap.permits p = true`
    *unsatisfiable*, which is asserted directly: proving it equals `false` shows
    no proof of `= true` can exist, which is a stronger statement than matching
    an error message (and necessary, since auto-param failures escape
    `#guard_msgs`).

  Plus a real round-trip through `IO.FS` under a capability.
-/
import Linen.Control.Monad.Effect.FileSystem

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.FileSystem

namespace Tests.Control.Monad.Effect.FileSystem

-- ── The `p!` path macro ─────────────────────────────────────────────────────

#guard (p!"/tmp/sandbox/a.txt" : Path) == ["tmp", "sandbox", "a.txt"]
#guard (p!"tmp/sandbox" : Path) == ["tmp", "sandbox"]
#guard (p!"/" : Path) == ([] : Path)
#guard (p!"/a//b/" : Path) == ["a", "b"]
#guard Path.toFilePath p!"/tmp/x/y" == ("tmp/x/y" : System.FilePath)

-- ── Permission obligations ──────────────────────────────────────────────────

-- A granted permission is discharged by `rfl` on a concrete capability.
example : readOnly.canRead = true := rfl
example : readWrite.canWrite = true := rfl
example : full.canDelete = true := rfl

-- A withheld permission is provably absent, not merely unproven.
example : readOnly.canWrite = false := rfl
example : readOnly.canDelete = false := rfl
example : readWrite.canDelete = false := rfl

-- The instances exist exactly when the corresponding bit is set.
example : CanRead readOnly := inferInstance
example : CanWrite readWrite := inferInstance
example : CanDelete full := inferInstance

-- The proof carried by an instance really is the field equation — this is why
-- an instance cannot be forged for a capability lacking the bit.
example : CanWrite.proof (cap := readWrite) = (rfl : readWrite.canWrite = true) := rfl

-- ── Positive: permitted operations elaborate ────────────────────────────────

-- `cap` is inferred from the row rather than supplied — what the `outParam` on
-- `HasFileSystem` buys — and it works inside `do`-notation.
example : Eff [FileSystem readOnly] ByteArray := readFile p!"/tmp/example"

example : Eff [FileSystem readWrite] Unit :=
  writeFile p!"/tmp/example" (String.toUTF8 "data")

example : Eff [FileSystem full] Unit := deleteFile p!"/tmp/example"

example : Eff [FileSystem full] (Option String) := do
  writeFileString p!"/tmp/example" "contents"
  let s ← readFileString? p!"/tmp/example"
  deleteFile p!"/tmp/example"
  pure s

-- ── Negative: withheld operations do not elaborate ──────────────────────────

-- Writing under a read-only capability. `FileSystem` *is* in the row, so a
-- name-only effect row would admit this call; the capability value rejects it.
/--
error: failed to synthesize instance of type class
  CanWrite readOnly

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
#check (writeFile (effs := [FileSystem readOnly]) p!"/tmp/x" (String.toUTF8 "x"))

-- Deleting under a read-write capability: read/write/delete are split within a
-- single effect.
/--
error: failed to synthesize instance of type class
  CanDelete readWrite

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
#check (deleteFile (effs := [FileSystem readWrite]) p!"/tmp/x")

-- Reading under a write-only capability.
/--
error: failed to synthesize instance of type class
  CanRead writeOnly

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
#check (readFile (effs := [FileSystem writeOnly]) p!"/tmp/x")

-- ── Path scope ──────────────────────────────────────────────────────────────

abbrev box : Capability := sandboxed p!"/tmp/sandbox"

-- Inside the sandbox: the obligation holds, so the operations elaborate.
#guard box.permits p!"/tmp/sandbox/a.txt"
#guard box.permits p!"/tmp/sandbox/sub/deep/b.txt"
#guard box.permits p!"/tmp/sandbox"

example : Eff [FileSystem box] ByteArray := readFile p!"/tmp/sandbox/a.txt"
example : Eff [FileSystem box] Unit :=
  writeFile p!"/tmp/sandbox/sub/b.txt" (String.toUTF8 "ok")
example : Eff [FileSystem box] (Option String) := do
  writeFileString p!"/tmp/sandbox/x" "hi"
  readFileString? p!"/tmp/sandbox/x"

-- Outside the sandbox the obligation is provably *false*, so no proof of
-- `= true` exists and the call cannot be written at all.
#guard !box.permits p!"/etc/passwd"
example : box.permits p!"/etc/passwd" = false := rfl
theorem no_read_outside_sandbox : box.permits p!"/etc/passwd" ≠ true := by decide

-- The string-prefix trap: `/tmp/sandbox-evil` *is* a string-prefix extension of
-- `/tmp/sandbox`, but is not inside it. Component-wise scoping rejects it —
-- which is why `Path` is a component list rather than a `String`.
#guard !box.permits p!"/tmp/sandbox-evil/secrets"
example : box.permits p!"/tmp/sandbox-evil/secrets" = false := rfl
theorem no_sibling_prefix_escape :
    box.permits p!"/tmp/sandbox-evil/secrets" ≠ true := by decide

-- A parent of the root is not inside the root either.
#guard !box.permits p!"/tmp"
example : box.permits p!"/tmp" = false := rfl

-- An empty `roots` means unrestricted, so an unscoped capability admits
-- anything — this is what keeps the permission-only capabilities usable.
#guard readOnly.roots.isEmpty
#guard readOnly.permits p!"/etc/passwd"
example (p : Path) : readOnly.permits p = true := rfl

-- Two capabilities granting the *same operations* but differing in scope: this
-- is the distinction a type-level effect row cannot draw at all.
example : readWrite.canRead = box.canRead ∧ readWrite.canWrite = box.canWrite :=
  ⟨rfl, rfl⟩
example : readWrite.permits p!"/etc/passwd" = true := rfl
example : box.permits p!"/etc/passwd" = false := rfl

-- ── Runtime-validated paths ─────────────────────────────────────────────────

-- A path known only at runtime cannot be checked by `decide`; `check?`
-- validates it and hands back the evidence.
#guard (ScopedPath.check? box p!"/tmp/sandbox/ok.txt").isSome
#guard (ScopedPath.check? box p!"/etc/passwd").isNone

-- The evidence a `ScopedPath` carries is exactly the operations' obligation, so
-- `readFileAt` needs no `decide`.
example (sp : ScopedPath box) : Eff [FileSystem box] ByteArray := readFileAt sp

-- Validating a path built at runtime, then using it.
example (name : String) : Eff [FileSystem box] (Option ByteArray) :=
  match ScopedPath.check? box (p!"/tmp/sandbox" ++ [name]) with
  | some sp => some <$> readFileAt sp
  | none    => pure none

-- ── End-to-end: a real round-trip through `IO.FS` ───────────────────────────

/-- Write a file, read it back, then remove it — under `full`, the only
    capability that permits all three. -/
def roundTrip (path : Path) (hs : full.permits path = true := by decide) :
    Eff [FileSystem full] (Option String) := do
  writeFileString path "capability round-trip" hs
  let contents ← readFileString? path hs
  deleteFile path hs
  pure contents

/-- info: (some "capability round-trip", false) -/
#guard_msgs in
#eval show IO (Option String × Bool) from do
  let dir ← IO.currentDir
  let rel : Path := ["linen-freer-fs-test.tmp"]
  let contents ← runFileSystem full (roundTrip rel)
  -- `deleteFile` really ran, so the scratch file is gone afterwards.
  pure (contents, ← (dir / "linen-freer-fs-test.tmp").pathExists)

end Tests.Control.Monad.Effect.FileSystem
