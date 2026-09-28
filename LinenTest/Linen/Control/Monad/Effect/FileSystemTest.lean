/-
  Tests for `Linen.Control.Monad.Effect.FileSystem`.

  Covers both halves of the capability system:

  - **Permissions** — which operations are allowed. A withheld permission makes
    the call fail to elaborate (asserted with `#guard_msgs`, since instance
    synthesis failures are capturable).
  - **Path scope** — which arguments those operations may be called on, *per
    operation*: every `Scope` carries its own operation list, so one capability
    can read-write under one directory and read-only under another. A path no
    scope covers for that operation makes the obligation
    `cap.permits op p = true` *unsatisfiable*, which is asserted directly:
    proving it equals `false` shows no proof of `= true` can exist, which is a
    stronger statement than matching an error message (and necessary, since
    auto-param failures escape `#guard_msgs`).
  - **Union** — scopes only ever add access, so `Capability.union` cannot take
    away what either operand had (`permits_union_left`/`_right`).

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
#guard box.permits .read p!"/tmp/sandbox/a.txt"
#guard box.permits .write p!"/tmp/sandbox/sub/deep/b.txt"
#guard box.permits .read p!"/tmp/sandbox"

example : Eff [FileSystem box] ByteArray := readFile p!"/tmp/sandbox/a.txt"
example : Eff [FileSystem box] Unit :=
  writeFile p!"/tmp/sandbox/sub/b.txt" (String.toUTF8 "ok")
example : Eff [FileSystem box] (Option String) := do
  writeFileString p!"/tmp/sandbox/x" "hi"
  readFileString? p!"/tmp/sandbox/x"

-- Outside the sandbox the obligation is provably *false*, so no proof of
-- `= true` exists and the call cannot be written at all.
#guard !box.permits .read p!"/etc/passwd"
example : box.permits .read p!"/etc/passwd" = false := rfl
theorem no_read_outside_sandbox : box.permits .read p!"/etc/passwd" ≠ true := by decide

-- The string-prefix trap: `/tmp/sandbox-evil` *is* a string-prefix extension of
-- `/tmp/sandbox`, but is not inside it. Component-wise scoping rejects it —
-- which is why `Path` is a component list rather than a `String`.
#guard !box.permits .read p!"/tmp/sandbox-evil/secrets"
example : box.permits .read p!"/tmp/sandbox-evil/secrets" = false := rfl
theorem no_sibling_prefix_escape :
    box.permits .read p!"/tmp/sandbox-evil/secrets" ≠ true := by decide

-- A parent of the root is not inside the root either.
#guard !box.permits .read p!"/tmp"
example : box.permits .read p!"/tmp" = false := rfl

-- An empty `scopes` means unrestricted, so an unscoped capability admits
-- anything — this is what keeps the permission-only capabilities usable.
#guard readOnly.scopes.isEmpty
#guard readOnly.permits .read p!"/etc/passwd"
example (op : Op) (p : Path) : readOnly.permits op p = true := rfl

-- Two capabilities granting the *same operations* but differing in scope: this
-- is the distinction a type-level effect row cannot draw at all.
example : readWrite.canRead = box.canRead ∧ readWrite.canWrite = box.canWrite :=
  ⟨rfl, rfl⟩
example : readWrite.permits .read p!"/etc/passwd" = true := rfl
example : box.permits .read p!"/etc/passwd" = false := rfl

-- ── A permission set per prefix ─────────────────────────────────────────────

-- The shape a single `roots` list cannot express: read and write under one
-- directory, read-only under another, nothing anywhere else.
abbrev deployer : Capability :=
  { canRead := true, canWrite := true, canDelete := true
  , scopes := [ under p!"/srv/app/releases" [.read, .write, .delete]
              , under p!"/srv/app/current"  [.read, .write]
              , under p!"/etc/app"          [.read] ] }

-- The bits are the global upper bound and are written out, because instance
-- resolution matches them syntactically. `consistent` is what keeps them honest
-- against the scopes they are meant to bound.
#guard deployer.consistent
#guard deployer.canRead && deployer.canWrite && deployer.canDelete

-- A capability whose bits withhold what a scope names is *sound* but almost
-- certainly a typo — the scope is simply dead, since `CanDelete` cannot be had.
#guard !({ canRead := true, scopes := [under p!"/tmp" [.read, .delete]] }
          : Capability).consistent

-- Under `/srv/app/releases`, all three operations are permitted …
#guard deployer.permits .read   p!"/srv/app/releases/2026-09-09/app"
#guard deployer.permits .write  p!"/srv/app/releases/2026-09-09/app"
#guard deployer.permits .delete p!"/srv/app/releases/2026-01-01/app"

-- … under `/srv/app/current`, reading and writing but *not* deleting …
#guard deployer.permits .read  p!"/srv/app/current/app"
#guard deployer.permits .write p!"/srv/app/current/app"
#guard !deployer.permits .delete p!"/srv/app/current/app"
theorem no_delete_current :
    deployer.permits .delete p!"/srv/app/current/app" ≠ true := by decide

-- … under `/etc/app`, reading only …
#guard deployer.permits .read p!"/etc/app/config.toml"
#guard !deployer.permits .write p!"/etc/app/config.toml"
theorem no_write_config :
    deployer.permits .write p!"/etc/app/config.toml" ≠ true := by decide

-- … and nothing at all outside every scope.
#guard !deployer.permits .read p!"/etc/passwd"
theorem no_read_passwd : deployer.permits .read p!"/etc/passwd" ≠ true := by decide

-- The permitted calls elaborate, and `deleteFile` is available *somewhere* —
-- `CanDelete deployer` holds — yet still not at `/srv/app/current`. The global
-- bit and the per-path obligation are genuinely two different checks.
example : CanDelete deployer := inferInstance
example : Eff [FileSystem deployer] Unit :=
  deleteFile p!"/srv/app/releases/2026-01-01/app"
example : Eff [FileSystem deployer] (Option String) := do
  let cfg ← readFileString? p!"/etc/app/config.toml"
  writeFileString p!"/srv/app/current/app" (cfg.getD "")
  pure cfg

-- A scope with an empty `ops` grants whatever the capability grants, which is
-- how `sandboxed` stays a two-line definition.
#guard (under p!"/tmp/sandbox" : Scope).ops.isEmpty
#guard box.scopes == [under p!"/tmp/sandbox"]

-- ── Unioning capabilities ───────────────────────────────────────────────────

abbrev logs : Capability :=
  { canRead := true, scopes := [under p!"/var/log/app" [.read]] }
abbrev cache : Capability :=
  { canRead := true, canWrite := true, canDelete := true
  , scopes := [under p!"/var/cache/app" [.read, .write, .delete]] }
abbrev both : Capability := logs.union cache

-- A union's bits are computed, so instance search cannot match them; declaring
-- the instances once discharges each bit by `decide`, and the operations then
-- elaborate exactly as they do for a literal capability.
instance : CanRead both := .of
instance : CanWrite both := .of
instance : CanDelete both := .of

example : Eff [FileSystem both] ByteArray := readFile p!"/var/log/app/app.log"
example : Eff [FileSystem both] Unit := deleteFile p!"/var/cache/app/tmp"

-- Still no write access to the logs, and no access at all outside both scopes.
theorem no_write_logs : both.permits .write p!"/var/log/app/app.log" ≠ true := by decide
theorem no_read_etc : both.permits .read p!"/etc/passwd" ≠ true := by decide

-- Neither operand loses anything, which is what `permits_union_*` state in
-- general rather than for these two values.
#guard both.permits .read p!"/var/log/app/app.log"
#guard both.permits .delete p!"/var/cache/app/x"
#guard !both.permits .write p!"/var/log/app/app.log"
#guard !both.permits .read p!"/etc/passwd"

example : both.permits .read p!"/var/log/app/app.log" = true :=
  permits_union_left (by decide)
example : both.permits .delete p!"/var/cache/app/x" = true :=
  permits_union_right (by decide)
example : both.allows .delete = true := allows_union_right (by decide)

-- An *unscoped* capability means "any path", so a union involving one stays
-- unscoped: appending the scope lists would wrongly narrow it.
#guard (readOnly.union cache).scopes.isEmpty
#guard (readOnly.union cache).permits .read p!"/anywhere/at/all"
example : (readOnly.union cache).permits .read p!"/etc/passwd" = true :=
  permits_union_left rfl

-- ── Runtime-validated paths ─────────────────────────────────────────────────

-- A path known only at runtime cannot be checked by `decide`; `check?`
-- validates it and hands back the evidence.
#guard (ScopedPath.check? box .read p!"/tmp/sandbox/ok.txt").isSome
#guard (ScopedPath.check? box .read p!"/etc/passwd").isNone

-- The evidence is per *operation*, not per path: under `deployer`, a path in
-- `/etc/app` validates for reading and not for writing.
#guard (ScopedPath.check? deployer .read p!"/etc/app/config.toml").isSome
#guard (ScopedPath.check? deployer .write p!"/etc/app/config.toml").isNone

-- The evidence a `ScopedPath` carries is exactly the operation's obligation, so
-- `readFileAt` needs no `decide` — and a read-scoped path cannot be passed to
-- `writeFileAt`, since the two ask for differently-indexed evidence.
example (sp : ScopedPath box .read) : Eff [FileSystem box] ByteArray := readFileAt sp

-- Validating a path built at runtime, then using it.
example (name : String) : Eff [FileSystem box] (Option ByteArray) :=
  match ScopedPath.check? box .read (p!"/tmp/sandbox" ++ [name]) with
  | some sp => some <$> readFileAt sp
  | none    => pure none

-- ── End-to-end: a real round-trip through `IO.FS` ───────────────────────────

/-- Write a file, read it back, then remove it — under `full`, the only
    capability that permits all three. -/
def roundTrip (path : Path)
    (hw : full.permits .write path = true := by decide)
    (hr : full.permits .read path = true := by decide)
    (hd : full.permits .delete path = true := by decide) :
    Eff [FileSystem full] (Option String) := do
  writeFileString path "capability round-trip" hw
  let contents ← readFileString? path hr
  deleteFile path hd
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
