/-
  `Control.Monad.Freer.FileSystem` — a capability-restricted filesystem effect

  ## Not a Haskell port

  This module is `linen`-original: it has no counterpart in `freer-simple` (or
  in `polysemy`, `effectful`, or `fused-effects`), and is not part of the
  `FreerSimple` import's topological checklist. It exists to demonstrate what an
  effect system gains from dependent types, and is documented as such in
  `docs/imports/FreerSimple/dependencies.md`.

  ## What Haskell's effect rows can and cannot say

  An effect row bounds *which* effects a computation may perform:
  `Eff [FileSystem] α` cannot reach the network. Every mainstream Haskell effect
  library stops there, because its row is a list of type constructors — it names
  effects, and a name has no structure. Saying "may write files but never delete
  them", or "may only touch files under this directory", therefore requires
  either a separate effect type per combination, or a runtime check inside the
  handler that the type system knows nothing about.

  In Lean a permission can be an ordinary **value** indexing the effect, and the
  obligation to hold it an ordinary **proposition**. This module uses that twice,
  at two different strengths:

  1. **Permissions** — which *operations* are allowed (`canRead`/`canWrite`/
     `canDelete`), each demanded as a `Prop`-class instance.
  2. **Path scope** — which *arguments* those operations may be called on
     (`roots`), demanded as a proof obligation discharged by `decide` at the call
     site. This constrains the effect's argument space, not just its operation
     set, and is the part no type-level row can express at all.

  So `writeFile` under a read-only capability fails to elaborate, and so does
  `readFile (p!"/etc/passwd")` under a capability rooted at `/tmp/sandbox`.

  ## Design

  - `Capability` has `Bool` permission fields, so a literal capability is
    computable and each permission instance is discharged by `rfl`.
  - `CanRead`/`CanWrite`/`CanDelete` are `Prop`-classes carrying the proof.
    Encoding an obligation as an *instance* rather than an auto-param is what
    makes it work here: an auto-param tactic runs before `cap` has been unified
    from the expected type and fails on a metavariable, whereas instance
    resolution is postponed until `cap` is known.
  - `HasFileSystem` locates the effect in the row with `cap` as an `outParam`,
    so resolving it *determines* `cap`. Without that, `cap` stays a metavariable
    inside a `do`-block continuation and the obligations get stuck even with an
    explicit type annotation.
  - The **path scope** obligation, by contrast, *is* an auto-param
    (`:= by decide`), because it depends on the path argument and so cannot be an
    instance keyed on `cap` alone. This works precisely because `cap` is already
    determined by `HasFileSystem`'s `outParam` by the time the tactic runs.
  - The handler performs **no checks at all**. Every proof is carried by the
    constructor, so a `FileSystem.writeFile` value cannot exist unless both its
    permission and its path scope were established; `runFileSystem` is pure
    plumbing onto Lean core's `IO.FS`. Enforcement happens once, statically, at
    construction — not on every dispatch.

  ## Why paths are component lists

  `Path` is `List String`, not `System.FilePath`. Two reasons, both load-bearing:

  - **Decidability.** Lean's `String.startsWith`/`String.take` do not reduce
    under `decide` (they get stuck on the slice representation), so a
    string-prefix scope check could not be discharged at elaboration time at all.
    `List String` prefix comparison does reduce, so the obligation is decidable
    where it needs to be.
  - **Correctness.** Component-wise containment is the right meaning of "under
    this directory". String prefixes get it wrong: `/tmp/sandbox-evil` *is* a
    string-prefix extension of `/tmp/sandbox` but is not inside it. This module's
    tests pin that case.

  Write literals with the `p!` macro — `p!"/tmp/sandbox/a.txt"` expands at macro
  time to `["tmp", "sandbox", "a.txt"]`, so the result is still a literal list
  that `decide` can reduce.

  ## Soundness

  `CanWrite` is evidence, not a marker: it carries a `cap.canWrite = true` field,
  so a bogus instance for a capability lacking the bit would require proving
  `false = true`. Likewise the scope obligation is a real equation about
  `cap.permits`. The guarantee rests on the proofs, not on users declining to
  write instances.

  ## Statically-unknown paths

  A path computed at runtime cannot have its scope proved by `decide`. That is
  not a gap but the honest shape of the problem: use `ScopedPath.check?`, which
  validates a path and *returns the evidence* on success, so the operations
  still cannot be called on an unvalidated path.

  ## Backend

  `linen` has no filesystem wrapper of its own (`Linen.System.IO` is
  `streamly-core`'s buffer-size constants), so per AGENTS.md's stdlib-first
  precedence the handler calls Lean core's `IO.FS` directly.
-/
import Linen.Control.Monad.Freer

namespace Control.Monad.Freer.FileSystem

open Data.OpenUnion Control.Monad.Freer

-- ── Paths ───────────────────────────────────────────────────────────────────

/-- A filesystem path as its components.

    `List String` rather than `System.FilePath` so that scope checks are
    decidable at elaboration time and are component-wise; see the module header
    for why both matter. -/
abbrev Path := List String

/-- Render a path for the handler to hand to `IO.FS`. -/
def Path.toFilePath (p : Path) : System.FilePath :=
  String.intercalate "/" p

open Lean in
/-- Path literal: `p!"/tmp/sandbox/a.txt"` expands to
    `["tmp", "sandbox", "a.txt"]`.

    The split happens at macro-expansion time, so the result is a literal list
    and a scope obligation about it still reduces under `decide`.

    Declared at `max` precedence so it can be passed as a bare function
    argument (`readFile p!"/tmp/x"`) without parentheses. -/
macro:max "p!" s:str : term => do
  let parts := (s.getString.splitOn "/").filter (· ≠ "")
  let elems := parts.map (fun c => Syntax.mkStrLit c) |>.toArray
  `([$elems,*])

-- ── Capabilities ────────────────────────────────────────────────────────────

/-- What a computation is permitted to do to the filesystem: which operations,
    and under which directories.

    Permission fields default to `false`, so `{ canRead := true }` denies writing
    and deleting by construction — a capability grants only what it names.
    `roots` defaults to `[]`, meaning *no path restriction*; a non-empty `roots`
    confines every operation to paths under one of them.

    Declare capability constants with `abbrev`, not `def`: instance resolution
    does not unfold a non-reducible `def`, so `def myCap` leaves
    `CanWrite myCap` unresolvable. -/
structure Capability where
  /-- May read file contents. -/
  canRead   : Bool := false
  /-- May create and overwrite files. -/
  canWrite  : Bool := false
  /-- May remove files. -/
  canDelete : Bool := false
  /-- Directories the capability is confined to; `[]` means unrestricted. -/
  roots     : List Path := []
  deriving DecidableEq, Repr

/-- Does this capability allow touching `p` at all?

    True when the capability is unrestricted (`roots = []`) or `p` lies under one
    of its roots, component-wise. -/
def Capability.permits (cap : Capability) (p : Path) : Bool :=
  cap.roots.isEmpty || cap.roots.any (fun r => r.isPrefixOf p)

/-- `cap` grants reading. Carries the proof, so it cannot be forged. -/
class CanRead (cap : Capability) : Prop where
  /-- Evidence that the read bit is set. -/
  proof : cap.canRead = true

/-- `cap` grants writing. Carries the proof, so it cannot be forged. -/
class CanWrite (cap : Capability) : Prop where
  /-- Evidence that the write bit is set. -/
  proof : cap.canWrite = true

/-- `cap` grants deletion. Carries the proof, so it cannot be forged. -/
class CanDelete (cap : Capability) : Prop where
  /-- Evidence that the delete bit is set. -/
  proof : cap.canDelete = true

instance instCanRead   {w d : Bool} {rs : List Path} : CanRead   ⟨true, w, d, rs⟩ := ⟨rfl⟩
instance instCanWrite  {r d : Bool} {rs : List Path} : CanWrite  ⟨r, true, d, rs⟩ := ⟨rfl⟩
instance instCanDelete {r w : Bool} {rs : List Path} : CanDelete ⟨r, w, true, rs⟩ := ⟨rfl⟩

-- ── Scoped paths, for paths not known statically ────────────────────────────

/-- A path together with a proof that `cap` allows touching it.

    For paths known at compile time the obligation on each operation is
    discharged by `decide` and this type is not needed. It exists for paths
    computed at runtime: `check?` validates one and hands back the evidence, so
    the operations still cannot be reached without it. -/
structure ScopedPath (cap : Capability) where
  /-- The path. -/
  path    : Path
  /-- Evidence that `cap` permits it. -/
  inScope : cap.permits path = true

/-- Validate a runtime path against `cap`, returning the evidence on success. -/
def ScopedPath.check? (cap : Capability) (p : Path) : Option (ScopedPath cap) :=
  if h : cap.permits p = true then some ⟨p, h⟩ else none

-- ── The effect ──────────────────────────────────────────────────────────────

/-- Filesystem operations available under the capability `cap`.

    Each constructor takes a proof that `cap` grants the operation *and* a proof
    that `cap` allows the path, so both the permission and the scope are part of
    what it means for the request to exist. -/
inductive FileSystem (cap : Capability) : Type → Type where
  /-- Read a file's contents. Requires read permission and path scope. -/
  | readFile   (hp : cap.canRead = true) (path : Path)
      (hs : cap.permits path = true) : FileSystem cap ByteArray
  /-- Write (creating or overwriting) a file. Requires write permission and
      path scope. -/
  | writeFile  (hp : cap.canWrite = true) (path : Path)
      (hs : cap.permits path = true) (data : ByteArray) : FileSystem cap Unit
  /-- Remove a file. Requires delete permission and path scope. -/
  | deleteFile (hp : cap.canDelete = true) (path : Path)
      (hs : cap.permits path = true) : FileSystem cap Unit

/-- Locates a `FileSystem` effect in the row and recovers *which* capability it
    carries.

    `cap` is an `outParam`: it is an output of resolving against `effs`, not
    something the caller must supply. That is what keeps both the permission
    obligations and the path-scope obligation solvable inside `do`-notation,
    where `cap` would otherwise remain a metavariable. -/
class HasFileSystem (effs : List (Type → Type)) (cap : outParam Capability) where
  /-- Inject a filesystem request into the row. -/
  inject : {α : Type} → FileSystem cap α → Union effs α

/-- The filesystem effect is the row's head. -/
instance instHasFileSystemHere {cap : Capability} {effs : List (Type → Type)} :
    HasFileSystem (FileSystem cap :: effs) cap where
  inject e := .here e

/-- The filesystem effect is somewhere in the row's tail. -/
instance instHasFileSystemThere {cap : Capability} {eff : Type → Type}
    {effs : List (Type → Type)} [HasFileSystem effs cap] :
    HasFileSystem (eff :: effs) cap where
  inject e := .there (HasFileSystem.inject e)

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Read a file's contents.

    Requires `CanRead cap` and a proof that `cap` allows `path`, both resolved
    from the capability the row carries. Under a capability without the read bit,
    or for a path outside its roots, this call does not elaborate. -/
def readFile {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanRead cap] (path : Path)
    (hs : cap.permits path = true := by decide) : Eff effs ByteArray :=
  .impure (fs.inject (.readFile perm.proof path hs)) .protect

/-- Write a file, creating or overwriting it.

    Requires `CanWrite cap` and path scope. Under a read-only capability, or for
    a path outside its roots, this call does not elaborate. -/
def writeFile {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanWrite cap] (path : Path)
    (data : ByteArray) (hs : cap.permits path = true := by decide) :
    Eff effs Unit :=
  .impure (fs.inject (.writeFile perm.proof path hs data)) .protect

/-- Remove a file.

    Requires `CanDelete cap` and path scope. A capability granting read and write
    but not delete makes this call fail to elaborate — the distinction Haskell's
    type-level effect rows cannot draw. -/
def deleteFile {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanDelete cap] (path : Path)
    (hs : cap.permits path = true := by decide) : Eff effs Unit :=
  .impure (fs.inject (.deleteFile perm.proof path hs)) .protect

/-- Read a file's contents and decode them as UTF-8, or `none` if the bytes are
    not valid UTF-8.

    Total by construction: `String.fromUTF8?` rather than the panicking
    `String.fromUTF8!`, so a malformed file is a value the caller handles, not a
    crash. -/
def readFileString? {effs : List (Type → Type)} {cap : Capability}
    [HasFileSystem effs cap] [CanRead cap] (path : Path)
    (hs : cap.permits path = true := by decide) : Eff effs (Option String) :=
  String.fromUTF8? <$> readFile path hs

/-- Write a string to a file as UTF-8. -/
def writeFileString {effs : List (Type → Type)} {cap : Capability}
    [HasFileSystem effs cap] [CanWrite cap] (path : Path) (contents : String)
    (hs : cap.permits path = true := by decide) : Eff effs Unit :=
  writeFile path contents.toUTF8 hs

-- ── Operations on runtime-validated paths ───────────────────────────────────

/-- Read a file at a path validated at runtime.

    The `ScopedPath` supplies the scope evidence, so no `decide` is involved and
    the path need not be statically known. -/
def readFileAt {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanRead cap] (sp : ScopedPath cap) :
    Eff effs ByteArray :=
  .impure (fs.inject (.readFile perm.proof sp.path sp.inScope)) .protect

/-- Write a file at a path validated at runtime. -/
def writeFileAt {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanWrite cap] (sp : ScopedPath cap)
    (data : ByteArray) : Eff effs Unit :=
  .impure (fs.inject (.writeFile perm.proof sp.path sp.inScope data)) .protect

/-- Delete a file at a path validated at runtime. -/
def deleteFileAt {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanDelete cap] (sp : ScopedPath cap) :
    Eff effs Unit :=
  .impure (fs.inject (.deleteFile perm.proof sp.path sp.inScope)) .protect

-- ── Handler ─────────────────────────────────────────────────────────────────

/-- Run a filesystem computation in `IO`.

    Performs no permission or scope check: every request already carries both
    proofs, so reaching this point means they were established at construction.
    The handler only renders the path and dispatches to `IO.FS`. -/
def runFileSystem (cap : Capability) {α : Type} :
    Eff [FileSystem cap] α → IO α :=
  interpretM fun
    | .readFile   _ path _      => IO.FS.readBinFile path.toFilePath
    | .writeFile  _ path _ data => IO.FS.writeBinFile path.toFilePath data
    | .deleteFile _ path _      => IO.FS.removeFile path.toFilePath

-- ── Common capabilities ─────────────────────────────────────────────────────

/-- Read-only access, anywhere: `writeFile` and `deleteFile` will not
    elaborate. -/
abbrev readOnly : Capability := { canRead := true }

/-- Write-only access, e.g. for an append-only log or report writer. -/
abbrev writeOnly : Capability := { canWrite := true }

/-- Read and write, but **not** delete — the permission split a type-level
    effect row cannot express. -/
abbrev readWrite : Capability := { canRead := true, canWrite := true }

/-- Unrestricted filesystem access. -/
abbrev full : Capability :=
  { canRead := true, canWrite := true, canDelete := true }

/-- Read and write, confined to paths under `root`.

    The interesting shape: two capabilities can grant the same *operations* and
    still differ in which *arguments* they admit. -/
abbrev sandboxed (root : Path) : Capability :=
  { canRead := true, canWrite := true, roots := [root] }

end Control.Monad.Freer.FileSystem
