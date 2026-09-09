/-
  `Control.Monad.Effect.FileSystem` — a capability-restricted filesystem effect

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
     (`scopes`), demanded as a proof obligation discharged by `decide` at the
     call site. This constrains the effect's argument space, not just its
     operation set, and is the part no type-level row can express at all.

  So `writeFile` under a read-only capability fails to elaborate, and so does
  `readFile (p!"/etc/passwd")` under a capability rooted at `/tmp/sandbox`.

  ## A permission set per prefix, unioned

  The scope is not a bare list of roots: each `Scope` carries **its own operation
  list** alongside its root, so one capability can say

      read, write and delete under /tmp/work
      read only            under /etc/config
      nothing              anywhere else

  as

      { canRead := true, canWrite := true, canDelete := true
      , scopes := [ under p!"/tmp/work"   [.read, .write, .delete]
                  , under p!"/etc/config" [.read] ] }

  That is a restriction on (operation, argument) *pairs*, not on arguments
  alone — the same shape `Control.Monad.Effect.HTTP.Scope` uses to say "GET
  anywhere under `/v1`, POST only to `/v1/events`". A `Scope` whose `ops` is
  empty grants every operation the capability itself grants, so a scope only
  names operations when it is *more* restrictive than the capability as a whole.

  Scopes **union**: `permits` holds when *some* scope covers the pair. There are
  deliberately no deny rules and no most-specific-wins precedence, so adding a
  scope can only ever add access. That monotonicity is what makes
  `Capability.union` sound (`permits_union_left`/`_right` below): two capabilities
  can be combined without either losing what it had. An overlap between scopes is
  therefore a union of their operation sets, not a conflict to resolve.

  ## Why the global bits are still written out

  `canRead`/`canWrite`/`canDelete` remain the capability's *global upper bound*,
  checked separately as a `Prop`-class instance: no scope can grant an operation
  the capability as a whole withholds. They are written as literals rather than
  derived from `scopes`, because instance resolution matches them **syntactically**
  — it runs at `instances` transparency and will not evaluate a `List.any` over
  the scope list, so a computed bit leaves `CanRead cap` unsolvable. This is the
  same arrangement `HTTP.restClient` uses.

  Two consequences worth knowing:

  - `Capability.consistent` checks that the bits really do cover every operation
    the scopes name. It is not needed for soundness — a scope naming an operation
    the bits withhold is simply dead — but the two disagreeing is almost always a
    mistake, so assert `#guard cap.consistent` beside a capability definition.
  - A capability whose bits are *computed*, such as a `Capability.union`, has the
    same problem: write `instance : CanRead (a.union b) := .of` to discharge the
    bit by `decide` once, and the operations elaborate as usual.

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
  validates a path *for one operation* and returns the evidence on success, so
  the operations still cannot be called on an unvalidated path.

  ## Backend

  `linen` has no filesystem wrapper of its own (`Linen.System.IO` is
  `streamly-core`'s buffer-size constants), so per AGENTS.md's stdlib-first
  precedence the handler calls Lean core's `IO.FS` directly.
-/
import Linen.Control.Monad.Effect

namespace Control.Monad.Effect.FileSystem

open Data.OpenUnion Control.Monad.Effect

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

-- ── Operations ──────────────────────────────────────────────────────────────

/-- The kind of thing a request does to a file. Each kind is separately
    grantable, both globally and per scope. -/
inductive Op
  /-- Read a file's contents. -/
  | read
  /-- Create or overwrite a file. -/
  | write
  /-- Remove a file. -/
  | delete
  deriving DecidableEq, BEq, Repr

-- ── Capabilities ────────────────────────────────────────────────────────────

/-- One region of the filesystem a capability opens up: a directory, and the
    operations permitted beneath it.

    `ops := []` means "every operation the capability's own bits allow", so a
    scope need only name operations when it is *more* restrictive than the
    capability as a whole. `root := []` is the whole filesystem. -/
structure Scope where
  /-- Operations allowed in this scope; `[]` means every operation the
      capability has. -/
  ops : List Op := []
  /-- The directory this scope covers, as components; `[]` is everywhere. -/
  root : Path := []
  deriving DecidableEq, Repr

/-- What a computation is permitted to do to the filesystem: which operations,
    and under which directories.

    Permission fields default to `false`, so `{ canRead := true }` denies writing
    and deleting by construction — a capability grants only what it names.
    `scopes` defaults to `[]`, meaning *no path restriction*; a non-empty
    `scopes` confines every operation to a scope that covers it. The bits are the
    global upper bound: no scope can grant what they withhold.

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
  /-- Regions of the filesystem the capability is confined to; `[]` means
      unrestricted. -/
  scopes    : List Scope := []
  deriving DecidableEq, Repr

/-- Does this capability's global permission set include `op`? -/
def Capability.allows (cap : Capability) : Op → Bool
  | .read   => cap.canRead
  | .write  => cap.canWrite
  | .delete => cap.canDelete

/-- Does `s` cover operation `op` at `p`?

    The root must be a *component-wise* prefix of the path, so
    `/tmp/sandbox-evil` is not under `/tmp/sandbox` however the two compare as
    strings. -/
def Scope.covers (s : Scope) (op : Op) (p : Path) : Bool :=
  (s.ops.isEmpty || s.ops.contains op) && s.root.isPrefixOf p

/-- Does this capability allow performing `op` on `p` at all?

    True when the capability is unscoped (`scopes = []`) or some scope covers the
    pair. This is the *argument* half of the capability; the *operation* half is
    `allows`, demanded separately as a `Prop`-class instance. -/
def Capability.permits (cap : Capability) (op : Op) (p : Path) : Bool :=
  cap.scopes.isEmpty || cap.scopes.any (fun s => s.covers op p)

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

instance instCanRead   {w d : Bool} {ss : List Scope} : CanRead   ⟨true, w, d, ss⟩ := ⟨rfl⟩
instance instCanWrite  {r d : Bool} {ss : List Scope} : CanWrite  ⟨r, true, d, ss⟩ := ⟨rfl⟩
instance instCanDelete {r w : Bool} {ss : List Scope} : CanDelete ⟨r, w, true, ss⟩ := ⟨rfl⟩

/-- The permission evidence for a capability whose bits are *computed* rather
    than written literally — a `Capability.union`, say.

    The instances above match the bit syntactically, which is all instance
    resolution can do (see the module header), so a computed capability needs its
    instance declared once: `instance : CanRead (a.union b) := .of`. The
    obligation is discharged by `decide`, so this is evidence exactly as much as
    the literal case is — it just cannot be found by search. -/
theorem CanRead.of {cap : Capability} (h : cap.canRead = true := by decide) :
    CanRead cap := ⟨h⟩

/-- `CanRead.of` for writing. -/
theorem CanWrite.of {cap : Capability} (h : cap.canWrite = true := by decide) :
    CanWrite cap := ⟨h⟩

/-- `CanRead.of` for deletion. -/
theorem CanDelete.of {cap : Capability} (h : cap.canDelete = true := by decide) :
    CanDelete cap := ⟨h⟩

-- ── Building capabilities ───────────────────────────────────────────────────

/-- The scope covering `root` and everything beneath it, for `ops` (or for every
    operation the capability grants, when `ops` is empty). -/
abbrev under (root : Path) (ops : List Op := []) : Scope :=
  { ops := ops, root := root }

/-- Do the global bits cover every operation the scopes name?

    Not required for soundness: the bits are the upper bound, so a scope naming
    an operation the bits withhold is simply dead, and no request can be built
    for it either way. But such a capability is almost always a typo, and the
    bits cannot be derived from the scopes (see the module header), so assert
    `#guard cap.consistent` beside a capability definition to keep the two
    halves honest. -/
def Capability.consistent (cap : Capability) : Bool :=
  cap.scopes.all (fun s => s.ops.all cap.allows)

/-- Combine two capabilities: everything either one allows.

    An *unscoped* capability means "any path", so a union involving one is
    unscoped too — appending the scope lists would wrongly narrow it to the other
    capability's roots. -/
def Capability.union (a b : Capability) : Capability :=
  { canRead   := a.canRead   || b.canRead
  , canWrite  := a.canWrite  || b.canWrite
  , canDelete := a.canDelete || b.canDelete
  , scopes    := if a.scopes.isEmpty || b.scopes.isEmpty then [] else a.scopes ++ b.scopes }

/-- A union grants everything its left operand granted. -/
theorem allows_union_left {a b : Capability} {op : Op} (h : a.allows op = true) :
    (a.union b).allows op = true := by
  cases op <;> simp_all [Capability.allows, Capability.union]

/-- A union grants everything its right operand granted. -/
theorem allows_union_right {a b : Capability} {op : Op} (h : b.allows op = true) :
    (a.union b).allows op = true := by
  cases op <;> simp_all [Capability.allows, Capability.union]

/-- A union admits every (operation, path) pair its left operand admitted —
    the monotonicity that makes combining capabilities safe. -/
theorem permits_union_left {a b : Capability} {op : Op} {p : Path}
    (h : a.permits op p = true) : (a.union b).permits op p = true := by
  by_cases hb : b.scopes.isEmpty = true
  · simp [Capability.permits, Capability.union, hb]
  · by_cases ha : a.scopes.isEmpty = true
    · simp [Capability.permits, Capability.union, ha]
    · rw [Capability.union, Capability.permits, if_neg (by simp [ha, hb])]
      simp only [Capability.permits, ha, Bool.false_or] at h
      simp only [List.any_append, h, Bool.true_or, Bool.or_true]

/-- A union admits every (operation, path) pair its right operand admitted. -/
theorem permits_union_right {a b : Capability} {op : Op} {p : Path}
    (h : b.permits op p = true) : (a.union b).permits op p = true := by
  by_cases ha : a.scopes.isEmpty = true
  · simp [Capability.permits, Capability.union, ha]
  · by_cases hb : b.scopes.isEmpty = true
    · simp [Capability.permits, Capability.union, hb]
    · rw [Capability.union, Capability.permits, if_neg (by simp [ha, hb])]
      simp only [Capability.permits, hb, Bool.false_or] at h
      simp only [List.any_append, h, Bool.or_true]

-- ── Scoped paths, for paths not known statically ────────────────────────────

/-- A path together with a proof that `cap` allows `op` on it.

    For paths known at compile time the obligation on each operation is
    discharged by `decide` and this type is not needed. It exists for paths
    computed at runtime: `check?` validates one and hands back the evidence, so
    the operations still cannot be reached without it.

    Indexed by the operation, because a capability may permit reading a path
    without permitting writing it — evidence for one is not evidence for the
    other. -/
structure ScopedPath (cap : Capability) (op : Op) where
  /-- The path. -/
  path    : Path
  /-- Evidence that `cap` permits `op` on it. -/
  inScope : cap.permits op path = true

/-- Validate a runtime path against `cap` for one operation, returning the
    evidence on success. -/
def ScopedPath.check? (cap : Capability) (op : Op) (p : Path) :
    Option (ScopedPath cap op) :=
  if h : cap.permits op p = true then some ⟨p, h⟩ else none

-- ── The effect ──────────────────────────────────────────────────────────────

/-- Filesystem operations available under the capability `cap`.

    Each constructor takes a proof that `cap` grants the operation *and* a proof
    that `cap` allows that operation on the path, so both the permission and the
    scope are part of what it means for the request to exist. -/
inductive FileSystem (cap : Capability) : Type → Type where
  /-- Read a file's contents. Requires read permission and path scope. -/
  | readFile   (hp : cap.canRead = true) (path : Path)
      (hs : cap.permits .read path = true) : FileSystem cap ByteArray
  /-- Write (creating or overwriting) a file. Requires write permission and
      path scope. -/
  | writeFile  (hp : cap.canWrite = true) (path : Path)
      (hs : cap.permits .write path = true) (data : ByteArray) : FileSystem cap Unit
  /-- Remove a file. Requires delete permission and path scope. -/
  | deleteFile (hp : cap.canDelete = true) (path : Path)
      (hs : cap.permits .delete path = true) : FileSystem cap Unit

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

    Requires `CanRead cap` and a proof that `cap` allows reading `path`, both
    resolved from the capability the row carries. Under a capability without the
    read bit, or for a path no read-granting scope covers, this call does not
    elaborate. -/
def readFile {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanRead cap] (path : Path)
    (hs : cap.permits .read path = true := by decide) : Eff effs ByteArray :=
  .impure (fs.inject (.readFile perm.proof path hs)) .protect

/-- Write a file, creating or overwriting it.

    Requires `CanWrite cap` and write scope on `path`. Under a read-only
    capability, or for a path only a read-granting scope covers, this call does
    not elaborate. -/
def writeFile {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanWrite cap] (path : Path)
    (data : ByteArray) (hs : cap.permits .write path = true := by decide) :
    Eff effs Unit :=
  .impure (fs.inject (.writeFile perm.proof path hs data)) .protect

/-- Remove a file.

    Requires `CanDelete cap` and delete scope on `path`. A capability granting
    read and write but not delete makes this call fail to elaborate — as does one
    granting all three globally but only reading under `path`'s directory. Those
    are the distinctions Haskell's type-level effect rows cannot draw. -/
def deleteFile {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanDelete cap] (path : Path)
    (hs : cap.permits .delete path = true := by decide) : Eff effs Unit :=
  .impure (fs.inject (.deleteFile perm.proof path hs)) .protect

/-- Read a file's contents and decode them as UTF-8, or `none` if the bytes are
    not valid UTF-8.

    Total by construction: `String.fromUTF8?` rather than the panicking
    `String.fromUTF8!`, so a malformed file is a value the caller handles, not a
    crash. -/
def readFileString? {effs : List (Type → Type)} {cap : Capability}
    [HasFileSystem effs cap] [CanRead cap] (path : Path)
    (hs : cap.permits .read path = true := by decide) : Eff effs (Option String) :=
  String.fromUTF8? <$> readFile path hs

/-- Write a string to a file as UTF-8. -/
def writeFileString {effs : List (Type → Type)} {cap : Capability}
    [HasFileSystem effs cap] [CanWrite cap] (path : Path) (contents : String)
    (hs : cap.permits .write path = true := by decide) : Eff effs Unit :=
  writeFile path contents.toUTF8 hs

-- ── Operations on runtime-validated paths ───────────────────────────────────

/-- Read a file at a path validated at runtime.

    The `ScopedPath` supplies the scope evidence, so no `decide` is involved and
    the path need not be statically known. -/
def readFileAt {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanRead cap] (sp : ScopedPath cap .read) :
    Eff effs ByteArray :=
  .impure (fs.inject (.readFile perm.proof sp.path sp.inScope)) .protect

/-- Write a file at a path validated at runtime. -/
def writeFileAt {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanWrite cap] (sp : ScopedPath cap .write)
    (data : ByteArray) : Eff effs Unit :=
  .impure (fs.inject (.writeFile perm.proof sp.path sp.inScope data)) .protect

/-- Delete a file at a path validated at runtime. -/
def deleteFileAt {effs : List (Type → Type)} {cap : Capability}
    [fs : HasFileSystem effs cap] [perm : CanDelete cap] (sp : ScopedPath cap .delete) :
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
  { canRead := true, canWrite := true, scopes := [under root] }

/-- Read and write under `workRoot`, read-only under `configRoot`, nothing
    anywhere else — the two-tier shape a single `roots` list cannot express. -/
abbrev workspace (workRoot configRoot : Path) : Capability :=
  { canRead := true, canWrite := true
  , scopes := [under workRoot [.read, .write], under configRoot [.read]] }

end Control.Monad.Effect.FileSystem
