/-
  `Control.Monad.Effect.SecretStore` — a capability-restricted secret store

  ## Not a Haskell port

  `linen`-original, the sixth instance of the pattern `Effect.FileSystem`
  established, and the one that makes the strongest case for it. See
  `docs/imports/FreerSimple/dependencies.md`.

  ## The distinction this module exists for

  **Reading a secret's metadata and reading its value are separate
  permissions.** A capability granting `describe` and not `getValue` makes

      getValue "db-password"

  **fail to elaborate**, while

      describe "db-password"

  compiles. Both operations have the same effect type and differ only in a
  *value* indexing it — so no type-level effect row can draw this distinction
  at all, and any row-based system must either split the effect in two or check
  at runtime.

  This is not a hypothetical. "Does this secret exist, and which version is
  deployed?" is the question a health check, a deployment gate and a config
  validator all need, and none of them need the plaintext. Granting them the
  value because the interface has one method is exactly how a secret ends up in
  a log line. The sibling `typednotes/infra` keeps the same split at runtime,
  describing `secretValue` as "the only inbound plaintext path in this
  interface"; here it is a compile-time fact.

  The value itself is `Cloud.Secret.Value`, which does not render, has no
  `ToJSON` instance and no `BEq` — so even a program *with* `canGetValue`
  cannot print or serialise one by accident. The two mechanisms compose:
  permission controls who may read, the opaque type controls what may then
  happen to it.

  ## Names are segments, so a prefix can be scoped

  `Name` is `List String`, so `prod/*` is expressible: a capability can grant
  the production secrets and withhold the staging ones. On clouds with flat
  names — all three, in practice — a name is a single segment and the prefix
  check degenerates to equality, which costs nothing.

  As in `Effect.ObjectStore`, the component-wise check is *stricter* than a
  byte prefix: `n!"prod"` does not cover `prod-legacy/db`, which is the safe
  direction.

  ## Design

  Same arrangement as `Effect.FileSystem` — `Bool` bits, `Prop`-class
  permissions, a `decide`-discharged scope obligation, `cap` as an `outParam`,
  and a handler that checks nothing because the constructors carry the proofs.
  See that module's header for the full argument, and note the same divergence
  as `Effect.ObjectStore`: **`scopes := []` grants nothing.**
-/
import Linen.Control.Monad.Effect
import Linen.Cloud.Secret

namespace Control.Monad.Effect.SecretStore

open Data.OpenUnion Control.Monad.Effect

-- ── Names ───────────────────────────────────────────────────────────────────

/-- A secret's name as its `/`-separated segments.

    `List String` so that a prefix is scopeable and the check reduces under
    `decide`. Single-segment on all three clouds in practice, where the prefix
    check degenerates to equality. -/
abbrev Name := List String

/-- Render a name for the backend. -/
def Name.render (n : Name) : String := "/".intercalate n

open Lean in
/-- Name literal: `n!"prod/db-password"` expands to
    `["prod", "db-password"]`. Splits at macro-expansion time, so a scope
    obligation about it still reduces under `decide`. -/
macro:max "n!" s:str : term => do
  let parts := s.getString.splitOn "/"
  let elems := parts.map (fun c => Syntax.mkStrLit c) |>.toArray
  `([$elems,*])

-- ── Operations ──────────────────────────────────────────────────────────────

/-- The kind of thing a request does to a secret.

    Note that `describe` and `getValue` are *separate operations*, not one
    operation at two levels of detail. That is the whole point of the
    module. -/
inductive Op
  /-- Read metadata: existence, current version, labels. **Never the
      value.** -/
  | describe
  /-- Read **the plaintext.** Deliberately a distinct operation with its own
      permission bit. -/
  | getValue
  /-- Store a new version. -/
  | put
  /-- Enumerate the secrets under a prefix. Metadata only. -/
  | list
  deriving DecidableEq, BEq, Repr

-- ── Capabilities ────────────────────────────────────────────────────────────

/-- One family of secrets a capability opens up.

    `ops := []` means every operation the capability's own bits allow;
    `namePrefix := []` is every secret in the store. -/
structure Scope where
  /-- Operations allowed here; `[]` means every one the capability has. -/
  ops        : List Op := []
  /-- The name prefix this scope covers, as segments. Component-wise, so
      `prod` does not cover `prod-legacy`. -/
  namePrefix : Name := []
  deriving DecidableEq, Repr

/-- What a computation is permitted to do to a secret store.

    The bit worth staring at is `canGetValue`. It defaults to `false` like the
    rest, so a capability grants plaintext access only by naming it — and a
    reader of the capability can see at a glance whether the program can read
    secrets at all.

    **`scopes := []` permits nothing**, as in `Effect.ObjectStore`. -/
structure Capability where
  /-- May read metadata. -/
  canDescribe : Bool := false
  /-- **May read plaintext.** -/
  canGetValue : Bool := false
  /-- May store new versions. -/
  canPut      : Bool := false
  /-- May enumerate secrets. -/
  canList     : Bool := false
  /-- The families of secrets this capability opens up. Empty grants
      nothing. -/
  scopes      : List Scope := []
  deriving DecidableEq, Repr

/-- Does this capability's global permission set include `op`? -/
def Capability.allows (cap : Capability) : Op → Bool
  | .describe => cap.canDescribe
  | .getValue => cap.canGetValue
  | .put      => cap.canPut
  | .list     => cap.canList

/-- Does `s` cover `op` on `name`?

    Component-wise prefix, so `prod-legacy/db` is not under `prod`. -/
def Scope.covers (s : Scope) (op : Op) (name : Name) : Bool :=
  (s.ops.isEmpty || s.ops.contains op) && s.namePrefix.isPrefixOf name

/-- Does this capability allow `op` on `name`? -/
def Capability.permits (cap : Capability) (op : Op) (name : Name) : Bool :=
  cap.scopes.any (fun s => s.covers op name)

/-- `cap` grants reading metadata. -/
class CanDescribe (cap : Capability) : Prop where
  /-- Evidence that the describe bit is set. -/
  proof : cap.canDescribe = true

/-- `cap` grants reading **plaintext**. The instance a reviewer looks for. -/
class CanGetValue (cap : Capability) : Prop where
  /-- Evidence that the value bit is set. -/
  proof : cap.canGetValue = true

/-- `cap` grants storing new versions. -/
class CanPut (cap : Capability) : Prop where
  /-- Evidence that the put bit is set. -/
  proof : cap.canPut = true

/-- `cap` grants enumeration. -/
class CanList (cap : Capability) : Prop where
  /-- Evidence that the list bit is set. -/
  proof : cap.canList = true

instance instCanDescribe {v p l : Bool} {ss : List Scope} :
    CanDescribe ⟨true, v, p, l, ss⟩ := ⟨rfl⟩
instance instCanGetValue {d p l : Bool} {ss : List Scope} :
    CanGetValue ⟨d, true, p, l, ss⟩ := ⟨rfl⟩
instance instCanPut {d v l : Bool} {ss : List Scope} :
    CanPut ⟨d, v, true, l, ss⟩ := ⟨rfl⟩
instance instCanList {d v p : Bool} {ss : List Scope} :
    CanList ⟨d, v, p, true, ss⟩ := ⟨rfl⟩

/-- Permission evidence for a capability whose bits are computed. -/
theorem CanDescribe.of {cap : Capability} (h : cap.canDescribe = true := by decide) :
    CanDescribe cap := ⟨h⟩

/-- `CanDescribe.of` for plaintext. -/
theorem CanGetValue.of {cap : Capability} (h : cap.canGetValue = true := by decide) :
    CanGetValue cap := ⟨h⟩

/-- `CanDescribe.of` for writing. -/
theorem CanPut.of {cap : Capability} (h : cap.canPut = true := by decide) :
    CanPut cap := ⟨h⟩

/-- `CanDescribe.of` for enumeration. -/
theorem CanList.of {cap : Capability} (h : cap.canList = true := by decide) :
    CanList cap := ⟨h⟩

-- ── Building capabilities ───────────────────────────────────────────────────

/-- The scope covering everything under `namePrefix`, for `ops`. -/
abbrev under (namePrefix : Name := []) (ops : List Op := []) : Scope :=
  { ops, namePrefix }

/-- Do the global bits cover every operation the scopes name? -/
def Capability.consistent (cap : Capability) : Bool :=
  cap.scopes.all (fun s => s.ops.all cap.allows)

/-- Combine two capabilities: everything either one allows.

    Note that this **can** turn two harmless capabilities into a plaintext
    reader: one granting `describe` under `prod` and one granting `getValue`
    under `staging` union to a capability granting both bits, and the scopes
    append. The bits are a global upper bound, so the union grants `getValue`
    on `staging` only — but read the result rather than assuming, and
    `consistent` is what checks the two halves agree. -/
def Capability.union (a b : Capability) : Capability :=
  { canDescribe := a.canDescribe || b.canDescribe
  , canGetValue := a.canGetValue || b.canGetValue
  , canPut      := a.canPut      || b.canPut
  , canList     := a.canList     || b.canList
  , scopes      := a.scopes ++ b.scopes }

/-- A union grants everything its left operand granted. -/
theorem allows_union_left {a b : Capability} {op : Op} (h : a.allows op = true) :
    (a.union b).allows op = true := by
  cases op <;> simp_all [Capability.allows, Capability.union]

/-- A union grants everything its right operand granted. -/
theorem allows_union_right {a b : Capability} {op : Op} (h : b.allows op = true) :
    (a.union b).allows op = true := by
  cases op <;> simp_all [Capability.allows, Capability.union]

/-- A union admits every (operation, name) pair its left operand admitted. -/
theorem permits_union_left {a b : Capability} {op : Op} {n : Name}
    (h : a.permits op n = true) : (a.union b).permits op n = true := by
  simp only [Capability.permits, Capability.union, List.any_append]
  simp only [Capability.permits] at h
  simp [h]

/-- A union admits every pair its right operand admitted. -/
theorem permits_union_right {a b : Capability} {op : Op} {n : Name}
    (h : b.permits op n = true) : (a.union b).permits op n = true := by
  simp only [Capability.permits, Capability.union, List.any_append]
  simp only [Capability.permits] at h
  simp [h]

-- ── Scoped names, for names not known statically ────────────────────────────

/-- A secret name together with a proof that `cap` allows `op` on it.

    Indexed by the operation, because "may describe" is not evidence of "may
    read the value" — which is the whole distinction, so conflating the two
    here would undo it. -/
structure ScopedName (cap : Capability) (op : Op) where
  /-- The name. -/
  name    : Name
  /-- Evidence that `cap` permits `op` on it. -/
  inScope : cap.permits op name = true

/-- Validate a runtime name against `cap` for one operation. -/
def ScopedName.check? (cap : Capability) (op : Op) (name : Name) :
    Option (ScopedName cap op) :=
  if h : cap.permits op name = true then some ⟨name, h⟩ else none

-- ── The effect ──────────────────────────────────────────────────────────────

/-- Secret-store operations available under the capability `cap`. -/
inductive SecretStore (cap : Capability) : Type → Type where
  /-- Read metadata. Never the value. -/
  | describe   (hp : cap.canDescribe = true) (name : Name)
      (hs : cap.permits .describe name = true) :
      SecretStore cap (Except Cloud.Error (Option Cloud.Secret.Metadata))
  /-- Read **the plaintext**, at the current version. -/
  | getValue   (hp : cap.canGetValue = true) (name : Name)
      (hs : cap.permits .getValue name = true) :
      SecretStore cap (Except Cloud.Error Cloud.Secret.Value)
  /-- Read the plaintext at a named version. -/
  | getVersion (hp : cap.canGetValue = true) (name : Name)
      (hs : cap.permits .getValue name = true) (version : String) :
      SecretStore cap (Except Cloud.Error Cloud.Secret.Value)
  /-- Store a new version. -/
  | put        (hp : cap.canPut = true) (name : Name)
      (hs : cap.permits .put name = true) (value : Cloud.Secret.Value) :
      SecretStore cap (Except Cloud.Error Cloud.Secret.Metadata)
  /-- Enumerate one page of secrets. Metadata only. -/
  | list       (hp : cap.canList = true) (namePrefix : Name)
      (hs : cap.permits .list namePrefix = true) (cursor : Option Cloud.Cursor) :
      SecretStore cap (Except Cloud.Error (Cloud.Page Cloud.Secret.Metadata))

/-- Locates a `SecretStore` effect in the row and recovers which capability it
    carries. -/
class HasSecretStore (effs : List (Type → Type)) (cap : outParam Capability) where
  /-- Inject a secret-store request into the row. -/
  inject : {α : Type} → SecretStore cap α → Union effs α

/-- The secret-store effect is the row's head. -/
instance instHasSecretStoreHere {cap : Capability} {effs : List (Type → Type)} :
    HasSecretStore (SecretStore cap :: effs) cap where
  inject e := .here e

/-- The secret-store effect is somewhere in the row's tail. -/
instance instHasSecretStoreThere {cap : Capability} {eff : Type → Type}
    {effs : List (Type → Type)} [HasSecretStore effs cap] :
    HasSecretStore (eff :: effs) cap where
  inject e := .there (HasSecretStore.inject e)

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Read a secret's metadata: whether it exists, and which version is current.

    Needs only `CanDescribe`, so a health check or a deployment gate can be
    written without any authority to read plaintext. -/
def describe {effs : List (Type → Type)} {cap : Capability}
    [h : HasSecretStore effs cap] [perm : CanDescribe cap] (name : Name)
    (hs : cap.permits .describe name = true := by decide) :
    Eff effs (Except Cloud.Error (Option Cloud.Secret.Metadata)) :=
  .impure (h.inject (.describe perm.proof name hs)) .protect

/-- Read a secret's **plaintext**.

    Requires `CanGetValue`, which `describe`-only capabilities do not have — so
    under one of those this call does not elaborate. That is the module's
    reason for existing. -/
def getValue {effs : List (Type → Type)} {cap : Capability}
    [h : HasSecretStore effs cap] [perm : CanGetValue cap] (name : Name)
    (hs : cap.permits .getValue name = true := by decide) :
    Eff effs (Except Cloud.Error Cloud.Secret.Value) :=
  .impure (h.inject (.getValue perm.proof name hs)) .protect

/-- Read a secret's plaintext at a named version. -/
def getVersion {effs : List (Type → Type)} {cap : Capability}
    [h : HasSecretStore effs cap] [perm : CanGetValue cap] (name : Name)
    (version : String)
    (hs : cap.permits .getValue name = true := by decide) :
    Eff effs (Except Cloud.Error Cloud.Secret.Value) :=
  .impure (h.inject (.getVersion perm.proof name hs version)) .protect

/-- Read a secret's plaintext as text.

    A `protocol` error for a secret that is not valid UTF-8, which is the
    honest answer for a binary secret read as a string. -/
def getString {effs : List (Type → Type)} {cap : Capability}
    [HasSecretStore effs cap] [CanGetValue cap] (name : Name)
    (hs : cap.permits .getValue name = true := by decide) :
    Eff effs (Except Cloud.Error String) :=
  (fun r => r.bind fun v =>
    match v.exposeString? with
    | some s => .ok s
    | none   =>
      .error (Cloud.Error.protocol s!"secret '{Name.render name}' is not valid UTF-8"))
  <$> getValue name hs

/-- Whether a secret exists. Metadata only, so it needs no authority to read
    the value. -/
def exists? {effs : List (Type → Type)} {cap : Capability}
    [HasSecretStore effs cap] [CanDescribe cap] (name : Name)
    (hs : cap.permits .describe name = true := by decide) :
    Eff effs (Except Cloud.Error Bool) :=
  (fun r => r.map (·.isSome)) <$> describe name hs

/-- Store a new version. -/
def put {effs : List (Type → Type)} {cap : Capability}
    [h : HasSecretStore effs cap] [perm : CanPut cap] (name : Name)
    (value : Cloud.Secret.Value)
    (hs : cap.permits .put name = true := by decide) :
    Eff effs (Except Cloud.Error Cloud.Secret.Metadata) :=
  .impure (h.inject (.put perm.proof name hs value)) .protect

/-- Store a new version from text. -/
def putString {effs : List (Type → Type)} {cap : Capability}
    [HasSecretStore effs cap] [CanPut cap] (name : Name) (contents : String)
    (hs : cap.permits .put name = true := by decide) :
    Eff effs (Except Cloud.Error Cloud.Secret.Metadata) :=
  put name (Cloud.Secret.Value.ofString contents) hs

/-- Enumerate one page of secrets under a prefix. Metadata only. -/
def list {effs : List (Type → Type)} {cap : Capability}
    [h : HasSecretStore effs cap] [perm : CanList cap] (namePrefix : Name := [])
    (cursor : Option Cloud.Cursor := none)
    (hs : cap.permits .list namePrefix = true := by decide) :
    Eff effs (Except Cloud.Error (Cloud.Page Cloud.Secret.Metadata)) :=
  .impure (h.inject (.list perm.proof namePrefix hs cursor)) .protect

-- ── Operations on runtime-validated names ───────────────────────────────────

/-- Read metadata for a name validated at runtime. -/
def describeAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasSecretStore effs cap] [perm : CanDescribe cap]
    (sn : ScopedName cap .describe) :
    Eff effs (Except Cloud.Error (Option Cloud.Secret.Metadata)) :=
  .impure (h.inject (.describe perm.proof sn.name sn.inScope)) .protect

/-- Read the plaintext for a name validated at runtime.

    Takes a `ScopedName cap .getValue` specifically: evidence for `.describe`
    is not evidence for this, which is what stops the runtime-validated path
    from being a way around the split. -/
def getValueAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasSecretStore effs cap] [perm : CanGetValue cap]
    (sn : ScopedName cap .getValue) :
    Eff effs (Except Cloud.Error Cloud.Secret.Value) :=
  .impure (h.inject (.getValue perm.proof sn.name sn.inScope)) .protect

-- ── Handler ─────────────────────────────────────────────────────────────────

/-- Run a secret-store computation against a backend.

    Performs no permission or scope check: the constructors carry both proofs.
    The same program therefore runs against AWS Secrets Manager, Google Secret
    Manager, Scaleway's, or `Cloud.SecretStore.inMemory`. -/
def runSecretStoreWith (cap : Capability) {α : Type} (backend : Cloud.SecretStore) :
    Eff [SecretStore cap] α → IO α :=
  interpretM fun
    | .describe _ n _        => backend.metadata (Name.render n)
    | .getValue _ n _        => backend.getValue (Name.render n)
    | .getVersion _ n _ v    => backend.getVersion (Name.render n) v
    | .put _ n _ value       => backend.put (Name.render n) value
    | .list _ _ _ cursor     => backend.list cursor

/-- Interpret purely, into the operations the computation would perform.

    Note what the log records: `getValue db-password` names the *secret*, never
    its value — a dry run of a program that reads secrets is safe to print,
    which it would not be if the answers were logged. Reads answer an empty
    value. -/
def dryRun {cap : Capability} {α : Type} : Eff [SecretStore cap] α → α × List String :=
  go []
where
  /-- The accumulator carries the operations seen so far, most recent first. -/
  go (acc : List String) : Eff [SecretStore cap] α → α × List String
    | .protect a  => (a, acc.reverse)
    | .impure u k => match u with
      | .here e => match e with
        | .describe _ n _ =>
            go (s!"describe {Name.render n}" :: acc) (k (.ok none))
        | .getValue _ n _ =>
            go (s!"getValue {Name.render n}" :: acc)
              (k (.ok (Cloud.Secret.Value.ofString "")))
        | .getVersion _ n _ v =>
            go (s!"getVersion {Name.render n} {v}" :: acc)
              (k (.ok (Cloud.Secret.Value.ofString "")))
        | .put _ n _ value =>
            go (s!"put {Name.render n} ({value.size} bytes)" :: acc)
              (k (.ok { name := Name.render n }))
        | .list _ p _ _ =>
            go (s!"list {Name.render p}" :: acc) (k (.ok Cloud.Page.empty))
      | .there u' => u'.elim0

-- ── Common capabilities ─────────────────────────────────────────────────────

/-- Metadata only, over every secret. **No plaintext access at all.**

    The capability a health check, a deployment gate or a config validator
    wants: it can confirm that every secret it needs exists and see which
    version is current, and `getValue` under it does not compile. -/
abbrev describeOnly : Capability :=
  { canDescribe := true, canList := true, scopes := [under [] [.describe, .list]] }

/-- Read the plaintext of secrets under one prefix, and nothing else.

    The shape a service wants at start-up: it reads its own secrets and cannot
    write them, cannot enumerate the store, and cannot touch another
    environment's. -/
abbrev readUnder (namePrefix : Name) : Capability :=
  { canDescribe := true, canGetValue := true
  , scopes := [under namePrefix [.describe, .getValue]] }

/-- Read one named secret's plaintext, and nothing else.

    The tightest useful capability, and the one to prefer: a program that needs
    the database password should be able to read the database password. -/
abbrev readOne (name : Name) : Capability :=
  { canDescribe := true, canGetValue := true
  , scopes := [under name [.describe, .getValue]] }

/-- Write secrets under one prefix without being able to read them back — a
    rotation job.

    Worth noting as the mirror of the module's point: a rotator needs to
    *replace* a secret's value and has no business reading the old one. -/
abbrev rotateUnder (namePrefix : Name) : Capability :=
  { canDescribe := true, canPut := true
  , scopes := [under namePrefix [.describe, .put]] }

end Control.Monad.Effect.SecretStore
