/-
  `Control.Monad.Effect.ObjectStore` — a capability-restricted object store

  ## Not a Haskell port

  `linen`-original, like `Effect.FileSystem`, `Effect.HTTP` and
  `Effect.PostgreSQL`, and documented as such in
  `docs/imports/FreerSimple/dependencies.md`. It is the fourth instance of that
  pattern and the first over a **remote** backend.

  ## What this adds over the row

  `Eff [ObjectStore cap] α` cannot reach the network or the filesystem — that
  much a Haskell-style effect row gives you. What it cannot give you is *which
  bucket* and *which keys*, because a row is a list of type constructors and a
  name has no structure.

  Here the permission is a **value** indexing the effect, so a capability can
  say

      read anything under logs/ in the analytics bucket
      read and write anything under reports/ in the same bucket
      nothing anywhere else

  and `put` to `logs/` fails to *elaborate*, as does `get` from another bucket
  entirely. Neither is a runtime check the type system knows nothing about.

  ## The handler takes the backend, so one program runs anywhere

  `runObjectStoreWith` is parameterised by a `Cloud.ObjectStore`, exactly as
  `Effect.HTTP`'s `runHTTPWith` is parameterised by its transport. So the *same*
  `Eff [ObjectStore cap] α` value runs against S3, against Google Cloud
  Storage, or against `Cloud.ObjectStore.inMemory` — and the tests below
  exercise it end to end with no network and no credentials.

  ## Two divergences from `Effect.FileSystem`, both deliberate

  **1. An empty scope list permits nothing.** In `FileSystem`, `scopes := []`
  means "no path restriction". Here it means "no access at all", so there is no
  `full` capability and none can be written by accident. A filesystem
  capability with no scopes is bounded by the process's own permissions; a
  cloud credential's blast radius is every bucket in the account, and defaulting
  to that is not a default anyone wants. Every capability therefore names its
  buckets.

  **2. `k!` does not filter empty segments, while `p!` does.** An S3 key is an
  opaque byte string that merely looks like a path: `a//b`, `a/` and `/a` are
  three *different* keys, and filtering would make them unaddressable.
  `String.splitOn "/"` and `"/".intercalate` are exact inverses on every
  string, so `List String` is a faithful representation and needs no
  well-formedness field. This looks like a bug and is not; the tests pin it.

  ## The scope prefix is stricter than S3's

  S3's own `prefix=` parameter is a **byte** prefix, so `prefix=log` matches
  `logs/a`. A `Scope` with `keyPrefix := k!"logs"` does *not* admit
  `logs-2026/a`, because `List.isPrefixOf ["logs"] ["logs-2026", "a"]` is
  false. That is the safe direction, and the same trap `FileSystem` pins with
  `/tmp/sandbox-evil` and `HTTP` with `/v1-admin`.

  The handler compensates: `list` sends a **delimiter-terminated** prefix on
  the wire (`Key.render p ++ "/"`), so what the provider enumerates cannot
  exceed what the capability authorised. Sending the bare prefix would let a
  permitted `list` return keys a `get` would have been refused.

  ## Design

  Same arrangement as `FileSystem`, for the same reasons — see its header for
  the full argument:

  - `Capability` has `Bool` permission fields, so a literal capability is
    computable and each permission instance is discharged by `rfl`.
  - The permission obligations are **instances** (`CanGet`, …) and the scope
    obligation is an **auto-param** (`:= by decide`). An auto-param fires before
    `cap` is unified and fails on a metavariable, so permissions cannot be
    auto-params; the scope obligation depends on the key argument, so it cannot
    be an instance keyed on `cap` alone.
  - `HasObjectStore` locates the effect with `cap` as an `outParam`, which is
    what determines `cap` in time for `decide` to run inside a `do` block.
  - The handler performs **no checks**: every constructor carries both proofs,
    so enforcement happens once, at elaboration.

  ## A note on the cost of `decide`

  The bucket is a `String` and is compared for equality, which *does* reduce in
  the kernel — `Effect.PostgreSQL` relies on the same for its table names. What
  does not survive is a *fold* of string equalities over a long list: the
  sibling `typednotes/infra` records taking Lean down with a kernel stack
  overflow that way. So keep scope lists short, and never put an ARN, a URL or
  a bucket's full endpoint in a scope — bare names only.
-/
import Linen.Control.Monad.Effect
import Linen.Cloud.ObjectStore

namespace Control.Monad.Effect.ObjectStore

open Data.OpenUnion Control.Monad.Effect

-- ── Keys ────────────────────────────────────────────────────────────────────

/-- An object key as its `/`-separated segments.

    `List String` rather than `String` for the reason `FileSystem.Path` is:
    `String.startsWith` does not reduce under `decide`, so a string-prefix
    scope check could not be discharged at elaboration time at all, while
    `List.isPrefixOf` reduces. -/
abbrev Key := List String

/-- Render a key for the wire.

    Exactly inverse to the split `k!` performs, on every string — which is what
    makes `Key` a faithful representation of an opaque S3 key rather than a
    lossy one. -/
def Key.render (k : Key) : String := "/".intercalate k

open Lean in
/-- Key literal: `k!"logs/2026/a.json"` expands to
    `["logs", "2026", "a.json"]`.

    **Does not filter empty segments**, unlike `FileSystem`'s `p!`: `a//b`,
    `a/` and `/a` are three different S3 keys and all three must be
    addressable. See the module header.

    The split happens at macro-expansion time, so the result is a literal list
    and a scope obligation about it still reduces under `decide`. -/
macro:max "k!" s:str : term => do
  let parts := s.getString.splitOn "/"
  let elems := parts.map (fun c => Syntax.mkStrLit c) |>.toArray
  `([$elems,*])

-- ── Operations ──────────────────────────────────────────────────────────────

/-- The kind of thing a request does to an object. Each is separately
    grantable, both globally and per scope. -/
inductive Op
  /-- Read an object's contents, or its metadata. -/
  | get
  /-- Create or overwrite an object. -/
  | put
  /-- Remove an object. -/
  | delete
  /-- Enumerate keys under a prefix. -/
  | list
  deriving DecidableEq, BEq, Repr

-- ── Capabilities ────────────────────────────────────────────────────────────

/-- One region of one bucket a capability opens up.

    `ops := []` means "every operation the capability's own bits allow", so a
    scope names operations only when it is *more* restrictive than the
    capability as a whole. `keyPrefix := []` is the whole bucket. -/
structure Scope where
  /-- Operations allowed in this scope; `[]` means every operation the
      capability has. -/
  ops       : List Op := []
  /-- The bucket this scope covers. Matched exactly — there is no wildcard,
      because a bucket-name pattern is how a capability accidentally covers a
      bucket created next week. -/
  bucket    : String
  /-- The key prefix this scope covers, as segments; `[]` is the whole bucket.
      Component-wise, so `logs` does not cover `logs-2026`. -/
  keyPrefix : Key := []
  deriving DecidableEq, Repr

/-- What a computation is permitted to do to an object store: which
    operations, in which buckets, under which key prefixes.

    Permission fields default to `false`, so `{ canGet := true, scopes := … }`
    denies writing and deleting by construction.

    **`scopes := []` permits nothing.** This is the deliberate divergence from
    `FileSystem`, where an empty scope list means "unrestricted" — see the
    module header. Every capability names its buckets.

    Declare capability constants with `abbrev`, not `def`: instance resolution
    does not unfold a non-reducible `def`, so `def myCap` leaves
    `CanGet myCap` unresolvable. -/
structure Capability where
  /-- May read object contents and metadata. -/
  canGet    : Bool := false
  /-- May create and overwrite objects. -/
  canPut    : Bool := false
  /-- May remove objects. -/
  canDelete : Bool := false
  /-- May enumerate keys. -/
  canList   : Bool := false
  /-- The regions of which buckets this capability opens up. Empty grants
      nothing. -/
  scopes    : List Scope := []
  deriving DecidableEq, Repr

/-- Does this capability's global permission set include `op`? -/
def Capability.allows (cap : Capability) : Op → Bool
  | .get    => cap.canGet
  | .put    => cap.canPut
  | .delete => cap.canDelete
  | .list   => cap.canList

/-- Does `s` cover operation `op` on `key` in `bucket`?

    The prefix must be a *component-wise* prefix, so `logs-2026/a` is not under
    `logs` however the two compare as strings. -/
def Scope.covers (s : Scope) (op : Op) (bucket : String) (key : Key) : Bool :=
  (s.ops.isEmpty || s.ops.contains op) && s.bucket == bucket
    && s.keyPrefix.isPrefixOf key

/-- Does this capability allow performing `op` on `key` in `bucket`?

    True when *some* scope covers the triple. Unlike `FileSystem.permits`,
    there is no unscoped fallback: no scopes, no access. -/
def Capability.permits (cap : Capability) (op : Op) (bucket : String) (key : Key) : Bool :=
  cap.scopes.any (fun s => s.covers op bucket key)

/-- `cap` grants reading. Carries the proof, so it cannot be forged. -/
class CanGet (cap : Capability) : Prop where
  /-- Evidence that the read bit is set. -/
  proof : cap.canGet = true

/-- `cap` grants writing. -/
class CanPut (cap : Capability) : Prop where
  /-- Evidence that the write bit is set. -/
  proof : cap.canPut = true

/-- `cap` grants deletion. -/
class CanDelete (cap : Capability) : Prop where
  /-- Evidence that the delete bit is set. -/
  proof : cap.canDelete = true

/-- `cap` grants enumeration. -/
class CanList (cap : Capability) : Prop where
  /-- Evidence that the list bit is set. -/
  proof : cap.canList = true

instance instCanGet {p d l : Bool} {ss : List Scope} : CanGet ⟨true, p, d, l, ss⟩ := ⟨rfl⟩
instance instCanPut {g d l : Bool} {ss : List Scope} : CanPut ⟨g, true, d, l, ss⟩ := ⟨rfl⟩
instance instCanDelete {g p l : Bool} {ss : List Scope} :
    CanDelete ⟨g, p, true, l, ss⟩ := ⟨rfl⟩
instance instCanList {g p d : Bool} {ss : List Scope} : CanList ⟨g, p, d, true, ss⟩ := ⟨rfl⟩

/-- The permission evidence for a capability whose bits are *computed* rather
    than written literally — a `Capability.union`, say.

    The instances above match the bit syntactically, which is all instance
    resolution can do, so a computed capability needs its instance declared
    once: `instance : CanGet (a.union b) := .of`. -/
theorem CanGet.of {cap : Capability} (h : cap.canGet = true := by decide) :
    CanGet cap := ⟨h⟩

/-- `CanGet.of` for writing. -/
theorem CanPut.of {cap : Capability} (h : cap.canPut = true := by decide) :
    CanPut cap := ⟨h⟩

/-- `CanGet.of` for deletion. -/
theorem CanDelete.of {cap : Capability} (h : cap.canDelete = true := by decide) :
    CanDelete cap := ⟨h⟩

/-- `CanGet.of` for enumeration. -/
theorem CanList.of {cap : Capability} (h : cap.canList = true := by decide) :
    CanList cap := ⟨h⟩

-- ── Building capabilities ───────────────────────────────────────────────────

/-- The scope covering everything under `keyPrefix` in `bucket`, for `ops` (or
    for every operation the capability grants, when `ops` is empty). -/
abbrev under (bucket : String) (keyPrefix : Key := []) (ops : List Op := []) : Scope :=
  { ops, bucket, keyPrefix }

/-- Do the global bits cover every operation the scopes name?

    Not required for soundness — the bits are an upper bound, so a scope naming
    an operation the bits withhold is simply dead — but the two disagreeing is
    almost always a mistake, so assert `#guard cap.consistent` beside a
    capability definition. -/
def Capability.consistent (cap : Capability) : Bool :=
  cap.scopes.all (fun s => s.ops.all cap.allows)

/-- Combine two capabilities: everything either one allows.

    Simpler than `FileSystem.union`, which has to special-case an unscoped
    capability: here `scopes := []` grants nothing, so appending is always
    right. -/
def Capability.union (a b : Capability) : Capability :=
  { canGet    := a.canGet    || b.canGet
  , canPut    := a.canPut    || b.canPut
  , canDelete := a.canDelete || b.canDelete
  , canList   := a.canList   || b.canList
  , scopes    := a.scopes ++ b.scopes }

/-- A union grants everything its left operand granted. -/
theorem allows_union_left {a b : Capability} {op : Op} (h : a.allows op = true) :
    (a.union b).allows op = true := by
  cases op <;> simp_all [Capability.allows, Capability.union]

/-- A union grants everything its right operand granted. -/
theorem allows_union_right {a b : Capability} {op : Op} (h : b.allows op = true) :
    (a.union b).allows op = true := by
  cases op <;> simp_all [Capability.allows, Capability.union]

/-- A union admits every (operation, bucket, key) triple its left operand
    admitted — the monotonicity that makes combining capabilities safe. -/
theorem permits_union_left {a b : Capability} {op : Op} {bucket : String} {key : Key}
    (h : a.permits op bucket key = true) : (a.union b).permits op bucket key = true := by
  simp only [Capability.permits, Capability.union, List.any_append]
  simp only [Capability.permits] at h
  simp [h]

/-- A union admits every triple its right operand admitted. -/
theorem permits_union_right {a b : Capability} {op : Op} {bucket : String} {key : Key}
    (h : b.permits op bucket key = true) : (a.union b).permits op bucket key = true := by
  simp only [Capability.permits, Capability.union, List.any_append]
  simp only [Capability.permits] at h
  simp [h]

-- ── Scoped keys, for keys not known statically ──────────────────────────────

/-- A bucket and key together with a proof that `cap` allows `op` on them.

    For keys known at compile time the obligation is discharged by `decide` and
    this type is not needed. It exists for keys computed at runtime — a key
    derived from a request parameter, say: `check?` validates one and hands
    back the evidence, so the operations still cannot be reached without it.

    Indexed by the operation, because a capability may permit reading a key
    without permitting writing it. -/
structure ScopedKey (cap : Capability) (op : Op) where
  /-- The bucket. -/
  bucket  : String
  /-- The key. -/
  key     : Key
  /-- Evidence that `cap` permits `op` on it. -/
  inScope : cap.permits op bucket key = true

/-- Validate a runtime bucket and key against `cap` for one operation,
    returning the evidence on success. -/
def ScopedKey.check? (cap : Capability) (op : Op) (bucket : String) (key : Key) :
    Option (ScopedKey cap op) :=
  if h : cap.permits op bucket key = true then some ⟨bucket, key, h⟩ else none

-- ── The effect ──────────────────────────────────────────────────────────────

/-- Object-store operations available under the capability `cap`.

    Each constructor takes a proof that `cap` grants the operation *and* a
    proof that `cap` allows it on that bucket and key, so both the permission
    and the scope are part of what it means for the request to exist. -/
inductive ObjectStore (cap : Capability) : Type → Type where
  /-- Read an object's contents. -/
  | get    (hp : cap.canGet = true) (bucket : String) (key : Key)
      (hs : cap.permits .get bucket key = true) :
      ObjectStore cap (Except Cloud.Error ByteArray)
  /-- Read an object's metadata, without its contents. Requires the same
      permission as `get`: metadata is not a weaker thing to know here, unlike
      for a secret. -/
  | head   (hp : cap.canGet = true) (bucket : String) (key : Key)
      (hs : cap.permits .get bucket key = true) :
      ObjectStore cap (Except Cloud.Error (Option Cloud.ObjectMeta))
  /-- Create or overwrite an object. -/
  | put    (hp : cap.canPut = true) (bucket : String) (key : Key)
      (hs : cap.permits .put bucket key = true)
      (body : ByteArray) (opts : Cloud.PutOptions) :
      ObjectStore cap (Except Cloud.Error Cloud.ObjectMeta)
  /-- Remove an object. -/
  | delete (hp : cap.canDelete = true) (bucket : String) (key : Key)
      (hs : cap.permits .delete bucket key = true) :
      ObjectStore cap (Except Cloud.Error Unit)
  /-- Enumerate one page of keys under a prefix. -/
  | list   (hp : cap.canList = true) (bucket : String) (keyPrefix : Key)
      (hs : cap.permits .list bucket keyPrefix = true) (cursor : Option Cloud.Cursor) :
      ObjectStore cap (Except Cloud.Error (Cloud.Page Cloud.ObjectMeta))

/-- Locates an `ObjectStore` effect in the row and recovers *which* capability
    it carries.

    `cap` is an `outParam`: it is an output of resolving against `effs`, not
    something the caller must supply. That is what keeps both the permission
    obligations and the scope obligation solvable inside `do`-notation. -/
class HasObjectStore (effs : List (Type → Type)) (cap : outParam Capability) where
  /-- Inject an object-store request into the row. -/
  inject : {α : Type} → ObjectStore cap α → Union effs α

/-- The object-store effect is the row's head. -/
instance instHasObjectStoreHere {cap : Capability} {effs : List (Type → Type)} :
    HasObjectStore (ObjectStore cap :: effs) cap where
  inject e := .here e

/-- The object-store effect is somewhere in the row's tail. -/
instance instHasObjectStoreThere {cap : Capability} {eff : Type → Type}
    {effs : List (Type → Type)} [HasObjectStore effs cap] :
    HasObjectStore (eff :: effs) cap where
  inject e := .there (HasObjectStore.inject e)

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Read an object's contents.

    Requires `CanGet cap` and a proof that `cap` allows reading that key in
    that bucket. Under a capability without the read bit, or for a bucket or
    prefix no read-granting scope covers, this call does not elaborate. -/
def get {effs : List (Type → Type)} {cap : Capability}
    [h : HasObjectStore effs cap] [perm : CanGet cap] (bucket : String) (key : Key)
    (hs : cap.permits .get bucket key = true := by decide) :
    Eff effs (Except Cloud.Error ByteArray) :=
  .impure (h.inject (.get perm.proof bucket key hs)) .protect

/-- Read an object's metadata. -/
def head {effs : List (Type → Type)} {cap : Capability}
    [h : HasObjectStore effs cap] [perm : CanGet cap] (bucket : String) (key : Key)
    (hs : cap.permits .get bucket key = true := by decide) :
    Eff effs (Except Cloud.Error (Option Cloud.ObjectMeta)) :=
  .impure (h.inject (.head perm.proof bucket key hs)) .protect

/-- Create or overwrite an object.

    Under a read-only capability, or for a prefix only a read-granting scope
    covers, this call does not elaborate. -/
def put {effs : List (Type → Type)} {cap : Capability}
    [h : HasObjectStore effs cap] [perm : CanPut cap] (bucket : String) (key : Key)
    (body : ByteArray) (opts : Cloud.PutOptions := {})
    (hs : cap.permits .put bucket key = true := by decide) :
    Eff effs (Except Cloud.Error Cloud.ObjectMeta) :=
  .impure (h.inject (.put perm.proof bucket key hs body opts)) .protect

/-- Remove an object.

    A capability granting get, put and list but not delete makes this call fail
    to elaborate — as does one granting all four but only reading under this
    key's prefix. Those are the distinctions a type-level effect row cannot
    draw. -/
def delete {effs : List (Type → Type)} {cap : Capability}
    [h : HasObjectStore effs cap] [perm : CanDelete cap] (bucket : String) (key : Key)
    (hs : cap.permits .delete bucket key = true := by decide) :
    Eff effs (Except Cloud.Error Unit) :=
  .impure (h.inject (.delete perm.proof bucket key hs)) .protect

/-- Enumerate one page of keys under a prefix. -/
def list {effs : List (Type → Type)} {cap : Capability}
    [h : HasObjectStore effs cap] [perm : CanList cap] (bucket : String)
    (keyPrefix : Key := []) (cursor : Option Cloud.Cursor := none)
    (hs : cap.permits .list bucket keyPrefix = true := by decide) :
    Eff effs (Except Cloud.Error (Cloud.Page Cloud.ObjectMeta)) :=
  .impure (h.inject (.list perm.proof bucket keyPrefix hs cursor)) .protect

/-- Read an object and decode it as UTF-8.

    Total by construction: a malformed object is a value the caller handles. -/
def getString {effs : List (Type → Type)} {cap : Capability}
    [HasObjectStore effs cap] [CanGet cap] (bucket : String) (key : Key)
    (hs : cap.permits .get bucket key = true := by decide) :
    Eff effs (Except Cloud.Error String) :=
  (fun r => r.bind fun bytes =>
    match String.fromUTF8? bytes with
    | some s => .ok s
    | none   => .error (Cloud.Error.protocol s!"object '{Key.render key}' is not valid UTF-8"))
  <$> get bucket key hs

/-- Write a string to an object as UTF-8. -/
def putString {effs : List (Type → Type)} {cap : Capability}
    [HasObjectStore effs cap] [CanPut cap] (bucket : String) (key : Key)
    (contents : String)
    (opts : Cloud.PutOptions := { contentType := some "text/plain; charset=utf-8" })
    (hs : cap.permits .put bucket key = true := by decide) :
    Eff effs (Except Cloud.Error Cloud.ObjectMeta) :=
  put bucket key contents.toUTF8 opts hs

-- ── Operations on runtime-validated keys ────────────────────────────────────

/-- Read an object at a key validated at runtime.

    The `ScopedKey` supplies the scope evidence, so no `decide` is involved and
    the key need not be statically known. -/
def getAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasObjectStore effs cap] [perm : CanGet cap] (sk : ScopedKey cap .get) :
    Eff effs (Except Cloud.Error ByteArray) :=
  .impure (h.inject (.get perm.proof sk.bucket sk.key sk.inScope)) .protect

/-- Write an object at a key validated at runtime. -/
def putAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasObjectStore effs cap] [perm : CanPut cap] (sk : ScopedKey cap .put)
    (body : ByteArray) (opts : Cloud.PutOptions := {}) :
    Eff effs (Except Cloud.Error Cloud.ObjectMeta) :=
  .impure (h.inject (.put perm.proof sk.bucket sk.key sk.inScope body opts)) .protect

/-- Delete an object at a key validated at runtime. -/
def deleteAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasObjectStore effs cap] [perm : CanDelete cap] (sk : ScopedKey cap .delete) :
    Eff effs (Except Cloud.Error Unit) :=
  .impure (h.inject (.delete perm.proof sk.bucket sk.key sk.inScope)) .protect

-- ── Handler ─────────────────────────────────────────────────────────────────

/-- The wire prefix for a `list`.

    **Delimiter-terminated.** S3's `prefix=` is a byte prefix, while the scope
    check is component-wise, so sending the bare prefix would let a permitted
    `list` enumerate keys under `logs-2026/` that a `get` would have been
    refused. Appending the delimiter makes the wire no broader than the check.
    An empty prefix stays empty — `"/"` would match nothing. -/
def wirePrefix (keyPrefix : Key) : String :=
  if keyPrefix.isEmpty then "" else Key.render keyPrefix ++ "/"

/-- Run an object-store computation against a backend.

    `backend` maps a bucket name to a store, because a capability may span
    several buckets; a one-bucket program passes `fun _ => store`.

    Performs no permission or scope check: every request already carries both
    proofs, so reaching this point means they were established at construction.
    The handler only renders the key and dispatches — which is what lets the
    *same* program run against S3, Cloud Storage, or an in-memory double. -/
def runObjectStoreWith (cap : Capability) {α : Type}
    (backend : String → Cloud.ObjectStore) : Eff [ObjectStore cap] α → IO α :=
  interpretM fun
    | .get _ b k _          => (backend b).get (Key.render k)
    | .head _ b k _         => (backend b).head (Key.render k)
    | .put _ b k _ body o   => (backend b).put (Key.render k) body o
    | .delete _ b k _       => (backend b).delete (Key.render k)
    | .list _ b p _ cursor  => (backend b).list (wirePrefix p) cursor

/-- Run against a single bucket's store. -/
def runObjectStore (cap : Capability) {α : Type} (store : Cloud.ObjectStore) :
    Eff [ObjectStore cap] α → IO α :=
  runObjectStoreWith cap (fun _ => store)

/-- Interpret purely, into the operations the computation would perform.

    No `IO` at all, so a test can assert the exact request sequence with
    `#guard` — the analogue of `Effect.PostgreSQL.dryRun`, and the more
    valuable of the two testing routes because it needs no stub at all. Reads
    answer empty, listings answer one empty final page. -/
def dryRun {cap : Capability} {α : Type} : Eff [ObjectStore cap] α → α × List String :=
  go []
where
  /-- The accumulator carries the operations seen so far, most recent first. -/
  go (acc : List String) : Eff [ObjectStore cap] α → α × List String
    | .protect a  => (a, acc.reverse)
    | .impure u k => match u with
      | .here e => match e with
        | .get _ b key _ =>
            go (s!"get {b} {Key.render key}" :: acc) (k (.ok ByteArray.empty))
        | .head _ b key _ =>
            go (s!"head {b} {Key.render key}" :: acc) (k (.ok none))
        | .put _ b key _ body _ =>
            go (s!"put {b} {Key.render key} ({body.size} bytes)" :: acc)
              (k (.ok { key := Key.render key, size := body.size }))
        | .delete _ b key _ =>
            go (s!"delete {b} {Key.render key}" :: acc) (k (.ok ()))
        | .list _ b p _ _ =>
            go (s!"list {b} {wirePrefix p}" :: acc) (k (.ok Cloud.Page.empty))
      | .there u' => u'.elim0

-- ── Common capabilities ─────────────────────────────────────────────────────

/-- Read-only access to a whole bucket. `put` and `delete` will not
    elaborate. -/
abbrev readOnly (bucket : String) : Capability :=
  { canGet := true, canList := true, scopes := [under bucket] }

/-- Read and write a whole bucket, but **not** delete — the permission split a
    type-level effect row cannot express. -/
abbrev readWrite (bucket : String) : Capability :=
  { canGet := true, canPut := true, canList := true, scopes := [under bucket] }

/-- Read, write and delete a whole bucket. Named for what it grants rather than
    called `full`, because there is deliberately no capability that spans every
    bucket. -/
abbrev readWriteDelete (bucket : String) : Capability :=
  { canGet := true, canPut := true, canDelete := true, canList := true
  , scopes := [under bucket] }

/-- Read and write, confined to keys under one prefix of one bucket.

    The interesting shape: two capabilities can grant the same *operations* and
    still differ in which *arguments* they admit. -/
abbrev underPrefix (bucket : String) (keyPrefix : Key) : Capability :=
  { canGet := true, canPut := true, canList := true
  , scopes := [under bucket keyPrefix] }

/-- Write-only, confined to one prefix — an uploader that cannot read back what
    anyone else put there. -/
abbrev writeOnlyPrefix (bucket : String) (keyPrefix : Key) : Capability :=
  { canPut := true, scopes := [under bucket keyPrefix [.put]] }

/-- Read and write under one prefix, read-only under another, nothing anywhere
    else — the two-tier shape a single prefix list cannot express. -/
abbrev tiered (bucket : String) (writable readable : Key) : Capability :=
  { canGet := true, canPut := true, canList := true
  , scopes := [ under bucket writable [.get, .put, .list]
              , under bucket readable [.get, .list] ] }

end Control.Monad.Effect.ObjectStore
