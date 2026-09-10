/-
  `Cloud.Secret` — reading secret values, carefully

  ## The value is not a `String`

  `Secret.Value` wraps a `ByteArray` and is deliberately awkward to get out of:

  - **It does not render.** `Repr` and `ToString` print `<redacted>`, so no
    `dbg_trace`, error message, `#eval` or panic can spill one.
  - **It has no `ToJSON` instance.** A secret therefore *cannot* be serialised
    into a log line or an HTTP response, however convenient that might be at
    the time. This is the strongest guarantee available here: the leak is not
    discouraged, it is unwritable.
  - **It has no `BEq` or `DecidableEq`.** Comparing secrets is a timing side
    channel, and no operation in this namespace needs it.
  - **`expose` is the single named exit.** Every use of the bytes goes through
    it, so `grep expose` finds them all — which is what makes an audit
    possible at all.

  `ByteArray` rather than `String` because AWS distinguishes `SecretString`
  from `SecretBinary`, and both Scaleway and GCP store bytes; a string-only
  type would make binary secrets unreachable.

  ## Metadata is separate from the value, on purpose

  `metadata` answers what a secret *is* — that it exists, its current version,
  its labels — and never its contents. `getValue` is the only inbound plaintext
  path in the interface.

  Keeping them apart is not tidiness. It is what lets
  `Control.Monad.Effect.SecretStore` grant them **separately**, so a capability
  with `canDescribe` and not `canGetValue` makes `getValue` *fail to
  elaborate*. The sibling `typednotes/infra` keeps the same split at runtime,
  calling `secretValue` "the only inbound plaintext path in this interface";
  the effect layer turns that convention into a compile-time fact.

  ## Versions

  All three clouds version secrets, and all three let a caller ask for "the
  current one" without naming it. That is the portable operation. Asking for a
  *specific* version is also portable, but note that the three number them
  differently — AWS uses opaque version ids and stage labels, GCP monotonic
  integers, Scaleway integers with a `latest` alias — so a version string from
  one cloud means nothing on another.
-/
import Linen.Cloud.Transport
import Linen.Cloud.Page

namespace Cloud

namespace Secret

-- ── The value ───────────────────────────────────────────────────────────────

/-- A secret's plaintext.

    Opaque, non-rendering, non-serialisable and non-comparable — see the module
    header for why each of those matters. -/
structure Value where
  /-- The bytes. Private: reach them through `expose`, which is the audit
      point. -/
  private bytes : ByteArray

/-- Renders as `<redacted>`, so nothing that prints can leak a secret. -/
instance : Repr Value where
  reprPrec _ _ := "<redacted>"

/-- Renders as `<redacted>`. -/
instance : ToString Value where
  toString _ := "<redacted>"

/-- The plaintext bytes.

    **The single audit point.** Named so that reaching for a secret's contents
    is visible in the source and findable with `grep`. -/
def Value.expose (v : Value) : ByteArray := v.bytes

/-- The plaintext as UTF-8, or `none` if the bytes are not valid UTF-8.

    Total: `String.fromUTF8?`, not the panicking `!` — a binary secret read as
    text is a value the caller handles, not a crash. (The sibling
    `typednotes/infra` uses the panicking form here; this is the correction.) -/
def Value.exposeString? (v : Value) : Option String := String.fromUTF8? v.bytes

/-- A secret from bytes. -/
def Value.ofBytes (bytes : ByteArray) : Value := ⟨bytes⟩

/-- A secret from text, encoded as UTF-8. -/
def Value.ofString (s : String) : Value := ⟨s.toUTF8⟩

/-- The plaintext's length in bytes.

    Safe to log: a length is not a secret, and it is often the only thing
    needed to tell "empty" from "wrong". -/
def Value.size (v : Value) : Nat := v.bytes.size

/-- Whether the secret is empty — which usually means misconfigured rather than
    deliberately blank. -/
def Value.isEmpty (v : Value) : Bool := v.bytes.isEmpty

-- ── Metadata ────────────────────────────────────────────────────────────────

/-- What a secret is, never what it holds.

    Ordinary `Repr`, unlike `Value`: everything here is safe to log, and being
    able to log it is the point — "which version is deployed" is the first
    question of every incident involving a secret. -/
structure Metadata where
  /-- The secret's name, as the caller knows it. -/
  name      : String
  /-- The current version's identifier, in the cloud's own numbering. Not
      comparable across clouds; see the module header. -/
  version   : Option String := none
  /-- When it was created or last updated, as the provider formatted it. -/
  updatedAt : Option String := none
  /-- Provider labels or tags. -/
  labels    : List (String × String) := []
  deriving Repr, DecidableEq, Inhabited

end Secret

-- ── The interface ───────────────────────────────────────────────────────────

/-- A secret store, bound to one project or account and region. -/
structure SecretStore where
  /-- What a secret is — existence, version, labels — and **never** its value.
      `none` if there is no such secret. -/
  metadata : String → IO (Except Error (Option Secret.Metadata))
  /-- The current value. The only inbound plaintext path in this interface. -/
  getValue : String → IO (Except Error Secret.Value)
  /-- A specific version's value, in the cloud's own numbering. -/
  getVersion : String → String → IO (Except Error Secret.Value)
  /-- Store a new version, creating the secret if it does not exist. -/
  put : String → Secret.Value → IO (Except Error Secret.Metadata)
  /-- One page of the secrets in this store. Metadata only. -/
  list : Option Cursor → IO (Except Error (Page Secret.Metadata))
  /-- Where this store is, for diagnostics. Never a credential. -/
  describe : String := "secret store"

-- ── Derived operations ──────────────────────────────────────────────────────

/-- The current value, or `none` if there is no such secret.

    The "read it if it is configured" idiom, for an optional setting. -/
def SecretStore.getValue? (s : SecretStore) (name : String) :
    IO (Except Error (Option Secret.Value)) := do
  return absentAsNone (← s.getValue name)

/-- The current value as text.

    A `protocol` error for a secret that is not valid UTF-8, which is the
    honest answer for a binary secret read as a string. -/
def SecretStore.getString (s : SecretStore) (name : String) :
    IO (Except Error String) := do
  match ← s.getValue name with
  | .error e => return .error e
  | .ok v    =>
    match v.exposeString? with
    | some str => return .ok str
    | none     =>
      return .error (Error.protocol s!"secret '{name}' is not valid UTF-8")

/-- The current value as text, or `none` if there is no such secret. -/
def SecretStore.getString? (s : SecretStore) (name : String) :
    IO (Except Error (Option String)) := do
  match ← s.getValue? name with
  | .error e     => return .error e
  | .ok none     => return .ok none
  | .ok (some v) =>
    match v.exposeString? with
    | some str => return .ok (some str)
    | none     =>
      return .error (Error.protocol s!"secret '{name}' is not valid UTF-8")

/-- Store a new version from text. -/
def SecretStore.putString (s : SecretStore) (name contents : String) :
    IO (Except Error Secret.Metadata) :=
  s.put name (Secret.Value.ofString contents)

/-- Whether a secret exists. Metadata only — this does not read the value, so
    it needs no permission to. -/
def SecretStore.exists? (s : SecretStore) (name : String) : IO (Except Error Bool) := do
  match ← s.metadata name with
  | .error e => return .error e
  | .ok m    => return .ok m.isSome

/-- Read up to `maxPages` pages of the secret listing. -/
def SecretStore.listAll (s : SecretStore) (maxPages : Nat := defaultMaxPages) :
    IO (Except Error (Listing Secret.Metadata)) :=
  paginate maxPages (fun cursor => s.list cursor)

/-- The names of the secrets in this store. -/
def SecretStore.names (s : SecretStore) (maxPages : Nat := defaultMaxPages) :
    IO (Except Error (List String)) := do
  match ← s.listAll maxPages with
  | .error e => return .error e
  | .ok l    => return .ok (l.items.map (·.name))

-- ── A local backend ─────────────────────────────────────────────────────────

/-- One secret's versions in the in-memory store, newest first. -/
structure MemSecret where
  versions : List (String × ByteArray) := []

/-- The in-memory store's state. -/
structure MemSecretStore where
  /-- Secrets by name, in insertion order. -/
  secrets : List (String × MemSecret) := []
  /-- Counter for version numbering. -/
  counter : Nat := 1

/-- A secret store held in memory, for local development, debugging and tests.

    Faithful where it matters: a missing secret is `notFound` rather than an
    empty value — the failure that otherwise surfaces as a service starting up
    with a blank password and authenticating to nothing. Versions accumulate
    and are numbered, `metadata` never returns a value, and the listing is
    paginated.

    Values are held in plain memory, which is fine for a test and is the reason
    this is not something to reach for in a deployed process.

    ```
    let store ← SecretStore.inMemory
    let _ ← store.putString "db-password" "hunter2"
    IO.println (← store.getString "db-password")
    ``` -/
def SecretStore.inMemoryOf (ref : IO.Ref MemSecretStore) : SecretStore :=
  let find (name : String) : IO (Option MemSecret) := do
    return ((← ref.get).secrets.find? (·.1 == name)).map (·.2)
  let missing (name : String) : Error :=
    { klass := .notFound, status := 404, code := "ResourceNotFoundException"
    , message := s!"no secret named '{name}'" }
  { describe := "secret store (in memory)"
  , metadata := fun name => do
      match ← find name with
      | none   => return .ok none
      | some s =>
        return .ok (some
          { name, version := (s.versions.head?.map (·.1)) })
  , getValue := fun name => do
      match ← find name with
      | none   => return .error (missing name)
      | some s =>
        match s.versions.head? with
        | some (_, bytes) => return .ok (Secret.Value.ofBytes bytes)
        | none            => return .error (missing name)
  , getVersion := fun name version => do
      match ← find name with
      | none   => return .error (missing name)
      | some s =>
        match s.versions.find? (·.1 == version) with
        | some (_, bytes) => return .ok (Secret.Value.ofBytes bytes)
        | none            =>
          return .error {
              klass := .notFound, status := 404
            , message := s!"secret '{name}' has no version '{version}'" }
  , put := fun name value => do
      let version ← ref.modifyGet fun st =>
        let v := toString st.counter
        let bytes := value.expose
        let existing := (st.secrets.find? (·.1 == name)).map (·.2)
        let updated : MemSecret :=
          { versions := (v, bytes) :: (existing.map (·.versions)).getD [] }
        ( v
        , { secrets :=
              if (st.secrets.any (·.1 == name)) then
                st.secrets.map (fun kv => if kv.1 == name then (name, updated) else kv)
              else st.secrets ++ [(name, updated)]
          , counter := st.counter + 1 } )
      return .ok { name, version := some version }
  , list := fun cursor => do
      let all : List (String × MemSecret) := (← ref.get).secrets
      let remaining : List (String × MemSecret) := match cursor with
        | none   => all
        | some c => (all.dropWhile (fun (kv : String × MemSecret) => kv.1 != c.token)).drop 1
      let page : List (String × MemSecret) := remaining.take inMemoryPageSize
      let rest : List (String × MemSecret) := remaining.drop inMemoryPageSize
      let entry (kv : String × MemSecret) : Secret.Metadata :=
        { name := kv.1, version := kv.2.versions.head?.map (fun v => v.1) }
      return .ok {
          items := page.map entry
        , next :=
            if rest.isEmpty then none
            else page.getLast?.map (fun (kv : String × MemSecret) => Cursor.mk kv.1) } }

/-- A secret store held in memory. See `SecretStore.inMemoryOf`. -/
def SecretStore.inMemory : IO SecretStore := do
  return SecretStore.inMemoryOf (← IO.mkRef ({} : MemSecretStore))

-- ── Self-checks ─────────────────────────────────────────────────────────────

-- Nothing that prints can leak a secret.
#guard toString (Secret.Value.ofString "hunter2") == "<redacted>"
#guard toString (repr (Secret.Value.ofString "hunter2")) == "<redacted>"

-- A length is not a secret, and is often what distinguishes "empty" from
-- "wrong".
#guard (Secret.Value.ofString "hunter2").size == 7
#guard (Secret.Value.ofString "").isEmpty
#guard (Secret.Value.ofString "hunter2").exposeString? == some "hunter2"

-- A binary secret read as text is a value, not a panic.
#guard (Secret.Value.ofBytes ⟨#[0xff]⟩).exposeString? == none

end Cloud
