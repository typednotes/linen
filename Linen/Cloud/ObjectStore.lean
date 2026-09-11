/-
  `Cloud.ObjectStore` — one object store interface, three clouds behind it

  ## A record of closures, not a type class

  `ObjectStore` is a record whose fields are the operations. That is a
  deliberate choice over a class with per-provider instances, and it buys three
  things a class would not:

  - **Provider dispatch is a total function.** `ObjectStore.s3` and
    `ObjectStore.gcs` are ordinary functions returning a value, so choosing one
    at runtime from a configuration string needs no existential wrapper and no
    dictionary juggling.
  - **Per-store configuration lives in the closure.** A store is created once
    against one bucket with one set of credentials, and the operations close
    over them. Nothing downstream has to thread them.
  - **A local backend is free.** `ObjectStore.inMemory` is the same type as the
    real thing, so every consumer — including the capability effect in
    `Control.Monad.Effect.ObjectStore` — can be driven with no network, no
    credentials and no container.

  This follows the sibling `typednotes/infra`'s `Backend` record, which
  documents the same reasoning: defunctionalised as a record so that dispatch
  can be a total function over the provider without sigma gymnastics.

  ## Keys are strings here

  At this layer a key is the `String` that goes on the wire, because that is
  what the providers take. The capability effect above it uses `List String`
  segments instead — not for tidiness but because `String.startsWith` does not
  reduce under `decide`, so a *statically checked* prefix scope needs a
  structure the kernel can compute with. The two meet at the handler, which
  renders segments back to a string.

  ## What is portable and what is not

  Everything here is supported by all three clouds. Anything that is not lives
  on the provider's own module, so reaching for it is visible in the source:
  storage classes, object locks, server-side encryption keys, multipart
  uploads, batch delete. `Cloud.Provider.supports` records the matrix.

  Two operations are portable but **optional**, with defaults that fail rather
  than pretend: `presign`, which needs SigV4, and the versioning trio
  (`listVersions`, `getVersion`, `deleteVersion`). A store that cannot do them
  says so, which is why they are fields with defaults rather than extra
  required ones — `ObjectStore.inMemory` keeps no history and holds no
  credentials, and should not have to fake either.

  ## Absence is not an error

  `head` answers `Option`, and `get?` exists alongside `get`, because "read it
  if it is there" is the most common thing a caller wants. `get` on a missing
  key still fails, because a caller that asked unconditionally deserves to know.
-/
import Linen.Cloud.Transport
import Linen.Cloud.Page
import Linen.Data.Hex
import Linen.Crypto.MD5

namespace Cloud

-- ── What a store holds ──────────────────────────────────────────────────────

/-- What is known about an object without fetching its contents.

    Every field except the key is optional because the three clouds disagree
    about which they return on which operation — S3's `HEAD` gives all of them,
    its `ListObjectsV2` omits the content type, and GCS names them differently
    again. A caller that needs one asks for it rather than assuming. -/
structure ObjectMeta where
  /-- The object's key, exactly as the provider spells it. -/
  key          : String
  /-- Size in bytes. -/
  size         : Nat := 0
  /-- The entity tag, **with any surrounding quotes removed**. S3 sends
      `"d41d8…"` including the quote characters; comparing an unstripped tag
      against a computed digest silently never matches. -/
  etag         : Option String := none
  /-- The declared MIME type, when the provider reported one. -/
  contentType  : Option String := none
  /-- Last modification time, as the provider formatted it — RFC 1123 on S3,
      RFC 3339 on GCS. Left as text rather than parsed, because the two
      formats differ and no caller here needs to compare them. -/
  lastModified : Option String := none
  deriving Repr, DecidableEq, Inhabited

/-- Options for a write.

    All optional: a `put` with none of them is the common case and needs no
    ceremony. -/
structure PutOptions where
  /-- The MIME type to store. Providers default this to
      `application/octet-stream` or `binary/octet-stream`, which is rarely what
      is wanted for text. -/
  contentType  : Option String := none
  /-- `Cache-Control`, stored and served back with the object. -/
  cacheControl : Option String := none
  /-- Arbitrary user metadata. Sent as `x-amz-meta-*` on S3 and in the
      `metadata` object on GCS. Keys are lowercased by S3, so do not rely on
      case surviving. -/
  metadata     : List (String × String) := []
  deriving Repr, DecidableEq, Inhabited

-- ── The interface ───────────────────────────────────────────────────────────

/-- Which operation a presigned URL grants.

    An enumeration rather than an HTTP method string, so a URL cannot be minted
    for an operation the store does not mean to delegate. A presigned URL's
    signature covers the method, so this is the whole of what the holder may
    do — the grant is genuinely one verb on one key until it expires. -/
inductive PresignedOp where
  /-- `GET`: read the object. -/
  | download
  /-- `PUT`: replace the object. -/
  | upload
  deriving Repr, DecidableEq, BEq

/-- The HTTP method a presigned operation signs. -/
def PresignedOp.method : PresignedOp → String
  | .download => "GET"
  | .upload   => "PUT"

/-- One version of an object, in a bucket where versioning is enabled.

    The three clouds name this differently — S3 and Scaleway call it a
    `VersionId`, Cloud Storage a `generation` — but all three make it an opaque
    token identifying one immutable snapshot of one key. It is a `String` here
    for that reason: GCS's generations are numeric, and treating them as numbers
    would invite arithmetic on an identifier that only ever gets compared. -/
structure ObjectVersion where
  /-- What is known about this version. `key` is the object's key, shared by
      every version of it.

      Named `info` rather than `meta`, which Lean 4 reserves. -/
  info           : ObjectMeta
  /-- The provider's identifier for this version, to pass back to
      `getVersion` or `deleteVersion`. -/
  versionId      : String
  /-- Whether this is the version a plain `get` would return. -/
  isLatest       : Bool := false
  /-- Whether this entry is a **delete marker** rather than data.

      S3 records a delete in a versioned bucket by adding a marker version
      rather than removing anything, so a listing interleaves markers with real
      versions. A marker has no body: `getVersion` on one fails, which is
      correct and is why this flag is surfaced rather than hidden. GCS has no
      equivalent and always reports `false`. -/
  isDeleteMarker : Bool := false
  deriving Repr, DecidableEq, Inhabited

/-- An object store, bound to one bucket.

    Every operation answers `Except Error`, never raises; see `Cloud.Error` on
    why absence is a value. -/
structure ObjectStore where
  /-- Fetch an object's contents. Fails with `notFound` if it is not there. -/
  get      : String → IO (Except Error ByteArray)
  /-- Fetch an object's metadata, or `none` if it is not there. -/
  head     : String → IO (Except Error (Option ObjectMeta))
  /-- Store an object, replacing any existing one at that key. -/
  put      : String → ByteArray → PutOptions → IO (Except Error ObjectMeta)
  /-- Remove an object.

      **Removing a key that is not there succeeds** on all three clouds, and
      this interface preserves that: delete is idempotent, and a caller
      retrying after a timeout must not see a spurious failure. -/
  delete   : String → IO (Except Error Unit)
  /-- One page of the keys under a prefix, in the provider's order —
      lexicographic on all three. -/
  list     : String → Option Cursor → IO (Except Error (Page ObjectMeta))
  /-- Where this store is, for diagnostics and logs. Never a credential. -/
  describe : String := "object store"
  /-- A time-limited URL granting one operation on one key to a holder with no
      credentials — a download link, or an upload target.

      Defaults to an error naming the store, so an implementation that cannot
      mint one says so rather than silently answering something unusable. The
      S3-compatible stores override it; Cloud Storage's signed URLs use an
      unrelated construction and are not implemented, which is what
      `Provider.supports .gcp .presignedUrl = false` records.

      Whoever receives such a URL must send it unmodified: its signature covers
      the query exactly as encoded and ordered. `Cloud.performPresigned` exists
      for that. -/
  presign  : String → PresignedOp → Nat → IO (Except Error String) :=
    fun _ _ _ => pure (.error
      { klass := .invalid
      , message := "this object store cannot mint presigned URLs" })
  /-- One page of the versions of the keys under a prefix, newest first.

      A versioned bucket keeps every write, so this is how the history is
      reached. In a bucket where versioning was never enabled, all three clouds
      answer one entry per key — the current one — rather than failing, so a
      caller need not know which kind of bucket it has.

      Interleaves delete markers with data versions on S3; see
      `ObjectVersion.isDeleteMarker`. -/
  listVersions  : String → Option Cursor → IO (Except Error (Page ObjectVersion)) :=
    fun _ _ => pure (.error
      { klass := .invalid
      , message := "this object store does not support object versioning" })
  /-- Fetch one specific version's contents.

      Fails with `notFound` if the version is gone, and with an error rather
      than empty bytes for an S3 delete marker, which has no body. -/
  getVersion    : String → String → IO (Except Error ByteArray) :=
    fun _ _ => pure (.error
      { klass := .invalid
      , message := "this object store does not support object versioning" })
  /-- Remove one specific version, permanently.

      **Not the same as `delete`.** In a versioned bucket `delete` adds a delete
      marker and keeps the data; this destroys the named version and cannot be
      undone. Removing the newest version makes the previous one current
      again. -/
  deleteVersion : String → String → IO (Except Error Unit) :=
    fun _ _ => pure (.error
      { klass := .invalid
      , message := "this object store does not support object versioning" })

-- ── Derived operations ──────────────────────────────────────────────────────

/-- Fetch an object's contents, or `none` if it is not there.

    The "read it if it is there" idiom, which is what most callers want. -/
def ObjectStore.get? (s : ObjectStore) (key : String) :
    IO (Except Error (Option ByteArray)) := do
  return absentAsNone (← s.get key)

/-- Fetch an object and decode it as UTF-8.

    `none` for a missing object; a `protocol` error for bytes that are not
    UTF-8, rather than the panic `String.fromUTF8!` would give. -/
def ObjectStore.getString? (s : ObjectStore) (key : String) :
    IO (Except Error (Option String)) := do
  match ← s.get? key with
  | .error e      => return .error e
  | .ok none      => return .ok none
  | .ok (some bs) =>
    match String.fromUTF8? bs with
    | some str => return .ok (some str)
    | none     =>
      return .error (Error.protocol s!"object '{key}' is not valid UTF-8")

/-- Store a string as UTF-8.

    Defaults the content type to `text/plain; charset=utf-8`, because the
    providers' own default of `application/octet-stream` makes a browser
    download the object rather than show it. -/
def ObjectStore.putString (s : ObjectStore) (key contents : String)
    (opts : PutOptions := { contentType := some "text/plain; charset=utf-8" }) :
    IO (Except Error ObjectMeta) :=
  s.put key contents.toUTF8 opts

/-- Whether an object exists. One `HEAD`, no body transferred. -/
def ObjectStore.exists? (s : ObjectStore) (key : String) : IO (Except Error Bool) := do
  match ← s.head key with
  | .error e => return .error e
  | .ok m    => return .ok m.isSome

/-- Read up to `maxPages` pages of a prefix listing.

    The result says whether it is complete; see `Cloud.Page` on why a bound is
    part of the specification here rather than a workaround. -/
def ObjectStore.listAll (s : ObjectStore) (prefix' : String := "")
    (maxPages : Nat := defaultMaxPages) : IO (Except Error (Listing ObjectMeta)) :=
  paginate maxPages (fun cursor => s.list prefix' cursor)

/-- Every key under a prefix, up to the page bound.

    Convenience over `listAll` for the common case of wanting names. Note that
    a truncated listing yields a short list with no indication here — use
    `listAll` when completeness matters. -/
def ObjectStore.keys (s : ObjectStore) (prefix' : String := "")
    (maxPages : Nat := defaultMaxPages) : IO (Except Error (List String)) := do
  match ← s.listAll prefix' maxPages with
  | .error e => return .error e
  | .ok l    => return .ok (l.items.map (·.key))

/-- Copy an object within this store, by reading and writing it.

    Not a server-side copy: all three clouds have one, and all three spell it
    differently, so the portable version moves the bytes. For a large object
    prefer the provider module's own copy. -/
def ObjectStore.copyVia (s : ObjectStore) (from' to : String)
    (opts : PutOptions := {}) : IO (Except Error ObjectMeta) := do
  match ← s.get from' with
  | .error e  => return .error e
  | .ok bytes => s.put to bytes opts

-- ── A local backend ─────────────────────────────────────────────────────────

/-- One object in the in-memory store. -/
private structure MemObject where
  bytes : ByteArray
  opts  : PutOptions

/-- Insert into a list kept in ascending key order, replacing an existing
    entry.

    Sorted because all three providers list keys lexicographically, and a local
    backend that returned insertion order would let a test pass against it and
    fail against the real thing. -/
private def insertSorted (key : String) (v : MemObject) :
    List (String × MemObject) → List (String × MemObject)
  | [] => [(key, v)]
  | (k, v') :: rest =>
    if key == k then (key, v) :: rest
    else if key < k then (key, v) :: (k, v') :: rest
    else (k, v') :: insertSorted key v rest

/-- The metadata the in-memory store reports.

    The ETag is the MD5 hex digest, which is what S3 reports for a
    single-part upload — so a caller checking integrity against it behaves the
    same locally. -/
private def memMeta (key : String) (o : MemObject) : ObjectMeta :=
  { key
  , size := o.bytes.size
  , etag := some (Data.Hex.encode (Crypto.MD5.hash o.bytes))
  , contentType := o.opts.contentType }

/-- An object store held in memory, for local development, debugging and tests.

    Behaves like the real thing in the ways that catch bugs: keys list in
    lexicographic order, listings are **paginated** (see `inMemoryPageSize`),
    a missing object is `notFound`, deleting a missing object succeeds, and the
    reported ETag is the MD5 hex digest as S3 reports for a single-part upload.

    What it does not do is fail. There is no throttling, no network error and
    no eventual consistency, so a caller's retry paths are not exercised —
    compose it with a wrapper that injects failures if that is what is wanted.

    ```
    let store ← ObjectStore.inMemory
    let _ ← store.putString "greeting.txt" "hello"
    IO.println (← store.getString? "greeting.txt")
    ``` -/
def ObjectStore.inMemory : IO ObjectStore := do
  let ref ← IO.mkRef ([] : List (String × MemObject))
  return {
      describe := "object store (in memory)"
    , get := fun key => do
        match (← ref.get).find? (·.1 == key) with
        | some (_, o) => return .ok o.bytes
        | none        =>
          return .error {
              klass := .notFound, status := 404, code := "NoSuchKey"
            , message := s!"no object at key '{key}'" }
    , head := fun key => do
        match (← ref.get).find? (·.1 == key) with
        | some (_, o) => return .ok (some (memMeta key o))
        | none        => return .ok none
    , put := fun key bytes opts => do
        let o : MemObject := { bytes, opts }
        ref.modify (insertSorted key o)
        return .ok (memMeta key o)
    , delete := fun key => do
        -- Idempotent, as it is on all three clouds.
        ref.modify (·.filter (·.1 != key))
        return .ok ()
    , list := fun prefix' cursor => do
        let all := (← ref.get).filter (·.1.startsWith prefix')
        -- The cursor is the key to resume *after*, which is how S3's own
        -- continuation behaves.
        let remaining := match cursor with
          | none   => all
          | some c => all.filter (fun kv => c.token < kv.1)
        let page := remaining.take inMemoryPageSize
        let rest := remaining.drop inMemoryPageSize
        return .ok {
            items := page.map (fun (k, o) => memMeta k o)
          , next := if rest.isEmpty then none else (page.getLast?.map (fun kv => Cursor.mk kv.1)) } }

end Cloud
