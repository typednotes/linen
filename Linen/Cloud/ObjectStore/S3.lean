/-
  `Cloud.ObjectStore.S3` — the S3 object data plane, for AWS **and** Scaleway

  ## One implementation, two clouds

  This file is the payoff of `Cloud.Endpoint`. Scaleway's Object Storage speaks
  the S3 API, so the only thing that differs between the two clouds is the host
  `S3.endpoint?` returns. There is no per-cloud branch anywhere below.

  It also serves anything else S3-compatible — MinIO, LocalStack, Ceph — through
  `Endpoint.raw`, which is what makes this namespace testable against a local
  container as well as against an account.

  ## GCP is not here

  Cloud Storage exposes an S3-compatible XML API, but it authenticates with HMAC
  interoperability keys rather than the bearer token every GCP credential in
  this namespace carries. So `of` answers `unsupported` for GCP and
  `Cloud.ObjectStore.Gcs` speaks the JSON API instead. That is a genuine
  difference in credentials, not a gap that could be closed by writing more
  code here.

  ## `HEAD` has no body to explain itself

  S3 answers `HEAD` with headers only, so a failed `HEAD` carries no `<Error>`
  document — the status is the entire diagnosis. `head` therefore reads the
  status itself rather than going through the usual error-body path, and maps
  404 to `none`. A 403 on `HEAD` is genuinely ambiguous on S3 (it is what you
  get for an object you may not see *and*, on some bucket policies, for one
  that does not exist); it is reported as `denied`, which is the safe reading.

  ## Metadata comes from headers

  `get` and `head` read size, ETag and content type from response headers;
  `list` reads them from XML elements. The two paths therefore report slightly
  different subsets — `list` has no content type — which is why every field of
  `ObjectMeta` except the key is optional.

  ## Provenance

  New code. The sibling `typednotes/infra` has an S3 client but its own header
  says "bucket-level operations only: no object CRUD", so the dialect and the
  endpoints came from there and every operation below is new.
-/
import Linen.Cloud.ObjectStore
import Linen.Cloud.Protocol.S3

namespace Cloud.ObjectStore.S3

open Cloud
open Cloud.Protocol.S3 (objectPath bucketPath call sendXml sendUnit unquoteETag
  parseNat parseBool nextContinuationToken?)
open Network.HTTP.Client (Response)
open Network.HTTP.Types (Query)

-- ── Reading metadata off a response ─────────────────────────────────────────

/-- A response header, by name, case-insensitively. -/
private def header? (resp : Response) (name : String) : Option String :=
  resp.findHeader (Data.CI.mk' name)

/-- The metadata S3 reports in the headers of a `GET` or `HEAD` reply.

    `Content-Length` is preferred over the actual body size only for `HEAD`,
    where there is no body; `get` uses the body it received, which is the
    length that actually matters. -/
private def metaOfHeaders (key : String) (resp : Response) (size : Nat) : ObjectMeta :=
  { key
  , size
  , etag := (header? resp "etag").map unquoteETag
  , contentType := header? resp "content-type"
  , lastModified := header? resp "last-modified" }

/-- The metadata S3 reports for one `<Contents>` element of a listing.

    No content type: `ListObjectsV2` does not send one, which is why that field
    of `ObjectMeta` is optional. -/
private def metaOfElement (el : Text.XML.Element) : Option ObjectMeta :=
  (el.childText "Key").map fun key =>
    { key
    , size := parseNat (el.childText "Size")
    , etag := (el.childText "ETag").map unquoteETag
    , lastModified := el.childText "LastModified" }

/-- One `<Version>` or `<DeleteMarker>` element of a `ListObjectVersions`
    reply.

    `isDeleteMarker` is passed in rather than read from the element, because the
    two are distinguished by their *tag name* and carry the same child
    elements — a marker simply has no `Size` or `ETag`. -/
private def versionOfElement (isDeleteMarker : Bool) (el : Text.XML.Element) :
    Option ObjectVersion :=
  match el.childText "Key", el.childText "VersionId" with
  | some key, some versionId =>
    some
      { info :=
          { key
          , size := parseNat (el.childText "Size")
          , etag := (el.childText "ETag").map unquoteETag
          , lastModified := el.childText "LastModified" }
      , versionId
      , isLatest := parseBool (el.childText "IsLatest")
      , isDeleteMarker }
  | _, _ => none

-- ── Building the store ──────────────────────────────────────────────────────

/-- The `ListObjectsV2` query for one page.

    `list-type=2` selects the newer listing API — version 1 pages by marker key
    rather than by an opaque token and is deprecated. `max-keys` is left to the
    provider's default of 1000. -/
private def listQuery (prefix' : String) (cursor : Option Cursor) : Query :=
  [("list-type", some "2")]
  ++ (if prefix'.isEmpty then [] else [("prefix", some prefix')])
  ++ (match cursor with
      | some c => [("continuation-token", some c.token)]
      | none   => [])

/-- The `ListObjectVersions` query for one page.

    Two things differ from `ListObjectsV2` and both matter. The operation is
    selected by a **valueless** `versions` parameter rather than by
    `list-type`, and it pages by *two* markers — `key-marker` and
    `version-id-marker` — because a key can have more versions than fit in a
    page, so a position in the listing is a (key, version) pair rather than one
    token. The two are packed into the single opaque `Cursor` this interface
    carries, separated by a NUL, which cannot occur in either. -/
private def listVersionsQuery (prefix' : String) (cursor : Option Cursor) : Query :=
  [("versions", none)]
  ++ (if prefix'.isEmpty then [] else [("prefix", some prefix')])
  ++ (match cursor with
      | some c =>
        match c.token.splitOn "\x00" with
        | [k, v] => [("key-marker", some k), ("version-id-marker", some v)]
        | _      => [("key-marker", some c.token)]
      | none   => [])

/-- The cursor for the next page of a `ListObjectVersions` reply, packing the
    two markers S3 requires. -/
private def nextVersionMarker? (el : Text.XML.Element) : Option Cursor :=
  if parseBool (el.childText "IsTruncated") then
    match el.childText "NextKeyMarker", el.childText "NextVersionIdMarker" with
    | some k, some v => some ⟨k ++ "\x00" ++ v⟩
    | some k, none   => some ⟨k⟩
    | none,   _      => none
  else none

/-- Headers for a write, from the caller's options.

    User metadata goes in `x-amz-meta-*`. S3 lowercases those keys on the way
    in, so a caller must not rely on case surviving a round trip. -/
private def putHeaders (opts : PutOptions) : List (String × String) :=
  (match opts.contentType with  | some v => [("Content-Type", v)]  | none => [])
  ++ (match opts.cacheControl with | some v => [("Cache-Control", v)] | none => [])
  ++ opts.metadata.map (fun (k, v) => ("x-amz-meta-" ++ k, v))

/-- An object store over an S3-compatible endpoint.

    The `Endpoint` is explicit, so this reaches a local MinIO or LocalStack
    through `Endpoint.raw` as readily as it reaches AWS. -/
def atEndpoint (t : Transport) (creds : Credentials) (ep : Endpoint) (bucket : String) :
    ObjectStore :=
  { describe := s!"s3://{bucket} at {ep.host}"
  , presign := fun key op expiresSeconds => do
      -- The path is signed unencoded and single-encoded on the wire, the same
      -- rule `Call.wirePath` follows, so `objectPath` is passed through
      -- untouched and `presignedUrl` canonicalises it. `doubleEncodePath` stays
      -- false: S3 signs the path as sent.
      (Auth.forEndpoint creds ep).presignedUrlAt ep (← Data.Time.getCurrentTime)
        op.method (objectPath bucket key) [] expiresSeconds
  , get := fun key => do
      match ← performNow t (call creds ep "GET" (objectPath bucket key)) with
      | .error e => return .error e
      | .ok resp => return .ok resp.body
  , head := fun key => do
      -- `HEAD` sends no body, so the status is the whole diagnosis; see the
      -- module header.
      match ← performRaw t (call creds ep "HEAD" (objectPath bucket key))
          (← Data.Time.getCurrentTime) with
      | .error e => return .error e
      | .ok resp =>
        let status := statusOf resp
        if status == 404 then return .ok none
        else if isSuccess resp then
          let size := (header? resp "content-length").bind (·.toNat?) |>.getD 0
          return .ok (some (metaOfHeaders key resp size))
        else
          return .error {
              klass := classify status "", status
            , message := s!"HEAD {objectPath bucket key} answered {status}" }
  , put := fun key bytes opts => do
      let c := call creds ep "PUT" (objectPath bucket key)
        (headers := putHeaders opts) (body := bytes)
      match ← performNow t c with
      | .error e => return .error e
      | .ok resp =>
        return .ok {
            key, size := bytes.size
          , etag := (header? resp "etag").map unquoteETag
          , contentType := opts.contentType }
  , delete := fun key => do
      -- S3 answers 204 whether or not the key was there, so this is already
      -- idempotent and needs no special case.
      match ← performNow t (call creds ep "DELETE" (objectPath bucket key)) with
      | .error e => return .error e
      | .ok _    => return .ok ()
  , list := fun prefix' cursor => do
      let c := call creds ep "GET" (bucketPath bucket) (query := listQuery prefix' cursor)
      match ← sendXml t c with
      | .error e => return .error e
      | .ok root =>
        return .ok {
            items := (root.named "Contents").filterMap metaOfElement
          , next := nextContinuationToken? root }
  , listVersions := fun prefix' cursor => do
      let c := call creds ep "GET" (bucketPath bucket)
        (query := listVersionsQuery prefix' cursor)
      match ← sendXml t c with
      | .error e => return .error e
      | .ok root =>
        -- Data versions and delete markers are separate element names in the
        -- same reply, and both belong in the listing: a caller reconstructing
        -- history needs to see that a key was deleted at a point in it.
        -- Interleaved in document order, which is S3's newest-first order per
        -- key.
        let versions := (root.named "Version").filterMap (versionOfElement false)
        let markers  := (root.named "DeleteMarker").filterMap (versionOfElement true)
        return .ok {
            items := versions ++ markers
          , next := nextVersionMarker? root }
  , getVersion := fun key versionId => do
      match ← performNow t (call creds ep "GET" (objectPath bucket key)
          (query := [("versionId", some versionId)])) with
      | .error e => return .error e
      | .ok resp => return .ok resp.body
  , deleteVersion := fun key versionId =>
      -- Unlike `delete`, this is destructive: it removes the named version
      -- rather than adding a delete marker over it.
      sendUnit t (call creds ep "DELETE" (objectPath bucket key)
        (query := [("versionId", some versionId)])) }

/-- An object store on a cloud's S3-compatible endpoint.

    `unsupported` for GCP: see the module header. The region comes from the
    credentials unless one is given, because a bucket lives in exactly one
    region and signing for another fails in a way that names nothing useful. -/
def of (t : Transport) (provider : Provider) (creds : Credentials) (bucket : String)
    (region : Option String := none) : Except Error ObjectStore := do
  let region ← match region with
    | some r => .ok r
    | none   => creds.requireRegion provider
  match S3.endpoint? provider region with
  | some ep => .ok (atEndpoint t creds ep bucket)
  | none    =>
    .error (Error.unsupported provider.name
      "the S3 API (its object store needs interoperability keys this library does not hold)")

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard listQuery "" none == [("list-type", some "2")]
#guard listQuery "logs/" none == [("list-type", some "2"), ("prefix", some "logs/")]
#guard listQuery "logs/" (some ⟨"tok"⟩)
  == [("list-type", some "2"), ("prefix", some "logs/"), ("continuation-token", some "tok")]

#guard putHeaders {} == []
#guard putHeaders { contentType := some "application/json" }
  == [("Content-Type", "application/json")]
#guard putHeaders { metadata := [("origin", "batch-7")] }
  == [("x-amz-meta-origin", "batch-7")]

end Cloud.ObjectStore.S3
