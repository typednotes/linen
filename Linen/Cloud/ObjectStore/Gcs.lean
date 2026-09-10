/-
  `Cloud.ObjectStore.Gcs` — Cloud Storage over the JSON API

  ## Why not the S3 client

  Cloud Storage *does* expose an S3-compatible XML API, which would have made
  `Cloud.ObjectStore.S3` serve all three clouds. It authenticates with **HMAC
  interoperability keys** — a separate credential an operator creates by hand —
  rather than the OAuth2 bearer token every GCP credential in this namespace
  carries. So reaching it is not a matter of writing more code here; it needs a
  credential the caller does not have.

  The sibling `typednotes/infra` records that this went unnoticed for a while:
  its GCP object-store backend was silently unimplemented without appearing in
  its own coverage checks. Hence the explicitness here and the `unsupported`
  answer from `Cloud.ObjectStore.S3.of` for GCP.

  ## Object names are one path segment

  This is the API's sharpest edge. An object called `logs/2026/a.json` is
  addressed as

      /storage/v1/b/assets/o/logs%2F2026%2Fa.json

  — its name is a **single** path segment, with every `/` percent-encoded.
  Leaving them literal addresses a different resource and answers 404. That is
  why `Cloud.Call.pathPreEncoded` exists, and why this module is the only user
  of it.

  ## Metadata and media are two different paths

  Reading an object's *metadata* and reading its *contents* are the same URL
  distinguished by `?alt=media`, and **uploading** is a different path prefix
  entirely (`/upload/storage/v1/…`). Getting either wrong yields a reply that
  parses but means something else — a metadata document where bytes were
  wanted.

  ## `Int64` fields arrive as strings

  `size` and `generation` cross the wire quoted, because JSON numbers are
  doubles and would lose precision. Reading `size` as a number silently yields
  `none`, making every object appear empty. `GoogleRest.natString?` is the
  accessor for these.

  ## Unverified against a live account

  The upload path prefix and the `%2F` encoding of object names are written
  from Google's REST reference and are **not** exercised against a real
  project here. They are the shape of detail that only a live account settles;
  see the module's tests for what *is* pinned.
-/
import Linen.Cloud.ObjectStore
import Linen.Cloud.Protocol.GoogleRest
import Linen.Network.URI

namespace Cloud.ObjectStore.Gcs

open Cloud
open Cloud.Protocol.GoogleRest (string? natString? array nextPageToken? field?)
open Network.HTTP.Types (Query)

-- ── Paths ───────────────────────────────────────────────────────────────────

/-- Percent-encode an object name for use as a single path segment.

    Encodes `/` as `%2F` along with everything else that is not unreserved —
    the whole point, and what distinguishes this from a structural path. -/
def encodeName (name : String) : String :=
  Network.URI.escapeURIString Network.URI.isUnreserved name

/-- The bucket's object collection: `/storage/v1/b/{bucket}/o`. -/
def objectsPath (bucket : String) : String := s!"/storage/v1/b/{bucket}/o"

/-- One object: `/storage/v1/b/{bucket}/o/{name}`, with the name encoded as a
    single segment. -/
def objectPath (bucket name : String) : String :=
  s!"/storage/v1/b/{bucket}/o/{encodeName name}"

/-- The **upload** collection, which lives under a different path prefix from
    everything else. -/
def uploadPath (bucket : String) : String := s!"/upload/storage/v1/b/{bucket}/o"

-- ── Reading metadata ────────────────────────────────────────────────────────

/-- The metadata GCS reports for one object.

    `size` is read with `natString?` because it arrives quoted; see the module
    header. -/
def metaOfJson (v : Data.Json.Value) : Option ObjectMeta :=
  (string? v "name").map fun key =>
    { key
    , size := (natString? v "size").getD 0
    , etag := string? v "etag"
    , contentType := string? v "contentType"
    , lastModified := string? v "updated" }

-- ── Building the store ──────────────────────────────────────────────────────

/-- The listing query for one page. -/
private def listQuery (prefix' : String) (cursor : Option Cursor) : Query :=
  (if prefix'.isEmpty then [] else [("prefix", some prefix')])
  ++ (match cursor with
      | some c => [("pageToken", some c.token)]
      | none   => [])

/-- A Cloud Storage object store.

    `token` is an OAuth2 access token; `Cloud.Credentials.Gcp.tokenFor` mints
    one from a service-account key. A token expires within the hour, so a
    long-running caller should re-create the store rather than hold one
    indefinitely — the reason a token is a parameter here and not read from a
    hidden cache. -/
def withToken (t : Transport) (token bucket : String) : ObjectStore :=
  let call (method path : String) (query : Query := [])
      (payload : Option Data.Json.Value := none) : Call :=
    { (Cloud.Protocol.GoogleRest.call token Gcp.storageHost method path query payload) with
      pathPreEncoded := true }
  { describe := s!"gs://{bucket}"
  , get := fun key => do
      -- `?alt=media` is what distinguishes "the bytes" from "the metadata
      -- document"; without it this returns JSON that parses fine and is the
      -- wrong thing entirely.
      match ← performNow t (call "GET" (objectPath bucket key) [("alt", some "media")]) with
      | .error e => return .error e
      | .ok resp => return .ok resp.body
  , head := fun key => do
      match ← performRaw t (call "GET" (objectPath bucket key)) (← Data.Time.getCurrentTime) with
      | .error e => return .error e
      | .ok resp =>
        let status := statusOf resp
        if status == 404 then return .ok none
        else if isSuccess resp then
          match bodyText resp with
          | .error e => return .error e
          | .ok text =>
            match Data.Json.Decode.decode text with
            | .error m => return .error (Error.protocol s!"malformed object metadata: {m}")
            | .ok v    => return .ok (metaOfJson v)
        else
          return .error (describeError status (bodyTextLossy resp))
  , put := fun key bytes opts => do
      -- Uploads live under a *different* path prefix, and the object name
      -- travels as a query parameter rather than in the path.
      let query : Query :=
        [("uploadType", some "media"), ("name", some key)]
      let c : Call :=
        { Cloud.Protocol.GoogleRest.callBytes token Gcp.storageHost "POST" (uploadPath bucket)
            query bytes (opts.contentType.getD "application/octet-stream") with
          pathPreEncoded := true }
      match ← performNow t c with
      | .error e => return .error e
      | .ok resp =>
        match bodyText resp with
        | .error _ => return .ok { key, size := bytes.size, contentType := opts.contentType }
        | .ok text =>
          match Data.Json.Decode.decode text with
          | .error _ => return .ok { key, size := bytes.size, contentType := opts.contentType }
          | .ok v =>
            return .ok ((metaOfJson v).getD
              { key, size := bytes.size, contentType := opts.contentType })
  , delete := fun key => do
      match ← performRaw t (call "DELETE" (objectPath bucket key))
          (← Data.Time.getCurrentTime) with
      | .error e => return .error e
      | .ok resp =>
        -- GCS answers 404 for a name that is not there, where S3 answers 204.
        -- Normalised to success so that `delete` is idempotent on every cloud,
        -- which is what a caller retrying after a timeout needs.
        let status := statusOf resp
        if isSuccess resp || status == 404 then return .ok ()
        else return .error (describeError status (bodyTextLossy resp))
  , list := fun prefix' cursor => do
      match ← Cloud.Protocol.GoogleRest.send t
          (call "GET" (objectsPath bucket) (listQuery prefix' cursor)) with
      | .error e => return .error e
      | .ok v =>
        return .ok {
            items := (array v "items").filterMap metaOfJson
          , next := nextPageToken? v } }

/-- A Cloud Storage object store, taking the token from credentials.

    Fails with `unbound` when the credentials carry no token, which is a
    clearer answer than a `denied` on the first request. -/
def of (t : Transport) (creds : Credentials) (bucket : String) :
    Except Error ObjectStore := do
  let token ← creds.requireToken .gcp
  .ok (withToken t token bucket)

-- ── Self-checks ─────────────────────────────────────────────────────────────

/- **The encoding that makes or breaks this module.** An object name is one
   path segment, so its slashes must be `%2F` on the wire. -/
#guard encodeName "logs/2026/a.json" == "logs%2F2026%2Fa.json"
#guard objectPath "assets" "logs/2026/a.json"
  == "/storage/v1/b/assets/o/logs%2F2026%2Fa.json"

-- Unreserved characters are left alone, so ordinary names stay readable.
#guard encodeName "a-b_c.d~e" == "a-b_c.d~e"
#guard encodeName "my report.json" == "my%20report.json"

#guard objectsPath "assets" == "/storage/v1/b/assets/o"
#guard uploadPath "assets" == "/upload/storage/v1/b/assets/o"

-- The upload path is genuinely a different prefix, not a suffix of the other.
#guard uploadPath "assets" != objectsPath "assets"

#guard listQuery "" none == []
#guard listQuery "logs/" (some ⟨"tok"⟩)
  == [("prefix", some "logs/"), ("pageToken", some "tok")]

end Cloud.ObjectStore.Gcs
