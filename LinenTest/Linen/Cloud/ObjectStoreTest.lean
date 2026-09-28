/-
  Tests for `Cloud.ObjectStore` and its three backends.

  The in-memory backend is tested first and hardest, because it is the one
  developers will actually run: if it behaves differently from S3 in a way a
  caller can notice, it will teach them something false. So the properties
  pinned below are the ones that differ between a naive local store and a real
  one — lexicographic key order, genuine pagination, idempotent delete, an
  MD5 ETag.

  The two cloud backends are then driven through stub transports, which checks
  the part that cannot be checked any other way without an account: the exact
  paths, queries and encodings that go on the wire.
-/
import Linen.Cloud.ObjectStore.S3
import Linen.Cloud.ObjectStore.Gcs

open Cloud
open Network.HTTP.Types (status200 status204 status404 status403)

namespace Tests.Cloud.ObjectStore

-- ── The in-memory backend ───────────────────────────────────────────────────

/-- info: (some "hello", some 5) -/
#guard_msgs in
#eval show IO (Option String × Option Nat) from do
  let s ← ObjectStore.inMemory
  let _ ← s.putString "greeting.txt" "hello"
  let contents ← s.getString? "greeting.txt"
  let m ← s.head "greeting.txt"
  return (contents.toOption.getD none, (m.toOption.getD none).map (·.size))

/- A missing object is `notFound` from `get`, and `none` from `get?` and
   `head`. Both shapes exist because a caller that asked unconditionally
   deserves to know, and one that asked conditionally does not want to catch
   anything. -/
/-- info: (Cloud.Class.notFound, none, none) -/
#guard_msgs in
#eval show IO (Class × Option Nat × Option String) from do
  let s ← ObjectStore.inMemory
  let missing ← s.get "nope"
  let opt ← s.get?  "nope"
  let head ← s.head "nope"
  return ( (match missing with | .error e => e.klass | .ok _ => .protocol)
         , (opt.toOption.getD none).map (·.size)
         , (head.toOption.getD none).map (·.key) )

/- **Delete is idempotent**, as it is on all three clouds. A caller retrying
   after a timeout must not see a spurious failure. -/
/-- info: (true, true, false) -/
#guard_msgs in
#eval show IO (Bool × Bool × Bool) from do
  let s ← ObjectStore.inMemory
  let _ ← s.putString "a" "x"
  let first ← s.delete "a"
  let again ← s.delete "a"
  let stillThere ← s.exists? "a"
  return ((first.toOption).isSome, (again.toOption).isSome, stillThere.toOption.getD true)

/- **Keys list in lexicographic order, not insertion order.** All three
   providers do, and a local backend that returned insertion order would let a
   test pass locally and fail in production. -/
/-- info: ["a.txt", "b.txt", "m.txt", "z.txt"] -/
#guard_msgs in
#eval show IO (List String) from do
  let s ← ObjectStore.inMemory
  let _ ← s.putString "m.txt" "1"
  let _ ← s.putString "z.txt" "2"
  let _ ← s.putString "a.txt" "3"
  let _ ← s.putString "b.txt" "4"
  return ((← s.keys).toOption.getD [])

/- Writing the same key twice replaces rather than duplicating. -/
/-- info: (1, some "second") -/
#guard_msgs in
#eval show IO (Nat × Option String) from do
  let s ← ObjectStore.inMemory
  let _ ← s.putString "k" "first"
  let _ ← s.putString "k" "second"
  return (((← s.keys).toOption.getD []).length, (← s.getString? "k").toOption.getD none)

/- **The listing is genuinely paginated.** A local backend that answered
   everything in one page would never exercise a caller's pagination — which is
   the bug this backend exists to catch before an account is involved. -/
/-- info: (3, false, 7, true) -/
#guard_msgs in
#eval show IO (Nat × Bool × Nat × Bool) from do
  let s ← ObjectStore.inMemory
  for i in [0, 1, 2, 3, 4, 5, 6] do
    let _ ← s.putString s!"k{i}" "v"
  let firstPage ← s.list "" none
  let whole ← s.listAll ""
  return ( (firstPage.toOption.map (·.items.length)).getD 0
         , (firstPage.toOption.map (·.isLast)).getD true
         , (whole.toOption.map (·.items.length)).getD 0
         , (whole.toOption.map (·.complete)).getD false )

/- A bounded read of a longer listing reports itself incomplete, rather than
   looking like a short bucket. -/
/-- info: (3, false) -/
#guard_msgs in
#eval show IO (Nat × Bool) from do
  let s ← ObjectStore.inMemory
  for i in [0, 1, 2, 3, 4, 5, 6] do
    let _ ← s.putString s!"k{i}" "v"
  match ← s.listAll "" (maxPages := 1) with
  | .ok l => return (l.items.length, l.complete)
  | .error _ => return (0, true)

/- Prefixes filter, and — matching S3 — the prefix is a **byte** prefix, so
   `log` also matches `logs/`. The capability effect above this layer is
   deliberately stricter; see `Control.Monad.Effect.ObjectStore`. -/
/-- info: (["logs/a", "logs/b"], 3) -/
#guard_msgs in
#eval show IO (List String × Nat) from do
  let s ← ObjectStore.inMemory
  let _ ← s.putString "logs/a" "1"
  let _ ← s.putString "logs/b" "2"
  let _ ← s.putString "logs-old" "3"
  let _ ← s.putString "other" "4"
  let scoped' ← s.keys "logs/"
  let bytePrefix ← s.keys "log"
  return (scoped'.toOption.getD [], (bytePrefix.toOption.getD []).length)

/- The reported ETag is the MD5 hex digest, which is what S3 reports for a
   single-part upload — so a caller checking integrity against it behaves the
   same locally. -/
/-- info: some "5d41402abc4b2a76b9719d911017c592" -/
#guard_msgs in
#eval show IO (Option String) from do
  let s ← ObjectStore.inMemory
  let m ← s.putString "k" "hello"
  return (m.toOption.bind (·.etag))

/- Bytes that are not UTF-8 round-trip through `get`, and `getString?` reports
   a `protocol` error rather than panicking — the correction over
   `String.fromUTF8!`. -/
/-- info: (#[0, 255, 128], Cloud.Class.protocol) -/
#guard_msgs in
#eval show IO (Array UInt8 × Class) from do
  let s ← ObjectStore.inMemory
  let _ ← s.put "raw" ⟨#[0, 255, 128]⟩ {}
  let bytes ← s.get "raw"
  let asText ← s.getString? "raw"
  return ( (bytes.toOption.map (·.toList.toArray)).getD #[]
         , (match asText with | .error e => e.klass | .ok _ => .notFound) )

/- `copyVia` moves the bytes, for the common case where a server-side copy is
   not worth the per-cloud spelling. -/
/-- info: (some "payload", some "payload") -/
#guard_msgs in
#eval show IO (Option String × Option String) from do
  let s ← ObjectStore.inMemory
  let _ ← s.putString "from" "payload"
  let _ ← s.copyVia "from" "to"
  return ((← s.getString? "from").toOption.getD none, (← s.getString? "to").toOption.getD none)

/- `putString` defaults the content type to text, because the providers' own
   default of `application/octet-stream` makes a browser download the object
   rather than show it. -/
/-- info: some "text/plain; charset=utf-8" -/
#guard_msgs in
#eval show IO (Option String) from do
  let s ← ObjectStore.inMemory
  let m ← s.putString "a.txt" "hi"
  return (m.toOption.bind (·.contentType))

-- ── The S3 backend ──────────────────────────────────────────────────────────

def creds : Credentials :=
  { accessKey := "AKIDEXAMPLE"
  , secretKey := "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
  , region := "eu-west-3" }

def ep : Endpoint :=
  { host := "s3.eu-west-3.amazonaws.com", service := "s3", region := "eu-west-3" }

/-- A stub that records the request line and answers with a fixed status,
    body and headers. -/
def wire (log : IO.Ref (List String)) (st : Network.HTTP.Types.Status) (body : String)
    (headers : List (String × String) := []) : Transport :=
  Transport.stub fun req => do
    log.modify (fun l => l ++ [s!"{req.method} {req.host}{req.path}{req.queryString}"])
    pure { statusCode := st, headers := headers.map (fun (n, v) => (Data.CI.mk' n, v))
         , body := body.toUTF8 }

/-- info: (["GET s3.eu-west-3.amazonaws.com/assets/logs/2026/a.json"], "{}") -/
#guard_msgs in
#eval show IO (List String × String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 "{}") creds ep "assets"
  let got ← s.get "logs/2026/a.json"
  return (← log.get, (got.toOption.bind String.fromUTF8?).getD "?")

/- A key needing escapes signs and sends correctly without the caller doing
   anything, because the path is held unencoded and both encodings are derived
   from it. -/
/-- info: ["GET s3.eu-west-3.amazonaws.com/assets/my%20report.json"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 "x") creds ep "assets"
  let _ ← s.get "my report.json"
  log.get

/- `HEAD` reads its metadata from headers, and the ETag arrives quoted — the
   quotes are stripped, or comparing against a digest silently never matches. -/
/--
info: (["HEAD s3.eu-west-3.amazonaws.com/assets/a.json"],
 some 1024,
 some "d41d8cd98f00b204e9800998ecf8427e",
 some "application/json")
-/
#guard_msgs in
#eval show IO (List String × Option Nat × Option String × Option String) from do
  let log ← IO.mkRef []
  let hdrs := [("Content-Length", "1024"), ("ETag", "\"d41d8cd98f00b204e9800998ecf8427e\""),
               ("Content-Type", "application/json")]
  let s := ObjectStore.S3.atEndpoint (wire log status200 "" hdrs) creds ep "assets"
  match ← s.head "a.json" with
  | .ok (some m) => return (← log.get, some m.size, m.etag, m.contentType)
  | _ => return (← log.get, none, none, none)

/- A 404 `HEAD` is `none`, not an error: `HEAD` sends no body, so the status is
   the whole diagnosis and this is the one place the status has to be read
   directly. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status404 "") creds ep "assets"
  match ← s.head "gone" with
  | .ok none => return true
  | _ => return false

/- A 403 on `HEAD` is reported as `denied` rather than absence. S3 genuinely
   answers 403 for both "may not see it" and, under some bucket policies, "not
   there" — and treating that as absence would make a caller create something
   it does not own. -/
/-- info: Cloud.Class.denied -/
#guard_msgs in
#eval show IO Class from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status403 "") creds ep "assets"
  match ← s.head "forbidden" with
  | .error e => return e.klass
  | .ok _ => return .notFound

/-- info: (["PUT s3.eu-west-3.amazonaws.com/assets/a.json"], some "abc123") -/
#guard_msgs in
#eval show IO (List String × Option String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 "" [("ETag", "\"abc123\"")]) creds ep "assets"
  match ← s.putString "a.json" "{}" with
  | .ok m => return (← log.get, m.etag)
  | .error _ => return (← log.get, none)

/-- The listing query, and the XML it parses. -/
def listReply : String :=
  "<ListBucketResult><Name>assets</Name><IsTruncated>false</IsTruncated>" ++
  "<Contents><Key>logs/a.json</Key><Size>10</Size>" ++
  "<ETag>&quot;aaa&quot;</ETag></Contents>" ++
  "<Contents><Key>logs/b.json</Key><Size>20</Size></Contents>" ++
  "</ListBucketResult>"

/-- info: (["GET s3.eu-west-3.amazonaws.com/assets?list-type=2&prefix=logs%2F"], ["logs/a.json", "logs/b.json"], [10, 20], true) -/
#guard_msgs in
#eval show IO (List String × List String × List Nat × Bool) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 listReply) creds ep "assets"
  match ← s.list "logs/" none with
  | .ok page => return (← log.get, page.items.map (·.key), page.items.map (·.size), page.isLast)
  | .error _ => return (← log.get, [], [], false)

-- ── Object versioning ──────────────────────────────────────────────────────

/- `Provider.supports` reported `objectVersioning` for all three clouds long
   before anything could use it. These pin the wire form, because that is where
   the two dialects differ and where a mistake is invisible from Lean: S3 selects
   the operation with a valueless `versions` parameter and pages by *two*
   markers, GCS uses `versions=true` and one page token. -/

/-- A `ListObjectVersions` reply: two data versions and a delete marker, plus a
    truncation that carries both of S3's markers. -/
def versionsReply : String :=
  "<ListVersionsResult><Name>assets</Name><IsTruncated>true</IsTruncated>" ++
  "<NextKeyMarker>logs/b.json</NextKeyMarker>" ++
  "<NextVersionIdMarker>v9</NextVersionIdMarker>" ++
  "<Version><Key>logs/a.json</Key><VersionId>v2</VersionId>" ++
  "<IsLatest>true</IsLatest><Size>20</Size><ETag>&quot;bbb&quot;</ETag></Version>" ++
  "<Version><Key>logs/a.json</Key><VersionId>v1</VersionId>" ++
  "<IsLatest>false</IsLatest><Size>10</Size></Version>" ++
  "<DeleteMarker><Key>logs/gone.json</Key><VersionId>v3</VersionId>" ++
  "<IsLatest>true</IsLatest></DeleteMarker>" ++
  "</ListVersionsResult>"

/- The versions listing: `versions` is valueless, and every version and marker
   comes back with its id, its latest flag and its kind. -/
/-- info: ["GET s3.eu-west-3.amazonaws.com/assets?prefix=logs%2F&versions="] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 versionsReply) creds ep "assets"
  let _ ← s.listVersions "logs/" none
  log.get

/- Every version and marker comes back with its id, its latest flag, and which
   kind of entry it is. The delete marker is listed rather than dropped: a
   caller reconstructing history needs to see that a key was deleted at a point
   in it. -/
/-- info: (["v2", "v1", "v3"], [true, false, true], [false, false, true]) -/
#guard_msgs in
#eval show IO (List String × List Bool × List Bool) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 versionsReply) creds ep "assets"
  match ← s.listVersions "logs/" none with
  | .ok page =>
    return (page.items.map (·.versionId), page.items.map (·.isLatest),
            page.items.map (·.isDeleteMarker))
  | .error _ => return ([], [], [])

/- A truncated page is *not* reported as the last one, and the cursor packs both
   markers — a key can have more versions than fit in a page, so a position in
   this listing is a (key, version) pair. -/
/-- info: (false, true) -/
#guard_msgs in
#eval show IO (Bool × Bool) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 versionsReply) creds ep "assets"
  match ← s.listVersions "logs/" none with
  | .ok page =>
    return (page.isLast, (page.next.map (·.token)) == some "logs/b.json\x00v9")
  | .error _ => return (true, false)

/- Following that cursor sends both markers back. -/
/-- info: ["GET s3.eu-west-3.amazonaws.com/assets?key-marker=logs%2Fb.json&prefix=logs%2F&version-id-marker=v9&versions="] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 versionsReply) creds ep "assets"
  let _ ← s.listVersions "logs/" (some ⟨"logs/b.json\x00v9"⟩)
  log.get

/- Reading and destroying one version address it by `versionId`. `deleteVersion`
   is destructive where `delete` merely adds a marker, which is why they are
   separate operations rather than one with an optional argument. -/
/-- info: ["GET s3.eu-west-3.amazonaws.com/assets/a.json?versionId=v1"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status200 "old bytes") creds ep "assets"
  let _ ← s.getVersion "a.json" "v1"
  log.get

/-- info: ["DELETE s3.eu-west-3.amazonaws.com/assets/a.json?versionId=v1"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status204 "") creds ep "assets"
  let _ ← s.deleteVersion "a.json" "v1"
  log.get

/- A store that cannot version says so rather than answering something
   unusable. `inMemory` keeps no history, so all three operations decline. -/
/-- info: (true, true, true) -/
#guard_msgs in
#eval show IO (Bool × Bool × Bool) from do
  let s ← ObjectStore.inMemory
  let l ← s.listVersions "" none
  let g ← s.getVersion "a" "v1"
  let d ← s.deleteVersion "a" "v1"
  return (l.toOption.isNone, g.toOption.isNone, d.toOption.isNone)

/-- info: ["DELETE s3.eu-west-3.amazonaws.com/assets/a.json"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.S3.atEndpoint (wire log status204 "") creds ep "assets"
  let _ ← s.delete "a.json"
  log.get

/- **GCP is refused here, explicitly.** Its S3-compatible API needs
   interoperability keys this library does not hold, so this is a genuine
   credential difference rather than an unimplemented path — and saying so
   beats a signature failure. -/
#guard match ObjectStore.S3.of Transport.network .gcp { region := "europe-west9" } "assets" with
  | .error e => e.klass == .unsupported
  | .ok _ => false

/- AWS and Scaleway both succeed, from the same code path. -/
#guard match ObjectStore.S3.of Transport.network .aws creds "assets" with
  | .ok s => (s.describe.splitOn "s3.eu-west-3.amazonaws.com").length == 2
  | .error _ => false

#guard match ObjectStore.S3.of Transport.network .scaleway
    { accessKey := "S", secretKey := "s", region := "fr-par" } "assets" with
  | .ok s => (s.describe.splitOn "s3.fr-par.scw.cloud").length == 2
  | .error _ => false

/- A cloud with no region configured says so, rather than signing for the
   empty region and failing obscurely. -/
#guard match ObjectStore.S3.of Transport.network .aws { accessKey := "A", secretKey := "s" }
    "assets" with
  | .error e => e.klass == .unbound
  | .ok _ => false

-- ── The GCS backend ─────────────────────────────────────────────────────────

/- **The encoding that distinguishes GCS from S3.** An object name is a single
   path segment, so its slashes are `%2F` on the wire; leaving them literal
   addresses a different resource and answers 404. And `?alt=media` is what
   asks for the bytes rather than the metadata document. -/
/-- info: ["GET storage.googleapis.com/storage/v1/b/assets/o/logs%2F2026%2Fa.json?alt=media"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let s := ObjectStore.Gcs.withToken (wire log status200 "bytes") "ya29.tok" "assets"
  let _ ← s.get "logs/2026/a.json"
  log.get

/- Metadata is the same URL *without* `alt=media`, and `size` arrives quoted. -/
/-- info: (["GET storage.googleapis.com/storage/v1/b/assets/o/a.json"], some 1024, some "application/json") -/
#guard_msgs in
#eval show IO (List String × Option Nat × Option String) from do
  let log ← IO.mkRef []
  let body := "{\"name\":\"a.json\",\"size\":\"1024\",\"contentType\":\"application/json\"," ++
              "\"etag\":\"CJC9\",\"updated\":\"2026-09-01T10:00:00.000Z\"}"
  let s := ObjectStore.Gcs.withToken (wire log status200 body) "ya29.tok" "assets"
  match ← s.head "a.json" with
  | .ok (some m) => return (← log.get, some m.size, m.contentType)
  | _ => return (← log.get, none, none)

/- Uploads go to a **different path prefix**, with the name as a query
   parameter rather than in the path. -/
/-- info: ["POST storage.googleapis.com/upload/storage/v1/b/assets/o?name=logs%2Fa.json&uploadType=media"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let body := "{\"name\":\"logs/a.json\",\"size\":\"2\"}"
  let s := ObjectStore.Gcs.withToken (wire log status200 body) "ya29.tok" "assets"
  let _ ← s.putString "logs/a.json" "hi"
  log.get

/-- info: (["GET storage.googleapis.com/storage/v1/b/assets/o?prefix=logs%2F"], ["logs/a.json"], some "CgtsYWJz") -/
#guard_msgs in
#eval show IO (List String × List String × Option String) from do
  let log ← IO.mkRef []
  let body := "{\"items\":[{\"name\":\"logs/a.json\",\"size\":\"3\"}]," ++
              "\"nextPageToken\":\"CgtsYWJz\"}"
  let s := ObjectStore.Gcs.withToken (wire log status200 body) "ya29.tok" "assets"
  match ← s.list "logs/" none with
  | .ok page => return (← log.get, page.items.map (·.key), page.next.map (·.token))
  | .error _ => return (← log.get, [], none)

/- **GCS answers 404 for deleting a name that is not there, where S3 answers
   204.** Normalised to success, so `delete` is idempotent on every cloud —
   which is what a caller retrying after a timeout needs. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let log ← IO.mkRef []
  let s := ObjectStore.Gcs.withToken (wire log status404 "") "ya29.tok" "assets"
  match ← s.delete "gone" with
  | .ok _ => return true
  | .error _ => return false

/- No token is `unbound` before a request, not a `denied` after one. -/
#guard match ObjectStore.Gcs.of Transport.network {} "assets" with
  | .error e => e.klass == .unbound
  | .ok _ => false

-- ── The pure pieces ─────────────────────────────────────────────────────────

#guard ObjectStore.Gcs.encodeName "logs/2026/a.json" == "logs%2F2026%2Fa.json"
#guard ObjectStore.Gcs.objectPath "assets" "a/b" == "/storage/v1/b/assets/o/a%2Fb"
#guard ObjectStore.Gcs.uploadPath "assets" == "/upload/storage/v1/b/assets/o"

#guard inMemoryPageSize == 3

end Tests.Cloud.ObjectStore
