/-
  Tests for `Cloud.Protocol.GoogleRest`.

  Two Google-specific traps are pinned here, because both fail silently:

  1. **`Int64` fields arrive as strings.** A GCS object's `size` is
     `"1024"`, not `1024`, because JSON numbers are doubles and would lose
     precision. Reading it as a number yields `none` and an object of size
     zero.
  2. **Errors nest under `error`.** Reading only the top level renders every
     GCP failure with an empty message — the only part a human can act on.
-/
import Linen.Cloud.Protocol.GoogleRest

open Cloud Cloud.Protocol.GoogleRest
open Network.HTTP.Types (status200 status403 status404)

namespace Tests.Cloud.Protocol.GoogleRest

-- ── Resource names ──────────────────────────────────────────────────────────

#guard projectPath "typednotes" "topics" == "projects/typednotes/topics"
#guard resourcePath "typednotes" "secrets" "db-password"
  == "projects/typednotes/secrets/db-password"

/- Google answers with a fully-qualified name where the caller asked about a
   bare one, so this is how a reply is matched to a request. -/
#guard Gcp.shortName (resourcePath "p" "topics" "jobs") == "jobs"

-- ── Fixtures ────────────────────────────────────────────────────────────────

def respondWith (log : IO.Ref (List String)) (st : Network.HTTP.Types.Status)
    (body : String) : Transport :=
  Transport.stub fun req => do
    let auth := (req.headers.find? (fun h => h.1 == Data.CI.mk' "Authorization")).map (·.2)
    log.modify (fun l => l ++
      [s!"{req.method} {req.host}{req.path}{req.queryString} auth={auth.getD "-"}"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/-- A real `storage.objects.list` reply. Note `size` as a quoted string and the
    pagination token. -/
def objectsList : String :=
  "{\"kind\":\"storage#objects\"," ++
  "\"items\":[" ++
    "{\"kind\":\"storage#object\",\"name\":\"logs/2026/a.json\"," ++
     "\"bucket\":\"assets\",\"size\":\"1024\"," ++
     "\"contentType\":\"application/json\"," ++
     "\"etag\":\"CJC9pfnLmYIDEAE=\"," ++
     "\"updated\":\"2026-09-01T10:00:00.000Z\"," ++
     "\"generation\":\"1725184800000000\"}]," ++
  "\"nextPageToken\":\"CgtsYWJzLzIwMjYv\"}"

-- ── Bearer, not signed ──────────────────────────────────────────────────────

/-- info: ["GET storage.googleapis.com/storage/v1/b/assets/o?prefix=logs%2F auth=Bearer ya29.tok"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let _ ← invoke (respondWith log status200 objectsList) "ya29.tok" Gcp.storageHost
    "GET" "/storage/v1/b/assets/o" (query := [("prefix", some "logs/")])
  log.get

/- No body means no `Content-Type`, so a `GET` does not claim to carry JSON. -/
#guard (call "tok" Gcp.storageHost "GET" "/v1/x").headers.length == 0
#guard (call "tok" Gcp.storageHost "POST" "/v1/x" (payload := some (.object []))).headers
  == [("Content-Type", "application/json")]

-- ── Reading a reply ─────────────────────────────────────────────────────────

/-- info: (1, some "logs/2026/a.json", some 1024, some "CgtsYWJzLzIwMjYv") -/
#guard_msgs in
#eval show IO (Nat × Option String × Option Nat × Option String) from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 objectsList) "tok" Gcp.storageHost "GET" "/o" with
  | .error _ => return (0, none, none, none)
  | .ok v =>
    let items := array v "items"
    return ( items.length
           , items.head?.bind (string? · "name")
           , items.head?.bind (natString? · "size")
           , (nextPageToken? v).map (·.token) )

/- **The `Int64`-as-string trap.** `size` read as a number finds nothing, which
   would make every object appear to be zero bytes. -/
#guard natString? (.object [("size", .string "1024")]) "size" == some 1024
#guard nat? (.object [("size", .string "1024")]) "size" == none
#guard natString? (.object [("size", .number 1024.0)]) "size" == none

/- An empty bucket omits `items` entirely rather than sending `[]`. -/
/-- info: (0, none) -/
#guard_msgs in
#eval show IO (Nat × Option String) from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 "{\"kind\":\"storage#objects\"}") "tok"
    Gcp.storageHost "GET" "/o" with
  | .ok v => return ((array v "items").length, (nextPageToken? v).map (·.token))
  | .error _ => return (99, none)

/- `DELETE` answers 200 with no body, and that is success. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 "") "tok" Gcp.storageHost "DELETE" "/o/x" with
  | .ok v => return v.isNull
  | .error _ => return false

-- ── Errors nest ─────────────────────────────────────────────────────────────

/- **The nesting matters.** Read at the top level, this message would be empty
   — and the message is the only part that says which permission is missing. -/
/--
info: (Cloud.Class.denied,
 "PERMISSION_DENIED",
 "Permission 'secretmanager.versions.access' denied for resource 'projects/typednotes/secrets/db-password'.")
-/
#guard_msgs in
#eval show IO (Class × String × String) from do
  let log ← IO.mkRef []
  let body :=
    "{\"error\":{\"code\":403," ++
    "\"message\":\"Permission 'secretmanager.versions.access' denied for resource " ++
    "'projects/typednotes/secrets/db-password'.\"," ++
    "\"status\":\"PERMISSION_DENIED\"}}"
  match ← invoke (respondWith log status403 body) "tok" Gcp.secretManagerHost "GET" "/v1/x" with
  | .error e => return (e.klass, e.code, e.message)
  | .ok _ => return (.protocol, "", "")

/-- info: (Cloud.Class.notFound, "NOT_FOUND") -/
#guard_msgs in
#eval show IO (Class × String) from do
  let log ← IO.mkRef []
  let body := "{\"error\":{\"code\":404,\"message\":\"Not found.\",\"status\":\"NOT_FOUND\"}}"
  match ← invoke (respondWith log status404 body) "tok" Gcp.storageHost "GET" "/o/gone" with
  | .error e => return (e.klass, e.code)
  | .ok _ => return (.protocol, "")

-- ── Media downloads keep their bytes ────────────────────────────────────────

/- An object's contents are not JSON, so `sendBytes` hands back the body
   untouched — decoding it as text would corrupt anything that is not UTF-8. -/
/-- info: #[0, 1, 254, 255] -/
#guard_msgs in
#eval show IO (Array UInt8) from do
  let t := Transport.stub fun _ =>
    pure { statusCode := status200, headers := [], body := ⟨#[0, 1, 254, 255]⟩ }
  match ← sendBytes t (call "tok" Gcp.storageHost "GET" "/o/x"
    (query := [("alt", some "media")])) with
  | .ok bytes => return bytes.toList.toArray
  | .error _ => return #[]

/- An upload declares its own content type rather than `application/json`. -/
#guard (callBytes "tok" Gcp.storageHost "POST" "/upload/storage/v1/b/assets/o"
  (body := "x".toUTF8) (contentType := "text/plain")).headers
  == [("Content-Type", "text/plain")]

end Tests.Cloud.Protocol.GoogleRest
