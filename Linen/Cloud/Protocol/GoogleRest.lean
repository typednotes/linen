/-
  `Cloud.Protocol.GoogleRest` — method-and-path REST with a bearer token

  The dialect Cloud Storage, Pub/Sub and Secret Manager speak. An ordinary REST
  API: the method and path say what to do, JSON in and JSON out, and
  `Authorization: Bearer` says who is asking.

  ## Nothing is signed

  Every GCP credential in this namespace is a short-lived OAuth2 token, so a
  request carries a bearer credential rather than a signature. Consequences
  worth knowing: the token expires (typically within the hour), and anything
  that can read it can use it. `Cloud.Credentials.Gcp` mints fresh ones from a
  service-account key, which is why that source is preferred over `gcloud`'s.

  ## Errors nest

  Google answers failures as `{"error": {"code": …, "status": …, "message": …}}`
  rather than at the top level. `Cloud.Error.parseJsonError` reads through the
  nesting; without that, every GCP failure renders with an empty message, which
  is the only part a human can act on.

  ## Paths are sent verbatim

  Google resource paths carry a literal `:` — an operation is a `:verb` suffix
  on the resource, as in `…/topics/jobs:publish` — and several also carry a
  pre-encoded object name. Percent-encoding the path would turn the colon into
  `%3A` and address a resource that does not exist, so calls built here set
  `Cloud.Call.pathPreEncoded`.

  That is safe precisely because **nothing here is signed**: a SigV4 signature
  is computed over the unencoded path, so a pre-encoded one could not be
  signed, and `Cloud.Call` refuses the combination. With a bearer token there
  is no signature to disagree with the wire.

  Callers building a path must therefore encode any component that needs it —
  `Cloud.ObjectStore.Gcs.encodeName` is the one case that does.

  ## Long-running operations are not here

  Several GCP control-plane calls answer with an `Operation` to poll. None of
  the object, queue or secret *data*-plane calls do, so no poller is needed —
  and a fuel-bounded polling loop is exactly the kind of thing that should not
  be written speculatively.

  ## Provenance

  Moved down from the sibling `typednotes/infra`
  (`Infra/Providers/Gcp/Rest.lean`), minus its two long-running-operation
  pollers, which stayed with the control plane that needs them.
-/
import Linen.Cloud.Transport
import Linen.Cloud.Page
import Linen.Data.Json.Encode
import Linen.Data.Json.Decode

namespace Cloud.Protocol.GoogleRest

open Cloud
open Data.Json (Value)
open Network.HTTP.Client (Response)
open Network.HTTP.Types (Query)

-- ── Calls ───────────────────────────────────────────────────────────────────

/-- Build a call to a Google API host.

    `token` is an OAuth2 access token; `Cloud.Credentials.Gcp.tokenFor` is how
    one is obtained. -/
def call (token host method path : String) (query : Query := [])
    (payload : Option Value := none) (headers : List (String × String) := []) : Call :=
  let body := match payload with
    | some v => (Data.Json.Encode.encode v).toUTF8
    | none   => ByteArray.empty
  let contentType := match payload with
    | some _ => [("Content-Type", "application/json")]
    | none   => []
  { method
  , endpoint := Gcp.endpoint host
  , path, query
  , headers := contentType ++ headers
  , body
  , auth := Auth.bearer token
    -- Google paths carry a literal `:` and sometimes a pre-encoded name; see
    -- the module header on why sending them verbatim is both necessary and
    -- safe here.
  , pathPreEncoded := true }

/-- Build a call whose body is raw bytes rather than JSON — a media upload. -/
def callBytes (token host method path : String) (query : Query := [])
    (body : ByteArray := ByteArray.empty) (contentType : String := "application/octet-stream")
    (headers : List (String × String) := []) : Call :=
  { method
  , endpoint := Gcp.endpoint host
  , path, query
  , headers := ("Content-Type", contentType) :: headers
  , body
  , auth := Auth.bearer token
  , pathPreEncoded := true }

/-- Issue a call and parse the reply as JSON.

    An empty body becomes `.null`: `DELETE` answers `200` with nothing. -/
def send (t : Transport) (c : Call) : IO (Except Error Value) := do
  match ← performNow t c with
  | .error e => return .error e
  | .ok resp =>
    match bodyText resp with
    | .error e => return .error e
    | .ok text =>
      let trimmed := text.trimAscii.toString
      if trimmed.isEmpty then return .ok .null
      match Data.Json.Decode.decode trimmed with
      | .ok v    => return .ok v
      | .error m => return .error (Error.protocol s!"malformed JSON response: {m}")

/-- Issue a call and return the response body as bytes — a media download. -/
def sendBytes (t : Transport) (c : Call) : IO (Except Error ByteArray) := do
  match ← performNow t c with
  | .error e => return .error e
  | .ok resp => return .ok resp.body

/-- Issue a call and keep the whole response, for a caller that needs its
    headers. -/
def sendRaw (t : Transport) (c : Call) : IO (Except Error Response) :=
  performNow t c

/-- Build and issue a JSON call in one step. -/
def invoke (t : Transport) (token host method path : String) (query : Query := [])
    (payload : Option Value := none) : IO (Except Error Value) :=
  send t (call token host method path query payload)

-- ── Resource names ──────────────────────────────────────────────────────────

/-- A project-scoped resource collection, e.g. `projects/p/topics`. -/
def projectPath (project collection : String) : String :=
  s!"projects/{project}/{collection}"

/-- A project-scoped resource, e.g. `projects/p/topics/jobs`. -/
def resourcePath (project collection name : String) : String :=
  s!"projects/{project}/{collection}/{name}"

-- ── Reading replies ─────────────────────────────────────────────────────────

/-- A field of a JSON object, or `none`. -/
def field? (v : Value) (name : String) : Option Value :=
  v.asObject.bind fun fields => (fields.find? (·.1 == name)).map (·.2)

/-- A required string field, or a `protocol` error naming what was missing. -/
def requireString (v : Value) (name : String) : Except Error String :=
  match (field? v name).bind Data.Json.Value.asString with
  | some s => .ok s
  | none   => .error (Error.protocol s!"response is missing string field '{name}'")

/-- An optional string field. -/
def string? (v : Value) (name : String) : Option String :=
  (field? v name).bind Data.Json.Value.asString

/-- An optional numeric field, for the fields Google really does send as
    numbers — Pub/Sub's `ackDeadlineSeconds`, an error's `code`. Contrast
    `natString?`. -/
def nat? (v : Value) (name : String) : Option Nat :=
  (field? v name).bind Data.Json.Value.asNumber |>.map (fun f => f.toUInt64.toNat)

/-- A field Google sends as a *string* even though it holds a number.

    `Int64` fields cross the wire quoted, because JSON numbers are doubles and
    would lose precision. `size` on a GCS object is the one that matters here;
    reading it as a number silently yields `none`. -/
def natString? (v : Value) (name : String) : Option Nat :=
  (string? v name).bind (·.toNat?)

/-- An array field, or the empty list — how these APIs represent an empty
    collection, which they also express by omitting the field entirely. -/
def array (v : Value) (name : String) : List Value :=
  match (field? v name).bind Data.Json.Value.asArray with
  | some a => a.toList
  | none   => []

/-- Google's pagination cursor, if the reply carries one. -/
def nextPageToken? (v : Value) : Option Cursor :=
  (string? v "nextPageToken").map Cursor.mk

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard projectPath "typednotes" "topics" == "projects/typednotes/topics"
#guard resourcePath "typednotes" "secrets" "db-password"
  == "projects/typednotes/secrets/db-password"

#guard string? (.object [("name", .string "projects/p/topics/jobs")]) "name"
  == some "projects/p/topics/jobs"

-- An Int64 crosses the wire quoted, so reading it as a number finds nothing.
#guard natString? (.object [("size", .string "1024")]) "size" == some 1024
#guard natString? (.object [("size", .number 1024.0)]) "size" == none

#guard nextPageToken? (.object [("nextPageToken", .string "abc")]) == some ⟨"abc"⟩
#guard nextPageToken? (.object []) == none

-- A Google path reaches the wire with its `:` intact: `%3A` addresses nothing.
#guard (call "tok" "pubsub.googleapis.com" "POST" "/v1/projects/p/topics/jobs:publish").wirePath
  == "/v1/projects/p/topics/jobs:publish"

end Cloud.Protocol.GoogleRest
