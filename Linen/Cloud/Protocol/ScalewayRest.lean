/-
  `Cloud.Protocol.ScalewayRest` — Scaleway's own API

  Used for Secret Manager, and for minting the dedicated credential Scaleway's
  Queues need. Scaleway's Object Storage and Queues *themselves* are reached
  through `Cloud.Protocol.S3` and `Cloud.Protocol.AwsJson`, because they are
  S3- and SQS-compatible — which is the reuse this namespace is built on.

  ## One host, region in the path

  `api.scaleway.com` serves every region, with the region in the path
  (`/secret-manager/v1beta1/regions/fr-par/secrets`). The opposite of AWS's
  arrangement, where the region is in the hostname.

  ## Unsigned

  Authentication is the secret key in an `X-Auth-Token` header. Nothing is
  signed, so — as with GCP's bearer token — anything that can read the header
  can make the call. Unlike GCP's, this credential is long-lived.

  ## Resources are addressed by UUID

  Scaleway's API takes a UUID where a caller knows a name, so most operations
  need a list-and-find first. That is a real extra round trip, not an
  implementation detail, and it is why the secret client caches what it
  resolves.

  ## Provenance

  Moved down from the sibling `typednotes/infra`
  (`Infra/Providers/Scaleway/Rest.lean`).
-/
import Linen.Cloud.Transport
import Linen.Data.Json.Encode
import Linen.Data.Json.Decode

namespace Cloud.Protocol.ScalewayRest

open Cloud
open Data.Json (Value)
open Network.HTTP.Types (Query)

-- ── Calls ───────────────────────────────────────────────────────────────────

/-- Build a call to Scaleway's own API.

    `path` is a full path, normally built from
    `Cloud.Scaleway.regionalPrefix`. -/
def call (creds : Credentials) (region method path : String) (query : Query := [])
    (payload : Option Value := none) : Call :=
  let body := match payload with
    | some v => (Data.Json.Encode.encode v).toUTF8
    | none   => ByteArray.empty
  let contentType := match payload with
    | some _ => [("Content-Type", "application/json")]
    | none   => []
  { method
  , endpoint := Scaleway.endpoint region
  , path, query
  , headers := contentType
  , body
  , auth := Auth.authToken creds.secretKey }

/-- Issue a call and parse the reply as JSON. An empty body becomes `.null`. -/
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

/-- Build and issue a call in one step. -/
def invoke (t : Transport) (creds : Credentials) (region method path : String)
    (query : Query := []) (payload : Option Value := none) : IO (Except Error Value) :=
  send t (call creds region method path query payload)

-- ── Paths ───────────────────────────────────────────────────────────────────

/-- A path under a regional product, e.g.
    `/secret-manager/v1beta1/regions/fr-par/secrets`.

    `product` is the `(name, version)` pair from `Cloud.Scaleway`, so the
    version cannot drift between call sites. -/
def regionalPath (product : String × String) (region suffix : String) : String :=
  Scaleway.regionalPrefix product.1 product.2 region ++ suffix

-- ── Reading replies ─────────────────────────────────────────────────────────

/-- A field of a JSON object, or `none`. -/
def field? (v : Value) (name : String) : Option Value :=
  v.asObject.bind fun fields => (fields.find? (·.1 == name)).map (·.2)

/-- An optional string field. -/
def string? (v : Value) (name : String) : Option String :=
  (field? v name).bind Data.Json.Value.asString

/-- A required string field, or a `protocol` error naming what was missing. -/
def requireString (v : Value) (name : String) : Except Error String :=
  match string? v name with
  | some s => .ok s
  | none   => .error (Error.protocol s!"response is missing string field '{name}'")

/-- An optional numeric field. -/
def nat? (v : Value) (name : String) : Option Nat :=
  (field? v name).bind Data.Json.Value.asNumber |>.map (fun f => f.toUInt64.toNat)

/-- An array field, or the empty list. -/
def array (v : Value) (name : String) : List Value :=
  match (field? v name).bind Data.Json.Value.asArray with
  | some a => a.toList
  | none   => []

/-- Find a named resource's UUID in a listing.

    The list-and-find that Scaleway's UUID addressing forces. `collection` is
    the field holding the array, e.g. `secrets`. -/
def findId? (v : Value) (collection nameField idField name : String) : Option String :=
  (array v collection).findSome? fun item =>
    if string? item nameField == some name then string? item idField else none

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard regionalPath Scaleway.secretProduct "fr-par" "/secrets"
  == "/secret-manager/v1beta1/regions/fr-par/secrets"
#guard regionalPath Scaleway.queuesProduct "fr-par" "/activate-sqs"
  == "/mnq/v1beta1/regions/fr-par/activate-sqs"

/- The list-and-find Scaleway's UUID addressing forces on every operation. -/
#guard findId?
  (.object [("secrets", .array
    #[ .object [("name", .string "other"), ("id", .string "uuid-1")]
     , .object [("name", .string "db-password"), ("id", .string "uuid-2")] ])])
  "secrets" "name" "id" "db-password" == some "uuid-2"

#guard findId? (.object [("secrets", .array #[])]) "secrets" "name" "id" "x" == none

end Cloud.Protocol.ScalewayRest
