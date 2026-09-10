/-
  `Cloud.Protocol.AwsJson` — `POST /` with `X-Amz-Target` and a JSON body

  The dialect AWS Secrets Manager and SQS speak. Every operation is a `POST` to
  `/` with the operation named in a header and its arguments as a JSON object;
  the reply is JSON.

  ## Two versions, and getting it wrong is fatal

  **SQS speaks AWS-JSON 1.0. Secrets Manager speaks 1.1.** The version appears
  in the `Content-Type` (`application/x-amz-json-1.0`), and sending the wrong
  one is refused rather than negotiated. So the version is a parameter here and
  pinned per service in `Cloud.Endpoint` (`Sqs.jsonVersion`,
  `SecretsManager.jsonVersion`) rather than defaulted to whichever was written
  first.

  ## The path is double-encoded in the signature

  Unlike S3. The path is always `/` so it makes no observable difference here,
  but it is set correctly rather than left to look like an oversight.

  ## Empty replies

  Several operations answer `200` with no body at all. That becomes `.null`
  rather than a parse error, because for those operations the absence *is* the
  successful answer.

  ## Provenance

  Moved down from the sibling `typednotes/infra`
  (`Infra/Providers/Aws/Protocols.lean`, the `Json` namespace).
-/
import Linen.Cloud.Transport
import Linen.Data.Json.Encode
import Linen.Data.Json.Decode

namespace Cloud.Protocol.AwsJson

open Cloud
open Data.Json (Value)

-- ── Calls ───────────────────────────────────────────────────────────────────

/-- The `Content-Type` for an AWS-JSON version, e.g. `1.0` or `1.1`. -/
def contentType (version : String) : String := s!"application/x-amz-json-{version}"

/-- Build an AWS-JSON call.

    `target` is the fully-qualified operation name the service expects, e.g.
    `AmazonSQS.SendMessage` or `secretsmanager.GetSecretValue`. -/
def call (creds : Credentials) (ep : Endpoint) (version target : String)
    (payload : Value) : Call :=
  let body := (Data.Json.Encode.encode payload).toUTF8
  { method := "POST"
  , endpoint := ep
  , path := "/"
  , headers := [("Content-Type", contentType version), ("X-Amz-Target", target)]
  , body
  , auth := Auth.forEndpoint creds ep
  , doubleEncodePath := true }

/-- Issue an AWS-JSON call and parse the reply.

    An empty body becomes `.null`: for several operations that is the
    successful answer. -/
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
      | .error m =>
        return .error (Error.protocol s!"malformed JSON response: {m}")

/-- Build and issue an AWS-JSON call in one step. -/
def invoke (t : Transport) (creds : Credentials) (ep : Endpoint)
    (version target : String) (payload : Value) : IO (Except Error Value) :=
  send t (call creds ep version target payload)

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

/-- An optional numeric field, read as a `Nat`. -/
def nat? (v : Value) (name : String) : Option Nat :=
  (field? v name).bind Data.Json.Value.asNumber |>.map (fun f => f.toUInt64.toNat)

/-- An array field, or the empty list — which is how these APIs represent "none
    of them" for most collections. -/
def array (v : Value) (name : String) : List Value :=
  match (field? v name).bind Data.Json.Value.asArray with
  | some a => a.toList
  | none   => []

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard contentType "1.0" == "application/x-amz-json-1.0"
#guard contentType "1.1" == "application/x-amz-json-1.1"

#guard string? (.object [("QueueUrl", .string "https://q")]) "QueueUrl" == some "https://q"
#guard string? (.object [("QueueUrl", .string "https://q")]) "Other" == none
#guard nat? (.object [("n", .number 30.0)]) "n" == some 30
#guard (array (.object [("Messages", .array #[.null, .null])]) "Messages").length == 2
#guard (array (.object []) "Messages").length == 0

end Cloud.Protocol.AwsJson
