/-
  `Cloud.Queue.Sqs` — the SQS data plane, for AWS **and** Scaleway

  Scaleway's Queues product is SQS-compatible, so one implementation serves
  both clouds and the only difference is the host `Sqs.endpoint?` returns.

  ## AWS-JSON 1.0, not 1.1

  SQS is the outlier: every other AWS-JSON service in this namespace speaks
  1.1, and sending 1.1 to SQS is refused rather than negotiated. The version
  comes from `Cloud.Sqs.jsonVersion` so it cannot drift.

  ## Every request needs the queue **URL**, not its name

  SQS identifies a queue by a URL containing the account id, which a caller
  does not generally know. `GetQueueUrl` resolves a name to one, so both
  constructors here are `IO` and cache the result in the closure — one extra
  round trip at construction, none afterwards.

  ## Batch operations, always

  `SendMessageBatch`, `DeleteMessageBatch` and `ChangeMessageVisibilityBatch`
  are used even for a single message, because the batch forms subsume the
  singular ones and using one code path means the batch semantics — **partial
  failure** — are handled rather than discovered later. A batch reply has
  `Successful` and `Failed` lists, and a call where every entry failed is
  reported as an error rather than as success with nothing done.

  ## Scaleway needs a different key pair

  Scaleway's Queues will not accept the account's main API key: it requires a
  credential minted through its Messaging-and-Queuing API. Pass those
  credentials explicitly. This module deliberately does **not** mint one: the
  sibling `typednotes/infra` does, and its minting reclaims any credential
  named `infra`, deleting whatever held that name — defensible for a tool that
  owns its fleet, unacceptable in a library.

  ## Provenance

  New code. `typednotes/infra`'s SQS client is control-plane only — create,
  list, set attributes, delete — with no message operations at all.
-/
import Linen.Cloud.Queue
import Linen.Cloud.Protocol.AwsJson

namespace Cloud.Queue.Sqs

open Cloud
open Cloud.Protocol.AwsJson (invoke string? nat? array field?)
open Data.Json (Value)

-- ── Targets ─────────────────────────────────────────────────────────────────

/-- The `X-Amz-Target` for an SQS operation. -/
def target (op : String) : String := "AmazonSQS." ++ op

-- ── Message attributes ──────────────────────────────────────────────────────

/-- SQS's message-attribute encoding.

    Each attribute is an object naming its type, so a bare string will not do:
    `{"origin": {"DataType": "String", "StringValue": "batch-7"}}`. Only the
    `String` type is used — SQS also has `Number` and `Binary`, which Pub/Sub
    has no equivalent for, so they are out of the portable interface. -/
def encodeAttributes (attrs : List (String × String)) : Value :=
  .object (attrs.map fun (k, v) =>
    (k, .object [("DataType", .string "String"), ("StringValue", .string v)]))

/-- Read SQS's message attributes back into pairs, keeping only the string
    ones. -/
def decodeAttributes (v : Value) : List (String × String) :=
  match (field? v "MessageAttributes").bind Data.Json.Value.asObject with
  | none => []
  | some fields =>
    fields.filterMap fun (k, av) => (string? av "StringValue").map (fun s => (k, s))

/-- Read SQS's *system* attributes — a flat string map, unlike the message
    attributes above. `ApproximateReceiveCount` is the one worth having. -/
def receiveCount? (v : Value) : Option Nat :=
  ((field? v "Attributes").bind Data.Json.Value.asObject).bind fun fields =>
    ((fields.find? (·.1 == "ApproximateReceiveCount")).map (·.2)).bind
      Data.Json.Value.asString |>.bind (·.toNat?)

-- ── Resolving the queue URL ─────────────────────────────────────────────────

/-- Resolve a queue name to the URL every other operation needs. -/
def queueUrl (t : Transport) (creds : Credentials) (ep : Endpoint) (name : String) :
    IO (Except Error String) := do
  match ← invoke t creds ep Cloud.Sqs.jsonVersion (target "GetQueueUrl")
      (.object [("QueueName", .string name)]) with
  | .error e => return .error e
  | .ok v    => return Cloud.Protocol.AwsJson.requireString v "QueueUrl"

-- ── Batch replies ───────────────────────────────────────────────────────────

/-- Turn a batch reply into either the successful entries or an error.

    A reply whose `Failed` list is non-empty and `Successful` list is empty is
    an error: nothing happened, and reporting success would lose a message
    silently. A *partial* failure answers the successes, with the failures
    folded into the message so they are at least visible. -/
private def batchOutcome (v : Value) (what : String) :
    Except Error (List Value) :=
  let ok := array v "Successful"
  let failed := array v "Failed"
  if failed.isEmpty then .ok ok
  else if ok.isEmpty then
    let first := failed.head?.bind (string? · "Message") |>.getD "no reason given"
    .error
      { klass := .invalid
      , message := s!"{what}: all {failed.length} entries failed: {first}" }
  else
    .error
      { klass := .invalid
      , message := s!"{what}: {failed.length} of {ok.length + failed.length} entries failed" }

-- ── The producer ────────────────────────────────────────────────────────────

/-- A producer publishing to an SQS queue, given its resolved URL. -/
def producerAt (t : Transport) (creds : Credentials) (ep : Endpoint) (url : String) :
    Producer :=
  { target := url
  , send := fun msgs => do
      if msgs.isEmpty then return .ok []
      let entries := msgs.zipIdx.map fun (m, i) =>
        Value.object <|
          [ ("Id", .string s!"e{i}")
          , ("MessageBody", .string m.body) ]
          ++ (if m.attributes.isEmpty then []
              else [("MessageAttributes", encodeAttributes m.attributes)])
      match ← invoke t creds ep Cloud.Sqs.jsonVersion (target "SendMessageBatch")
          (.object [("QueueUrl", .string url), ("Entries", .array entries.toArray)]) with
      | .error e => return .error e
      | .ok v =>
        match batchOutcome v "SendMessageBatch" with
        | .error e => return .error e
        | .ok ok   =>
          -- Answer in the caller's order. SQS may reorder `Successful`, so the
          -- entry ids are matched rather than assumed positional.
          let idFor (i : Nat) : Option String :=
            (ok.find? (fun e => string? e "Id" == some s!"e{i}")).bind (string? · "MessageId")
          return .ok ((List.range msgs.length).filterMap idFor) }

-- ── The consumer ────────────────────────────────────────────────────────────

/-- Read one message out of a `ReceiveMessage` reply. -/
private def messageOf (v : Value) : Option Message :=
  match string? v "MessageId", string? v "ReceiptHandle", string? v "Body" with
  | some id, some handle, some body =>
    some { id, body, receipt := ⟨handle⟩
         , attributes := decodeAttributes v
         , receiveCount := receiveCount? v }
  | _, _, _ => none

/-- Entries for a receipt-handle batch operation, optionally carrying a new
    visibility timeout. -/
private def receiptEntries (receipts : List Receipt) (visibility : Option Nat) :
    Array Value :=
  (receipts.zipIdx.map fun (r, i) =>
    Value.object <|
      [("Id", .string s!"e{i}"), ("ReceiptHandle", .string r.handle)]
      ++ (match visibility with
          | some s => [("VisibilityTimeout", .number (Float.ofNat s))]
          | none   => [])).toArray

/-- A consumer reading from an SQS queue, given its resolved URL. -/
def consumerAt (t : Transport) (creds : Credentials) (ep : Endpoint) (url : String) :
    Consumer :=
  { source := url
  , receive := fun params => do
      let body := Value.object <|
        [ ("QueueUrl", .string url)
        , ("MaxNumberOfMessages", .number (Float.ofNat params.maxMessages))
        -- Ask for both attribute families, or `receiveCount` and the caller's
        -- own attributes are silently absent.
        , ("MessageSystemAttributeNames", .array #[.string "All"])
        , ("MessageAttributeNames", .array #[.string "All"]) ]
        ++ (match params.waitSeconds with
            | some s => [("WaitTimeSeconds", .number (Float.ofNat s))]
            | none   => [])
        ++ (match params.leaseSeconds with
            | some s => [("VisibilityTimeout", .number (Float.ofNat s))]
            | none   => [])
      match ← invoke t creds ep Cloud.Sqs.jsonVersion (target "ReceiveMessage") body with
      | .error e => return .error e
      | .ok v    => return .ok ((array v "Messages").filterMap messageOf)
  , ack := fun receipts => do
      if receipts.isEmpty then return .ok ()
      match ← invoke t creds ep Cloud.Sqs.jsonVersion (target "DeleteMessageBatch")
          (.object [("QueueUrl", .string url)
                   , ("Entries", .array (receiptEntries receipts none))]) with
      | .error e => return .error e
      | .ok v    => return (batchOutcome v "DeleteMessageBatch").map (fun _ => ())
  , extendLease := fun receipts seconds => do
      if receipts.isEmpty then return .ok ()
      match ← invoke t creds ep Cloud.Sqs.jsonVersion
          (target "ChangeMessageVisibilityBatch")
          (.object [("QueueUrl", .string url)
                   , ("Entries", .array (receiptEntries receipts (some seconds)))]) with
      | .error e => return .error e
      | .ok v    => return (batchOutcome v "ChangeMessageVisibilityBatch").map (fun _ => ())
  , purge := do
      match ← invoke t creds ep Cloud.Sqs.jsonVersion (target "PurgeQueue")
          (.object [("QueueUrl", .string url)]) with
      | .error e => return .error e
      | .ok _    => return .ok () }

-- ── Constructors ────────────────────────────────────────────────────────────

/-- Resolve the endpoint for a cloud, or say why there is none. -/
private def endpointFor (provider : Provider) (creds : Credentials)
    (region : Option String) : Except Error Endpoint := do
  let region ← match region with
    | some r => .ok r
    | none   => creds.requireRegion provider
  match Cloud.Sqs.endpoint? provider region with
  | some ep => .ok ep
  | none    =>
    .error (Error.unsupported provider.name
      "the SQS API (its queues are Pub/Sub topics and subscriptions — see Cloud.Queue.PubSub)")

/-- A producer for a named SQS queue.

    `IO` because the queue's URL has to be resolved once; it is then cached in
    the returned closure. -/
def producer (t : Transport) (provider : Provider) (creds : Credentials) (name : String)
    (region : Option String := none) : IO (Except Error Producer) := do
  match endpointFor provider creds region with
  | .error e => return .error e
  | .ok ep =>
    match ← queueUrl t creds ep name with
    | .error e  => return .error e
    | .ok url   => return .ok (producerAt t creds ep url)

/-- A consumer for a named SQS queue.

    On SQS the consumer reads from the same queue the producer writes to, so
    unlike Pub/Sub there is no second name to supply. -/
def consumer (t : Transport) (provider : Provider) (creds : Credentials) (name : String)
    (region : Option String := none) : IO (Except Error Consumer) := do
  match endpointFor provider creds region with
  | .error e => return .error e
  | .ok ep =>
    match ← queueUrl t creds ep name with
    | .error e  => return .error e
    | .ok url   => return .ok (consumerAt t creds ep url)

/-- Both halves of a named SQS queue, resolving its URL once. -/
def of (t : Transport) (provider : Provider) (creds : Credentials) (name : String)
    (region : Option String := none) : IO (Except Error Queue) := do
  match endpointFor provider creds region with
  | .error e => return .error e
  | .ok ep =>
    match ← queueUrl t creds ep name with
    | .error e => return .error e
    | .ok url  =>
      return .ok { producer := producerAt t creds ep url, consumer := consumerAt t creds ep url }

/-- The queue name in an SQS queue URL — its last path segment.

    Inverts `GetQueueUrl`, for reporting a queue by the name a caller
    recognises rather than by a URL containing an account id. -/
def nameOfUrl (url : String) : String :=
  (url.splitOn "/").getLast?.getD url

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard target "ReceiveMessage" == "AmazonSQS.ReceiveMessage"

#guard nameOfUrl "https://sqs.eu-west-3.amazonaws.com/123456789012/jobs" == "jobs"
#guard nameOfUrl "https://sqs.mnq.fr-par.scaleway.com/project-8460bf58/infra-example"
  == "infra-example"
#guard nameOfUrl "jobs" == "jobs"

-- An attribute is an object naming its type, not a bare string.
#guard Data.Json.Encode.encode (encodeAttributes [("origin", "batch-7")])
  == "{\"origin\":{\"DataType\":\"String\",\"StringValue\":\"batch-7\"}}"
#guard Data.Json.Encode.encode (encodeAttributes []) == "{}"

end Cloud.Queue.Sqs
