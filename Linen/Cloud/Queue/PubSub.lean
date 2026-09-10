/-
  `Cloud.Queue.PubSub` — Google Pub/Sub as a producer and a consumer

  ## Not a queue, and the interface says so

  Pub/Sub is a **topic** you publish to plus a **subscription** you pull from.
  A topic with no subscription discards everything published to it, silently.
  So `producer` takes a topic and `consumer` takes a subscription, and there is
  no constructor that takes one name and pretends to give you both — see
  `Cloud.Queue`'s header for why that split is the whole design.

  ## Bodies are base64 on the wire

  Pub/Sub's `data` field is base64-encoded bytes. `Cloud.OutgoingMessage.body`
  is text, so it is encoded on the way out and decoded on the way in. A message
  published by something else whose `data` is not valid UTF-8 comes back as a
  `protocol` error rather than mojibake.

  ## What is not available here

  - **Long polling.** Pub/Sub's pull blocks by its own rules and takes no
    `waitSeconds`, so `ReceiveParams.waitSeconds` is ignored.
  - **A per-pull lease.** The ack deadline is a property of the subscription,
    not of the pull, so `ReceiveParams.leaseSeconds` is ignored.
    `extendLease` still works — `modifyAckDeadline` changes the deadline of
    messages already pulled, which is the useful half.
  - **Purge.** Discarding a backlog is at best `subscriptions.seek` to the
    current time, which is **unverified here** and may not be permitted on an
    ordinary subscription. Rather than issue a call that might silently do
    something else, `purge` answers `unsupported` — matching
    `Provider.supports .queuePurge`, which is `false` for GCP.

  Each of these is recorded in `Cloud.Provider.supports`, so a caller can ask
  before it calls rather than discovering it from an error.

  ## Provenance

  New code, and the module with the least prior art: the sibling
  `typednotes/infra` manages Pub/Sub **topics only** and has no subscription
  support whatsoever, so `pull`, `acknowledge` and `modifyAckDeadline` are
  written from Google's REST reference with no reference implementation in
  either repository.
-/
import Linen.Cloud.Queue
import Linen.Cloud.Protocol.GoogleRest
import Linen.Data.Base64

namespace Cloud.Queue.PubSub

open Cloud
open Cloud.Protocol.GoogleRest (invoke string? nat? array field?)
open Data.Json (Value)

-- ── Resource paths ──────────────────────────────────────────────────────────

/-- A topic's REST path: `/v1/projects/{project}/topics/{topic}`. -/
def topicPath (project topic : String) : String :=
  s!"/v1/projects/{project}/topics/{topic}"

/-- A subscription's REST path. -/
def subscriptionPath (project subscription : String) : String :=
  s!"/v1/projects/{project}/subscriptions/{subscription}"

/-- Pub/Sub spells an operation as a `:verb` suffix on the resource path, e.g.
    `…/topics/jobs:publish`. -/
def action (path verb : String) : String := s!"{path}:{verb}"

-- ── Attributes ──────────────────────────────────────────────────────────────

/-- Pub/Sub attributes are a flat string map — simpler than SQS's typed
    attributes, and the reason the portable interface offers only strings. -/
def encodeAttributes (attrs : List (String × String)) : Value :=
  .object (attrs.map fun (k, v) => (k, .string v))

/-- Read attributes back into pairs. -/
def decodeAttributes (v : Value) : List (String × String) :=
  match (field? v "attributes").bind Data.Json.Value.asObject with
  | none        => []
  | some fields => fields.filterMap fun (k, av) => av.asString.map (fun s => (k, s))

-- ── The producer ────────────────────────────────────────────────────────────

/-- A producer publishing to a Pub/Sub topic.

    A topic with no subscription attached discards what is published to it, and
    neither this call nor Pub/Sub itself will say so — the one failure mode of
    this system that has no error to report. -/
def producerAt (t : Transport) (token project topic : String) : Producer :=
  { target := s!"pubsub://{project}/topics/{topic}"
  , send := fun msgs => do
      if msgs.isEmpty then return .ok []
      let entries := msgs.map fun m =>
        Value.object <|
          [("data", .string (Data.Base64.encode m.body.toUTF8))]
          ++ (if m.attributes.isEmpty then []
              else [("attributes", encodeAttributes m.attributes)])
      match ← invoke t token Gcp.pubSubHost "POST"
          (action (topicPath project topic) "publish") []
          (some (.object [("messages", .array entries.toArray)])) with
      | .error e => return .error e
      | .ok v    =>
        return .ok ((array v "messageIds").filterMap Data.Json.Value.asString) }

-- ── The consumer ────────────────────────────────────────────────────────────

/-- Read one message out of a `pull` reply.

    The shape is nested: each entry has an `ackId` and a `message` object, and
    the identifier lives on the inner one. Reading `messageId` from the outer
    object — the obvious mistake — yields `none` for every message. -/
private def messageOf (entry : Value) : Except Error (Option Message) :=
  match string? entry "ackId", (field? entry "message") with
  | some ackId, some inner =>
    match string? inner "messageId" with
    | none => .ok none
    | some id =>
      let encoded := (string? inner "data").getD ""
      match Data.Base64.decode encoded with
      | none => .error (Error.protocol s!"message '{id}' has a malformed base64 body")
      | some bytes =>
        match String.fromUTF8? bytes with
        | none      =>
          .error (Error.protocol s!"message '{id}' has a body that is not valid UTF-8")
        | some body =>
          .ok (some
            { id, body, receipt := ⟨ackId⟩
            , attributes := decodeAttributes inner
            -- Pub/Sub sends this only when a dead-letter policy is configured.
            , receiveCount := nat? entry "deliveryAttempt" })
  | _, _ => .ok none

/-- A consumer pulling from a Pub/Sub subscription.

    A `Consumer` cannot be built without naming a subscription — which is the
    point of the split, since on Pub/Sub there is no way to read from a topic
    directly. -/
def consumerAt (t : Transport) (token project subscription : String) : Consumer :=
  let path := subscriptionPath project subscription
  { source := s!"pubsub://{project}/subscriptions/{subscription}"
  , receive := fun params => do
      -- `waitSeconds` and `leaseSeconds` have no per-pull equivalent and are
      -- deliberately not sent; see the module header.
      match ← invoke t token Gcp.pubSubHost "POST" (action path "pull") []
          (some (.object [("maxMessages", .number (Float.ofNat params.maxMessages))])) with
      | .error e => return .error e
      | .ok v    =>
        let entries := array v "receivedMessages"
        return entries.foldl (init := .ok []) fun acc entry =>
          match acc with
          | .error e => .error e
          | .ok got =>
            match messageOf entry with
            | .error e      => .error e
            | .ok none      => .ok got
            | .ok (some m)  => .ok (got ++ [m])
  , ack := fun receipts => do
      if receipts.isEmpty then return .ok ()
      let ids := receipts.map (fun r => Value.string r.handle)
      match ← invoke t token Gcp.pubSubHost "POST" (action path "acknowledge") []
          (some (.object [("ackIds", .array ids.toArray)])) with
      | .error e => return .error e
      | .ok _    => return .ok ()
  , extendLease := fun receipts seconds => do
      if receipts.isEmpty then return .ok ()
      let ids := receipts.map (fun r => Value.string r.handle)
      match ← invoke t token Gcp.pubSubHost "POST" (action path "modifyAckDeadline") []
          (some (.object
            [ ("ackIds", .array ids.toArray)
            , ("ackDeadlineSeconds", .number (Float.ofNat seconds)) ])) with
      | .error e => return .error e
      | .ok _    => return .ok ()
  , purge := do
      -- `subscriptions.seek` to now might do this, and might not be permitted
      -- on an ordinary subscription. Unverified, so this reports the gap
      -- rather than issuing a call whose effect is not known. See the module
      -- header and `Provider.supports .queuePurge`.
      return .error (Error.unsupported "gcp"
        "purging a subscription (seek-to-now is unverified; delete and recreate instead)") }

-- ── Constructors ────────────────────────────────────────────────────────────

/-- A producer for a topic, taking the project and token from credentials. -/
def producer (t : Transport) (creds : Credentials) (topic : String) :
    Except Error Producer := do
  let token ← creds.requireToken .gcp
  let project ← creds.requireProject
  .ok (producerAt t token project topic)

/-- A consumer for a subscription, taking the project and token from
    credentials.

    The subscription is named explicitly and has no default: on Pub/Sub there
    is no "the subscription for this topic", and guessing one would read
    somebody else's backlog. -/
def consumer (t : Transport) (creds : Credentials) (subscription : String) :
    Except Error Consumer := do
  let token ← creds.requireToken .gcp
  let project ← creds.requireProject
  .ok (consumerAt t token project subscription)

/-- Both halves, given **both** names.

    Two arguments rather than one, because a topic and a subscription are
    different objects and only the caller knows which subscription it should be
    reading. -/
def of (t : Transport) (creds : Credentials) (topic subscription : String) :
    Except Error Queue := do
  let token ← creds.requireToken .gcp
  let project ← creds.requireProject
  .ok { producer := producerAt t token project topic
      , consumer := consumerAt t token project subscription }

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard topicPath "typednotes" "jobs" == "/v1/projects/typednotes/topics/jobs"
#guard subscriptionPath "typednotes" "jobs-worker"
  == "/v1/projects/typednotes/subscriptions/jobs-worker"

#guard action (topicPath "p" "jobs") "publish" == "/v1/projects/p/topics/jobs:publish"
#guard action (subscriptionPath "p" "s") "pull" == "/v1/projects/p/subscriptions/s:pull"

-- A topic path and a subscription path are different resources, which is the
-- fact the whole module is arranged around.
#guard topicPath "p" "jobs" != subscriptionPath "p" "jobs"

#guard Data.Json.Encode.encode (encodeAttributes [("origin", "batch-7")])
  == "{\"origin\":\"batch-7\"}"

end Cloud.Queue.PubSub
