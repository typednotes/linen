/-
  Tests for `Cloud.Queue` and its three backends.

  The in-memory backend is tested hardest, because it is the one that will
  actually be run locally and therefore the one whose lies would be believed.
  The properties below are the ones that separate a plausible local queue from
  a faithful one: a received message becomes invisible, a released one comes
  back, delivery counts rise, and a receipt is not a message id.

  The two cloud backends are driven through stub transports, which pins the
  wire shapes — the SQS batch envelope and Pub/Sub's nested, base64 one — that
  cannot otherwise be checked without an account.
-/
import Linen.Cloud.Queue.Sqs
import Linen.Cloud.Queue.PubSub

open Cloud
open Network.HTTP.Types (status200 status400)

namespace Tests.Cloud.Queue

-- ── The in-memory backend ───────────────────────────────────────────────────

/-- info: (["a", "b"], ["msg-0", "msg-1"]) -/
#guard_msgs in
#eval show IO (List String × List String) from do
  let q ← Queue.inMemory
  let ids ← q.producer.send [{ body := "a" }, { body := "b" }]
  let got ← q.consumer.receive { maxMessages := 10 }
  return ((got.toOption.getD []).map (·.body), ids.toOption.getD [])

/- FIFO, oldest first, as a standard SQS queue is in practice. -/
/-- info: ["first", "second", "third"] -/
#guard_msgs in
#eval show IO (List String) from do
  let q ← Queue.inMemory
  let _ ← q.producer.sendOne "first"
  let _ ← q.producer.sendOne "second"
  let _ ← q.producer.sendOne "third"
  let got ← q.consumer.receive { maxMessages := 10 }
  return ((got.toOption.getD []).map (·.body))

/- **A received message is invisible until acknowledged or released.** A
   consumer that forgets to `ack` sees the backlog stop moving — the same
   symptom as in production, rather than the message being handed out twice. -/
/-- info: (some "work", none, 0, 1) -/
#guard_msgs in
#eval show IO (Option String × Option String × Nat × Nat) from do
  let (q, depth) ← Queue.inMemoryInspectable
  let _ ← q.producer.sendOne "work"
  let first ← q.consumer.receiveOne
  let second ← q.consumer.receiveOne
  let (waiting, inFlight) ← depth
  return ( (first.toOption.getD none).map (·.body)
         , (second.toOption.getD none).map (·.body)
         , waiting, inFlight )

/- Acknowledging removes it for good. -/
/-- info: (0, 0, none) -/
#guard_msgs in
#eval show IO (Nat × Nat × Option String) from do
  let (q, depth) ← Queue.inMemoryInspectable
  let _ ← q.producer.sendOne "work"
  match ← q.consumer.receiveOne with
  | .ok (some m) => do
    let _ ← q.consumer.ackOne m
    let (w, f) ← depth
    let again ← q.consumer.receiveOne
    return (w, f, (again.toOption.getD none).map (·.body))
  | _ => return (99, 99, none)

/- **Releasing returns it to the front**, which is how both real systems spell
   a negative acknowledgement — there is no separate nack on either. -/
/-- info: (some "work", 1, 0) -/
#guard_msgs in
#eval show IO (Option String × Nat × Nat) from do
  let (q, depth) ← Queue.inMemoryInspectable
  let _ ← q.producer.sendOne "work"
  match ← q.consumer.receiveOne with
  | .ok (some m) => do
    let _ ← q.consumer.release m
    let (w, f) ← depth
    let again ← q.consumer.receiveOne
    return ((again.toOption.getD none).map (·.body), w, f)
  | _ => return (none, 99, 99)

/- **Delivery counts rise across redeliveries**, so poison-message handling can
   be exercised locally rather than only in production. -/
/-- info: [some 1, some 2, some 3] -/
#guard_msgs in
#eval show IO (List (Option Nat)) from do
  let q ← Queue.inMemory
  let _ ← q.producer.sendOne "poison"
  let mut counts := []
  for _ in [0, 1, 2] do
    match ← q.consumer.receiveOne with
    | .ok (some m) => do
      counts := counts ++ [m.receiveCount]
      let _ ← q.consumer.release m
    | _ => pure ()
  return counts

/- **A receipt is not a message id**, and a receipt from an earlier delivery is
   not reusable — code that confuses the two fails here as it would against a
   real provider. -/
/-- info: (true, false) -/
#guard_msgs in
#eval show IO (Bool × Bool) from do
  let q ← Queue.inMemory
  let _ ← q.producer.sendOne "x"
  match ← q.consumer.receiveOne with
  | .ok (some first) => do
    let _ ← q.consumer.release first
    match ← q.consumer.receiveOne with
    | .ok (some second) =>
      return (first.id == second.id, first.receipt == second.receipt)
    | _ => return (false, true)
  | _ => return (false, true)

/- `maxMessages` is a maximum, and asking for more than is there is not an
   error. -/
/-- info: 2 -/
#guard_msgs in
#eval show IO Nat from do
  let q ← Queue.inMemory
  let _ ← q.producer.send [{ body := "a" }, { body := "b" }]
  let got ← q.consumer.receive { maxMessages := 10 }
  return (got.toOption.getD []).length

/- Attributes travel with the body. -/
/-- info: [("origin", "batch-7")] -/
#guard_msgs in
#eval show IO (List (String × String)) from do
  let q ← Queue.inMemory
  let _ ← q.producer.sendOne "x" [("origin", "batch-7")]
  match ← q.consumer.receiveOne with
  | .ok (some m) => return m.attributes
  | _ => return []

/-- info: (0, 0) -/
#guard_msgs in
#eval show IO (Nat × Nat) from do
  let (q, depth) ← Queue.inMemoryInspectable
  let _ ← q.producer.send [{ body := "a" }, { body := "b" }]
  let _ ← q.consumer.receiveOne
  let _ ← q.consumer.purge
  depth

-- ── `withMessage` ───────────────────────────────────────────────────────────

/- On success the message is acknowledged, so the queue drains. -/
/-- info: (some 4, 0, 0) -/
#guard_msgs in
#eval show IO (Option Nat × Nat × Nat) from do
  let (q, depth) ← Queue.inMemoryInspectable
  let _ ← q.producer.sendOne "work"
  let r ← q.consumer.withMessage (fun m => return .ok m.body.length)
  let (w, f) ← depth
  return (r.toOption.getD none, w, f)

/- **On failure the message is released rather than left to time out**, so a
   transient error is retried promptly instead of after the lease expires. The
   message is back in `waiting`, not stuck in flight. -/
/-- info: (Cloud.Class.transport, 1, 0) -/
#guard_msgs in
#eval show IO (Class × Nat × Nat) from do
  let (q, depth) ← Queue.inMemoryInspectable
  let _ ← q.producer.sendOne "work"
  let r ← q.consumer.withMessage
    (fun _ => (return .error (Error.transport "downstream down") : IO (Except Error Unit)))
  let (w, f) ← depth
  let klass := match r with | .error e => e.klass | .ok _ => Class.notFound
  return (klass, w, f)

/- An empty queue is `none`, not an error — an idle consumer must not loop on
   failures. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let q ← Queue.inMemory
  match ← q.consumer.withMessage (fun _ => return .ok ()) with
  | .ok none => return true
  | _ => return false

-- ── The SQS backend ─────────────────────────────────────────────────────────

def creds : Credentials :=
  { accessKey := "AKIDEXAMPLE"
  , secretKey := "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
  , region := "eu-west-3" }

def ep : Endpoint :=
  { host := "sqs.eu-west-3.amazonaws.com", service := "sqs", region := "eu-west-3" }

def url : String := "https://sqs.eu-west-3.amazonaws.com/123456789012/jobs"

/-- A stub recording the operation and request body, answering with `body`. -/
def wire (log : IO.Ref (List String)) (body : String)
    (st : Network.HTTP.Types.Status := status200) : Transport :=
  Transport.stub fun req => do
    let target := (req.headers.find? (fun h => h.1 == Data.CI.mk' "X-Amz-Target")).map (·.2)
    let sent := (req.body.bind String.fromUTF8?).getD ""
    log.modify (fun l => l ++ [s!"{target.getD "-"} {sent}"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/- The queue URL is resolved once, by name. -/
/-- info: (["AmazonSQS.GetQueueUrl {\"QueueName\":\"jobs\"}"], "https://sqs.eu-west-3.amazonaws.com/123456789012/jobs") -/
#guard_msgs in
#eval show IO (List String × String) from do
  let log ← IO.mkRef []
  let reply := "{\"QueueUrl\":\"" ++ url ++ "\"}"
  match ← Queue.Sqs.queueUrl (wire log reply) creds ep "jobs" with
  | .ok u    => return (← log.get, u)
  | .error e => return (← log.get, toString e)

/- **Sending always uses the batch form**, even for one message, so partial
   failure is handled on one code path rather than discovered later. -/
/--
info: (["AmazonSQS.SendMessageBatch {\"QueueUrl\":\"https:\\/\\/sqs.eu-west-3.amazonaws.com\\/123456789012\\/jobs\",\"Entries\":[{\"Id\":\"e0\",\"MessageBody\":\"work\"}]}"],
 ["m-1"])
-/
#guard_msgs in
#eval show IO (List String × List String) from do
  let log ← IO.mkRef []
  let reply := "{\"Successful\":[{\"Id\":\"e0\",\"MessageId\":\"m-1\"}]}"
  let p := Queue.Sqs.producerAt (wire log reply) creds ep url
  match ← p.send [{ body := "work" }] with
  | .ok ids  => return (← log.get, ids)
  | .error _ => return (← log.get, [])

/- **Message ids come back in the caller's order**, matched by entry id rather
   than assumed positional — SQS may reorder `Successful`. -/
/-- info: ["m-a", "m-b", "m-c"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let reply := "{\"Successful\":[" ++
    "{\"Id\":\"e2\",\"MessageId\":\"m-c\"}," ++
    "{\"Id\":\"e0\",\"MessageId\":\"m-a\"}," ++
    "{\"Id\":\"e1\",\"MessageId\":\"m-b\"}]}"
  let p := Queue.Sqs.producerAt (wire log reply) creds ep url
  match ← p.send [{ body := "a" }, { body := "b" }, { body := "c" }] with
  | .ok ids  => return ids
  | .error _ => return []

/- **A batch where every entry failed is an error, not a silent success.**
   Reporting success here would lose messages without a trace. -/
/-- info: (Cloud.Class.invalid, true) -/
#guard_msgs in
#eval show IO (Class × Bool) from do
  let log ← IO.mkRef []
  let reply := "{\"Successful\":[],\"Failed\":[{\"Id\":\"e0\",\"Message\":\"too big\"}]}"
  let p := Queue.Sqs.producerAt (wire log reply) creds ep url
  match ← p.send [{ body := "work" }] with
  | .error e => return (e.klass, (e.message.splitOn "too big").length == 2)
  | .ok _ => return (.notFound, false)

/- Receiving asks for both attribute families, or the delivery count and the
   caller's own attributes are silently missing. -/
/-- info: (some "work", some "AQEBhandle", [("origin", "batch-7")], some 3) -/
#guard_msgs in
#eval show IO (Option String × Option String × List (String × String) × Option Nat) from do
  let log ← IO.mkRef []
  let reply := "{\"Messages\":[{" ++
    "\"MessageId\":\"m-1\",\"ReceiptHandle\":\"AQEBhandle\",\"Body\":\"work\"," ++
    "\"Attributes\":{\"ApproximateReceiveCount\":\"3\"}," ++
    "\"MessageAttributes\":{\"origin\":{\"DataType\":\"String\",\"StringValue\":\"batch-7\"}}" ++
    "}]}"
  let c := Queue.Sqs.consumerAt (wire log reply) creds ep url
  match ← c.receiveOne with
  | .ok (some m) => return (some m.body, some m.receipt.handle, m.attributes, m.receiveCount)
  | _ => return (none, none, [], none)

/-- info: ["AmazonSQS.ReceiveMessage {\"QueueUrl\":\"https:\\/\\/sqs.eu-west-3.amazonaws.com\\/123456789012\\/jobs\",\"MaxNumberOfMessages\":1,\"MessageSystemAttributeNames\":[\"All\"],\"MessageAttributeNames\":[\"All\"],\"WaitTimeSeconds\":20}"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let c := Queue.Sqs.consumerAt (wire log "{}") creds ep url
  let _ ← c.receive { maxMessages := 1, waitSeconds := some 20 }
  log.get

/- An empty queue answers `{}` with no `Messages` field at all — not an empty
   array — so an idle consumer must read that as zero messages. -/
/-- info: 0 -/
#guard_msgs in
#eval show IO Nat from do
  let log ← IO.mkRef []
  let c := Queue.Sqs.consumerAt (wire log "{}") creds ep url
  return ((← c.receive { maxMessages := 10 }).toOption.getD []).length

/- `extendLease … 0` is how a release reaches the wire. -/
/-- info: ["AmazonSQS.ChangeMessageVisibilityBatch {\"QueueUrl\":\"https:\\/\\/sqs.eu-west-3.amazonaws.com\\/123456789012\\/jobs\",\"Entries\":[{\"Id\":\"e0\",\"ReceiptHandle\":\"AQEB\",\"VisibilityTimeout\":0}]}"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let c := Queue.Sqs.consumerAt (wire log "{}") creds ep url
  let _ ← c.extendLease [⟨"AQEB"⟩] 0
  log.get

/-- info: ["AmazonSQS.DeleteMessageBatch {\"QueueUrl\":\"https:\\/\\/sqs.eu-west-3.amazonaws.com\\/123456789012\\/jobs\",\"Entries\":[{\"Id\":\"e0\",\"ReceiptHandle\":\"AQEB\"}]}"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let c := Queue.Sqs.consumerAt (wire log "{}") creds ep url
  let _ ← c.ack [⟨"AQEB"⟩]
  log.get

/- An empty batch issues no request rather than an empty one, which some
   S3/SQS-compatible stores reject. -/
/-- info: [] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let c := Queue.Sqs.consumerAt (wire log "{}") creds ep url
  let _ ← c.ack []
  log.get

/- **GCP is refused, and the message points at the right module.** Pub/Sub is
   not an SQS-compatible endpoint missing a feature; it is a different system. -/
#guard match Cloud.Sqs.endpoint? .gcp "europe-west9" with
  | none => true
  | some _ => false

/-- info: (Cloud.Class.unsupported, true) -/
#guard_msgs in
#eval show IO (Class × Bool) from do
  match ← Queue.Sqs.of Transport.network .gcp { region := "europe-west9" } "jobs" with
  | .error e => return (e.klass, (e.message.splitOn "Cloud.Queue.PubSub").length == 2)
  | .ok _ => return (.notFound, false)

#guard Queue.Sqs.nameOfUrl url == "jobs"
#guard Queue.Sqs.target "PurgeQueue" == "AmazonSQS.PurgeQueue"

-- ── The Pub/Sub backend ─────────────────────────────────────────────────────

/-- A stub recording the path and request body. -/
def gwire (log : IO.Ref (List String)) (body : String)
    (st : Network.HTTP.Types.Status := status200) : Transport :=
  Transport.stub fun req => do
    let sent := (req.body.bind String.fromUTF8?).getD ""
    log.modify (fun l => l ++ [s!"{req.method} {req.path} {sent}"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/- **Bodies are base64 on the wire**, and the operation is a `:verb` suffix on
   the resource path. -/
/-- info: (["POST /v1/projects/typednotes/topics/jobs:publish {\"messages\":[{\"data\":\"d29yaw==\"}]}"], ["1234"]) -/
#guard_msgs in
#eval show IO (List String × List String) from do
  let log ← IO.mkRef []
  let p := Queue.PubSub.producerAt (gwire log "{\"messageIds\":[\"1234\"]}")
    "ya29.tok" "typednotes" "jobs"
  match ← p.send [{ body := "work" }] with
  | .ok ids  => return (← log.get, ids)
  | .error _ => return (← log.get, [])

/- **The reply is nested**: the ack id is on the outer object and the message
   id on the inner one. Reading `messageId` from the outer object — the obvious
   mistake — finds nothing. -/
/-- info: (some "work", some "ack-1", some "m-1", [("origin", "batch-7")]) -/
#guard_msgs in
#eval show IO (Option String × Option String × Option String × List (String × String)) from do
  let log ← IO.mkRef []
  let reply := "{\"receivedMessages\":[{" ++
    "\"ackId\":\"ack-1\"," ++
    "\"message\":{\"messageId\":\"m-1\",\"data\":\"d29yaw==\"," ++
      "\"attributes\":{\"origin\":\"batch-7\"}}}]}"
  let c := Queue.PubSub.consumerAt (gwire log reply) "ya29.tok" "typednotes" "jobs-worker"
  match ← c.receiveOne with
  | .ok (some m) => return (some m.body, some m.receipt.handle, some m.id, m.attributes)
  | _ => return (none, none, none, [])

/- A pull reads from the **subscription** path, never the topic's. -/
/-- info: ["POST /v1/projects/typednotes/subscriptions/jobs-worker:pull {\"maxMessages\":5}"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let c := Queue.PubSub.consumerAt (gwire log "{}") "ya29.tok" "typednotes" "jobs-worker"
  let _ ← c.receive { maxMessages := 5 }
  log.get

/- `waitSeconds` and `leaseSeconds` are **not sent**: neither has a per-pull
   equivalent on Pub/Sub, and inventing one would silently do nothing. The body
   is identical to the call above. -/
/-- info: ["POST /v1/projects/typednotes/subscriptions/jobs-worker:pull {\"maxMessages\":5}"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let c := Queue.PubSub.consumerAt (gwire log "{}") "ya29.tok" "typednotes" "jobs-worker"
  let _ ← c.receive { maxMessages := 5, waitSeconds := some 20, leaseSeconds := some 60 }
  log.get

/-- info: ["POST /v1/projects/typednotes/subscriptions/jobs-worker:acknowledge {\"ackIds\":[\"ack-1\"]}"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let c := Queue.PubSub.consumerAt (gwire log "{}") "ya29.tok" "typednotes" "jobs-worker"
  let _ ← c.ack [⟨"ack-1"⟩]
  log.get

/- `extendLease` does work on Pub/Sub — `modifyAckDeadline` changes the
   deadline of messages already pulled, which is the useful half of the
   feature that has no per-pull form. -/
/-- info: ["POST /v1/projects/typednotes/subscriptions/jobs-worker:modifyAckDeadline {\"ackIds\":[\"ack-1\"],\"ackDeadlineSeconds\":0}"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let c := Queue.PubSub.consumerAt (gwire log "{}") "ya29.tok" "typednotes" "jobs-worker"
  let _ ← c.extendLease [⟨"ack-1"⟩] 0
  log.get

/- **Purge reports the gap rather than issuing a call whose effect is not
   known.** `seek`-to-now is unverified here, and `Provider.supports` says so,
   so a caller can ask before it tries. -/
/-- info: (Cloud.Class.unsupported, []) -/
#guard_msgs in
#eval show IO (Class × List String) from do
  let log ← IO.mkRef []
  let c := Queue.PubSub.consumerAt (gwire log "{}") "ya29.tok" "typednotes" "jobs-worker"
  match ← c.purge with
  | .error e => return (e.klass, ← log.get)
  | .ok _ => return (.notFound, ← log.get)

#guard Provider.gcp.supports .queuePurge == false

/- A body that is not valid base64, or not UTF-8 once decoded, is a `protocol`
   error rather than mojibake. -/
/-- info: Cloud.Class.protocol -/
#guard_msgs in
#eval show IO Class from do
  let log ← IO.mkRef []
  let reply := "{\"receivedMessages\":[{\"ackId\":\"a\"," ++
    "\"message\":{\"messageId\":\"m\",\"data\":\"/w==\"}}]}"
  let c := Queue.PubSub.consumerAt (gwire log reply) "ya29.tok" "p" "s"
  match ← c.receive { maxMessages := 1 } with
  | .error e => return e.klass
  | .ok _ => return .notFound

/- No token, or no project, is `unbound` before a request rather than a
   `denied` after one. -/
#guard match Queue.PubSub.producer Transport.network {} "jobs" with
  | .error e => e.klass == .unbound
  | .ok _ => false

#guard match Queue.PubSub.producer Transport.network { accessToken := some "t" } "jobs" with
  | .error e => e.klass == .unbound
  | .ok _ => false

#guard match Queue.PubSub.of Transport.network
    { accessToken := some "t", projectId := some "p" } "jobs" "jobs-worker" with
  | .ok q => (q.consumer.source.splitOn "jobs-worker").length == 2
  | .error _ => false

-- ── The feature matrix agrees with the backends ─────────────────────────────

#guard Provider.aws.supports .queueLongPoll == true
#guard Provider.gcp.supports .queueLongPoll == false
#guard Provider.gcp.supports .queuePerReceiveLease == false

end Tests.Cloud.Queue
