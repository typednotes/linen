/-
  Tests for `Cloud.Protocol.AwsJson`.

  The replies below are real SQS and Secrets Manager payloads. The one that
  earns its place is `ReceiveMessage`: it is the operation whose reply a client
  most easily mis-reads, and it is where SQS's AWS-JSON **1.0** — as against
  1.1 everywhere else — has to be right.
-/
import Linen.Cloud.Protocol.AwsJson

open Cloud Cloud.Protocol.AwsJson
open Network.HTTP.Types (status200 status400)

namespace Tests.Cloud.Protocol.AwsJson

-- ── Content types ───────────────────────────────────────────────────────────

/- **SQS speaks 1.0; everything else here speaks 1.1.** Sending the wrong one
   is refused rather than negotiated, so the version is pinned per service
   rather than defaulted to whichever was written first. -/
#guard contentType Sqs.jsonVersion == "application/x-amz-json-1.0"
#guard contentType SecretsManager.jsonVersion == "application/x-amz-json-1.1"

-- ── Fixtures ────────────────────────────────────────────────────────────────

def creds : Credentials :=
  { accessKey := "AKIDEXAMPLE"
  , secretKey := "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
  , region := "eu-west-3" }

def sqsEp : Endpoint :=
  { host := "sqs.eu-west-3.amazonaws.com", service := "sqs", region := "eu-west-3" }

def respondWith (log : IO.Ref (List String)) (st : Network.HTTP.Types.Status)
    (body : String) : Transport :=
  Transport.stub fun req => do
    let target := (req.headers.find? (fun h => h.1 == Data.CI.mk' "X-Amz-Target")).map (·.2)
    let ct := (req.headers.find? (fun h => h.1 == Data.CI.mk' "Content-Type")).map (·.2)
    log.modify (fun l => l ++
      [s!"{req.method} {req.path} target={target.getD "-"} type={ct.getD "-"}"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/-- A real `ReceiveMessage` reply: one message, with the receipt handle that
    the eventual `DeleteMessage` needs. -/
def receiveReply : String :=
  "{\"Messages\":[{" ++
    "\"MessageId\":\"c4a1b0d2-3e4f-5a6b-7c8d-9e0f1a2b3c4d\"," ++
    "\"ReceiptHandle\":\"AQEBwJnKyrHigUMZj6rYigCgxlaS3SLy0a\"," ++
    "\"MD5OfBody\":\"5eb63bbbe01eeed093cb22bb8f5acdc3\"," ++
    "\"Body\":\"hello world\"," ++
    "\"Attributes\":{\"ApproximateReceiveCount\":\"1\"}" ++
  "}]}"

-- ── Every operation is a `POST /` with the name in a header ─────────────────

/-- info: ["POST / target=AmazonSQS.ReceiveMessage type=application/x-amz-json-1.0"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let _ ← invoke (respondWith log status200 receiveReply) creds sqsEp
    Sqs.jsonVersion "AmazonSQS.ReceiveMessage"
    (.object [("QueueUrl", .string "https://q"), ("MaxNumberOfMessages", .number 1.0)])
  log.get

/- The path is `/` for every operation, and double-encoded in the signature —
   which makes no observable difference for `/` but is set correctly rather
   than left looking like an oversight. -/
#guard (call creds sqsEp "1.0" "AmazonSQS.ReceiveMessage" .null).path == "/"
#guard (call creds sqsEp "1.0" "AmazonSQS.ReceiveMessage" .null).doubleEncodePath == true

-- ── Reading a reply ─────────────────────────────────────────────────────────

/-- info: (1, some "hello world", some "AQEBwJnKyrHigUMZj6rYigCgxlaS3SLy0a") -/
#guard_msgs in
#eval show IO (Nat × Option String × Option String) from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 receiveReply) creds sqsEp
    Sqs.jsonVersion "AmazonSQS.ReceiveMessage" .null with
  | .error _ => return (0, none, none)
  | .ok v =>
    let msgs := array v "Messages"
    return ( msgs.length
           , msgs.head?.bind (string? · "Body")
           , msgs.head?.bind (string? · "ReceiptHandle") )

/- An empty queue answers `{}` — no `Messages` field at all, rather than an
   empty array. Reading it as an array must therefore yield the empty list, or
   an idle consumer becomes an error loop. -/
/-- info: 0 -/
#guard_msgs in
#eval show IO Nat from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 "{}") creds sqsEp "1.0" "AmazonSQS.ReceiveMessage"
    .null with
  | .ok v => return (array v "Messages").length
  | .error _ => return 99

/- Several operations answer 200 with **no body at all** — `DeleteMessage`
   among them — and for those the absence is the successful answer. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 "") creds sqsEp "1.0" "AmazonSQS.DeleteMessage"
    .null with
  | .ok v => return v.isNull
  | .error _ => return false

-- ── Field accessors ─────────────────────────────────────────────────────────

#guard string? (.object [("QueueUrl", .string "https://q/x")]) "QueueUrl" == some "https://q/x"
#guard string? (.object [("QueueUrl", .string "https://q/x")]) "Missing" == none
#guard nat? (.object [("VisibilityTimeout", .number 30.0)]) "VisibilityTimeout" == some 30

#guard match requireString (.object [("a", .string "b")]) "a" with
  | .ok v => v == "b"
  | .error _ => false

/- A missing required field names itself, rather than defaulting. -/
#guard match requireString (.object []) "QueueUrl" with
  | .error e => e.klass == .protocol && (e.message.splitOn "QueueUrl").length == 2
  | .ok _ => false

-- ── Errors ──────────────────────────────────────────────────────────────────

/- SQS qualifies its `__type`, and only the part after the `#` is the code —
   the case `Cloud.classify` splits for. -/
/-- info: (Cloud.Class.notFound, "com.amazonaws.sqs#QueueDoesNotExist") -/
#guard_msgs in
#eval show IO (Class × String) from do
  let log ← IO.mkRef []
  let body := "{\"__type\":\"com.amazonaws.sqs#QueueDoesNotExist\"," ++
              "\"message\":\"The specified queue does not exist.\"}"
  match ← invoke (respondWith log status400 body) creds sqsEp "1.0" "AmazonSQS.ReceiveMessage"
    .null with
  | .error e => return (e.klass, e.code)
  | .ok _ => return (.protocol, "")

/- A 200 whose body is not JSON is a `protocol` error naming the problem,
   rather than a silently empty result. -/
/-- info: Cloud.Class.protocol -/
#guard_msgs in
#eval show IO Class from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 "<html>gateway</html>") creds sqsEp "1.0" "X" .null with
  | .error e => return e.klass
  | .ok _ => return .notFound

end Tests.Cloud.Protocol.AwsJson
