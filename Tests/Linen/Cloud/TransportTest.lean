/-
  Tests for `Cloud.Transport`.

  Every case runs through a stub transport, so this file exercises the whole
  sign-send-classify path with no sockets and no credentials — which is the
  reason `Transport` is a record with a `send` field rather than a direct call
  to the HTTP client.

  Two properties get the most attention:

  1. **What is signed is what is sent.** The query string is rendered once, and
     the stub sees the fully-built request, so a divergence shows up here rather
     than as `SignatureDoesNotMatch` against a real provider.
  2. **Failure is a value.** A non-2xx becomes a classified error, a socket
     failure becomes `transport`, and neither raises.
-/
import Linen.Cloud.Transport

open Cloud
open Network.HTTP.Client (Request Response)
open Network.HTTP.Types (status200 status404 status403 status503)
open Data.Time (UTCTime)

namespace Tests.Cloud.Transport

-- ── Fixtures ────────────────────────────────────────────────────────────────

def creds : Credentials :=
  { accessKey := "AKIDEXAMPLE"
  , secretKey := "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
  , region := "eu-west-3" }

def fixedTime : UTCTime := UTCTime.ofNanosSinceEpoch (1440938160 * 1000000000)

/-- The S3 endpoint the calls below go to, written out so the fixture needs no
    `Inhabited Endpoint`, and pinned against the table it comes from. -/
def s3 : Endpoint :=
  { host := "s3.eu-west-3.amazonaws.com", service := "s3", region := "eu-west-3" }

#guard S3.endpoint? .aws "eu-west-3" == some s3

/-- A call with a query whose parameters are deliberately out of order and
    contain characters that must be percent-encoded. -/
def listCall : Call :=
  { method := "GET"
  , endpoint := s3
  , path := "/assets"
  , query := [("prefix", some "logs/2026/"), ("list-type", some "2"), ("max-keys", some "10")]
  , auth := Auth.forEndpoint creds s3 }

/-- A transport that records what it was asked to send and answers with a fixed
    status and body. -/
def echo (log : IO.Ref (List String)) (st : Network.HTTP.Types.Status) (body : String) :
    Transport :=
  Transport.stub fun req => do
    log.modify (fun l => l ++
      [s!"{req.method} {req.host}{req.path}{req.queryString} secure={req.isSecure}"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/-- A transport whose socket fails. -/
def broken : Transport :=
  Transport.stub fun _ => throw (IO.userError "connection reset by peer")

-- ── The query is rendered once, canonically ─────────────────────────────────

/- Sorted and percent-encoded by `canonicalQuery`, and the *same* rendering is
   both signed and sent. The `/` in the prefix is encoded, and the parameters
   come back in sorted order rather than the order written above. -/
#guard listCall.queryString == "list-type=2&max-keys=10&prefix=logs%2F2026%2F"

/-- info: ["GET s3.eu-west-3.amazonaws.com/assets?list-type=2&max-keys=10&prefix=logs%2F2026%2F secure=true"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let _ ← perform (echo log status200 "<ListBucketResult/>") listCall fixedTime
  log.get

/- An empty query sends no `?` at all. A bare `?` is legal but changes the
   signed string, and some S3-compatible stores reject it. -/
/-- info: ["GET s3.eu-west-3.amazonaws.com/assets/a.json secure=true"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let _ ← perform (echo log status200 "body") { listCall with path := "/assets/a.json", query := [] }
    fixedTime
  log.get

-- ── The request carries the signature ───────────────────────────────────────

/- The built request has the headers `Auth` produced, `Host` among them, and is
   TLS on port 443 — every one of these services is HTTPS-only. -/
/-- info: (true, true, 443, true) -/
#guard_msgs in
#eval show IO (Bool × Bool × UInt16 × Bool) from do
  let req ← listCall.toRequestAt fixedTime
  let has (n : String) : Bool := req.headers.any (fun h => h.1 == Data.CI.mk' n)
  return (has "Authorization", has "x-amz-date", req.port, req.isSecure)

/- A body is attached when non-empty and omitted when empty, rather than sent
   as a zero-length body — which changes `Content-Length` and therefore the
   request. -/
/-- info: (some 4, none) -/
#guard_msgs in
#eval show IO (Option Nat × Option Nat) from do
  let withBody ← ({ listCall with method := "PUT", body := "data".toUTF8 }).toRequestAt fixedTime
  let without ← listCall.toRequestAt fixedTime
  return (withBody.body.map (·.size), without.body.map (·.size))

-- ── Success ─────────────────────────────────────────────────────────────────

/-- info: (true, "<ListBucketResult/>") -/
#guard_msgs in
#eval show IO (Bool × String) from do
  let log ← IO.mkRef []
  match ← perform (echo log status200 "<ListBucketResult/>") listCall fixedTime with
  | .ok resp => return (isSuccess resp, (bodyText resp).toOption.getD "<undecodable>")
  | .error e => return (false, toString e)

-- ── Failure is a value, and it is classified ────────────────────────────────

/- A 404 with S3's own error document becomes `notFound` carrying the
   provider's code — the whole point of `Cloud.Error`. -/
/-- info: (Cloud.Class.notFound, "NoSuchKey") -/
#guard_msgs in
#eval show IO (Class × String) from do
  let log ← IO.mkRef []
  let body := "<Error><Code>NoSuchKey</Code><Message>The specified key does not exist.</Message></Error>"
  match ← perform (echo log status404 body) listCall fixedTime with
  | .ok _ => return (.protocol, "")
  | .error e => return (e.klass, e.code)

/- A 403 that is really a signing problem does not read as a permissions
   problem: the code says which, and the class follows the code. -/
/-- info: (Cloud.Class.denied, "SignatureDoesNotMatch") -/
#guard_msgs in
#eval show IO (Class × String) from do
  let log ← IO.mkRef []
  let body := "<Error><Code>SignatureDoesNotMatch</Code><Message>nope</Message></Error>"
  match ← perform (echo log status403 body) listCall fixedTime with
  | .ok _ => return (.protocol, "")
  | .error e => return (e.klass, e.code)

/- A throttle is retryable; the caller can see that without knowing the code. -/
/-- info: (Cloud.Class.throttled, true) -/
#guard_msgs in
#eval show IO (Class × Bool) from do
  let log ← IO.mkRef []
  match ← perform (echo log status503 "<Error><Code>SlowDown</Code><Message>x</Message></Error>")
    listCall fixedTime with
  | .ok _ => return (.protocol, false)
  | .error e => return (e.klass, e.retryable)

/- A socket failure is `transport`, not an exception escaping into the
   caller's `IO`. -/
/-- info: (Cloud.Class.transport, true) -/
#guard_msgs in
#eval show IO (Class × Bool) from do
  match ← perform broken listCall fixedTime with
  | .ok _ => return (.protocol, false)
  | .error e => return (e.klass, (e.message.splitOn "connection reset").length == 2)

/- **Incomplete credentials are caught before anything is sent.** The stub
   records nothing, so no half-signed request reached the network. -/
/-- info: (Cloud.Class.denied, []) -/
#guard_msgs in
#eval show IO (Class × List String) from do
  let log ← IO.mkRef []
  let bad := { listCall with auth := Auth.sigV4 { accessKey := "AKIA" } "s3" "eu-west-3" }
  match ← perform (echo log status200 "") bad fixedTime with
  | .ok _ => return (.protocol, ← log.get)
  | .error e => return (e.klass, ← log.get)

-- ── `performRaw` hands back the non-2xx ─────────────────────────────────────

/- For the operations where a specific failure status is the expected answer
   and the caller wants to see it rather than a classified error. -/
/-- info: 404 -/
#guard_msgs in
#eval show IO Nat from do
  let log ← IO.mkRef []
  match ← performRaw (echo log status404 "") listCall fixedTime with
  | .ok resp => return statusOf resp
  | .error _ => return 0

-- ── `absentAsNone`: absence as `none` ──────────────────────────────────────

/- The idiom for "read it if it is there". Everything that is not an absence
   still propagates, so a permissions problem is never silently a `none`. -/
#guard match absentAsNone (α := Nat) (.ok 3) with
  | .ok (some 3) => true
  | _ => false

#guard match absentAsNone (α := Nat) (.error { klass := .notFound }) with
  | .ok none => true
  | _ => false

#guard match absentAsNone (α := Nat) (.error { klass := .denied }) with
  | .error e => e.klass == .denied
  | .ok _ => false

-- ── Reading bodies ──────────────────────────────────────────────────────────

/- A body that is not UTF-8 is a value the caller handles, not a panic — the
   correction over `String.fromUTF8!`. -/
#guard match bodyText { statusCode := status200, headers := [], body := ⟨#[0xff, 0xfe]⟩ } with
  | .error e => e.klass == .protocol
  | .ok _ => false

/- For an *error* body, refusing to decode would discard the only diagnostic
   available, so that path is lossy on purpose. -/
#guard bodyTextLossy { statusCode := status200, headers := [], body := "ok".toUTF8 } == "ok"

#guard statusOf { statusCode := status404, headers := [], body := ByteArray.empty } == 404
#guard isSuccess { statusCode := status200, headers := [], body := ByteArray.empty }
#guard !isSuccess { statusCode := status404, headers := [], body := ByteArray.empty }

-- ── Presigned URLs are not re-rendered ──────────────────────────────────────

/- **The query passes through untouched.** A presigned URL's signature covers
   the exact encoded string in the exact order it arrived; `canonicalQuery`
   would sort and re-encode it, producing a request the issuer refuses with an
   error that says nothing about why. Note the parameters stay in the given
   order and `%2F` is not doubly encoded. -/
/-- info: ["GET s3.example.test/b/k?X-Amz-Signature=abc&X-Amz-Expires=900&prefix=a%2Fb secure=true"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let _ ← performPresigned (echo log status200 "x") "GET" "s3.example.test" "/b/k"
    "X-Amz-Signature=abc&X-Amz-Expires=900&prefix=a%2Fb"
  log.get

-- ── The recording transport ─────────────────────────────────────────────────

/- For debugging a client against a real provider: what went out, in order. -/
/-- info: ["GET s3.eu-west-3.amazonaws.com/assets?list-type=2&max-keys=10&prefix=logs%2F2026%2F"] -/
#guard_msgs in
#eval show IO (List String) from do
  let sent ← IO.mkRef []
  let inner ← IO.mkRef []
  let t := Transport.recording sent (echo inner status200 "x")
  let _ ← perform t listCall fixedTime
  sent.get

-- ── Policy ──────────────────────────────────────────────────────────────────

/- More patient than `linen`'s default, because a cloud service throttling a
   burst is normal rather than exceptional. -/
#guard retryPolicy.maxAttempts == 5
#guard timeoutMillis == 30000

-- ── The path: signed one way, sent another ──────────────────────────────────

/- `Call.path` is unencoded, and both the signature and the wire are derived
   from it, so a key with characters needing escapes works without the caller
   doing anything. Here a space and a `#` — the latter would otherwise
   truncate the request line at the fragment. -/
#guard ({ listCall with path := "/assets/my report #2.json" }).wirePath
  == "/assets/my%20report%20%232.json"

/- The wire path is single-encoded **even for a service that double-encodes it
   in the signature**, because that is AWS's actual rule. Sending what was
   signed would 404. -/
#guard ({ listCall with path := "/a b", doubleEncodePath := true }).wirePath == "/a%20b"
#guard Crypto.SigV4.canonicalUri "/a b" true == "/a%2520b"

/- Ordinary paths are unchanged, so nothing is gratuitously escaped: `-`, `.`,
   `_` and `~` are unreserved. -/
#guard ({ listCall with path := "/assets/logs-2026/a_b.c~d" }).wirePath
  == "/assets/logs-2026/a_b.c~d"
#guard ({ listCall with path := "/" }).wirePath == "/"

/-- info: ["GET s3.eu-west-3.amazonaws.com/assets/my%20report.json secure=true"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let _ ← perform (echo log status200 "x")
    { listCall with path := "/assets/my report.json", query := [] } fixedTime
  log.get

end Tests.Cloud.Transport
