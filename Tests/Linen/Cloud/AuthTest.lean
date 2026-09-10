/-
  Tests for `Cloud.Auth`.

  The important test here is that wrapping `Crypto.SigV4` **does not disturb the
  signature**: the same credentials, time and request that AWS's published
  worked example covers must produce the same `Authorization` header through
  `Auth.headersAt` as through `Crypto.SigV4.sign` directly. That is asserted
  against the literal expected header, so a refactor that quietly changed the
  header set, their order, or the `Host` handling would fail here rather than in
  production as `SignatureDoesNotMatch`.

  `AKIDEXAMPLE` / `wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY` are AWS's own
  documentation credentials and authenticate to nothing.
-/
import Linen.Cloud.Auth

open Cloud
open Data.Time (UTCTime)

namespace Tests.Cloud.Auth

-- ── AWS's published vector ──────────────────────────────────────────────────

/-- AWS's documentation key pair. Authenticates to nothing. -/
def exampleCreds : Credentials :=
  { accessKey := "AKIDEXAMPLE"
  , secretKey := "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
  , region := "us-east-1" }

/-- 2015-08-30T12:36:00Z, the instant the worked example signs at. Passed in
    rather than read from the clock, which is why this test is possible. -/
def exampleTime : UTCTime := UTCTime.ofNanosSinceEpoch (1440938160 * 1000000000)

/-- The IAM endpoint the vector uses. Global, hence `us-east-1` regardless of
    where the caller is. -/
def iamEndpoint : Endpoint :=
  { host := "iam.amazonaws.com", service := "iam", region := "us-east-1" }

-- ── SigV4 through `Auth` matches SigV4 directly ─────────────────────────────

/- **The signature is unchanged by going through `Auth`.** The expected value is
   AWS's, and it also appears in `Tests.Crypto.SigV4`; if these two ever
   disagree, this module has broken the signer rather than wrapped it. -/
/-- info: "AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, SignedHeaders=content-type;host;x-amz-content-sha256;x-amz-date, Signature=dd479fa8a80364edf2119ec24bebde66712ee9c9cb2b0d92eb3ab9ccdc0c3947" -/
#guard_msgs in
#eval show IO String from do
  let hdrs ← (Auth.forEndpoint exampleCreds iamEndpoint).headersAt exampleTime
    iamEndpoint.host "GET" "/"
    (query := [("Action", some "ListUsers"), ("Version", some "2010-05-08")])
    (headers := [("Content-Type", "application/x-www-form-urlencoded; charset=utf-8")])
  return (hdrs.find? (·.1 == "Authorization")).map (·.2) |>.getD "<missing>"

/- `Host` comes back with the signature rather than being left for the HTTP
   client to add, because SigV4 signs it. A transport that added it afterwards
   would send a request whose signature covered a different header set. -/
/-- info: some "iam.amazonaws.com" -/
#guard_msgs in
#eval show IO (Option String) from do
  let hdrs ← (Auth.forEndpoint exampleCreds iamEndpoint).headersAt exampleTime
    iamEndpoint.host "GET" "/"
  return (hdrs.find? (·.1 == "Host")).map (·.2)

/- The date and payload-hash headers are part of what is signed, so they are
   returned to be sent. -/
/-- info: (some "20150830T123600Z", true) -/
#guard_msgs in
#eval show IO (Option String × Bool) from do
  let hdrs ← (Auth.forEndpoint exampleCreds iamEndpoint).headersAt exampleTime
    iamEndpoint.host "GET" "/"
  return ( (hdrs.find? (·.1 == "x-amz-date")).map (·.2)
         , (hdrs.find? (·.1 == "x-amz-content-sha256")).map (·.2)
             == some Crypto.SigV4.emptyPayloadHash )

/- No session token means no `x-amz-security-token` header. A stray empty one
   would be signed and then rejected as tampering. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let hdrs ← (Auth.forEndpoint exampleCreds iamEndpoint).headersAt exampleTime
    iamEndpoint.host "GET" "/"
  return hdrs.all (·.1 != "x-amz-security-token")

/- Temporary credentials carry the token, and it is signed — the service
   refuses an unsigned one as tampering. -/
/-- info: (true, true) -/
#guard_msgs in
#eval show IO (Bool × Bool) from do
  let temp := { exampleCreds with sessionToken := some "FwoGZXIvYXdzEBYaDHNlY3JldA==" }
  let hdrs ← (Auth.forEndpoint temp iamEndpoint).headersAt exampleTime
    iamEndpoint.host "GET" "/"
  let auth := (hdrs.find? (·.1 == "Authorization")).map (·.2) |>.getD ""
  return ( (hdrs.find? (·.1 == "x-amz-security-token")).isSome
         , (auth.splitOn "x-amz-security-token").length == 2 )

/- A different signing region gives a different signature, which is what makes
   `Endpoint.region` load-bearing rather than decorative. -/
/-- info: false -/
#guard_msgs in
#eval show IO Bool from do
  let a ← (Auth.sigV4 exampleCreds "iam" "us-east-1").headersAt exampleTime
    iamEndpoint.host "GET" "/"
  let b ← (Auth.sigV4 exampleCreds "iam" "eu-west-3").headersAt exampleTime
    iamEndpoint.host "GET" "/"
  return (a.find? (·.1 == "Authorization")) == (b.find? (·.1 == "Authorization"))

-- ── The two bearer schemes ──────────────────────────────────────────────────

/-- info: [("Host", "secretmanager.googleapis.com"), ("Authorization", "Bearer ya29.token")] -/
#guard_msgs in
#eval show IO (List (String × String)) from do
  (Auth.bearer "ya29.token").headersAt exampleTime "secretmanager.googleapis.com" "GET" "/v1/x"

/-- info: [("Host", "api.scaleway.com"), ("X-Auth-Token", "scw-secret")] -/
#guard_msgs in
#eval show IO (List (String × String)) from do
  (Auth.authToken "scw-secret").headersAt exampleTime "api.scaleway.com" "GET" "/x"

/- Anonymous still sends `Host`: it is required by HTTP/1.1, not by the
   credential scheme. -/
/-- info: [("Host", "storage.googleapis.com")] -/
#guard_msgs in
#eval show IO (List (String × String)) from do
  Auth.anonymous.headersAt exampleTime "storage.googleapis.com" "GET" "/x"

-- ── The native scheme per cloud ─────────────────────────────────────────────

/- Scaleway's *own* API is header-authenticated while its S3 and SQS endpoints
   are SigV4 — the reason `Auth` travels with an `Endpoint` rather than being
   derived from a `Provider`. -/
#guard match Auth.native .scaleway { secretKey := "scw-secret" } with
  | .ok (.authToken t) => t == "scw-secret"
  | _ => false

#guard match Auth.native .gcp { accessToken := some "ya29.tok" } with
  | .ok (.bearer t) => t == "ya29.tok"
  | _ => false

/- GCP with no token is a clear failure here rather than a `denied` from the
   provider on the first request. -/
#guard match Auth.native .gcp {} with
  | .error e => e.klass == .unbound
  | .ok _ => false

-- ── Usability, checked before a request is built ────────────────────────────

#guard (Auth.forEndpoint exampleCreds iamEndpoint).usable == true
#guard (Auth.bearer "ya29.tok").usable == true
#guard Auth.anonymous.usable == true

/- Half a key pair cannot sign. Saying so before the request goes out beats a
   `SignatureDoesNotMatch` that names nothing. -/
#guard (Auth.sigV4 { accessKey := "AKIA" } "s3" "eu-west-3").usable == false
#guard (Auth.sigV4 {} "s3" "eu-west-3").usable == false
#guard (Auth.bearer "").usable == false
#guard (Auth.authToken "").usable == false

-- ── Nothing renders a credential ────────────────────────────────────────────

/- The scheme is named; the secret never is. `Auth` values end up in
   diagnostics, so this is the difference between a debuggable log and a
   leaked key. -/
#guard toString (Auth.forEndpoint exampleCreds iamEndpoint) == "sigv4(iam/us-east-1)"
#guard toString (Auth.bearer "ya29.super-secret") == "bearer"
#guard toString (Auth.authToken "scw-secret") == "x-auth-token"
#guard toString Auth.anonymous == "anonymous"

#guard ((toString (repr (Auth.bearer "ya29.super-secret"))).splitOn "super-secret").length == 1
#guard ((toString (repr (Auth.authToken "scw-secret"))).splitOn "scw-secret").length == 1
#guard ((toString (repr (Auth.forEndpoint exampleCreds iamEndpoint))).splitOn
  "wJalrXUtnFEMI").length == 1

end Tests.Cloud.Auth
