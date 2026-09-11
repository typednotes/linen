/-
  Tests for `Cloud.Error`.

  The response bodies below are the real shapes the three clouds send, one per
  wire dialect, kept verbatim rather than minimised: a parser tested only
  against its own idea of the format is tested against nothing. The `<Error>`
  document carries the sibling elements S3 actually includes (`Key`, `HostId`),
  and Google's carries the numeric `code` alongside the `status` string, because
  those are the parts a hand-written parser gets wrong.

  The classification tests are arranged around the one asymmetry that matters:
  mistaking a permission failure for an absence makes a caller try to create
  something that already exists, so `denied` codes are pinned as *not*
  `notFound` explicitly.
-/
import Linen.Cloud.Error

open Cloud

namespace Tests.Cloud.Error

-- ── Captured error bodies ───────────────────────────────────────────────────

/-- S3, `GET` on a key that is not there. Real shape, including the siblings
    S3 sends alongside `Code`/`Message`. -/
def s3NoSuchKey : String :=
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  "<Error><Code>NoSuchKey</Code>" ++
  "<Message>The specified key does not exist.</Message>" ++
  "<Key>logs/2026/a.json</Key>" ++
  "<RequestId>656c76696e6727732072657175657374</RequestId>" ++
  "<HostId>Uuag1LuByRx9e6j5Onimru9pO4ZVKnJ2Qz7/C1NPcfTWAtRPfTaOFg==</HostId></Error>"

/-- S3 refusing a signature. The status is the same `403` an IAM policy denial
    gives, which is why the code rather than the status is what classifies. -/
def s3SignatureMismatch : String :=
  "<Error><Code>SignatureDoesNotMatch</Code>" ++
  "<Message>The request signature we calculated does not match the signature " ++
  "you provided. Check your key and signing method.</Message>" ++
  "<RequestID>1F2A3B4C5D6E7F80</RequestID></Error>"

/-- AWS-JSON 1.1, Secrets Manager. -/
def secretsManagerNotFound : String :=
  "{\"__type\":\"ResourceNotFoundException\"," ++
  "\"message\":\"Secrets Manager can't find the specified secret.\"}"

/-- AWS-JSON 1.0, SQS. The `__type` is *qualified*, and only the part after the
    `#` is the code — the case `classify`'s `bare` split exists for. -/
def sqsNoSuchQueue : String :=
  "{\"__type\":\"com.amazonaws.sqs#QueueDoesNotExist\"," ++
  "\"message\":\"The specified queue does not exist.\"}"

/-- Scaleway, which spells the code `type` and uses lowercase snake case. -/
def scalewayNotFound : String :=
  "{\"message\":\"resource is not found\",\"type\":\"not_found\"," ++
  "\"resource\":\"secret\"}"

/-- Google, which nests the whole thing one level down. Reading only the flat
    shape yields an empty message here, discarding the only useful part. -/
def googleNotFound : String :=
  "{\"error\":{\"code\":404," ++
  "\"message\":\"Secret [projects/typednotes/secrets/db-password] not found.\"," ++
  "\"status\":\"NOT_FOUND\"}}"

/-- Google denying for a missing IAM permission. -/
def googleDenied : String :=
  "{\"error\":{\"code\":403," ++
  "\"message\":\"Permission 'secretmanager.versions.access' denied.\"," ++
  "\"status\":\"PERMISSION_DENIED\"}}"

-- ── The XML dialect ─────────────────────────────────────────────────────────

#guard (parseXmlError s3NoSuchKey).map (·.1) == some "NoSuchKey"
#guard (parseXmlError s3NoSuchKey).map (·.2.1) == some "The specified key does not exist."
#guard (parseXmlError s3NoSuchKey).bind (·.2.2) == some "656c76696e6727732072657175657374"

/- AWS spells the request id both ways; both are read. -/
#guard (parseXmlError s3SignatureMismatch).bind (·.2.2) == some "1F2A3B4C5D6E7F80"

/- A body that is not XML at all is `none`, not a crash and not a bogus code. -/
#guard (parseXmlError "not xml").isNone
#guard (parseXmlError "").isNone

-- ── The JSON dialects ───────────────────────────────────────────────────────

#guard parseJsonError secretsManagerNotFound
  == some ("ResourceNotFoundException", "Secrets Manager can't find the specified secret.")

#guard (parseJsonError sqsNoSuchQueue).map (·.1) == some "com.amazonaws.sqs#QueueDoesNotExist"

#guard parseJsonError scalewayNotFound == some ("not_found", "resource is not found")

/- Google's nesting. The message is the point: without reading through `error`
   this would be `("", "")` and every GCP failure would be undiagnosable. -/
#guard (parseJsonError googleNotFound).map (·.1) == some "NOT_FOUND"
#guard (parseJsonError googleNotFound).map (·.2)
  == some "Secret [projects/typednotes/secrets/db-password] not found."

#guard (parseJsonError googleDenied).map (·.1) == some "PERMISSION_DENIED"

#guard (parseJsonError "{}").isSome
#guard (parseJsonError "nonsense").isNone

-- ── Classification: the code decides ────────────────────────────────────────

#guard classify 404 "NoSuchKey" == .notFound
#guard classify 404 "NoSuchBucket" == .notFound
#guard classify 400 "QueueDoesNotExist" == .notFound
#guard classify 400 "com.amazonaws.sqs#QueueDoesNotExist" == .notFound
#guard classify 404 "not_found" == .notFound
#guard classify 404 "NOT_FOUND" == .notFound

#guard classify 403 "SignatureDoesNotMatch" == .denied
#guard classify 403 "AccessDenied" == .denied
#guard classify 403 "PERMISSION_DENIED" == .denied
#guard classify 400 "ExpiredTokenException" == .denied

#guard classify 409 "BucketAlreadyOwnedByYou" == .conflict
#guard classify 400 "QueueAlreadyExists" == .conflict
#guard classify 409 "ALREADY_EXISTS" == .conflict

#guard classify 503 "SlowDown" == .throttled
#guard classify 400 "ThrottlingException" == .throttled
#guard classify 429 "RESOURCE_EXHAUSTED" == .throttled

#guard classify 400 "MalformedXML" == .invalid
#guard classify 400 "INVALID_ARGUMENT" == .invalid

/- **The asymmetry that matters.** A permission failure must never classify as
   an absence: a caller that reads `notFound` concludes the resource is not
   there and creates it, which for a bucket it does not own is a hard failure
   and for one it does is data loss waiting to happen. Pinned over the whole
   `denied` list rather than by example.

   Asserted with `#guard` rather than as a `theorem … := by decide`, unlike the
   scope-escape statements in the capability-effect tests. The obligation is
   decidable but not *kernel*-decidable: `classify` splits the code on `#` to
   strip AWS's qualified `__type`, and `String.splitOn` does not reduce in the
   kernel (it gets stuck on the slice representation) — the same wall
   `String.startsWith` hits. `#guard` runs the compiled function instead, which
   is the honest way to check a fact about eighty string literals. -/
#guard deniedCodes.all (fun c => classify 403 c != Class.notFound)

/- And the converse: nothing in the not-found list classifies as denied, so a
   genuine absence is not reported as a permissions problem either. -/
#guard notFoundCodes.all (fun c => classify 404 c == Class.notFound)

/- The code lists do not overlap. Two lists claiming the same code would make
   classification depend on the order of the `if`s in `classify`, which is not
   a property anyone should have to know. -/
#guard (notFoundCodes ++ deniedCodes ++ conflictCodes ++ throttledCodes
        ++ invalidCodes).eraseDups.length
  == (notFoundCodes.length + deniedCodes.length + conflictCodes.length
      + throttledCodes.length + invalidCodes.length)

-- ── Classification: the status as fallback ──────────────────────────────────

#guard classify 404 "" == .notFound
#guard classify 403 "" == .denied
#guard classify 401 "" == .denied
#guard classify 409 "" == .conflict
#guard classify 429 "" == .throttled
#guard classify 500 "" == .server
#guard classify 503 "" == .server
#guard classify 400 "" == .invalid
#guard classify 0 "" == .protocol

/- An unknown code on an unremarkable status falls through to the status, so a
   provider inventing a new code still classifies sensibly. -/
#guard classify 404 "SomeCodeInventedTomorrow" == .notFound
#guard classify 500 "SomeCodeInventedTomorrow" == .server

-- ── `describeError` end to end ───────────────────────────────────────────────────

#guard (describeError 404 s3NoSuchKey).klass == .notFound
#guard (describeError 404 s3NoSuchKey).code == "NoSuchKey"
#guard (describeError 404 s3NoSuchKey).requestId == some "656c76696e6727732072657175657374"

#guard (describeError 403 s3SignatureMismatch).klass == .denied
#guard (describeError 400 sqsNoSuchQueue).klass == .notFound
#guard (describeError 404 scalewayNotFound).klass == .notFound
#guard (describeError 404 googleNotFound).klass == .notFound
#guard (describeError 403 googleDenied).klass == .denied

/- An unrecognised dialect keeps the body rather than discarding it — the thing
   that makes a provider integration undebuggable. -/
#guard (describeError 502 "<html><body>Bad Gateway</body></html>").klass == .server
#guard (describeError 502 "upstream connect error").message == "upstream connect error"
#guard (describeError 500 "").message == "(empty response body)"

/- A very long unintelligible body is truncated rather than dropped. -/
#guard (describeError 500 (String.ofList (List.replicate 900 'x'))).message.length
  == bodyExcerptLimit + 1

-- ── Retryability ────────────────────────────────────────────────────────────

#guard Class.throttled.retryable == true
#guard Class.server.retryable == true
#guard Class.transport.retryable == true
#guard Class.denied.retryable == false
#guard Class.invalid.retryable == false
#guard Class.unsupported.retryable == false

/- `notFound` is deliberately not retryable: a caller waiting out eventual
   consistency should say so, rather than have a retry loop hide the absence
   and turn it into a timeout. -/
#guard Class.notFound.retryable == false

#guard (describeError 503 "" |>.retryable) == true
#guard (describeError 404 s3NoSuchKey |>.retryable) == false

-- ── The non-HTTP failures ───────────────────────────────────────────────────

#guard (Error.transport "connection reset by peer").klass == .transport
#guard (Error.transport "connection reset by peer").status == 0

#guard (Error.unsupported "gcp" "purging a subscription").klass == .unsupported
#guard (Error.unsupported "gcp" "purging a subscription").message
  == "gcp does not support purging a subscription"

/- `unbound` names every place that was searched, which is what separates an
   actionable message from "not configured". -/
#guard (Error.unbound "assets" ["LINEN_CLOUD_OBJECTSTORE_ASSETS_HANDLE", "manifest"]).message
  == "no binding for 'assets'; looked in: LINEN_CLOUD_OBJECTSTORE_ASSETS_HANDLE, manifest"

#guard toString (describeError 404 s3NoSuchKey)
  == "not found 404 NoSuchKey: The specified key does not exist. (request 656c76696e6727732072657175657374)"


-- ── OAuth2's own error dialect (RFC 6749) ───────────────────────────────────

/-- A token endpoint rejecting a service-account assertion. `error` is a
    **string** here, where Google's API errors make it an object — the same
    field name carrying two shapes, which is why both are read. -/
def oauthInvalidGrant : String :=
  "{\"error\":\"invalid_grant\",\"error_description\":\"Invalid JWT Signature.\"}"

#guard parseJsonError oauthInvalidGrant
  == some ("invalid_grant", "Invalid JWT Signature.")

#guard (describeError 400 oauthInvalidGrant).code == "invalid_grant"
#guard (describeError 400 oauthInvalidGrant).message == "Invalid JWT Signature."

/- Without the prose field there is still a code, which is the actionable
   half. -/
#guard parseJsonError "{\"error\":\"unauthorized_client\"}"
  == some ("unauthorized_client", "")

/- Google's *nested* shape still wins where `error` is an object, so adding the
   OAuth2 dialect did not cost the one that was already there. -/
#guard (parseJsonError googleNotFound).map (·.1) == some "NOT_FOUND"

end Tests.Cloud.Error
