/-
  `Cloud.Error` — what a cloud API said went wrong

  ## Why an `Except`, not an exception

  Every operation in `Cloud.*` answers `Except Error β` rather than throwing.
  The reason is that **"not found" is ordinary control flow** for a cloud data
  plane: "read this object if it is there" is the single most common thing an
  application asks, and a caller that must catch an exception to express it will
  eventually catch too much. Making the absence a value also keeps the pure
  interpreters pure, so an effect's `dryRun` can be `#guard`ed with no `IO` at
  all.

  ## Classified, not just numbered

  A bare status number is not a diagnosis: `403` could be an expired token, a
  clock skew of six minutes, or a bucket policy, and the provider's own code
  says which. So an `Error` carries the code and message verbatim **and** a
  `Class` derived from them, and callers switch on the class while logs keep the
  detail.

  Classification reads the *code* first and the status only as a fallback,
  because the code is the more specific signal — S3 answers `404` with
  `NoSuchBucket` and `NoSuchKey`, which mean different things to a caller that
  is about to create one of them.

  ## Why the code list is short on purpose

  `Class.notFound` is the class it is dangerous to be wrong about in one
  direction: mistaking a *permission* error for an absence makes a caller
  conclude a resource does not exist and try to create it. So the not-found
  codes are an explicit list rather than anything inferred, and codes that
  merely sound absent are left to fall through to `denied` or `invalid`.

  ## Structured, not scraped

  A word on what this module deliberately does *not* do. It would be possible to
  classify by substring-matching a rendered message, and the sibling
  `typednotes/infra` does exactly that — because its error type is flattened
  into an `IO.userError` before the layer that classifies it can see the parts.
  Here the status and code survive as fields, so classification is a total
  function over data. Same curated code list, no string scraping.

  ## Four wire dialects

  The three clouds answer failures four different ways, and all four are read
  here so that `describeError` can be called on any response body without the caller
  knowing which service it came from:

  - **S3 and other REST-XML services** — `<Error><Code/><Message/></Error>`,
    sometimes wrapped one level deep.
  - **AWS-JSON (Secrets Manager, SQS)** — `{"__type": …, "message": …}`, with
    the code sometimes suffixed after a `#`.
  - **Scaleway** — `{"type": …, "message": …}`.
  - **Google** — nested: `{"error": {"status": …, "message": …}}`. Reading only
    the flat shape here would render every GCP failure as an empty message,
    discarding the one part worth having.

  And a fifth, which is not a cloud's dialect but OAuth2's own (RFC 6749): the
  token endpoints answer `{"error": "invalid_grant", "error_description": …}`,
  where `error` is a **string** rather than an object. That collides with
  Google's nesting on the same field name, so both are handled and the type of
  the value decides which.
-/
import Linen.Text.XML
import Linen.Data.Json.Decode

namespace Cloud

-- ── Classes ─────────────────────────────────────────────────────────────────

/-- What kind of failure this is, in terms a caller can act on.

    The classes are chosen so that a caller never has to consult the raw code to
    decide what to *do*; the code remains on the error for the log. -/
inductive Class
  /-- The bucket, object, queue or secret is not there. Ordinary control flow,
      and the class most dangerous to infer wrongly — see the module header. -/
  | notFound
  /-- Authentication or authorisation failed: bad signature, expired token,
      missing permission. -/
  | denied
  /-- The resource already exists, or its current state forbids the request. -/
  | conflict
  /-- Rate-limited. Retrying later is the right response. -/
  | throttled
  /-- The request itself was malformed or its arguments rejected. Retrying
      unchanged will fail identically. -/
  | invalid
  /-- The provider failed on its own side (a 5xx). Retryable. -/
  | server
  /-- No HTTP response at all: DNS, TLS, connection reset, timeout. -/
  | transport
  /-- A response arrived but could not be understood — an unparseable body, or
      a field the API is documented to send and did not. -/
  | protocol
  /-- This cloud does not offer this operation. The total-value alternative to
      raising, for the gaps in `Provider.supports`. -/
  | unsupported
  /-- A logical resource name could not be resolved to a physical one. Raised
      by `Cloud.Binding`, never by a provider. -/
  | unbound
  deriving Repr, DecidableEq, BEq

/-- Whether retrying the identical request could plausibly succeed.

    `notFound` is excluded deliberately: a caller waiting for eventual
    consistency should say so itself rather than have a retry loop hide it. -/
def Class.retryable : Class → Bool
  | .throttled | .server | .transport => true
  | .notFound | .denied | .conflict | .invalid | .protocol
  | .unsupported | .unbound => false

/-- A short human name, for diagnostics. -/
def Class.name : Class → String
  | .notFound => "not found"
  | .denied => "denied"
  | .conflict => "conflict"
  | .throttled => "throttled"
  | .invalid => "invalid"
  | .server => "server error"
  | .transport => "transport"
  | .protocol => "protocol"
  | .unsupported => "unsupported"
  | .unbound => "unbound"

-- ── The error ───────────────────────────────────────────────────────────────

/-- A failed cloud call, as the provider described it plus how to act on it.

    `status` is `0` when there was no HTTP response (a `transport` failure) or
    no request at all (`unsupported`, `unbound`). -/
structure Error where
  /-- What to do about it. -/
  klass     : Class
  /-- The HTTP status, or `0` if the failure happened before or after HTTP. -/
  status    : Nat := 0
  /-- The provider's own error code, e.g. `NoSuchBucket`. Empty if it sent
      none. -/
  code      : String := ""
  /-- The provider's message, or the best description available. -/
  message   : String := ""
  /-- The provider's request identifier, when it sent one. The first thing a
      provider's support will ask for. -/
  requestId : Option String := none
  deriving Repr, DecidableEq, BEq

instance : ToString Error where
  toString e :=
    let rid := match e.requestId with | some r => s!" (request {r})" | none => ""
    let code := if e.code.isEmpty then "" else s!" {e.code}"
    let status := if e.status == 0 then "" else s!" {e.status}"
    s!"{e.klass.name}{status}{code}: {e.message}{rid}"

/-- Whether retrying could plausibly succeed. -/
def Error.retryable (e : Error) : Bool := e.klass.retryable

-- ── Classification ──────────────────────────────────────────────────────────

/-- Provider codes that mean "the thing you named is not there".

    An explicit list, kept narrow on purpose: see the module header on why
    being wrong in this direction is worse than falling through to `denied`.
    Covers all three clouds — AWS's `…Exception` spellings, S3's `NoSuch…`,
    SQS's queue codes, Scaleway's `not_found`, and Google's `NOT_FOUND`. -/
def notFoundCodes : List String :=
  [ -- S3, and the S3-compatible clouds
    "NoSuchBucket", "NoSuchKey", "NoSuchUpload", "NoSuchVersion"
    -- SQS
  , "QueueDoesNotExist", "AWS.SimpleQueueService.NonExistentQueue"
    -- AWS JSON protocols (Secrets Manager among them)
  , "ResourceNotFoundException", "ResourceNotFound", "NotFoundException"
    -- Scaleway
  , "not_found", "unknown_resource"
    -- Google
  , "NOT_FOUND" ]

/-- Provider codes that mean "you are not allowed, or we do not believe you". -/
def deniedCodes : List String :=
  [ "AccessDenied", "AccessDeniedException", "AllAccessDisabled"
  , "SignatureDoesNotMatch", "InvalidAccessKeyId", "InvalidSecurity"
  , "TokenRefreshRequired", "ExpiredToken", "ExpiredTokenException"
  , "UnauthorizedOperation", "UnrecognizedClientException"
  , "MissingAuthenticationToken", "InvalidClientTokenId"
  , "permissions_denied", "denied_authentication", "invalid_auth"
  , "PERMISSION_DENIED", "UNAUTHENTICATED" ]

/-- Provider codes that mean "it already exists, or its state forbids this". -/
def conflictCodes : List String :=
  [ "BucketAlreadyExists", "BucketAlreadyOwnedByYou", "BucketNotEmpty"
  , "QueueAlreadyExists", "QueueNameExists"
  , "AWS.SimpleQueueService.QueueDeletedRecently"
  , "ResourceExistsException", "ResourceInUseException"
  , "InvalidStateException", "PreconditionFailed"
  , "already_exists", "ALREADY_EXISTS", "FAILED_PRECONDITION", "ABORTED" ]

/-- Provider codes that mean "slow down". -/
def throttledCodes : List String :=
  [ "Throttling", "ThrottlingException", "ThrottledException"
  , "RequestThrottled", "RequestThrottledException", "RequestLimitExceeded"
  , "TooManyRequestsException", "SlowDown", "ServiceUnavailable"
  , "ProvisionedThroughputExceededException", "LimitExceededException"
  , "quotas_exceeded", "too_many_requests"
  , "RESOURCE_EXHAUSTED", "UNAVAILABLE" ]

/-- Provider codes that mean "this request is wrong and will stay wrong". -/
def invalidCodes : List String :=
  [ "InvalidParameterValue", "InvalidParameterException", "InvalidRequest"
  , "InvalidArgument", "ValidationException", "ValidationError"
  , "MalformedXML", "MalformedPolicyDocument", "InvalidBucketName"
  , "MissingParameter", "MissingRequiredParameter", "EntityTooLarge"
  , "InvalidDigest", "BadDigest"
  , "invalid_arguments", "INVALID_ARGUMENT", "OUT_OF_RANGE" ]

/-- Classify a failure from the provider's code and HTTP status.

    The code decides when it is one this module knows; otherwise the status
    does. A `0` status with an unknown code is a `protocol` failure — something
    answered, and it was not intelligible.

    Note the AWS-JSON convention handled by `bareCode`: `__type` is often
    qualified, as in `com.amazonaws.secretsmanager#ResourceNotFoundException`,
    and only the part after the `#` is the code. -/
def classify (status : Nat) (code : String) : Class :=
  let bare :=
    match (code.splitOn "#").reverse with
    | last :: _ => last
    | []        => code
  if notFoundCodes.contains bare then .notFound
  else if deniedCodes.contains bare then .denied
  else if conflictCodes.contains bare then .conflict
  else if throttledCodes.contains bare then .throttled
  else if invalidCodes.contains bare then .invalid
  else if status == 404 || status == 410 then .notFound
  else if status == 401 || status == 403 then .denied
  else if status == 409 || status == 412 then .conflict
  else if status == 429 then .throttled
  else if status >= 500 && status < 600 then .server
  else if status >= 400 && status < 500 then .invalid
  else if status == 0 then .protocol
  else .protocol

-- ── Reading error bodies ────────────────────────────────────────────────────

/-- Read an AWS/S3-style `<Error><Code/><Message/></Error>` document.

    Accepts the element at the root or one level in, because some services wrap
    it. Returns the code, the message and the request id, the last under either
    of the two spellings AWS uses. -/
def parseXmlError (body : String) : Option (String × String × Option String) :=
  match Text.XML.parse body with
  | .error _ => none
  | .ok root =>
    let err := if root.name.local' == "Error" then some root else root.child "Error"
    err.map fun e =>
      ( (e.childText "Code").getD ""
      , (e.childText "Message").getD ""
      , (e.childText "RequestId").orElse fun _ => e.childText "RequestID" )

/-- Read a JSON error body in any of the three JSON dialects.

    Tries the flat AWS-JSON and Scaleway shapes first, then Google's nested
    `{"error": {…}}`. The nesting is not an optional nicety: without it every
    GCP failure carries an empty message, which is the only part a human can
    use. -/
def parseJsonError (body : String) : Option (String × String) :=
  match Data.Json.Decode.decode body with
  | .error _ => none
  | .ok (.object fields) =>
    let get (k : String) : Option String :=
      (fields.find? (·.1 == k)).bind (fun kv => kv.2.asString)
    let code :=
      (get "__type").orElse fun _ =>
        (get "type").orElse fun _ => (get "code").orElse fun _ => get "Code"
    let msg := (get "message").orElse fun _ => get "Message"
    -- OAuth2 (RFC 6749): `error` is a *string*, with the prose in
    -- `error_description`. Same field name as Google's nested object, so the
    -- value's type is what distinguishes them.
    let oauth : Option (String × String) :=
      (get "error").map fun c =>
        (c, ((get "error_description").orElse fun _ => get "error_uri").getD "")
    let nested : Option (String × String) :=
      (fields.find? (·.1 == "error")).bind fun kv =>
        match kv.2 with
        | .object inner =>
          let innerGet (k : String) : Option String :=
            (inner.find? (·.1 == k)).bind (fun kv => kv.2.asString)
          some ( ((innerGet "status").orElse fun _ => innerGet "code").getD ""
               , (innerGet "message").getD "" )
        | _ => none
    match code, msg with
    | none, none => (oauth.orElse fun _ => nested).orElse fun _ => some ("", "")
    | _, _       => some (code.getD "", msg.getD "")
  | .ok _ => none

/-- How much of an unintelligible body to keep in the message. Enough to
    diagnose, little enough not to fill a log line. -/
def bodyExcerptLimit : Nat := 400

/-- Turn a non-2xx response into the best error available.

    Falls back to the body itself, truncated, so that an unrecognised dialect
    is never silently swallowed — the thing that makes a provider integration
    undebuggable. -/
def describeError (status : Nat) (body : String) : Error :=
  match parseXmlError body with
  | some (code, message, rid) =>
    { klass := classify status code, status, code, message, requestId := rid }
  | none =>
    match parseJsonError body with
    | some (code, message) => { klass := classify status code, status, code, message }
    | none =>
      let trimmed := body.trimAscii.toString
      let shown :=
        if trimmed.length > bodyExcerptLimit then (trimmed.take bodyExcerptLimit).toString ++ "…"
        else trimmed
      { klass := classify status ""
      , status
      , message := if shown.isEmpty then "(empty response body)" else shown }

-- ── Constructors for the non-HTTP failures ──────────────────────────────────

/-- No response arrived: DNS, TLS, connection reset, timeout. -/
def Error.transport (message : String) : Error :=
  { klass := .transport, message }

/-- A response arrived and could not be understood. -/
def Error.protocol (message : String) : Error :=
  { klass := .protocol, message }

/-- This cloud does not offer this operation.

    The total-value alternative to raising, for the gaps `Provider.supports`
    records. Names the provider and the operation, because "unsupported" alone
    sends the reader to the source. -/
def Error.unsupported (provider operation : String) : Error :=
  { klass := .unsupported
  , message := s!"{provider} does not support {operation}" }

/-- A logical resource name could not be resolved.

    `where'` lists every place that was looked, which is the difference between
    an actionable message and "not configured". -/
def Error.unbound (name : String) (where' : List String) : Error :=
  { klass := .unbound
  , message :=
      s!"no binding for '{name}'; looked in: " ++ String.intercalate ", " where' }

end Cloud
