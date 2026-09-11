/-
  `Cloud.Auth` — the three ways these clouds decide who is asking

  ## Three schemes, not one

  | scheme         | who                          | how                              |
  |----------------|------------------------------|----------------------------------|
  | SigV4          | AWS, and Scaleway's S3/SQS   | signature over the whole request |
  | `Bearer`       | GCP                          | a short-lived OAuth2 token       |
  | `X-Auth-Token` | Scaleway's own API           | the secret key, sent as-is       |

  Only the first is a *signature*: it covers the method, path, query, headers
  and body, so a request cannot be altered in flight and a replay is bounded by
  the timestamp. The other two are bearer credentials in the literal sense —
  whoever holds the string can make the call. That asymmetry is not this
  module's to fix, but it is worth knowing which of the three a given call is
  relying on.

  Note the awkward middle row: Scaleway authenticates its **own** API with a
  header, but its **S3 and SQS-compatible** endpoints with SigV4, using the
  same key pair. So the scheme is a property of the endpoint, not of the cloud,
  which is why `Auth` is a value passed alongside an `Endpoint` rather than
  something derived from a `Provider`.

  ## The signing time is a parameter

  `headersAt` takes the time; `headers` reads the clock and calls it. Keeping
  the clock out of the signing path is what lets the tests check a real
  signature against AWS's published worked example without a fixed-clock hack —
  the reason `Crypto.SigV4.sign` takes a time too.

  ## `Host` is signed

  SigV4 signs the `Host` header, so it has to be present *before* the signature
  is computed rather than added by the HTTP client afterwards. `headersAt`
  therefore returns it along with the signature, and `Cloud.Transport` sends
  exactly what was signed. Getting this wrong yields `SignatureDoesNotMatch`
  with nothing indicating why.

  ## Provenance

  Moved down from the sibling `typednotes/infra`
  (`Infra/Providers/Aws/Sign.lean`), generalised from "sign for AWS" to the
  three schemes the three clouds actually use, and made a value rather than a
  code path so that `Cloud.Transport` has one shape to send.
-/
import Linen.Cloud.Credentials
import Linen.Cloud.Endpoint
import Linen.Crypto.SigV4
import Linen.Data.Time.Clock

namespace Cloud

open Network.HTTP.Types (Query)

-- ── The schemes ─────────────────────────────────────────────────────────────

/-- How to prove who is asking.

    A value rather than a code path, so that every protocol dialect hands
    `Cloud.Transport` the same shape and the transport has one thing to do. -/
inductive Auth
  /-- AWS Signature Version 4. Used by AWS, and by Scaleway's S3- and
      SQS-compatible endpoints, which take the same key pair.

      `service` and `region` are the *signing* scope, which is not always the
      caller's region — see `Endpoint.region`. -/
  | sigV4 (creds : Credentials) (service : String) (region : String)
  /-- An OAuth2 bearer token in `Authorization`. GCP's only scheme. -/
  | bearer (token : String)
  /-- Scaleway's own API: the secret key in `X-Auth-Token`, unsigned. -/
  | authToken (token : String)
  /-- No credentials. For a public object, or a presigned URL that already
      carries its own signature in the query string. -/
  | anonymous

/-- The scheme's name, for diagnostics. Never the credential. -/
def Auth.scheme : Auth → String
  | .sigV4 _ service region => s!"sigv4({service}/{region})"
  | .bearer _               => "bearer"
  | .authToken _            => "x-auth-token"
  | .anonymous              => "anonymous"

/-- Redacting: the scheme is named, the credential never rendered. -/
instance : Repr Auth where
  reprPrec a _ := f!"Auth.{a.scheme}"

instance : ToString Auth where
  toString a := a.scheme

-- ── Building the auth for an endpoint ───────────────────────────────────────

/-- SigV4 for an endpoint, taking the service and signing region from it.

    The common case, and the one worth having a constructor for: reading the
    scope off the endpoint rather than passing it separately is what keeps a
    global service signing `us-east-1` when the caller is in Paris. -/
def Auth.forEndpoint (creds : Credentials) (ep : Endpoint) : Auth :=
  .sigV4 creds ep.service ep.region

/-- The scheme a cloud's *own* API uses, for the two clouds where that is a
    single answer.

    Scaleway authenticates every one of its own APIs with an `X-Auth-Token`,
    and GCP with a bearer token, so for those two the provider determines the
    scheme outright.

    **AWS is not like that, and this returns an error for it.** Every AWS API
    signs SigV4 under *its own* service name — `s3`, `sqs`, `secretsmanager`,
    `dynamodb` — so there is no such thing as "the AWS native scheme" and no
    name this function could pick that would be right more than occasionally.
    It previously answered `execute-api`, which is API Gateway's name: correct
    for calling an API Gateway deployment and a `SignatureDoesNotMatch` for
    everything else. Use `Auth.forEndpoint`, which reads the service off the
    endpoint, or `Auth.nativeFor` when the name is known but an `Endpoint` is
    not to hand.

    Not for the S3- and SQS-compatible endpoints either: those are SigV4 on all
    three clouds that have them, whatever this says. `Auth.forEndpoint` again. -/
def Auth.native (provider : Provider) (creds : Credentials) : Except Error Auth :=
  match provider with
  | .aws      =>
    .error {
        klass := .invalid
      , message := "AWS has no single native signing scheme: each service signs \
under its own SigV4 service name. Use Auth.forEndpoint to read it off the \
endpoint, or Auth.nativeFor to name it." }
  | .scaleway => .ok (.authToken creds.secretKey)
  | .gcp      => creds.requireToken .gcp |>.map Auth.bearer

/-- `Auth.native`, with the AWS SigV4 service name supplied.

    For the case where the service is known but no `Endpoint` has been built —
    otherwise prefer `Auth.forEndpoint`, which cannot disagree with the host it
    is signing for. `service` is ignored for the two clouds whose native scheme
    does not sign. -/
def Auth.nativeFor (provider : Provider) (creds : Credentials) (service : String) :
    Except Error Auth :=
  match provider with
  | .aws      => .ok (.sigV4 creds service creds.region)
  | .scaleway => .ok (.authToken creds.secretKey)
  | .gcp      => creds.requireToken .gcp |>.map Auth.bearer

-- ── Producing the headers ───────────────────────────────────────────────────

/-- The headers that authenticate one request, at a given time.

    Returns **everything that must be sent**, including `Host` — SigV4 signs
    it, so it cannot be left for the HTTP client to add later. The caller's own
    headers are not returned; they were passed in because the signature covers
    them, and the caller already has them.

    `doubleEncodePath` reflects a real split in AWS's rules: S3 signs the path
    exactly as sent, every other service expects it encoded twice. Getting it
    wrong yields `SignatureDoesNotMatch` with nothing to indicate why, so it is
    explicit at every call site rather than defaulted. -/
def Auth.headersAt (auth : Auth) (now : Data.Time.UTCTime) (host method path : String)
    (query : Query := []) (headers : List (String × String) := [])
    (payload : ByteArray := ByteArray.empty)
    (doubleEncodePath : Bool := false) (unsignedBody : Bool := false) :
    IO (List (String × String)) := do
  match auth with
  | .sigV4 creds service region =>
    let signed ← Crypto.SigV4.sign
      { accessKeyId := creds.accessKey
      , secretAccessKey := creds.secretKey
      , sessionToken := creds.sessionToken }
      region service now
      { method, path, query
      , headers := ("Host", host) :: headers
      , payload
      , doubleEncodePath
      , unsignedBody }
    return ("Host", host) :: signed
  | .bearer token   => return [("Host", host), ("Authorization", "Bearer " ++ token)]
  | .authToken tok  => return [("Host", host), ("X-Auth-Token", tok)]
  | .anonymous      => return [("Host", host)]

/-- `headersAt` against the current wall clock. -/
def Auth.headers (auth : Auth) (host method path : String)
    (query : Query := []) (headers : List (String × String) := [])
    (payload : ByteArray := ByteArray.empty)
    (doubleEncodePath : Bool := false) (unsignedBody : Bool := false) :
    IO (List (String × String)) := do
  auth.headersAt (← Data.Time.getCurrentTime) host method path query headers payload
    doubleEncodePath unsignedBody

/-- Whether this scheme can actually authenticate — a SigV4 auth with half a
    key pair cannot, and saying so here beats a `denied` from the provider. -/
def Auth.usable : Auth → Bool
  | .sigV4 creds _ _ => creds.canSign
  | .bearer token    => !token.isEmpty
  | .authToken tok   => !tok.isEmpty
  | .anonymous       => true

end Cloud
