/-
  `Cloud.Endpoint` — which host to talk to, and how to sign for it

  ## The whole portability trick lives here

  Scaleway's Object Storage speaks the S3 API and its Queues product speaks the
  SQS API. So a single S3 client serves AWS *and* Scaleway, and a single SQS
  client likewise, differing in **nothing but the host in this file**:

      s3.{region}.amazonaws.com        s3.{region}.scw.cloud
      sqs.{region}.amazonaws.com       sqs.mnq.{region}.scaleway.com

  That is why `Cloud.ObjectStore.s3` and `Cloud.Queue.sqs` take a `Provider`
  rather than existing twice, and it is the single largest reason this namespace
  is smaller than three per-cloud SDKs would be.

  GCP is the cloud that does not fit. Cloud Storage *does* expose an
  S3-compatible XML API on one global host, but it authenticates with HMAC
  interoperability keys — a separate credential an operator must create by hand
  — rather than the bearer token every GCP credential here carries. So the
  S3-compatible path is unreachable in practice, and GCS gets its own JSON
  client. Pub/Sub is not SQS-shaped at all.

  Both facts are recorded in the types: `S3.endpoint?` and `Sqs.endpoint?`
  answer `none` for GCP rather than inventing a host. The sibling
  `typednotes/infra` returned a deliberately unresolvable
  `gcp-queues-are-pubsub-not-sqs.invalid` here so that a mistake failed loudly
  in DNS; an `Option` says the same thing before a request is built.

  ## Signing region is not the caller's region

  `Endpoint.region` is the region to *sign* for, which is not always where the
  caller is: IAM and STS are global services and always sign `us-east-1`
  whatever the credentials say. Keeping the signing region on the endpoint
  rather than reading it from the credentials at the point of signature is what
  makes that expressible.

  ## Provider facts go stale

  These host patterns are a snapshot. `Endpoint.raw` exists for a host no table
  predicts — a VPC endpoint, a compatible third-party store, a region added
  after this file was written.

  ## Provenance

  Moved down from the sibling `typednotes/infra`
  (`Infra/Providers/Aws/{Protocols,Sign}.lean`,
  `Infra/Providers/{Scaleway,Gcp}/Rest.lean`), which now uses this instead. The
  control-plane endpoints — IAM, RDS, EC2, Lambda, ECR, Cloud Run, Cloud SQL,
  Artifact Registry — stayed behind with the engine that calls them; what moved
  is the object, queue and secret surface, plus STS because identity checking
  is every caller's problem.
-/
import Linen.Cloud.Provider

namespace Cloud

-- ── The endpoint ────────────────────────────────────────────────────────────

/-- Where a request goes, and what to sign it as. -/
structure Endpoint where
  /-- The wire host, e.g. `s3.eu-west-3.amazonaws.com`. -/
  host    : String
  /-- The SigV4 service name, e.g. `s3`, `sqs`, `secretsmanager`. Ignored by
      the clouds that do not sign. -/
  service : String
  /-- The SigV4 region.

      **Not always the caller's region.** IAM and STS are global and always
      sign `us-east-1`, whatever region the credentials name — which is why
      this is a field rather than something read from the credentials when the
      signature is computed. -/
  region  : String
  deriving Repr, DecidableEq, BEq

/-- An endpoint for a host no table predicts: a VPC endpoint, a compatible
    third-party store, a region newer than this file.

    Spelled differently from the per-service constructors so that reaching for
    it is visible at the call site. -/
def Endpoint.raw (host service region : String) : Endpoint := { host, service, region }

/-- The `https://` origin, for logs and for constructing absolute URLs. All
    three clouds are TLS-only on these services, so the scheme is not a
    parameter. -/
def Endpoint.origin (ep : Endpoint) : String := s!"https://{ep.host}"

/-- The absolute URL of a path at this endpoint. `path` must already begin with
    `/` and be percent-encoded. -/
def Endpoint.url (ep : Endpoint) (path : String) : String := ep.origin ++ path

-- ── S3 and the S3-compatible clouds ─────────────────────────────────────────

namespace S3

/-- The S3-compatible endpoint for a cloud and region, or `none` where there is
    no usable one.

    AWS and Scaleway differ only in the host — the fact that lets one client
    serve both. GCP is `none`: see the module header on why its S3-compatible
    XML API is unreachable with a bearer token. -/
def endpoint? (provider : Provider) (region : String) : Option Endpoint :=
  match provider with
  | .aws      => some { host := s!"s3.{region}.amazonaws.com", service := "s3", region }
  | .scaleway => some { host := s!"s3.{region}.scw.cloud",     service := "s3", region }
  | .gcp      => none

/-- The host Cloud Storage's S3-compatible XML API answers on.

    Recorded because it is where a future HMAC-interoperability-key path would
    start, and deliberately **not** returned by `endpoint?`: reaching it needs a
    credential this namespace does not carry. -/
def gcsCompatibilityHost : String := "storage.googleapis.com"

/-- Path-style addressing: `/` for service-level calls, `/bucket` otherwise.

    Path-style rather than virtual-host style (`bucket.s3.…`) because it needs
    no per-bucket DNS or wildcard certificate, and because it is what
    S3-compatible clouds implement most consistently. -/
def bucketPath (bucket : Option String) : String :=
  match bucket with
  | some b => "/" ++ b
  | none   => "/"

end S3

-- ── SQS and the SQS-compatible clouds ───────────────────────────────────────

namespace Sqs

/-- The SQS-compatible endpoint for a cloud and region, or `none` where the
    cloud has no such thing.

    Scaleway's Queues product is SQS-compatible and reached on its own host.
    GCP is `none`, and not because of a missing feature: Pub/Sub is a
    topic-and-subscription system rather than a queue, and pretending otherwise
    is what `Cloud.Queue`'s `Producer`/`Consumer` split exists to avoid.

    Note that Scaleway's Queues wants a **dedicated** key pair, distinct from
    the account's main one — see `Cloud.Queue.Sqs`. -/
def endpoint? (provider : Provider) (region : String) : Option Endpoint :=
  match provider with
  | .aws      => some { host := s!"sqs.{region}.amazonaws.com",      service := "sqs", region }
  | .scaleway => some { host := s!"sqs.mnq.{region}.scaleway.com",   service := "sqs", region }
  | .gcp      => none

/-- SQS speaks AWS-JSON **1.0** while every other AWS-JSON service here speaks
    1.1. Sending 1.1 to SQS is rejected, so the version is pinned per service
    rather than defaulted. -/
def jsonVersion : String := "1.0"

end Sqs

-- ── AWS Secrets Manager ─────────────────────────────────────────────────────

namespace SecretsManager

/-- The AWS Secrets Manager endpoint. Regional, and AWS-only: Scaleway and GCP
    have their own secret services on their own hosts. -/
def endpoint (region : String) : Endpoint :=
  { host := s!"secretsmanager.{region}.amazonaws.com", service := "secretsmanager", region }

/-- AWS-JSON version for Secrets Manager. -/
def jsonVersion : String := "1.1"

end SecretsManager

-- ── AWS STS ─────────────────────────────────────────────────────────────────

namespace Sts

/-- The STS endpoint.

    Global, and therefore always signed `us-east-1` whatever region the
    credentials name — the case `Endpoint.region` exists to express. Included
    because `GetCallerIdentity` is how a caller checks *which account* it is
    about to write to, which is worth doing before it does. -/
def endpoint : Endpoint :=
  { host := "sts.amazonaws.com", service := "sts", region := "us-east-1" }

end Sts

-- ── Scaleway's own API ──────────────────────────────────────────────────────

namespace Scaleway

/-- Scaleway's single global API host. The region travels in the **path**, not
    the hostname — the opposite of AWS's arrangement. -/
def host : String := "api.scaleway.com"

/-- An endpoint for Scaleway's own API.

    `service` and `region` are unused: Scaleway does not sign requests, it
    carries an `X-Auth-Token` header. They are filled in anyway so that one
    `Endpoint` type serves all three clouds. -/
def endpoint (region : String) : Endpoint :=
  { host, service := "scaleway", region }

/-- The path prefix of a regional product, e.g.
    `/secret-manager/v1beta1/regions/fr-par`. -/
def regionalPrefix (product version region : String) : String :=
  s!"/{product}/{version}/regions/{region}"

/-- The path prefix of a global product, e.g. `/iam/v1alpha1`. -/
def globalPrefix (product version : String) : String :=
  s!"/{product}/{version}"

/-- Scaleway's Secret Manager product and API version. -/
def secretProduct : String × String := ("secret-manager", "v1beta1")

/-- Scaleway's Messaging and Queuing product, which mints the dedicated SQS
    credential its Queues need. -/
def queuesProduct : String × String := ("mnq", "v1beta1")

end Scaleway

-- ── Google's APIs ───────────────────────────────────────────────────────────

namespace Gcp

/-- An endpoint on a Google API host.

    `service` and `region` are unused — GCP authenticates with a bearer token
    and puts the location in the path or the resource name — and are filled in
    for uniformity. -/
def endpoint (host : String) : Endpoint :=
  { host, service := "gcp", region := "" }

/-- Cloud Storage's JSON API host. Also serves the media (upload/download)
    paths. -/
def storageHost : String := "storage.googleapis.com"

/-- Pub/Sub's host. -/
def pubSubHost : String := "pubsub.googleapis.com"

/-- Secret Manager's host. -/
def secretManagerHost : String := "secretmanager.googleapis.com"

/-- The OAuth2 token endpoint, where a service-account assertion is exchanged
    for an access token. -/
def oauthTokenHost : String := "oauth2.googleapis.com"

/-- The path on `oauthTokenHost` that performs the exchange. -/
def oauthTokenPath : String := "/token"

/-- The scope a token needs to reach the services in this namespace. -/
def cloudPlatformScope : String := "https://www.googleapis.com/auth/cloud-platform"

/-- The last segment of a Google fully-qualified resource name.

    Google answers with `projects/p/secrets/s` where a caller asked about `s`,
    so this is how an answer is matched against a question. -/
def shortName (resourceName : String) : String :=
  (resourceName.splitOn "/").getLast?.getD resourceName

end Gcp

-- ── Self-checks ─────────────────────────────────────────────────────────────

-- The one line of this file that carries the most weight: two clouds, one
-- client, one differing host.
#guard (S3.endpoint? .aws "eu-west-3").map Endpoint.host == some "s3.eu-west-3.amazonaws.com"
#guard (S3.endpoint? .scaleway "fr-par").map Endpoint.host == some "s3.fr-par.scw.cloud"
#guard (S3.endpoint? .gcp "europe-west9") == none

#guard (Sqs.endpoint? .aws "eu-west-3").map Endpoint.host == some "sqs.eu-west-3.amazonaws.com"
#guard (Sqs.endpoint? .scaleway "fr-par").map Endpoint.host
  == some "sqs.mnq.fr-par.scaleway.com"
#guard (Sqs.endpoint? .gcp "europe-west9") == none

end Cloud
