/-
  Tests for `Cloud.Endpoint`.

  Host tables are the kind of thing that is either exactly right or produces a
  DNS failure at the worst possible moment, so every one is pinned literally
  rather than by pattern. The two that carry the most weight are the AWS and
  Scaleway S3 and SQS hosts: they are the *only* difference between the two
  clouds in `Cloud.ObjectStore.s3` and `Cloud.Queue.sqs`, so they are what makes
  one client serve two clouds.
-/
import Linen.Cloud.Endpoint

open Cloud

namespace Tests.Cloud.Endpoint

-- ── S3, and the reuse that makes this namespace small ───────────────────────

#guard S3.endpoint? .aws "eu-west-3"
  == some { host := "s3.eu-west-3.amazonaws.com", service := "s3", region := "eu-west-3" }
#guard S3.endpoint? .aws "us-east-1"
  == some { host := "s3.us-east-1.amazonaws.com", service := "s3", region := "us-east-1" }
#guard S3.endpoint? .scaleway "fr-par"
  == some { host := "s3.fr-par.scw.cloud", service := "s3", region := "fr-par" }
#guard S3.endpoint? .scaleway "nl-ams"
  == some { host := "s3.nl-ams.scw.cloud", service := "s3", region := "nl-ams" }

/- Both clouds sign as service `s3`, which is what lets the *same* signer and
   the same client serve them. -/
#guard (S3.endpoint? .aws "eu-west-3").map Endpoint.service
  == (S3.endpoint? .scaleway "fr-par").map Endpoint.service

/- GCP has no usable S3 endpoint: its S3-compatible XML API needs HMAC
   interoperability keys, and every GCP credential here is a bearer token. An
   `Option` says so before a request is built, rather than after a signature is
   rejected. -/
#guard S3.endpoint? .gcp "europe-west9" == none

/- The host is recorded, though, since that is where a future
   interoperability-key path would start. -/
#guard S3.gcsCompatibilityHost == "storage.googleapis.com"

-- ── Path-style addressing ───────────────────────────────────────────────────

#guard S3.bucketPath none == "/"
#guard S3.bucketPath (some "assets") == "/assets"

-- ── SQS ─────────────────────────────────────────────────────────────────────

#guard Sqs.endpoint? .aws "eu-west-3"
  == some { host := "sqs.eu-west-3.amazonaws.com", service := "sqs", region := "eu-west-3" }

/- Scaleway's Queues live under `mnq` — Messaging and Queuing — rather than on
   an `sqs.`-prefixed host in the AWS shape. -/
#guard Sqs.endpoint? .scaleway "fr-par"
  == some { host := "sqs.mnq.fr-par.scaleway.com", service := "sqs", region := "fr-par" }

/- GCP is `none`, and not for want of a feature: Pub/Sub is topic-plus-
   subscription rather than a queue. `Cloud.Queue` splits `Producer` from
   `Consumer` for the same reason. -/
#guard Sqs.endpoint? .gcp "europe-west9" == none

/- **SQS speaks AWS-JSON 1.0 while every other AWS-JSON service speaks 1.1.**
   Sending 1.1 to SQS is refused, so the version is pinned per service. -/
#guard Sqs.jsonVersion == "1.0"
#guard SecretsManager.jsonVersion == "1.1"
#guard Sqs.jsonVersion != SecretsManager.jsonVersion

-- ── Secrets Manager ─────────────────────────────────────────────────────────

#guard SecretsManager.endpoint "eu-west-3"
  == { host := "secretsmanager.eu-west-3.amazonaws.com"
     , service := "secretsmanager", region := "eu-west-3" }

-- ── STS: a global service signs a region the caller is not in ───────────────

/- The case `Endpoint.region` exists for. STS is global and always signs
   `us-east-1`, whatever region the credentials name — so the signing region
   cannot be read off the credentials at the point of signature. -/
#guard Sts.endpoint.region == "us-east-1"
#guard Sts.endpoint.host == "sts.amazonaws.com"

-- ── Scaleway: region in the path, not the host ──────────────────────────────

#guard Scaleway.host == "api.scaleway.com"

/- One global host for every region — the opposite of AWS's arrangement, and
   the reason the region has to travel in the path. -/
#guard (Scaleway.endpoint "fr-par").host == (Scaleway.endpoint "pl-waw").host

#guard Scaleway.regionalPrefix "secret-manager" "v1beta1" "fr-par"
  == "/secret-manager/v1beta1/regions/fr-par"
#guard Scaleway.globalPrefix "iam" "v1alpha1" == "/iam/v1alpha1"

#guard Scaleway.secretProduct == ("secret-manager", "v1beta1")
#guard Scaleway.queuesProduct == ("mnq", "v1beta1")

-- ── Google ──────────────────────────────────────────────────────────────────

#guard Gcp.storageHost == "storage.googleapis.com"
#guard Gcp.pubSubHost == "pubsub.googleapis.com"
#guard Gcp.secretManagerHost == "secretmanager.googleapis.com"
#guard Gcp.cloudPlatformScope == "https://www.googleapis.com/auth/cloud-platform"

/- Google answers with fully-qualified resource names where the caller asked
   about a bare one, so this is how an answer is matched to a question. -/
#guard Gcp.shortName "projects/typednotes/secrets/db-password" == "db-password"
#guard Gcp.shortName "projects/p/topics/jobs" == "jobs"

/- A name that is already short is returned unchanged, so the function is safe
   to apply twice. -/
#guard Gcp.shortName "db-password" == "db-password"
#guard Gcp.shortName (Gcp.shortName "projects/p/secrets/s") == "s"

/- An empty segment is not silently swallowed into a wrong answer. -/
#guard Gcp.shortName "" == ""

-- ── URLs ────────────────────────────────────────────────────────────────────

#guard (Endpoint.raw "s3.example.test" "s3" "eu-west-3").origin == "https://s3.example.test"
#guard (Endpoint.raw "s3.example.test" "s3" "eu-west-3").url "/assets/a.json"
  == "https://s3.example.test/assets/a.json"

/- `raw` takes a host on trust — a VPC endpoint or a compatible third-party
   store — and is spelled differently from the per-service constructors so that
   using one is visible at the call site. -/
#guard (Endpoint.raw "minio.internal:9000" "s3" "us-east-1").host == "minio.internal:9000"

end Tests.Cloud.Endpoint
