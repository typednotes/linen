/-
  Tests for `Linen.Cloud`, the aggregator.

  Two things worth asserting about the namespace as a whole rather than about
  any one module:

  1. Importing the aggregator really does bring in every service and backend,
     so a consumer needs one import.
  2. **The same program runs against any backend.** The function below is
     written once against the portable interfaces and exercised against the
     in-memory ones; swapping in `ObjectStore.S3.of` or `ObjectStore.Gcs.of`
     changes the argument, not the program. That is the claim this namespace
     exists to make, and it is checked here rather than only described.
-/
import Linen.Cloud

open Cloud

namespace Tests.Cloud.Aggregate

-- ── One program, any backend ────────────────────────────────────────────────

/-- A small job written against the portable interfaces only: read a secret,
    store a report, and publish a notification.

    Nothing in its type or body names a cloud. -/
def runJob (store : ObjectStore) (queue : Queue) (secrets : SecretStore) :
    IO (Except Error String) := do
  match ← secrets.getString "api-token" with
  | .error e => return .error e
  | .ok token =>
    match ← store.putString "reports/latest.txt" s!"ran with a {token.length}-character token" with
    | .error e => return .error e
    | .ok written =>
      match ← queue.producer.sendOne s!"report {written.key} written ({written.size} bytes)" with
      | .error e => return .error e
      | .ok _ =>
        match ← queue.consumer.receiveOne with
        | .error e     => return .error e
        | .ok none     => return .error (Error.protocol "no notification arrived")
        | .ok (some m) => return .ok m.body

/- The job runs end to end against the local backends, with no credentials,
    no network and no containers. -/
/-- info: "report reports/latest.txt written (28 bytes)" -/
#guard_msgs in
#eval show IO String from do
  let store ← ObjectStore.inMemory
  let queue ← Queue.inMemory
  let secrets ← SecretStore.inMemory
  let _ ← secrets.putString "api-token" "abcdef"
  match ← runJob store queue secrets with
  | .ok body => return body
  | .error e => return toString e

/- A missing secret stops the job with a diagnosable error rather than running
    it with a blank token. -/
/-- info: Cloud.Class.notFound -/
#guard_msgs in
#eval show IO Class from do
  let store ← ObjectStore.inMemory
  let queue ← Queue.inMemory
  let secrets ← SecretStore.inMemory
  match ← runJob store queue secrets with
  | .error e => return e.klass
  | .ok _ => return .protocol

-- ── The aggregator brings in everything ─────────────────────────────────────

/- One import reaches every service interface, every provider backend, the
   credential chain and the wire dialects. -/
#guard providers.length == 3
#guard (S3.endpoint? .scaleway "fr-par").isSome
#guard (Cloud.Sqs.endpoint? .gcp "europe-west9").isNone
#guard Cloud.SecretsManager.jsonVersion == "1.1"
#guard ResourceKind.objectStore.name == "object-store"
#guard Provider.gcp.supports .queueLongPoll == false
#guard inMemoryPageSize == 3
#guard keychainService == "linen"

/- Every portable backend constructor is reachable from this one import. -/
example : Transport → Provider → Credentials → String → Except Error ObjectStore :=
  fun t p c b => ObjectStore.S3.of t p c b
example : Transport → Credentials → String → Except Error ObjectStore :=
  fun t c b => ObjectStore.Gcs.of t c b
example : Transport → Credentials → String → Except Error Producer :=
  fun t c topic => Queue.PubSub.producer t c topic
example : Transport → Credentials → Except Error SecretStore :=
  fun t c => Secret.GcpSecretManager.of t c

end Tests.Cloud.Aggregate
