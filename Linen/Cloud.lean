/-
  `Linen.Cloud` — one way to use cloud services across AWS, GCP and Scaleway

  Aggregator for the `Cloud.*` namespace. Importing this brings in every
  service interface and every provider backend.

  ## What is here

  Three portable service interfaces, each a record of closures with one
  implementation per cloud and an in-memory one for local work:

  | interface     | AWS            | GCP            | Scaleway         |
  |---------------|----------------|----------------|------------------|
  | `ObjectStore` | S3             | Cloud Storage  | Object Storage   |
  | `Queue`       | SQS            | Pub/Sub        | Queues (SQS)     |
  | `SecretStore` | Secrets Manager| Secret Manager | Secret Manager   |

  Under them: the credential chain, the region tables, request signing, the
  four wire dialects and a swappable transport.

  Above them, and **not** imported here because it is a different kind of
  thing: `Control.Monad.Effect.{ObjectStore,Queue,SecretStore}` wrap these in
  capability-restricted effects, so a program can be *statically* confined to
  one bucket's prefix or denied the ability to read a secret's value.

  ## The one thing worth knowing first

  Scaleway's Object Storage speaks the S3 API and its Queues speak the SQS API,
  so `ObjectStore.S3` and `Queue.Sqs` each serve **two** clouds and differ in
  nothing but the host in `Cloud.Endpoint`. GCP is the cloud that does not fit:
  Cloud Storage needs a credential this namespace does not carry, and Pub/Sub
  is a topic-and-subscription system rather than a queue — which is why
  `Cloud.Queue` splits into a `Producer` and a `Consumer`.

  ## Working without an account

  Every interface has an in-memory backend — `ObjectStore.inMemory`,
  `Queue.inMemory`, `SecretStore.inMemory` — that behaves like the real thing
  in the ways that catch bugs: lexicographic key order, real pagination,
  message invisibility and redelivery counts, `notFound` for a missing secret.
  And `Transport.stub` replaces the network for any of the real backends, so a
  provider client can be exercised against recorded payloads. Both are how this
  namespace is tested, with no credentials and no containers.
-/
import Linen.Cloud.Provider
import Linen.Cloud.Error
import Linen.Cloud.Credentials
import Linen.Cloud.Credentials.Keychain
import Linen.Cloud.Credentials.Gcp
import Linen.Cloud.Credentials.Chain
import Linen.Cloud.Endpoint
import Linen.Cloud.Page
import Linen.Cloud.Auth
import Linen.Cloud.Transport
import Linen.Cloud.Protocol.S3
import Linen.Cloud.Protocol.AwsJson
import Linen.Cloud.Protocol.GoogleRest
import Linen.Cloud.Protocol.ScalewayRest
import Linen.Cloud.ObjectStore
import Linen.Cloud.ObjectStore.S3
import Linen.Cloud.ObjectStore.Gcs
import Linen.Cloud.Queue
import Linen.Cloud.Queue.Sqs
import Linen.Cloud.Queue.PubSub
import Linen.Cloud.Secret
import Linen.Cloud.Secret.SecretsManager
import Linen.Cloud.Secret.ScalewaySecretManager
import Linen.Cloud.Secret.GcpSecretManager
import Linen.Cloud.Binding
