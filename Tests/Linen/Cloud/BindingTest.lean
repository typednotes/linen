/-
  Tests for `Cloud.Binding`.

  Lean has no `setenv`, so the environment-driven paths cannot be driven from a
  self-check. What *is* tested is everything pure: the variable and key naming
  that a deployment has to get right, the manifest reader, and the fall-back to
  the credentials — plus the property that matters most, that a failure names
  every place it looked.
-/
import Linen.Cloud.Binding

open Cloud

namespace Tests.Cloud.Binding

-- ── Names a deployment has to get right ─────────────────────────────────────

#guard ResourceKind.objectStore.name == "object-store"
#guard ResourceKind.queues.name == "queues"
#guard ResourceKind.secrets.name == "secrets"

/- The kind names match the sibling `typednotes/infra`'s own on-disk spelling,
   so a manifest emitted there needs no translation. -/
#guard manifestKey .objectStore "assets" == "object-store/assets"
#guard manifestKey .secrets "db-password" == "secrets/db-password"

/- A resource name becomes a legal variable name, so a bucket with dots or
   dashes is still configurable. -/
#guard envName "assets" == "ASSETS"
#guard envName "my-assets.v2" == "MY_ASSETS_V2"
#guard envName "Jobs-2026" == "JOBS_2026"

#guard envVar .objectStore "assets" "PROVIDER" == "LINEN_CLOUD_OBJECTSTORE_ASSETS_PROVIDER"
#guard envVar .objectStore "assets" "REGION" == "LINEN_CLOUD_OBJECTSTORE_ASSETS_REGION"
#guard envVar .queues "jobs" "URL" == "LINEN_CLOUD_QUEUES_JOBS_URL"
#guard envVar .queues "jobs" "SUBSCRIPTION" == "LINEN_CLOUD_QUEUES_JOBS_SUBSCRIPTION"
#guard envVar .secrets "db-password" "HANDLE" == "LINEN_CLOUD_SECRETS_DB_PASSWORD_HANDLE"

#guard manifestVar == "LINEN_CLOUD_MANIFEST"
#guard defaultProviderVar == "LINEN_CLOUD_PROVIDER"
#guard defaultRegionVar == "LINEN_CLOUD_REGION"

-- ── The manifest ────────────────────────────────────────────────────────────

/-- A manifest of the shape this module documents. The field names mirror
    `infra`'s `ObservedOf` records. -/
def manifest : String :=
  "{\"object-store/assets\":{\"provider\":\"scaleway\",\"region\":\"fr-par\"}," ++
  "\"queues/jobs\":{\"provider\":\"aws\",\"region\":\"eu-west-3\"," ++
    "\"url\":\"https://sqs.eu-west-3.amazonaws.com/1234/jobs\"}," ++
  "\"queues/events\":{\"provider\":\"gcp\",\"region\":\"europe-west9\"," ++
    "\"project_id\":\"typednotes\",\"subscription\":\"events-worker\"}}"

def parsed : Data.Json.Value :=
  match Data.Json.Decode.decode manifest with
  | .ok v => v
  | .error _ => .null

#guard match bindingOfManifest parsed .objectStore "assets" with
  | .ok (some b) => b.provider == .scaleway && b.region == "fr-par" && b.handle == "assets"
  | _ => false

#guard match bindingOfManifest parsed .queues "jobs" with
  | .ok (some b) => b.url == some "https://sqs.eu-west-3.amazonaws.com/1234/jobs"
  | _ => false

/- **The subscription is the one field that genuinely cannot be derived**: a
   Pub/Sub topic may have many, and reading the wrong one takes somebody else's
   messages. -/
#guard match bindingOfManifest parsed .queues "events" with
  | .ok (some b) => b.subscription == some "events-worker" && b.projectId == some "typednotes"
  | _ => false

/- A resource the manifest does not mention is `none`, so resolution falls
   through to the next source. -/
#guard match bindingOfManifest parsed .objectStore "not-listed" with
  | .ok none => true
  | _ => false

/- An entry naming a provider that does not exist is an **error**, not a
   silently ignored line — a typo must not look like an absent entry. -/
#guard match bindingOfManifest
    (match Data.Json.Decode.decode
      "{\"object-store/a\":{\"provider\":\"azure\",\"region\":\"x\"}}" with
     | .ok v => v | .error _ => .null) .objectStore "a" with
  | .error e => e.klass == .invalid && (e.message.splitOn "azure").length == 2
  | _ => false

/- An entry with no provider at all is likewise an error. -/
#guard match bindingOfManifest
    (match Data.Json.Decode.decode "{\"object-store/a\":{\"region\":\"x\"}}" with
     | .ok v => v | .error _ => .null) .objectStore "a" with
  | .error e => e.klass == .protocol
  | _ => false

-- ── The failure names every source ──────────────────────────────────────────

/- The property that separates an actionable message from "not configured".
   Four sources, in the order they are tried. -/
#guard (sourcesFor .objectStore "assets").length == 4
#guard ((sourcesFor .objectStore "assets").head!.splitOn
  "LINEN_CLOUD_OBJECTSTORE_ASSETS_PROVIDER").length == 2
#guard (((sourcesFor .queues "jobs")[1]!).splitOn "queues/jobs").length == 2
#guard (((sourcesFor .queues "jobs")[3]!).splitOn "credentials").length == 2

-- ── Resolution ──────────────────────────────────────────────────────────────

/- An explicit binding wins outright: configuration never overrides a program
   that asked for something specific. -/
/-- info: (Cloud.Provider.gcp, "europe-west9") -/
#guard_msgs in
#eval show IO (Provider × String) from do
  let explicit : Binding :=
    { provider := .gcp, region := "europe-west9", handle := "assets" }
  match ← resolve .objectStore "assets" { region := "eu-west-3" } (some explicit) with
  | .ok b => return (b.provider, b.region)
  | .error _ => return (.aws, "")

/- With nothing configured, resolution reaches the credentials for the region
   and project but has no provider to choose — so it fails, naming every
   source. There is deliberately no default cloud. -/
/-- info: (Cloud.Class.unbound, 4) -/
#guard_msgs in
#eval show IO (Class × Nat) from do
  match ← resolve .objectStore "no-such-resource-xyz"
      { region := "eu-west-3", projectId := some "p" } with
  | .error e => return (e.klass, (sourcesFor .objectStore "no-such-resource-xyz").length)
  | .ok _ => return (.protocol, 0)

end Tests.Cloud.Binding
