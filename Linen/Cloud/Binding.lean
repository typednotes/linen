/-
  `Cloud.Binding` — turning a name a program knows into a resource it can reach

  ## The problem

  A program says "the `assets` bucket" or "the `jobs` queue". To make a request
  it needs a cloud, a region, and sometimes a provider-assigned identifier it
  cannot guess — an SQS queue URL contains an account id; every Pub/Sub and
  Scaleway path contains a project id. Something has to supply those, and the
  program should not have them compiled in.

  ## What can and cannot be reconstructed

  Worth knowing before reaching for configuration, because most of it *can* be
  derived from `(name, provider, region, credentials)` plus at most one API
  call:

  - **Object stores** — reconstructed entirely. An S3 or GCS bucket is
    addressed by name.
  - **Secrets** — reconstructed. Scaleway needs a name-to-UUID lookup, which
    its client does.
  - **Queues** — SQS needs a URL, which `GetQueueUrl` resolves from the name;
    Pub/Sub needs the project, which the credentials carry.

  So this module is a **convenience and a latency saving**, not a necessity.
  What it genuinely adds is the ability to point a program at a different
  cloud, region or physical name without recompiling it — and to give a
  Pub/Sub consumer its subscription name, which is the one thing that really
  cannot be derived.

  ## Resolution order

  First hit wins:

  1. An explicit `Binding` the program constructs. Configuration never
     overrides code that asked for something specific.
  2. `LINEN_CLOUD_<KIND>_<NAME>_*` — the per-resource override.
  3. The manifest file named by `LINEN_CLOUD_MANIFEST`, keyed
     `"<kind>/<name>"`.
  4. `LINEN_CLOUD_PROVIDER` and `LINEN_CLOUD_REGION` — the defaults for every
     resource.
  5. The credential chain's own region and project.
  6. Otherwise `unbound`, naming **every place that was looked**.

  That last point is the one that matters in practice. A message saying "not
  configured" sends the reader to the source; one that lists the four variables
  and the file it tried does not. The diagnostic follows
  `Cloud.Credentials.sourceDescriptions`.

  ## The manifest

  A JSON object keyed `"<kind>/<name>"`, values carrying the fields below:

  ```json
  { "object-store/assets": { "provider": "scaleway", "region": "fr-par" },
    "queues/jobs": { "provider": "aws", "region": "eu-west-3",
                     "url": "https://sqs.eu-west-3.amazonaws.com/1234/jobs" } }
  ```

  The kind names and the field names deliberately match those the sibling
  `typednotes/infra` already uses on disk — its `Kind.name` strings and its
  `ObservedOf` field names — so that having it emit this file is a mapping with
  no renaming. **Nothing writes it today**; it is read here so the format is
  fixed and can be proposed to `infra` rather than negotiated later.
-/
import Linen.Cloud.Credentials
import Linen.Data.Json.Decode

namespace Cloud

-- ── Resource kinds ──────────────────────────────────────────────────────────

/-- A kind of resource a binding can name.

    The three the data plane serves. Spelled as `infra` spells them on disk, so
    a manifest emitted there needs no translation. -/
inductive ResourceKind
  /-- A bucket. -/
  | objectStore
  /-- A queue, or a topic-and-subscription pair. -/
  | queues
  /-- A secret store's secret. -/
  | secrets
  deriving Repr, DecidableEq, BEq

/-- The kebab-case name, matching `infra`'s `Kind.name`. -/
def ResourceKind.name : ResourceKind → String
  | .objectStore => "object-store"
  | .queues      => "queues"
  | .secrets     => "secrets"

/-- The environment-variable infix: the kind's name, uppercased with `-`
    replaced by `_`. -/
def ResourceKind.envInfix : ResourceKind → String
  | .objectStore => "OBJECTSTORE"
  | .queues      => "QUEUES"
  | .secrets     => "SECRETS"

-- ── A binding ───────────────────────────────────────────────────────────────

/-- Where a named resource actually is. -/
structure Binding where
  /-- Which cloud. -/
  provider     : Provider
  /-- The cloud's own region code. -/
  region       : String
  /-- The provider-assigned name, which is usually the logical name but need
      not be — a bucket may be globally-unique-prefixed, for instance. Named
      after `infra`'s `Handle.raw`. -/
  handle       : String
  /-- The provider-assigned URL, where there is one. `infra`'s
      `{ObjectStore,Queues}Observed.url`. -/
  url          : Option String := none
  /-- The project — GCP's, or Scaleway's. -/
  projectId    : Option String := none
  /-- For a Pub/Sub consumer: the subscription to pull from. **The one field
      that genuinely cannot be derived**, since a topic may have many
      subscriptions and reading the wrong one takes somebody else's
      messages. -/
  subscription : Option String := none
  deriving Repr, DecidableEq

-- ── Environment variable names ──────────────────────────────────────────────

/-- The prefix every variable this module reads begins with. -/
def envPrefix : String := "LINEN_CLOUD"

/-- Normalise a resource name for use in a variable name: uppercase, with
    anything that is not alphanumeric replaced by `_`.

    So a bucket called `my-assets.v2` is configured through
    `LINEN_CLOUD_OBJECTSTORE_MY_ASSETS_V2_PROVIDER`. -/
def envName (name : String) : String :=
  String.ofList (name.toList.map fun c =>
    if c.isAlphanum then c.toUpper else '_')

/-- The variable naming one field of one resource. -/
def envVar (kind : ResourceKind) (name field : String) : String :=
  s!"{envPrefix}_{kind.envInfix}_{envName name}_{field}"

/-- The variable naming the manifest file. -/
def manifestVar : String := s!"{envPrefix}_MANIFEST"

/-- The variable naming the default provider for every resource. -/
def defaultProviderVar : String := s!"{envPrefix}_PROVIDER"

/-- The variable naming the default region for every resource. -/
def defaultRegionVar : String := s!"{envPrefix}_REGION"

-- ── The manifest ────────────────────────────────────────────────────────────

/-- The manifest key for a resource: `"<kind>/<name>"`. -/
def manifestKey (kind : ResourceKind) (name : String) : String :=
  s!"{kind.name}/{name}"

/-- Read one binding out of a parsed manifest document.

    `none` when the manifest has no entry for this resource, so resolution
    falls through. A malformed *entry* — one whose provider is unrecognised —
    is an error, because silently ignoring it would look exactly like an
    absent entry. -/
def bindingOfManifest (doc : Data.Json.Value) (kind : ResourceKind) (name : String) :
    Except Error (Option Binding) := do
  let some entry := (doc.asObject.bind fun fs =>
      (fs.find? (·.1 == manifestKey kind name)).map (·.2)) | .ok none
  let str (k : String) : Option String :=
    (entry.asObject.bind fun fs => (fs.find? (·.1 == k)).map (·.2)).bind
      Data.Json.Value.asString
  let some providerName := str "provider"
    | .error (Error.protocol s!"manifest entry '{manifestKey kind name}' has no provider")
  let some provider := Provider.ofName? providerName
    | .error
        { klass := .invalid
        , message := s!"manifest entry '{manifestKey kind name}' names an unknown \
provider '{providerName}'" }
  .ok (some
    { provider
    , region := (str "region").getD ""
    , handle := (str "handle").getD name
    , url := str "url"
    , projectId := str "project_id"
    , subscription := str "subscription" })

/-- Read the manifest named by `LINEN_CLOUD_MANIFEST`, if there is one.

    A variable that is set and names an unreadable or malformed file is an
    error, on the same rule the credential chain follows: a typo must not look
    like an absence. -/
def readManifest : IO (Except Error (Option Data.Json.Value)) := do
  let some path := normalizeEnv (← IO.getEnv manifestVar) | return .ok none
  if !(← (System.FilePath.mk path).pathExists) then
    return .error
      { klass := .invalid
      , message := s!"{manifestVar} names '{path}', which does not exist" }
  let text ← try
      pure (Except.ok (← IO.FS.readFile path))
    catch e => pure (Except.error (Error.transport s!"reading '{path}': {toString e}"))
  match text with
  | .error e => return .error e
  | .ok text =>
    match Data.Json.Decode.decode text with
    | .ok v    => return .ok (some v)
    | .error m => return .error (Error.protocol s!"{path}: {m}")

-- ── Resolution ──────────────────────────────────────────────────────────────

/-- Every place `resolve` looks, in order, for the not-found message. -/
def sourcesFor (kind : ResourceKind) (name : String) : List String :=
  [ s!"environment {envVar kind name "PROVIDER"} and {envVar kind name "REGION"}"
  , s!"manifest named by {manifestVar}, key '{manifestKey kind name}'"
  , s!"environment {defaultProviderVar} and {defaultRegionVar}"
  , "the credentials' own region and project" ]

/-- An environment variable, empty treated as absent. -/
private def env? (name : String) : IO (Option String) :=
  normalizeEnv <$> IO.getEnv name

/-- Resolve a named resource to a binding.

    `creds` supplies the last-resort region and project. `explicit`, when
    given, wins outright: configuration never overrides a program that asked
    for something specific.

    See the module header for the full order, and note that the failure names
    every place that was tried. -/
def resolve (kind : ResourceKind) (name : String) (creds : Credentials)
    (explicit : Option Binding := none) : IO (Except Error Binding) := do
  if let some b := explicit then return .ok b
  -- 2. The per-resource variables.
  let perResourceProvider ← env? (envVar kind name "PROVIDER")
  let perResourceRegion ← env? (envVar kind name "REGION")
  let handle := ((← env? (envVar kind name "HANDLE"))).getD name
  let url ← env? (envVar kind name "URL")
  let projectFromEnv ← env? (envVar kind name "PROJECT_ID")
  let subscription ← env? (envVar kind name "SUBSCRIPTION")
  -- 3. The manifest.
  let fromManifest ← match ← readManifest with
    | .error e     => return .error e
    | .ok none     => pure none
    | .ok (some d) =>
      match bindingOfManifest d kind name with
      | .error e => return .error e
      | .ok b    => pure b
  -- 4. The global defaults.
  let defaultProvider ← env? defaultProviderVar
  let defaultRegion ← env? defaultRegionVar
  let providerName :=
    (perResourceProvider.orElse fun _ => defaultProvider)
  let provider : Option Provider :=
    match providerName with
    | some n => Provider.ofName? n
    | none   => fromManifest.map (·.provider)
  match providerName, provider with
  | some n, none =>
    return .error
      { klass := .invalid, message := s!"unknown provider '{n}'" }
  | _, _ => pure ()
  let some provider := provider
    | return .error (Error.unbound (manifestKey kind name) (sourcesFor kind name))
  -- 5. The credentials, last.
  let region :=
    ((perResourceRegion.orElse fun _ => (fromManifest.bind (fun b =>
        if b.region.isEmpty then none else some b.region))).orElse fun _ =>
      defaultRegion).getD creds.region
  return .ok
    { provider, region
    , handle := if handle == name then (fromManifest.map (·.handle)).getD name else handle
    , url := url.orElse fun _ => fromManifest.bind (·.url)
    , projectId :=
        (projectFromEnv.orElse fun _ => fromManifest.bind (·.projectId)).orElse fun _ =>
          creds.projectId
    , subscription := subscription.orElse fun _ => fromManifest.bind (·.subscription) }

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard ResourceKind.objectStore.name == "object-store"
#guard ResourceKind.queues.name == "queues"
#guard ResourceKind.secrets.name == "secrets"

#guard manifestKey .objectStore "assets" == "object-store/assets"
#guard manifestKey .queues "jobs" == "queues/jobs"

-- A resource name becomes a legal variable name, so a bucket with dots and
-- dashes is still configurable.
#guard envName "assets" == "ASSETS"
#guard envName "my-assets.v2" == "MY_ASSETS_V2"

#guard envVar .objectStore "assets" "PROVIDER" == "LINEN_CLOUD_OBJECTSTORE_ASSETS_PROVIDER"
#guard envVar .queues "jobs" "URL" == "LINEN_CLOUD_QUEUES_JOBS_URL"

-- Every source is named in the failure message.
#guard (sourcesFor .objectStore "assets").length == 4

end Cloud
