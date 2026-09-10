/-
  `Cloud.Provider` — which cloud, and where in it

  ## Two ways to name a place, one of them portable

  A `Locality` is a place — `.paris` — and each cloud maps it to its own code
  (`eu-west-3` on AWS, `fr-par` on Scaleway, `europe-west9` on GCP) or to
  nothing at all, because no cloud is everywhere. A `Region p` is one cloud's
  own code, *indexed by the cloud it belongs to*, so an AWS region cannot reach
  Scaleway: the index rules that out by typing, and `Region.of` adds the weaker,
  decidable check that the code is one that cloud actually has.

  Both are checked when the term is elaborated, never when a request is sent:
  `Region.of .scaleway "eu-west-3"` is a compile error, not a DNS failure.

  The escape hatch is `Region.raw`, and it exists because `knownRegions` is
  derived from a hand-maintained table: a region no locality names — a GovCloud
  region, or one a cloud added this morning — would otherwise be unwritable, and
  a stale table must never be a hard block. It is spelled differently from
  `Region.of` so that reaching for it is visible rather than silent.

  ## Regions, not zones

  `fr-par-1` is an availability zone inside `fr-par` and is not a value that
  belongs in these tables. Zone-scoped products exist on every cloud, but a
  zone is not a property of a *cloud*, and the object, queue and secret services
  this namespace serves are all regional.

  ## Why the code tables are written out rather than defaulted

  Every match over `Locality` here is total, with the absent cases *grouped*
  rather than covered by a wildcard. Adding a `Locality` must therefore fail to
  compile until all three clouds have been asked about it, because a silent
  `none` is a claim that a cloud is absent from a place — and that claim should
  be made deliberately or not at all.

  ## Provider facts go stale

  The tables below are a snapshot, checked on the dates their doc-comments name.
  Clouds add regions; this file will be behind at some point. That is what
  `Region.raw` is for, and it is why nothing in this namespace treats an unknown
  region as an error at request time.

  ## Provenance

  Moved down from the sibling `typednotes/infra`, which had these tables in
  `Infra/Core/{Kind,Region}.lean` and now uses these instead — a cloud's region
  codes are a building block, not an infrastructure-as-code concern. What stayed
  behind is everything that *is* one: infra's `Kind`, `Handle` and `ObservedOf`
  families, its per-resource placement map, and the fleet-coverage checks that
  read it.
-/

namespace Cloud

-- ── Providers ───────────────────────────────────────────────────────────────

/-- A supported cloud.

    Three constructors, and adding a fourth is deliberately *mechanical and
    loud* rather than a plugin boundary: every total match in this namespace
    fails to compile until the new cloud has been handled, which is the point.
    Azure and OVH are not supported. -/
inductive Provider
  /-- Amazon Web Services. -/
  | aws
  /-- Google Cloud Platform. -/
  | gcp
  /-- Scaleway. -/
  | scaleway
  deriving Repr, DecidableEq, BEq, Hashable

/-- Every supported cloud. Written out, and pinned by `#guard`s in the tests
    against each constructor, so it cannot fall behind the inductive. -/
def providers : List Provider := [.aws, .gcp, .scaleway]

/-- The cloud's short lowercase name, as its own CLI and documentation spell
    it. Used in diagnostics and in `Cloud.Binding`'s environment-variable
    names. -/
def Provider.name : Provider → String
  | .aws      => "aws"
  | .gcp      => "gcp"
  | .scaleway => "scaleway"

/-- Parse a cloud's short name, for reading configuration.

    `Option` rather than a default: a misspelled provider must not silently
    become AWS. -/
def Provider.ofName? (s : String) : Option Provider :=
  providers.find? (fun p => p.name == s)

-- ── Localities ──────────────────────────────────────────────────────────────

/-- A place a cloud may or may not have a region in.

    Named the way each cloud's own documentation names the place, which is why
    a few are countries (`.ireland`, `.spain`, `.uae`) or a compass direction
    (`.canadaCentral`) rather than cities: AWS's `eu-west-1` is "Europe
    (Ireland)" and its `ca-central-1` is "Canada (Central)" — it never says
    Dublin or Montréal, and `.montreal` would additionally have collided with
    `ca-west-1`, "Canada West (Calgary)".

    The list is the *union* of what the three clouds offer, so most entries
    exist on exactly one of them. That asymmetry is the useful part:
    `.warsaw` is a Scaleway and GCP region but not an AWS one, `.ireland` the
    reverse.

    **Not exhaustive, and a snapshot.** AWS's opt-in regions beyond these,
    GovCloud and China are reachable through `Region.raw`. -/
inductive Locality
  -- Europe
  | paris | amsterdam | warsaw | ireland | london
  | frankfurt | zurich | stockholm | milan | spain
  -- Americas
  | nVirginia | ohio | nCalifornia | oregon
  | canadaCentral | calgary | mexicoCentral | saoPaulo
  -- Middle East and Africa
  | uae | telAviv | capeTown
  -- Asia Pacific
  | tokyo | osaka | seoul | mumbai | hyderabad
  | singapore | jakarta | sydney | hongKong
  deriving Repr, DecidableEq, BEq

/-- Every locality. Written out because `knownRegions` folds over it; the tests
    pin it against the inductive one constructor at a time. -/
def localities : List Locality :=
  [ .paris, .amsterdam, .warsaw, .ireland, .london
  , .frankfurt, .zurich, .stockholm, .milan, .spain
  , .nVirginia, .ohio, .nCalifornia, .oregon
  , .canadaCentral, .calgary, .mexicoCentral, .saoPaulo
  , .uae, .telAviv, .capeTown
  , .tokyo, .osaka, .seoul, .mumbai, .hyderabad
  , .singapore, .jakarta, .sydney, .hongKong ]

/-- AWS's code for a place, or `none` where AWS is not.

    Checked against AWS's "Regions and Zones" table on 2026-09-05. -/
private def awsCode : Locality → Option String
  | .paris         => some "eu-west-3"
  | .ireland       => some "eu-west-1"
  | .london        => some "eu-west-2"
  | .frankfurt     => some "eu-central-1"
  | .zurich        => some "eu-central-2"
  | .stockholm     => some "eu-north-1"
  | .milan         => some "eu-south-1"
  | .spain         => some "eu-south-2"
  | .nVirginia     => some "us-east-1"
  | .ohio          => some "us-east-2"
  | .nCalifornia   => some "us-west-1"
  | .oregon        => some "us-west-2"
  | .canadaCentral => some "ca-central-1"
  | .calgary       => some "ca-west-1"
  | .mexicoCentral => some "mx-central-1"
  | .saoPaulo      => some "sa-east-1"
  | .uae           => some "me-central-1"
  | .telAviv       => some "il-central-1"
  | .capeTown      => some "af-south-1"
  | .tokyo         => some "ap-northeast-1"
  | .osaka         => some "ap-northeast-3"
  | .seoul         => some "ap-northeast-2"
  | .mumbai        => some "ap-south-1"
  | .hyderabad     => some "ap-south-2"
  | .singapore     => some "ap-southeast-1"
  | .jakarta       => some "ap-southeast-3"
  | .sydney        => some "ap-southeast-2"
  | .hongKong      => some "ap-east-1"
  -- AWS has no region in either.
  | .amsterdam | .warsaw => none

/-- Scaleway's code for a place, or `none` where Scaleway is not.

    Checked against Scaleway's VPC and Instance API region parameters on
    2026-09-05: four regions, `it-mil` having opened in March 2026. Scaleway
    has announced Sweden and Germany next, and `.stockholm` and `.frankfurt`
    are already in the `Locality` table waiting for them.

    The absent cases are grouped into one arm rather than wildcarded, so that
    adding a `Locality` still breaks this match. -/
private def scalewayCode : Locality → Option String
  | .paris     => some "fr-par"
  | .amsterdam => some "nl-ams"
  | .warsaw    => some "pl-waw"
  | .milan     => some "it-mil"
  | .ireland | .london | .frankfurt | .zurich | .stockholm | .spain
  | .nVirginia | .ohio | .nCalifornia | .oregon
  | .canadaCentral | .calgary | .mexicoCentral | .saoPaulo
  | .uae | .telAviv | .capeTown
  | .tokyo | .osaka | .seoul | .mumbai | .hyderabad
  | .singapore | .jakarta | .sydney | .hongKong => none

/-- GCP's code for a place, or `none` where GCP is not.

    Sparser than AWS's *on purpose*. GCP puts several regions in places this
    enumeration does not name, and mapping those onto the nearest locality
    would be wrong in the way that actually matters, since the whole point of a
    locality is that it names one place. Four of the `none`s below are traps
    rather than gaps:

    - `.amsterdam` — `europe-west4` is **Eemshaven**, a different Dutch city.
    - `.ireland` — `europe-west1` is St. Ghislain, **Belgium**; Google has a
      Dublin data centre but no GCP region behind it.
    - `.spain` — `europe-southwest1` is **Madrid**, while AWS's `eu-south-2` is
      Aragón, so these are not the same place.
    - `.canadaCentral` — `northamerica-northeast1` is **Montréal**, while AWS
      names its Canadian region only "Canada (Central)".

    Where GCP is somewhere this enumeration has no name for, the answer is
    `none` and the escape hatch is `Region.raw`; adding the locality is the real
    fix.

    Checked against Google's Cloud Run, Cloud Storage and region-carbon
    location lists on 2026-09-05, which agree on all of these. GCP has 43 GA
    regions; this names 20 of them, being the ones whose place the `Locality`
    table already has a name for. -/
private def gcpCode : Locality → Option String
  | .paris      => some "europe-west9"
  | .london     => some "europe-west2"
  | .frankfurt  => some "europe-west3"
  | .zurich     => some "europe-west6"
  | .milan      => some "europe-west8"
  | .warsaw     => some "europe-central2"
  | .nVirginia  => some "us-east4"
  | .ohio       => some "us-east5"
  | .oregon     => some "us-west1"
  | .saoPaulo   => some "southamerica-east1"
  | .tokyo      => some "asia-northeast1"
  | .osaka      => some "asia-northeast2"
  | .seoul      => some "asia-northeast3"
  | .mumbai     => some "asia-south1"
  | .singapore  => some "asia-southeast1"
  | .jakarta    => some "asia-southeast2"
  | .sydney     => some "australia-southeast1"
  | .hongKong   => some "asia-east2"
  | .telAviv    => some "me-west1"
  | .stockholm  => some "europe-north2"
  -- Somewhere else, or somewhere this enumeration does not name. Grouped
  -- rather than wildcarded, so adding a locality still breaks this match.
  | .amsterdam | .ireland | .spain
  | .nCalifornia | .canadaCentral | .calgary | .mexicoCentral
  | .uae | .capeTown | .hyderabad => none

/-- What this cloud calls this place, if it is there at all. -/
def Locality.code (l : Locality) : Provider → Option String
  | .aws      => awsCode l
  | .gcp      => gcpCode l
  | .scaleway => scalewayCode l

/-- The clouds that have a region at this place. -/
def Locality.providers (l : Locality) : List Provider :=
  Cloud.providers.filter (fun p => (l.code p).isSome)

/-- The places every one of `ps` has a region in — where a genuinely portable
    deployment can go.

    For all three clouds this is a two-element list, which is worth knowing
    before promising a multi-cloud deployment anywhere else. The tests pin the
    value. -/
def commonLocalities (ps : List Provider) : List Locality :=
  localities.filter (fun l => ps.all (fun p => (l.code p).isSome))

-- ── Regions ─────────────────────────────────────────────────────────────────

/-- A region code, indexed by the cloud it belongs to.

    The index is what stops `eu-west-3` reaching Scaleway *by typing*; the
    constructors below add the weaker, decidable check that the code is one
    that cloud actually has. -/
structure Region (p : Provider) where
  /-- The cloud's own region code, as it appears in a hostname or a URL path. -/
  code : String
  deriving Repr, DecidableEq

/-- Every region code the `Locality` table names for a cloud.

    Derived from that table rather than written out a second time, so the two
    cannot drift: every region with a code here has a portable name, and one
    without a portable name needs `Region.raw`. -/
def knownRegions (p : Provider) : List String :=
  localities.filterMap (Locality.code · p)

/-- A cloud's own region code, checked against `knownRegions` at elaboration.

    Catches both halves of "compatible": a code from the wrong cloud, and a
    typo in a code from the right one. Neither survives to a DNS failure. -/
def Region.of (p : Provider) (code : String)
    (_h : (knownRegions p).contains code = true := by decide) : Region p :=
  ⟨code⟩

/-- A region code taken on trust.

    `knownRegions` is a hand-maintained table and clouds add regions, so there
    has to be a way past it — but it is spelled differently from `Region.of`,
    so reaching for it is visible at the call site rather than silent. -/
def Region.raw (p : Provider) (code : String) : Region p := ⟨code⟩

/-- The region a cloud puts this place in, when it has one.

    The `Option` is the honest answer to "deploy this to Warsaw on AWS": there
    is no such region, and no code should be invented for it. -/
def Locality.region? (l : Locality) (p : Provider) : Option (Region p) :=
  (l.code p).map (Region.raw p)

-- ── Feature matrix ──────────────────────────────────────────────────────────

/-- A capability of a cloud's data plane that the *portable* interfaces cannot
    assume.

    This is a plain data table, deliberately **not** a `Prop`-class. A
    compile-time `Supports` class would have to put the provider inside the
    capability that indexes an effect, which would destroy the property this
    namespace exists for — that one `Eff [ObjectStore cap] α` program runs
    against any cloud. So feature gaps are handled three ways instead, in
    descending order of preference:

    1. The portable interface contains only what all three clouds genuinely do,
       which is what "portable" means.
    2. Anything provider-specific lives on the provider's own module, so
       choosing it is visible in the source.
    3. What is left becomes the total value `Error.Class.unsupported` in an
       `Except` — never a raised exception.

    This table is the single source of truth for (3), and every row is pinned
    by a `#guard`. -/
inductive Feature
  /-- Per-object version history (`x-amz-version-id`, GCS generations). -/
  | objectVersioning
  /-- A receive call that blocks server-side until a message arrives — SQS's
      `WaitTimeSeconds`. Pub/Sub's pull blocks by its own rules and takes no
      such parameter. -/
  | queueLongPoll
  /-- A lease duration chosen per receive call — SQS's per-request
      `VisibilityTimeout`. On Pub/Sub the ack deadline is a property of the
      subscription, so there is nothing to set per pull. -/
  | queuePerReceiveLease
  /-- Discarding every message in one call — SQS's `PurgeQueue`. -/
  | queuePurge
  /-- Storing a secret whose value is not valid UTF-8. -/
  | secretBinary
  /-- A time-limited URL that grants access without credentials. SigV4
      query-string signing covers the S3-compatible clouds; GCS signed URLs use
      a different scheme and are not implemented. -/
  | presignedUrl
  deriving Repr, DecidableEq, BEq

/-- Whether this cloud's data plane offers this feature.

    Where a row is an inference rather than a
    verified fact, the module that depends on it says so in its own
    doc-comment. -/
def Provider.supports : Provider → Feature → Bool
  | .aws, .objectVersioning      => true
  | .aws, .queueLongPoll         => true
  | .aws, .queuePerReceiveLease  => true
  | .aws, .queuePurge            => true
  | .aws, .secretBinary          => true
  | .aws, .presignedUrl          => true
  -- Scaleway's Object Storage and Queues are S3- and SQS-compatible, so they
  -- answer as AWS does. Its Secret Manager stores a base64 payload, hence
  -- binary secrets. It has no presigned-URL story of its own, but SigV4
  -- query-string signing against its S3 endpoint is the same construction.
  | .scaleway, .objectVersioning     => true
  | .scaleway, .queueLongPoll        => true
  | .scaleway, .queuePerReceiveLease => true
  | .scaleway, .queuePurge           => true
  | .scaleway, .secretBinary         => true
  | .scaleway, .presignedUrl         => true
  -- GCP is the cloud the portable interfaces bend around. Pub/Sub is a
  -- topic-plus-subscription system rather than a queue: its pull takes no
  -- long-poll parameter, its ack deadline belongs to the subscription rather
  -- than the pull, and purging is at best `subscriptions.seek` to now, which
  -- is unverified here and so reported as absent. Cloud Storage signed URLs
  -- use a scheme unrelated to SigV4.
  | .gcp, .objectVersioning     => true
  | .gcp, .secretBinary         => true
  | .gcp, .queueLongPoll        => false
  | .gcp, .queuePerReceiveLease => false
  | .gcp, .queuePurge           => false
  | .gcp, .presignedUrl         => false

end Cloud
