/-
  Tests for `Cloud.Provider`.

  Three bands, in order of how much they would cost to get wrong:

  1. **The enumerations are complete.** `providers` and `localities` are written
     out because `knownRegions` and `commonLocalities` fold over them, so a
     constructor missing from a list is a silent hole rather than a compile
     error. Each is pinned one constructor at a time.
  2. **The region tables say what they claim.** Spot checks per cloud, plus the
     absences that are the interesting half — a cloud is claimed *not* to be
     somewhere, and that claim is what `Region.of` refuses on.
  3. **The portability facts.** `commonLocalities` over all three clouds, which
     is the number to know before promising a multi-cloud deployment, and the
     feature matrix that the `unsupported` error class is derived from.
-/
import Linen.Cloud.Provider

open Cloud

namespace Tests.Cloud.Provider

-- ── The enumerations are complete ───────────────────────────────────────────

#guard providers.length == 3
#guard providers.contains .aws
#guard providers.contains .gcp
#guard providers.contains .scaleway

#guard localities.length == 30
#guard localities.eraseDups.length == 30

/-- Every locality is in `localities`. Stated as a decidable proposition over
    the inductive rather than as a list literal, so a new constructor breaks
    this and not just the tables. -/
theorem localities_complete : ∀ l : Locality, localities.contains l := by
  intro l; cases l <;> decide

-- ── Provider names round-trip ───────────────────────────────────────────────

#guard Provider.aws.name == "aws"
#guard Provider.gcp.name == "gcp"
#guard Provider.scaleway.name == "scaleway"

#guard Provider.ofName? "scaleway" == some .scaleway
#guard Provider.ofName? "AWS" == none
#guard Provider.ofName? "azure" == none

/-- A misspelled provider must never silently become a working one. -/
theorem name_roundTrip : ∀ p : Provider, Provider.ofName? p.name = some p := by
  intro p; cases p <;> rfl

-- ── Region tables ───────────────────────────────────────────────────────────

#guard Locality.paris.code .aws      == some "eu-west-3"
#guard Locality.paris.code .scaleway == some "fr-par"
#guard Locality.paris.code .gcp      == some "europe-west9"

#guard Locality.milan.code .aws      == some "eu-south-1"
#guard Locality.milan.code .scaleway == some "it-mil"
#guard Locality.milan.code .gcp      == some "europe-west8"

#guard Locality.nVirginia.code .aws == some "us-east-1"
#guard Locality.tokyo.code .gcp     == some "asia-northeast1"

/- Scaleway has exactly four regions. Worth pinning: the table is small enough
    that a stray entry would go unnoticed, and code elsewhere reasons about
    Scaleway being a four-region cloud. -/
#guard (knownRegions .scaleway).length == 4
#guard knownRegions .scaleway == ["fr-par", "nl-ams", "pl-waw", "it-mil"]

/- The absences, which are the half that `Region.of` refuses on. -/
#guard Locality.amsterdam.code .aws == none
#guard Locality.warsaw.code .aws    == none
#guard Locality.ireland.code .gcp   == none
#guard Locality.tokyo.code .scaleway == none

/-- `europe-west1` is St. Ghislain in Belgium, so GCP is claimed absent from
    Ireland rather than mapped onto the nearest region. Pinned because the
    tempting "fix" is to add it. -/
theorem gcp_not_in_ireland : Locality.ireland.code .gcp = none := by decide

/-- `europe-west4` is Eemshaven, a different Dutch city from Amsterdam. -/
theorem gcp_not_in_amsterdam : Locality.amsterdam.code .gcp = none := by decide

-- ── `Region.of` checks the code against the cloud ───────────────────────────

example : Region .aws      := Region.of .aws "eu-west-3"
example : Region .scaleway := Region.of .scaleway "fr-par"
example : Region .gcp      := Region.of .gcp "europe-west9"

#guard (Region.of .aws "eu-west-3").code == "eu-west-3"

/-- An AWS code is not a Scaleway code. The obligation is decidable, so this is
    a statement about the tables rather than about a diagnostic message — it
    does not rot when a compiler version rewords its errors. -/
theorem aws_code_is_not_scaleway :
    (knownRegions .scaleway).contains "eu-west-3" ≠ true := by decide

/-- A typo in a code from the *right* cloud is caught too. -/
theorem typo_rejected : (knownRegions .scaleway).contains "fr-par-1" ≠ true := by decide

/-- `fr-par-1` is a zone inside `fr-par`; zones are deliberately not in these
    tables, and the previous theorem is what enforces it. -/
theorem zone_is_not_a_region : (knownRegions .scaleway).contains "fr-par-1" ≠ true := by decide

/- `Region.raw` takes a code on trust, which is what keeps a stale table from
    being a hard block. -/
#guard (Region.raw .aws "us-gov-west-1").code == "us-gov-west-1"
#guard (knownRegions .aws).contains "us-gov-west-1" == false

-- ── Portability ─────────────────────────────────────────────────────────────

/- **All three clouds overlap in exactly two places.** The headline
    portability fact: a deployment that must run on AWS, GCP and Scaleway alike
    can be in Paris or Milan, and nowhere else. -/
#guard commonLocalities providers == [.paris, .milan]

#guard (commonLocalities [.aws, .gcp]).length == 19
#guard (commonLocalities [.aws, .scaleway]) == [.paris, .milan]

#guard Locality.paris.providers == [.aws, .gcp, .scaleway]
#guard Locality.amsterdam.providers == [.scaleway]
#guard Locality.ireland.providers == [.aws]

/- A locality no cloud in the list serves yields no region, rather than an
    invented code. -/
#guard (Locality.warsaw.region? .aws).isNone
#guard (Locality.paris.region? .scaleway).map Region.code == some "fr-par"

-- ── Feature matrix ──────────────────────────────────────────────────────────

/- The three clouds agree on object versioning and binary secrets. -/
#guard Provider.aws.supports .objectVersioning == true
#guard Provider.gcp.supports .objectVersioning == true
#guard Provider.scaleway.supports .objectVersioning == true

#guard Provider.aws.supports .secretBinary == true
#guard Provider.gcp.supports .secretBinary == true
#guard Provider.scaleway.supports .secretBinary == true

/- The queue rows are where the clouds genuinely differ, and they are the
    reason `Cloud.Queue` splits `Producer` from `Consumer`: Pub/Sub is a
    topic-plus-subscription system, so a long-poll parameter and a per-pull
    lease have nowhere to go. -/
#guard Provider.aws.supports .queueLongPoll == true
#guard Provider.scaleway.supports .queueLongPoll == true
#guard Provider.gcp.supports .queueLongPoll == false

#guard Provider.aws.supports .queuePerReceiveLease == true
#guard Provider.gcp.supports .queuePerReceiveLease == false

/- Purging is reported absent on GCP because `subscriptions.seek` is
    unverified here, not because it is known impossible. Recorded in
    `docs/imports/infra/dependencies.md`. -/
#guard Provider.gcp.supports .queuePurge == false
#guard Provider.aws.supports .queuePurge == true

/- Presigned URLs follow SigV4, so the two S3-compatible clouds have them and
    GCS — which signs by another scheme — does not. -/
#guard Provider.aws.supports .presignedUrl == true
#guard Provider.scaleway.supports .presignedUrl == true
#guard Provider.gcp.supports .presignedUrl == false

/-- GCP is the only cloud with a feature gap in this table, which is what makes
    it the one the portable interfaces bend around. -/
theorem gcp_is_the_awkward_one :
    ([Feature.queueLongPoll, .queuePerReceiveLease, .queuePurge, .presignedUrl]).all
      (fun f => !Provider.gcp.supports f) = true := by decide

end Tests.Cloud.Provider
