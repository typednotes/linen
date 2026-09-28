/-
  Tests for `Cloud.Credentials.Keychain`.

  The platform store itself is not exercised: a CI runner has no unlocked
  keychain, and a test that needed one would fail everywhere it matters. What is
  exercised is the part that would silently corrupt a stored credential — the
  INI body — and it is a pure function precisely so that it can be.

  The round-trip is the test worth having: `render` then `Data.Ini.parse` must
  return every field, because a write this module can produce and not read back
  is a credential lost with no diagnostic.
-/
import Linen.Cloud.Credentials.Keychain

open Cloud Cloud.Credentials.Keychain

namespace Tests.Cloud.Credentials.Keychain

-- ── The stored body ─────────────────────────────────────────────────────────

/- The three required keys, and nothing else, for a minimal credential. -/
#guard render { accessKey := "AKIA", secretKey := "s", region := "eu-west-3" }
  == "access_key = AKIA\nsecret_key = s\nregion = eu-west-3\n"

/- The optional fields appear only when set, so an AWS entry is not littered
   with empty Scaleway keys. -/
#guard ((render { accessKey := "AKIA", secretKey := "s" }).splitOn "project_id").length == 1
#guard ((render { accessKey := "AKIA", secretKey := "s" }).splitOn "session_token").length == 1

/-- Every field populated — a temporary Scaleway credential, the busiest shape
    this module stores. -/
def full : Credentials :=
  { accessKey := "SCWACCESSKEY"
  , secretKey := "scw-secret"
  , region := "fr-par"
  , sessionToken := some "tok"
  , projectId := some "8460bf58-4c44-431e-9df4-8eae3888b1ce"
  , organizationId := some "11111111-2222-3333-4444-555555555555" }

/- **The round trip.** A body this module writes must be one it reads back, or a
   stored credential is lost silently. Asserted field by field rather than by
   comparing rendered strings, which would pass even if `parse` dropped a key. -/
#guard match Data.Ini.parse (render full) with
  | .error _ => false
  | .ok ini =>
    ini.lookupGlobal "access_key" == some "SCWACCESSKEY"
    && ini.lookupGlobal "secret_key" == some "scw-secret"
    && ini.lookupGlobal "region" == some "fr-par"
    && ini.lookupGlobal "session_token" == some "tok"
    && ini.lookupGlobal "project_id" == some "8460bf58-4c44-431e-9df4-8eae3888b1ce"
    && ini.lookupGlobal "organization_id" == some "11111111-2222-3333-4444-555555555555"

/- A secret containing an `=` survives: `Data.Ini` splits on the *first*
   separator, so a base64 session token keeps its padding. This is the failure
   mode a naive `splitOn "="` would introduce, and it would only show up for
   some tokens. -/
#guard match Data.Ini.parse (render { accessKey := "A", secretKey := "s"
                                    , sessionToken := some "FwoGZXIvYXdzEBYaDHNlY3JldA==" }) with
  | .error _ => false
  | .ok ini => ini.lookupGlobal "session_token" == some "FwoGZXIvYXdzEBYaDHNlY3JldA=="

-- ── The round trip through `parseBody`, not just through `Data.Ini` ─────────

/- The tests above check that `render` writes a body `Data.Ini` can read. That
   is not the same as checking this module reads its *own* body back, and the
   difference hid two defects: `render` omitted `access_token` entirely, and
   the reader accepted blank fields as values. `parseBody` is pure precisely so
   this can be asserted here. -/

/-- A GCP credential: the key pair is legitimately empty and the bearer token
    is the whole thing. This shape used to round-trip to `none`. -/
def gcpCred : Credentials :=
  { accessToken := some "ya29.a0AfH6SMB"
  , projectId := some "my-project"
  , region := "europe-west1" }

/- The token survives the round trip. Before 0.18.0 `render` never wrote it, so
   a stored GCP credential came back without the only field it had — and the
   chain, having found *a* credential, stopped looking. -/
#guard match parseBody (render gcpCred) with
  | some c => c.accessToken == some "ya29.a0AfH6SMB"
  | none   => false

/- And the rest of it, so a fix that dropped a different field would fail. -/
#guard match parseBody (render gcpCred) with
  | some c => c.projectId == some "my-project" && c.region == "europe-west1"
  | none   => false

/- The busiest shape also survives whole, token included. -/
#guard match parseBody (render { full with accessToken := some "tok-2" }) with
  | some c =>
    c.accessKey == "SCWACCESSKEY" && c.secretKey == "scw-secret"
      && c.region == "fr-par" && c.sessionToken == some "tok"
      && c.accessToken == some "tok-2"
      && c.projectId == some "8460bf58-4c44-431e-9df4-8eae3888b1ce"
      && c.organizationId == some "11111111-2222-3333-4444-555555555555"
  | none => false

/- **Set but empty means unset**, the same rule the environment source follows.
   An entry whose fields are present but blank is not a credential; answering
   one would fail much later inside a handshake, having skipped the sources
   that would have worked. -/
#guard (parseBody "access_key =\nsecret_key =\n").isNone

/- Blank optional fields do not become `some ""` either. -/
#guard match parseBody "access_key = AKIA\nsecret_key = s\nsession_token =\n" with
  | some c => c.sessionToken == none && c.accessKey == "AKIA"
  | none   => false

/- An entry carrying only a token is usable — that is exactly GCP — while one
   carrying nothing is not. -/
#guard (parseBody "access_token = ya29.x\n").isSome
#guard (parseBody "region = eu-west-3\n").isNone

/- An unparseable body is `none`, so the chain falls through rather than
   aborting. -/
#guard (parseBody "\x00 not ini [[[").isNone

-- ── The service name this library claims ────────────────────────────────────

/- Named `linen`, not `infra`: an entry is addressed by (service, account), so
   two libraries sharing a service name would read each other's credentials. -/
#guard keychainService == "linen"

/- The accounts `forProvider` reads are the cloud names, and `sourceDescriptions`
   promises exactly these — a mismatch would advertise a source that is never
   consulted. -/
#guard Provider.aws.name == "aws"
#guard Provider.gcp.name == "gcp"
#guard Provider.scaleway.name == "scaleway"

end Tests.Cloud.Credentials.Keychain
