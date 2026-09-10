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
