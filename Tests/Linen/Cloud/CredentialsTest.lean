/-
  Tests for `Cloud.Credentials`.

  The pure rules are asserted with `#guard`; the file sources are driven against
  a scratch directory under `.lake/build`, which is what parameterising `Paths`
  rather than reading `$HOME` at the point of use makes possible.

  The environment source is deliberately **not** driven end to end: Lean has no
  `setenv`, so it cannot be. That is precisely why `normalizeEnv` is a separate
  pure function — the rule that actually causes trouble in CI is checkable even
  though the reader around it is not.
-/
import Linen.Cloud.Credentials

open Cloud

namespace Tests.Cloud.Credentials

-- ── "Set but empty" means unset ─────────────────────────────────────────────

/- The rule that prevents a whole class of CI failure. GitHub Actions binds an
   *undefined* secret to the empty string rather than omitting the variable, so
   a naive read yields credentials with an empty access key — which fails much
   later, inside a TLS handshake, with nothing pointing at the cause. -/
#guard normalizeEnv (some "") == none
#guard normalizeEnv (some "   ") == none
#guard normalizeEnv (some "\t\n") == none
#guard normalizeEnv none == none
#guard normalizeEnv (some "AKIAIOSFODNN7EXAMPLE") == some "AKIAIOSFODNN7EXAMPLE"

/- Whitespace *around* a real value is kept, not trimmed: only the
   entirely-blank case is treated as absent. Trimming would silently repair a
   genuinely malformed secret and make the resulting signature failure
   inexplicable. -/
#guard normalizeEnv (some " AKIA ") == some " AKIA "

-- ── The AWS two-file profile split ──────────────────────────────────────────

/- `~/.aws/credentials` names a profile `[dev]` while `~/.aws/config` names the
   same one `[profile dev]` — except the default, which is `[default]` in both.
   Getting this wrong loses the region silently. -/
#guard awsConfigSection "default" == "default"
#guard awsConfigSection "dev" == "profile dev"
#guard awsConfigSection "production" == "profile production"

-- ── Environment variable names ──────────────────────────────────────────────

#guard (envVars .aws).1 == "AWS_ACCESS_KEY_ID"
#guard (envVars .aws).2.1 == "AWS_SECRET_ACCESS_KEY"
#guard (envVars .aws).2.2.1 == "AWS_REGION"
#guard (envVars .aws).2.2.2 == some "AWS_SESSION_TOKEN"

#guard (envVars .scaleway).1 == "SCW_ACCESS_KEY"
#guard (envVars .scaleway).2.2.1 == "SCW_DEFAULT_REGION"

/- Scaleway has no session-token concept, so there is no variable to read. -/
#guard (envVars .scaleway).2.2.2 == none

/- GCP has no key pair at all: the first two names are empty, and
   `fromEnvironment` branches on the cloud rather than requiring them. -/
#guard (envVars .gcp).1 == ""
#guard (envVars .gcp).2.1 == ""
#guard (envVars .gcp).2.2.2 == some "GOOGLE_OAUTH_ACCESS_TOKEN"

-- ── Paths ───────────────────────────────────────────────────────────────────

#guard (Paths.under "/home/dev").awsCredentials.toString == "/home/dev/.aws/credentials"
#guard (Paths.under "/home/dev").awsConfig.toString == "/home/dev/.aws/config"
#guard (Paths.under "/home/dev").scwConfig.toString == "/home/dev/.config/scw/config.yaml"

-- ── Secrets do not render ───────────────────────────────────────────────────

/-- A credential with every secret field populated. -/
def loud : Credentials :=
  { accessKey := "AKIAIOSFODNN7EXAMPLE"
  , secretKey := "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  , region := "eu-west-3"
  , sessionToken := some "FwoGZXIvYXdzEBYaDHNlY3JldA=="
  , projectId := some "8460bf58-4c44-431e-9df4-8eae3888b1ce"
  , accessToken := some "ya29.a0AfB_byC-not-a-real-token" }

/- **No secret survives rendering.** Asserted as absence of the literal secret
   in the output, over each of the three secret-bearing fields, because a
   redaction that covers two of three is worse than none — it teaches the
   reader that printing is safe. -/
#guard ((toString (repr loud)).splitOn "wJalrXUtnFEMI").length == 1
#guard ((toString (repr loud)).splitOn "FwoGZXIvYXdz").length == 1
#guard ((toString (repr loud)).splitOn "ya29.a0AfB").length == 1
#guard ((toString loud).splitOn "wJalrXUtnFEMI").length == 1

/- The access key *does* render: it is an identifier that travels in every
   signed request, and seeing it is how one tells which credential is in play.
   Redacting it would make a misconfigured-profile diagnosis impossible. -/
#guard ((toString (repr loud)).splitOn "AKIAIOSFODNN7EXAMPLE").length == 2

/- A present secret renders as `<redacted>` rather than being omitted, so the
   reader can tell "set but hidden" from "not set". -/
#guard ((toString (repr loud)).splitOn "<redacted>").length == 4
#guard ((toString (repr ({} : Credentials))).splitOn "none").length == 5

-- ── `canSign` ───────────────────────────────────────────────────────────────

#guard loud.canSign == true
#guard ({ accessToken := some "ya29." } : Credentials).canSign == false
#guard ({ accessKey := "AKIA" } : Credentials).canSign == false
#guard ({ secretKey := "s" } : Credentials).canSign == false

-- ── Asking for the parts a cloud needs ──────────────────────────────────────

#guard (loud.requireRegion .aws).toOption == some "eu-west-3"
#guard (({ region := "" } : Credentials).requireRegion .aws).toOption == none
#guard (loud.requireToken .gcp).toOption == some "ya29.a0AfB_byC-not-a-real-token"
#guard (({} : Credentials).requireToken .gcp).toOption == none
#guard (loud.requireProject).toOption == some "8460bf58-4c44-431e-9df4-8eae3888b1ce"
#guard (({} : Credentials).requireOrganization).toOption == none

/- Every failure names what to set. A message that says only "no region" sends
   the reader to the source code. -/
#guard match ({ region := "" } : Credentials).requireRegion .scaleway with
  | .error e => (e.message.splitOn "SCW_DEFAULT_REGION").length == 2 && e.klass == .unbound
  | .ok _ => false

#guard match ({} : Credentials).requireToken .gcp with
  | .error e => (e.message.splitOn "GOOGLE_APPLICATION_CREDENTIALS").length == 2
  | .ok _ => false

-- ── Source descriptions ─────────────────────────────────────────────────────

/- Every source the chain tries must appear in the not-found message: a source
   the operator is never told about is a source they cannot use. Three for the
   key-pair clouds, four for GCP. -/
#guard (sourceDescriptions (Paths.under "/h") .aws "default").length == 3
#guard (sourceDescriptions (Paths.under "/h") .scaleway "default").length == 3
#guard (sourceDescriptions (Paths.under "/h") .gcp "default").length == 4

#guard (sourceDescriptions (Paths.under "/h") .aws "dev").head!
  == "config file /h/.aws/credentials (profile [dev])"

/- The keychain account is named per cloud, under the service name this library
   claims. -/
#guard (((sourceDescriptions (Paths.under "/h") .scaleway "default")[1]!).splitOn "'linen'").length == 2

/- GCP lists the key file first, because that is the order it is tried in. -/
#guard (sourceDescriptions (Paths.under "/h") .gcp "default").head!
  == "a service-account key file named by GOOGLE_APPLICATION_CREDENTIALS"

/- The message quotes every source, so it is as long as the list. -/
#guard ((noCredentialsMessage (Paths.under "/h") .gcp "default").splitOn "\n  - ").length == 5

-- ── The file sources, against a scratch directory ───────────────────────────

/-- A scratch directory under the build output, so the file sources can be
    driven for real. Deterministic and gitignored. -/
def scratch : System.FilePath := ".lake" / "build" / "test-scratch" / "cloud-credentials"

/-- Write the two AWS files and the Scaleway one, then read them back through
    the real loaders. -/
def writeFixtures : IO Paths := do
  let paths := Paths.under scratch
  IO.FS.createDirAll (scratch / ".aws")
  IO.FS.createDirAll (scratch / ".config" / "scw")
  -- Two profiles, and the region deliberately only in `config` for `dev` — the
  -- split the `awsConfigSection` rule exists for.
  IO.FS.writeFile paths.awsCredentials
    "[default]\n\
     aws_access_key_id = AKIADEFAULT\n\
     aws_secret_access_key = defaultsecret\n\
     \n\
     [dev]\n\
     aws_access_key_id = AKIADEV\n\
     aws_secret_access_key = devsecret\n\
     aws_session_token = devtoken\n"
  IO.FS.writeFile paths.awsConfig
    "[default]\n\
     region = us-east-1\n\
     \n\
     [profile dev]\n\
     region = eu-west-3\n"
  IO.FS.writeFile paths.scwConfig
    "access_key: SCWACCESSKEY\n\
     secret_key: scw-secret-key\n\
     default_region: fr-par\n\
     default_project_id: 8460bf58-4c44-431e-9df4-8eae3888b1ce\n\
     default_organization_id: 11111111-2222-3333-4444-555555555555\n"
  return paths

/-- info: (some "AKIADEFAULT", some "us-east-1", none) -/
#guard_msgs in
#eval show IO (Option String × Option String × Option String) from do
  let paths ← writeFixtures
  let c ← fromAwsFiles paths "default"
  return (c.map (·.accessKey), c.map (·.region), c.bind (·.sessionToken))

/- The `dev` profile: keys from `[dev]` in `credentials`, region from
   `[profile dev]` in `config`, and the session token that only `credentials`
   carries. Reading `[dev]` in `config` instead would lose the region. -/
/-- info: (some "AKIADEV", some "eu-west-3", some "devtoken") -/
#guard_msgs in
#eval show IO (Option String × Option String × Option String) from do
  let paths ← writeFixtures
  let c ← fromAwsFiles paths "dev"
  return (c.map (·.accessKey), c.map (·.region), c.bind (·.sessionToken))

/- A profile that is not in the file is `none`, not an error and not the
   default profile's keys. -/
/-- info: none -/
#guard_msgs in
#eval show IO (Option String) from do
  let paths ← writeFixtures
  let c ← fromAwsFiles paths "nonexistent"
  return c.map (·.accessKey)

/-- info: (some "SCWACCESSKEY", some "fr-par", some "8460bf58-4c44-431e-9df4-8eae3888b1ce") -/
#guard_msgs in
#eval show IO (Option String × Option String × Option String) from do
  let paths ← writeFixtures
  let c ← fromScalewayFile paths
  return (c.map (·.accessKey), c.map (·.region), c.bind (·.projectId))

/- Scaleway's organization id, which only IAM needs. -/
/-- info: some "11111111-2222-3333-4444-555555555555" -/
#guard_msgs in
#eval show IO (Option String) from do
  let paths ← writeFixtures
  let c ← fromScalewayFile paths
  return c.bind (·.organizationId)

/- A missing file is `none` — a source with nothing to offer, which must fall
   through to the next rather than abort the chain. -/
/-- info: (none, none) -/
#guard_msgs in
#eval show IO (Option String × Option String) from do
  let empty := Paths.under (scratch / "does-not-exist")
  let a ← fromAwsFiles empty "default"
  let s ← fromScalewayFile empty
  return (a.map (·.accessKey), s.map (·.accessKey))

/- A file that is *present but malformed* raises rather than falling through:
   silently skipping a config file with a typo in it looks exactly like having
   no credentials at all, and that is the failure that costs an afternoon. -/
/-- info: "raised" -/
#guard_msgs in
#eval show IO String from do
  IO.FS.createDirAll (scratch / "bad" / ".config" / "scw")
  let paths := Paths.under (scratch / "bad")
  IO.FS.writeFile paths.scwConfig "access_key: [unclosed\n"
  try
    let _ ← fromScalewayFile paths
    return "fell through"
  catch _ => return "raised"

/- The keychain-free chain over a directory with nothing in it fails with the
   message that names every source. `.unbound` rather than a raise, so a
   caller can decide what to do. -/
/-- info: (Cloud.Class.unbound, 3) -/
#guard_msgs in
#eval show IO (Class × Nat) from do
  let empty := Paths.under (scratch / "does-not-exist")
  match ← loadFrom empty .aws with
  | .ok _ => return (.protocol, 0)
  | .error e => return (e.klass, (e.message.splitOn "\n  - ").length - 1)

end Tests.Cloud.Credentials
