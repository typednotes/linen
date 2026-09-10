/-
  `Cloud.Credentials` — finding the keys, from wherever the operator put them

  ## One structure for three clouds that authenticate differently

  AWS and Scaleway keep a **long-lived key pair** on disk and sign each request
  from it. GCP keeps no such pair: its credential is a **short-lived bearer
  token**, and there is nothing to derive a signature from. So `accessKey` and
  `secretKey` are empty for GCP and `accessToken` carries the credential
  instead, which is why one structure with optional fields beats three types —
  every call site downstream takes `Credentials` and asks for the part its
  cloud uses.

  ## Three sources, first hit wins

  1. **The official CLI's own config file.** `~/.aws/credentials` plus
     `~/.aws/config`, `~/.config/scw/config.yaml`, or `gcloud`. Whatever the
     cloud's own tool has already set up should simply work — this is the
     source a developer expects and the one they never had to configure.
  2. **The OS credential store**, in `Cloud.Credentials.Keychain`. Kept in a
     separate module so that importing `Cloud.*` does not drag the
     Security.framework / libsecret FFI into a program that authenticates from
     the environment.
  3. **The environment.** What CI uses.

  A source that is merely *absent* falls through. A source that is **present but
  malformed** raises, because silently skipping a config file with a typo in it
  looks exactly like having no credentials at all.

  ## "Set but empty" means unset

  This rule earns its own function, `normalizeEnv`, and its own tests. A CI
  runner binds a variable to an *undefined* secret by setting it to the empty
  string rather than leaving it out — GitHub Actions does exactly this — so
  `IO.getEnv` answers `some ""` and a naive read yields credentials with an
  empty access key. Those fail much later, inside a TLS handshake or as an
  opaque provider error, with nothing pointing at the cause. Treating empty as
  absent makes the chain fall through to its not-found message instead.

  `normalizeEnv` is pure and separate from the `IO` that reads the variable
  precisely so the rule is checkable by `#guard`: Lean has no `setenv`, so the
  environment source itself cannot be driven from a self-check.

  ## Everything names where it looked

  `sourceDescriptions` lists every place a lookup would have tried, and
  `loadFrom`'s failure quotes it. That is the whole difference between an error
  an operator can act on and one that sends them to the source code.

  ## Secrets do not render

  The `Repr` and `ToString` instances redact the secret key, the session token
  and the bearer token, so no amount of debug printing or error formatting can
  spill them. The access key is *not* redacted: it is an identifier, it appears
  in every signed request, and seeing it is how one tells which credential is
  in play.

  ## Provenance

  Moved down from the sibling `typednotes/infra`, which had this chain in
  `Infra/Core/Credentials.lean` and now uses this instead. `linen`'s version is
  a **superset**: infra's structure is a projection of this one.

  One of infra's constraints is gone. Its comments record that minting a GCP
  token from a service-account key needs an RS256 signature "which Linen can
  verify but not produce", so it shelled out to `gcloud` and had no key-file
  source at all. `Crypto.JOSE` gained RSA signing in `linen` 0.13.0, so
  `Cloud.Credentials.Gcp` does the real RFC 7523 flow and `gcloud` is now the
  fallback rather than the only option.
-/
import Linen.Cloud.Provider
import Linen.Cloud.Error
import Linen.Data.Ini
import Linen.Data.Yaml

namespace Cloud

-- ── The credential ──────────────────────────────────────────────────────────

/-- What is needed to authenticate to one cloud.

    Which fields matter depends on the cloud: AWS and Scaleway use the key
    pair, GCP uses `accessToken` and leaves the pair empty. -/
structure Credentials where
  /-- The public half of the key pair. An identifier, not a secret — it travels
      in every signed request. Empty for GCP. -/
  accessKey      : String := ""
  /-- The signing secret. Never rendered. Empty for GCP. -/
  secretKey      : String := ""
  /-- The cloud's own region code. May be empty, in which case a caller that
      needs one asks for it explicitly via `requireRegion`. -/
  region         : String := ""
  /-- Present only for temporary credentials — STS, instance roles. -/
  sessionToken   : Option String := none
  /-- Scaleway scopes created resources to a project, and GCP puts the project
      in almost every API path. AWS has no equivalent. Travels with the
      credentials rather than being asked for at each call site, because
      nothing works without it on those two clouds. -/
  projectId      : Option String := none
  /-- Scaleway IAM is organization-scoped rather than project-scoped, so a
      second identifier is needed for that one product. -/
  organizationId : Option String := none
  /-- An OAuth2 bearer token — GCP's whole credential, and short-lived.

      GCP does not sign its requests: there is no key pair to derive a
      signature from, only this token sent as `Authorization: Bearer …`. -/
  accessToken    : Option String := none
  deriving Inhabited

/-- Redacting.

    The secret key, session token and bearer token never render, so no amount
    of debug printing or error formatting can spill them. The access key does
    render: it is an identifier rather than a secret, and seeing it is how one
    tells which credential is in play. -/
instance : Repr Credentials where
  reprPrec c _ :=
    let opt (o : Option String) := if o.isSome then "<redacted>" else "none"
    let plain (o : Option String) := match o with | some v => repr v | none => "none"
    f!"Credentials \{ accessKey := {repr c.accessKey}, secretKey := <redacted>, \
region := {repr c.region}, sessionToken := {opt c.sessionToken}, \
projectId := {plain c.projectId}, organizationId := {plain c.organizationId}, \
accessToken := {opt c.accessToken} }"

instance : ToString Credentials where
  toString c := toString (repr c)

/-- Whether this credential can sign a SigV4 request. False for GCP, and for a
    partially-configured key pair. -/
def Credentials.canSign (c : Credentials) : Bool :=
  !c.accessKey.isEmpty && !c.secretKey.isEmpty

-- ── Where the CLIs keep their files ─────────────────────────────────────────

/-- The files the official CLIs write.

    Parameterised rather than read from `$HOME` at the point of use, so the
    whole chain can be exercised against a scratch directory — which is what
    makes the file sources testable at all. -/
structure Paths where
  /-- `~/.aws/credentials`. -/
  awsCredentials : System.FilePath
  /-- `~/.aws/config`, which is where the region normally lives. -/
  awsConfig      : System.FilePath
  /-- `~/.config/scw/config.yaml`. -/
  scwConfig      : System.FilePath

/-- The conventional locations, relative to `home`. -/
def Paths.under (home : System.FilePath) : Paths where
  awsCredentials := home / ".aws" / "credentials"
  awsConfig      := home / ".aws" / "config"
  scwConfig      := home / ".config" / "scw" / "config.yaml"

/-- The conventional locations for the current user. -/
def Paths.default : IO Paths := do
  let home := (← IO.getEnv "HOME").getD "."
  return Paths.under home

private def readIfExists (p : System.FilePath) : IO (Option String) := do
  if ← p.pathExists then return some (← IO.FS.readFile p) else return none

-- ── Source 1: the CLI config files ──────────────────────────────────────────

/-- The AWS profile to read, from `$AWS_PROFILE`, defaulting to `default`. -/
def awsProfile : IO String := do
  return (← IO.getEnv "AWS_PROFILE").getD "default"

/-- The section holding a profile in `~/.aws/config`.

    The two AWS files name the same profile differently: `credentials` uses
    `[dev]` while `config` uses `[profile dev]` — except the default profile,
    which is `[default]` in both. -/
def awsConfigSection (profile : String) : String :=
  if profile == "default" then "default" else s!"profile {profile}"

/-- Read `~/.aws/credentials` and `~/.aws/config`.

    A missing file is `none`; a *malformed* one raises, naming the file — see
    the module header on why those two cases must not look alike. -/
def fromAwsFiles (paths : Paths) (profile : String) : IO (Option Credentials) := do
  let some credText ← readIfExists paths.awsCredentials | return none
  let credIni ← match Data.Ini.parse credText with
    | .ok i    => pure i
    | .error e => throw (IO.userError s!"{paths.awsCredentials}: {e}")
  let some accessKey := credIni.lookup profile "aws_access_key_id" | return none
  let some secretKey := credIni.lookup profile "aws_secret_access_key" | return none
  let configIni ← match ← readIfExists paths.awsConfig with
    | none      => pure ({} : Data.Ini.Ini)
    | some text => match Data.Ini.parse text with
      | .ok i    => pure i
      | .error e => throw (IO.userError s!"{paths.awsConfig}: {e}")
  let region :=
    (configIni.lookup (awsConfigSection profile) "region").getD
      ((credIni.lookup profile "region").getD "")
  return some
    { accessKey, secretKey, region
      sessionToken := credIni.lookup profile "aws_session_token" }

/-- Read `~/.config/scw/config.yaml`. -/
def fromScalewayFile (paths : Paths) : IO (Option Credentials) := do
  let some text ← readIfExists paths.scwConfig | return none
  let doc ← match Data.Yaml.parse text with
    | .ok v    => pure v
    | .error e => throw (IO.userError s!"{paths.scwConfig}: {e}")
  let str (k : String) : Option String := (doc.get? k).bind (·.asString?)
  let some accessKey := str "access_key" | return none
  let some secretKey := str "secret_key" | return none
  return some
    { accessKey, secretKey
      region         := (str "default_region").getD ""
      projectId      := str "default_project_id"
      organizationId := str "default_organization_id" }

/-- Ask the `gcloud` CLI for a token.

    The GCP analogue of reading the other two clouds' config files: whatever
    the official tool has already set up should simply work. Two consequences,
    both real:

    - **The token expires**, typically within the hour. A long-running program
      can outlive one; there is no refresh here, and the failure is a `denied`
      partway through. `Cloud.Credentials.Gcp` avoids this by minting from a
      service-account key instead, which is why that source is tried first.
    - **It needs `gcloud` on `PATH` and logged in.** A missing binary is not an
      error, it is a source with nothing to offer, so it falls through. -/
def fromGcloud : IO (Option Credentials) := do
  let run (args : Array String) : IO (Option String) := do
    try
      let out ← IO.Process.output { cmd := "gcloud", args }
      if out.exitCode == 0 then
        let v := out.stdout.trimAscii.toString
        -- `gcloud config get-value` prints this for an unset key.
        return if v.isEmpty || v == "(unset)" then none else some v
      else return none
    catch _ => return none
  let some token ← run #["auth", "print-access-token"] | return none
  return some
    { accessToken := some token
      projectId := ← run #["config", "get-value", "project"]
      region := (← run #["config", "get-value", "compute/region"]).getD "" }

/-- The variable naming a GCP service-account key file — the long-lived
    credential that cloud has, and the closest thing it offers to the key pair
    the other two use.

    The *name* lives here, with the other sources, even though the source
    itself cannot: reading one means minting a token, which needs HTTP, which
    needs this module. `Cloud.Credentials.Gcp` is what tries it, and takes this
    string from here, so a diagnostic cannot name a variable no loader reads. -/
def gcpKeyFileVar : String := "GOOGLE_APPLICATION_CREDENTIALS"

/-- The keychain service `Cloud.Credentials.Keychain` stores entries under.

    Named here rather than there so that `sourceDescriptions` can quote it
    without importing the keychain FFI. -/
def keychainService : String := "linen"

-- ── Source 3: the environment ───────────────────────────────────────────────

/-- The environment variables each cloud's own tooling reads: access key,
    secret key, region, and the optional session token. -/
def envVars : Provider → (String × String × String × Option String)
  | .aws      => ("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION",
                  some "AWS_SESSION_TOKEN")
  | .scaleway => ("SCW_ACCESS_KEY", "SCW_SECRET_KEY", "SCW_DEFAULT_REGION", none)
  -- GCP has no key pair. The first two are empty and `fromEnvironment`
  -- branches on that rather than requiring them; the token is the credential.
  | .gcp      => ("", "", "GOOGLE_CLOUD_REGION", some "GOOGLE_OAUTH_ACCESS_TOKEN")

/-- "Set but empty" means unset. See the module header — this rule prevents a
    class of failure that otherwise surfaces inside a TLS handshake.

    Pure, and separate from the `IO` that reads the variable, so it is
    checkable by `#guard`. -/
def normalizeEnv : Option String → Option String
  | some v => if v.trimAscii.isEmpty then none else some v
  | none   => none

/-- An environment variable, with `normalizeEnv` applied. -/
private def getEnvNonEmpty (name : String) : IO (Option String) :=
  normalizeEnv <$> IO.getEnv name

/-- Read credentials from the environment. -/
def fromEnvironment (provider : Provider) : IO (Option Credentials) := do
  let (kv, sv, rv, tv) := envVars provider
  let region := (← getEnvNonEmpty rv).getD ""
  match provider with
  | .gcp =>
    -- A bearer token is the whole credential; there is no pair to require.
    let some accessToken ← getEnvNonEmpty "GOOGLE_OAUTH_ACCESS_TOKEN" | return none
    return some
      { region, accessToken := some accessToken
        projectId := ← getEnvNonEmpty "GOOGLE_CLOUD_PROJECT" }
  | .aws | .scaleway =>
    let some accessKey ← getEnvNonEmpty kv | return none
    let some secretKey ← getEnvNonEmpty sv | return none
    let sessionToken ← match tv with
      | some v => getEnvNonEmpty v
      | none   => pure none
    let projectId ← match provider with
      | .scaleway => getEnvNonEmpty "SCW_DEFAULT_PROJECT_ID"
      | _         => pure none
    let organizationId ← match provider with
      | .scaleway => getEnvNonEmpty "SCW_DEFAULT_ORGANIZATION_ID"
      | _         => pure none
    return some { accessKey, secretKey, region, sessionToken, projectId, organizationId }

-- ── The chain ───────────────────────────────────────────────────────────────

/-- Where each source would have looked, in the order they are tried.

    Naming every one is the difference between a usable error and a mystery, so
    this is public and asserted by the tests: a source the operator is never
    told about is a source they cannot use. -/
def sourceDescriptions (paths : Paths) (provider : Provider) (profile : String) :
    List String :=
  let (kv, sv, _, _) := envVars provider
  match provider with
  | .aws =>
    [ s!"config file {paths.awsCredentials} (profile [{profile}])"
    , s!"keychain service '{keychainService}' account 'aws'"
    , s!"environment {kv} and {sv}" ]
  | .scaleway =>
    [ s!"config file {paths.scwConfig}"
    , s!"keychain service '{keychainService}' account 'scaleway'"
    , s!"environment {kv} and {sv}" ]
  | .gcp =>
    [ s!"a service-account key file named by {gcpKeyFileVar}"
    , "`gcloud auth print-access-token` (is the CLI installed and logged in?)"
    , s!"keychain service '{keychainService}' account 'gcp'"
    , "environment GOOGLE_OAUTH_ACCESS_TOKEN" ]

/-- The not-found message, listing every source that declined. -/
def noCredentialsMessage (paths : Paths) (provider : Provider) (profile : String) :
    String :=
  let tried :=
    String.join ((sourceDescriptions paths provider profile).map (s!"\n  - {·}"))
  s!"no {provider.name} credentials found; tried:{tried}"

/-- Try each source in order and return the first that yields credentials.

    Takes the keychain source as a parameter rather than importing it, so that
    this module stays free of the keychain FFI — `Cloud.Credentials.Keychain`
    supplies the real one and `loadFrom` below wires it up. A caller that wants
    no keychain lookup at all passes `fun _ => pure none`. -/
def loadWith (paths : Paths) (provider : Provider)
    (fromStore : Provider → IO (Option Credentials)) : IO (Except Error Credentials) := do
  let profile ← awsProfile
  let fromFiles ← match provider with
    | .aws      => fromAwsFiles paths profile
    | .scaleway => fromScalewayFile paths
    -- Not a file for GCP: `gcloud` mints the token rather than storing one.
    | .gcp      => fromGcloud
  let found ← match fromFiles with
    | some c => pure (some c)
    | none   => do
      match ← fromStore provider with
      | some c => pure (some c)
      | none   => fromEnvironment provider
  match found with
  | some c => return .ok c
  | none   =>
    return .error {
        klass := .unbound, message := noCredentialsMessage paths provider profile }

/-- Try the file and environment sources, skipping the OS credential store.

    The keychain-free chain, for a program that does not want the FFI. -/
def loadFrom (paths : Paths) (provider : Provider) : IO (Except Error Credentials) :=
  loadWith paths provider (fun _ => pure none)

-- ── Asking for the parts a given cloud needs ────────────────────────────────

/-- The bearer token, or a clear failure.

    Every GCP call carries one and nothing else, so its absence is otherwise a
    `denied` on the first request with no indication of which source was
    supposed to supply it. -/
def Credentials.requireToken (c : Credentials) (provider : Provider) :
    Except Error String :=
  match c.accessToken with
  | some t => .ok t
  | none   => .error
      { klass := .unbound
      , message := s!"no {provider.name} access token; point {gcpKeyFileVar} at a \
service-account key, run `gcloud auth login`, or set GOOGLE_OAUTH_ACCESS_TOKEN" }

/-- The project, or a clear failure.

    Scaleway scopes creates to a project and GCP puts it in nearly every API
    path; absent, it surfaces as an opaque provider error. -/
def Credentials.requireProject (c : Credentials) : Except Error String :=
  match c.projectId with
  | some p => .ok p
  | none   => .error
      { klass := .unbound
      , message := "no project configured; set SCW_DEFAULT_PROJECT_ID or \
GOOGLE_CLOUD_PROJECT, or default_project_id in ~/.config/scw/config.yaml" }

/-- The Scaleway organization, or a clear failure. IAM is organization-scoped
    and fails opaquely without one. -/
def Credentials.requireOrganization (c : Credentials) : Except Error String :=
  match c.organizationId with
  | some o => .ok o
  | none   => .error
      { klass := .unbound
      , message := "no Scaleway organization configured; set \
SCW_DEFAULT_ORGANIZATION_ID or default_organization_id in \
~/.config/scw/config.yaml" }

/-- The region, or a clear failure.

    Most of these APIs are regional and a blank region produces a baffling
    signing error much later, so it is caught here. -/
def Credentials.requireRegion (c : Credentials) (provider : Provider) :
    Except Error String :=
  if c.region.isEmpty then
    let (_, _, rv, _) := envVars provider
    .error
      { klass := .unbound
      , message := s!"no region configured for {provider.name}; set {rv}, or set \
the region in its config file, or pass one explicitly" }
  else .ok c.region

-- ── Self-checks ─────────────────────────────────────────────────────────────

-- An undefined CI secret arrives as `some ""` and must read as absent, so that
-- the chain reports "not found" instead of building empty credentials that
-- fail later inside a TLS handshake.
#guard normalizeEnv (some "") = none
#guard normalizeEnv (some "   ") = none
#guard normalizeEnv none = none
#guard normalizeEnv (some "SCWXXXXXXXXXXXXXXXXX") = some "SCWXXXXXXXXXXXXXXXXX"

end Cloud
