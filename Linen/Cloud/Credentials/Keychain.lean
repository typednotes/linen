/-
  `Cloud.Credentials.Keychain` — the OS credential store as a credential source

  ## Why this is its own module

  `Cloud.Credentials` deliberately does not import `System.Keychain`. Doing so
  would drag the platform credential-store FFI — macOS Security.framework,
  Linux libsecret, Windows `wincred` — into every program that so much as
  mentions a bucket, including the many that authenticate from environment
  variables and want nothing to do with a keychain.

  Those shims are already in `linen`'s build graph and already exercised on
  macOS and Linux in CI, so this is not a new dependency. It is an *unnecessary*
  one for most callers, and making it opt-in costs one import.

  Consequence: `Cloud.Credentials.loadWith` takes the store lookup as a
  parameter, and `load` here is what supplies the real one. A program that wants
  the full three-source chain imports this module; one that does not, does not.

  ## Stored as INI

  An entry's body is an INI document — `access_key`, `secret_key`, `region`,
  and optionally `session_token` — so the same parser reads it as reads
  `~/.aws/credentials`, and a human can inspect the entry in Keychain Access or
  `secret-tool` and see what it holds.

  ## A missing entry is not an error

  On a machine with no credential-store service running at all, the lookup
  *fails* rather than returning nothing — and that must fall through to the next
  source rather than abort the chain. So every failure here becomes `none`. The
  one thing that would justify raising, a malformed entry, is also `none`: an
  entry this module cannot parse is not a usable credential, and the two
  remaining sources deserve their turn.

  ## Provenance

  Moved down from the sibling `typednotes/infra`, which had this in
  `Infra/Core/Credentials.lean` and now uses this instead. Two changes on the
  way in: the service name is `linen` rather than `infra`, and the keychain
  source is separated from the rest of the chain as described above.
-/
import Linen.Cloud.Credentials
import Linen.System.Keychain

namespace Cloud.Credentials.Keychain

open Cloud

-- ── Reading ─────────────────────────────────────────────────────────────────

/-- Read an entry's INI body back into credentials.

    Pure, and separate from the `IO` that fetches it, for the same reason
    `render` is: the platform store cannot be driven from a `#guard`, so a
    round-trip that is not checkable here is not checked anywhere. Both of the
    defects this function had — a dropped `access_token` and blank fields
    accepted as values — were invisible to a test that could only exercise
    `render`. -/
def parseBody (text : String) : Option Credentials :=
  match Data.Ini.parse text with
  | .error _ => none
  | .ok ini =>
    -- `normalizeEnv`: the same "set but empty means unset" rule the
    -- environment source uses. A stored entry whose fields are present but
    -- blank used to be answered as credentials, which then failed inside a TLS
    -- handshake or as an opaque provider error instead of falling through to
    -- the next source.
    let field (k : String) : Option String := normalizeEnv (ini.lookupGlobal k)
    let accessKey   := (field "access_key").getD ""
    let secretKey   := (field "secret_key").getD ""
    let accessToken := field "access_token"
    -- An entry has to carry *something usable*: a key pair for the clouds that
    -- sign, or a bearer token for GCP, whose whole credential is the token and
    -- whose key pair is legitimately empty. Requiring the key pair
    -- unconditionally made a GCP credential impossible to store.
    if accessKey.isEmpty && secretKey.isEmpty && accessToken.isNone then
      none
    else
      some
        { accessKey, secretKey, accessToken
          region         := (field "region").getD ""
          sessionToken   := field "session_token"
          projectId      := field "project_id"
          organizationId := field "organization_id" }

/-- Read credentials from a named account under the `linen` keychain service.

    The account name is a parameter rather than a `Provider` because not every
    credential in play names a cloud: Scaleway's Queues product needs its own
    dedicated key pair, distinct from the account's main one, and it lives
    under its own account name.

    Every failure — no such entry, no keychain service, an unparseable body — is
    `none`, so the chain falls through. See the module header. -/
def fromAccount (account : String) : IO (Option Credentials) := do
  let entry := System.Keychain.Entry.new keychainService account
  let raw ← try pure (some (← entry.getPassword)) catch _ => pure none
  let some text := raw | return none
  return parseBody text

/-- Read the credentials stored for a cloud, under an account named after it. -/
def forProvider (provider : Provider) : IO (Option Credentials) :=
  fromAccount provider.name

-- ── Writing ─────────────────────────────────────────────────────────────────

/-- The INI body an entry holds. Pure, so the format is checkable by `#guard`
    without touching the platform store — which the tests could not otherwise
    do, since a CI runner has no unlocked keychain. -/
def render (c : Credentials) : String :=
  Data.Ini.render
    { globals :=
        [ ("access_key", c.accessKey)
        , ("secret_key", c.secretKey)
        , ("region", c.region) ]
        ++ (match c.sessionToken with   | some t => [("session_token", t)]   | none => [])
        -- GCP's whole credential. Omitted here until 0.18.0, so a stored GCP
        -- credential loaded back without its token and short-circuited the
        -- chain: `fromAccount` answered credentials that `requireToken` then
        -- rejected, having skipped the sources that would have worked.
        ++ (match c.accessToken with    | some t => [("access_token", t)]    | none => [])
        ++ (match c.projectId with      | some p => [("project_id", p)]      | none => [])
        ++ (match c.organizationId with | some o => [("organization_id", o)] | none => []) }

/-- Store credentials in a named account under the `linen` keychain service.

    Overwrites whatever that account held. The account name is the caller's to
    choose, and choosing one another tool already uses would take that tool's
    credential out from under it — so `forProvider`'s names (`aws`, `gcp`,
    `scaleway`) are the ones this library claims, and nothing else. -/
def storeInAccount (account : String) (c : Credentials) : IO Unit := do
  let entry := System.Keychain.Entry.new keychainService account
  entry.setPassword (render c)

/-- Store the credentials for a cloud, under an account named after it. -/
def store (provider : Provider) (c : Credentials) : IO Unit :=
  storeInAccount provider.name c

/-- Remove a named account's entry. `none` of the failure modes are errors, for
    the same reason reading's are not: a machine with no keychain service must
    not fail here. -/
def deleteAccount (account : String) : IO Unit := do
  let entry := System.Keychain.Entry.new keychainService account
  try entry.deleteCredential catch _ => pure ()

-- ── The full chain ──────────────────────────────────────────────────────────

/-- Try each source in order — config file, then keychain, then environment —
    and return the first that yields credentials.

    This is `Cloud.Credentials.loadWith` with the keychain source supplied. It
    lives here rather than there so that the FFI stays opt-in. -/
def loadFrom (paths : Paths) (provider : Provider) : IO (Except Error Credentials) :=
  Cloud.loadWith paths provider forProvider

/-- The full three-source chain, from the conventional file locations. -/
def load (provider : Provider) : IO (Except Error Credentials) := do
  loadFrom (← Paths.default) provider

-- ── Self-checks ─────────────────────────────────────────────────────────────

-- The stored body is INI the same parser reads back, and a credential with no
-- optional fields writes only the three required keys.
#guard render { accessKey := "AKIA", secretKey := "s", region := "eu-west-3" }
  == "access_key = AKIA\nsecret_key = s\nregion = eu-west-3\n"

end Cloud.Credentials.Keychain
