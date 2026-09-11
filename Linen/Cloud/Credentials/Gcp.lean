/-
  `Cloud.Credentials.Gcp` — minting a GCP access token from a key file

  ## Why this is a whole module

  AWS and Scaleway keep a long-lived secret on disk and sign each request with
  it. GCP does not: a request carries a short-lived OAuth2 **bearer token**, and
  the long-lived credential is a *service-account key file* from which tokens
  are minted. Minting one is RFC 7523's JWT-bearer grant:

  1. Build a JWT asserting who you are (`iss`), what you want
     (`scope`), who should accept it (`aud`) and for how long (`iat`/`exp`).
  2. Sign it **RS256** with the key file's private key.
  3. `POST` it to Google's token endpoint as an `assertion`.
  4. Receive an access token, valid for about an hour.

  ## What changed relative to the sibling project

  `typednotes/infra` could not do any of this. Its comments record why: minting
  a token "needs an RS256 signature, which Linen can verify but not produce",
  so it shelled out to `gcloud auth print-access-token` and had no key-file
  source at all. That was true when it was written; `Crypto.JOSE` gained RSA
  signing in `linen` 0.13.0. So the real flow lives here, and `gcloud` is the
  fallback rather than the only option.

  This matters beyond tidiness. `gcloud` requires the CLI installed and a human
  logged in, which is exactly what a deployed service does not have — and its
  token cannot be refreshed, so a long-running process fails partway through
  with a `denied` and no indication why. A key file can be re-minted from.

  ## The transport is a parameter

  `exchange` takes a `Cloud.Transport`, so the token exchange is exercised
  against a stub in the tests. Without that the only way to test this path
  would be to hold a real service-account key, which is precisely the thing one
  should not put in a repository.

  ## Refresh is the caller's business

  `tokenFor` mints one token. It does not cache and does not refresh, because
  the right lifetime policy depends on the caller — a batch job wants one
  token, a server wants a refresh loop. `expiresAt` is returned so either can
  be built; hiding a mutable cache in here would make the failure mode
  ("stopped working after an hour") worse rather than better.

  ## A note on what is *not* checked

  The token endpoint is authenticated by TLS and by the audience claim in the
  signed assertion. This module does not pin Google's certificate; it trusts
  `linen`'s TLS stack and the system trust store, like every other HTTPS client
  here.
-/
import Linen.Cloud.Credentials
import Linen.Cloud.Transport
import Linen.Crypto.JOSE.JWS
import Linen.Data.Json.Encode
import Linen.Data.Json.Decode

namespace Cloud.Credentials.Gcp

open Cloud
open Data.Json (Value)

-- ── The key file ────────────────────────────────────────────────────────────

/-- A GCP service-account key file.

    The shape `gcloud iam service-accounts keys create` writes. `privateKeyPem`
    is the whole PEM document including its header and footer lines. -/
structure ServiceAccount where
  /-- The service account's email address — the JWT's `iss`. -/
  clientEmail   : String
  /-- The PEM-encoded RSA private key. **Never rendered.** -/
  privateKeyPem : String
  /-- The key's id, which becomes the JWT header's `kid`. Google accepts an
      assertion without it, but sending it lets Google pick the right key when
      an account has several. -/
  privateKeyId  : Option String := none
  /-- The project the account belongs to, so a caller need not configure it
      separately. -/
  projectId     : Option String := none
  /-- Where to send the assertion. From the file, because Google has changed
      this host before and a key file names the one it expects. -/
  tokenUri      : String := "https://" ++ Gcp.oauthTokenHost ++ Gcp.oauthTokenPath

/-- Redacting: the private key never renders. A service-account key is a
    long-lived credential to a whole project, so this is the most important
    redaction in the namespace. -/
instance : Repr ServiceAccount where
  reprPrec sa _ :=
    f!"ServiceAccount \{ clientEmail := {repr sa.clientEmail}, \
privateKeyPem := <redacted>, projectId := {repr sa.projectId} }"

instance : ToString ServiceAccount where
  toString sa := toString (repr sa)

/-- Parse a service-account key file.

    Rejects anything whose `type` is not `service_account` — the other kind of
    file `gcloud` writes is an *authorized user* credential, which uses a
    refresh token rather than a key and would fail later with a signature
    error that named nothing. -/
def parseKeyFile (text : String) : Except Error ServiceAccount := do
  let v ← match Data.Json.Decode.decode text with
    | .ok v    => .ok v
    | .error m => .error (Error.protocol s!"service-account key file is not JSON: {m}")
  let str (k : String) : Option String :=
    (v.asObject.bind fun fs => (fs.find? (·.1 == k)).map (·.2)).bind Data.Json.Value.asString
  match str "type" with
  | some "service_account" => pure ()
  | some other =>
    .error
      { klass := .invalid
      , message := s!"expected a service_account key file, found type '{other}'; \
an authorized-user credential cannot mint a token this way" }
  | none =>
    .error (Error.protocol "service-account key file has no 'type' field")
  let some clientEmail := str "client_email"
    | .error (Error.protocol "service-account key file has no 'client_email'")
  let some privateKeyPem := str "private_key"
    | .error (Error.protocol "service-account key file has no 'private_key'")
  .ok
    { clientEmail, privateKeyPem
    , privateKeyId := str "private_key_id"
    , projectId    := str "project_id"
    , tokenUri     := (str "token_uri").getD
        ("https://" ++ Gcp.oauthTokenHost ++ Gcp.oauthTokenPath) }

/-- Read and parse the key file named by `GOOGLE_APPLICATION_CREDENTIALS`.

    `none` when the variable is unset or empty — a source with nothing to
    offer, which must fall through. A variable that is *set* and names an
    unreadable or malformed file is an error, on the same rule the rest of the
    credential chain follows: a typo must not look like an absence. -/
def fromKeyFileEnv : IO (Except Error (Option ServiceAccount)) := do
  let some path := normalizeEnv (← IO.getEnv gcpKeyFileVar) | return .ok none
  let exists' ← (System.FilePath.mk path).pathExists
  if !exists' then
    return .error {
        klass := .invalid
      , message := s!"{gcpKeyFileVar} names '{path}', which does not exist" }
  let text ← try
      pure (Except.ok (← IO.FS.readFile path))
    catch e => pure (Except.error (Error.transport s!"reading '{path}': {toString e}"))
  match text with
  | .error e => return .error e
  | .ok text => return (parseKeyFile text).map some

-- ── The assertion ───────────────────────────────────────────────────────────

/-- How long an assertion is valid for. Google's maximum is one hour; asking
    for more is rejected outright. -/
def assertionLifetimeSeconds : Nat := 3600

/-- The JWT header for an assertion. -/
def assertionHeader (sa : ServiceAccount) : String :=
  Data.Json.Encode.encode <| Value.object <|
    [("alg", .string "RS256"), ("typ", .string "JWT")]
    ++ (match sa.privateKeyId with | some k => [("kid", .string k)] | none => [])

/-- The JWT claims for an assertion, at a given time.

    `iat` and `exp` are seconds since the epoch. Google rejects an assertion
    whose `iat` is in the future by more than a small skew, so a machine with a
    badly wrong clock fails here — with a message about the assertion, which is
    at least a starting point. -/
def assertionClaims (sa : ServiceAccount) (scope : String) (issuedAtEpoch : Nat) : String :=
  Data.Json.Encode.encode <| Value.object
    [ ("iss", .string sa.clientEmail)
    , ("scope", .string scope)
    , ("aud", .string sa.tokenUri)
    , ("iat", .number (Float.ofNat issuedAtEpoch))
    , ("exp", .number (Float.ofNat (issuedAtEpoch + assertionLifetimeSeconds))) ]

/-- Build and sign the assertion.

    RS256 over the key file's private key. A PEM that OpenSSL cannot read, or a
    key that is not RSA, is a clear error here rather than a rejected
    assertion. -/
def assertion (sa : ServiceAccount) (scope : String) (issuedAtEpoch : Nat) :
    IO (Except Error String) := do
  let der ← try
      pure (Except.ok (← Crypto.JOSE.FFI.privkeyPemToDer sa.privateKeyPem))
    catch e =>
      pure (Except.error
        { klass := .invalid
        , message := s!"service-account private key is not readable PEM: {toString e}" })
  match der with
  | .error e => return .error e
  | .ok der =>
    let signed ← try
        pure (Except.ok (← Crypto.JOSE.JWS.signCompact .RS256 der
          (assertionHeader sa) (assertionClaims sa scope issuedAtEpoch)))
      catch e =>
        pure (Except.error
          { klass := .invalid
          , message := s!"signing the assertion failed: {toString e}" })
    match signed with
    | .error e     => return .error e
    | .ok none     =>
      return .error (Error.protocol "RS256 is unavailable in this build of Crypto.JOSE")
    | .ok (some j) => return .ok j

-- ── The exchange ────────────────────────────────────────────────────────────

/-- RFC 7523's grant type. Sent literally; Google matches it exactly. -/
def jwtBearerGrantType : String := "urn:ietf:params:oauth:grant-type:jwt-bearer"

/-- A minted access token and when it stops working. -/
structure Token where
  /-- The bearer token. **Never rendered.** -/
  accessToken : String
  /-- Seconds since the epoch after which it is no longer accepted. -/
  expiresAt   : Nat
  deriving DecidableEq

instance : Repr Token where
  reprPrec t _ := f!"Token \{ accessToken := <redacted>, expiresAt := {t.expiresAt} }"

instance : ToString Token where
  toString t := toString (repr t)

/-- The form body of a token request.

    `application/x-www-form-urlencoded`, and the assertion is percent-encoded
    because a JWT's base64url alphabet contains `-` and `_` but its separators
    are `.`, and a malformed body is rejected as `invalid_grant` with no
    explanation. -/
def exchangeBody (assertion : String) : ByteArray :=
  let enc (s : String) : String :=
    Network.HTTP.Types.urlEncode s
  s!"grant_type={enc jwtBearerGrantType}&assertion={enc assertion}".toUTF8

/-- Split a key file's `token_uri` into the host and path a `Call` needs.

    Required to be **https**: the assertion in the body is a bearer credential
    signed with the account's private key, and posting it in clear would hand it
    to anyone on the path. A `token_uri` carrying a query string is refused
    rather than silently folded into the path.

    This exists because the destination must be the key file's, not a constant
    — see `exchange`. -/
def splitTokenUri (url : String) : Except Error (String × String) :=
  if !url.startsWith "https://" then
    .error
      { klass := .invalid
      , message := s!"token_uri must be https, found '{url}'" }
  else if (url.splitOn "?").length != 1 then
    .error
      { klass := .invalid
      , message := s!"token_uri must carry no query string, found '{url}'" }
  else
    match (url.drop 8).toString.splitOn "/" with
    | []             => .error (Error.protocol s!"token_uri names no host: '{url}'")
    | host :: segs   =>
      if host.isEmpty then
        .error (Error.protocol s!"token_uri names no host: '{url}'")
      else
        .ok (host, "/" ++ "/".intercalate segs)

/-- Exchange a signed assertion for an access token.

    The transport is a parameter so this is testable against a stub — the
    alternative being to hold a real service-account key in the repository.

    **The destination comes from the key file**, via `sa.tokenUri`. That is what
    `ServiceAccount.tokenUri`'s doc-comment always claimed ("where to send the
    assertion… a key file names the one it expects"), but this function used to
    post to a hardcoded `Gcp.oauthTokenHost` and ignore `sa` entirely — the
    parameter was unused, which is how the discrepancy surfaced.

    It is not a cosmetic difference. `assertionClaims` already uses
    `sa.tokenUri` as the JWT's `aud`, so a key file naming a different endpoint
    produced an assertion audienced for one host and posted to another: rejected
    as an invalid audience at best, and a credential signed for a host it was
    not sent to at worst. Reading both from one field makes them agree by
    construction — the same rule `Cloud.Transport`'s header states about signing
    and sending. -/
def exchange (t : Transport) (sa : ServiceAccount) (assertion : String)
    (issuedAtEpoch : Nat) : IO (Except Error Token) := do
  let (host, path) ← match splitTokenUri sa.tokenUri with
    | .error e => return .error e
    | .ok hp   => pure hp
  let call : Call :=
    { method := "POST"
    , endpoint := Gcp.endpoint host
    , path := path
    , headers := [("Content-Type", "application/x-www-form-urlencoded")]
    , body := exchangeBody assertion
    , auth := .anonymous }
  match ← performNow t call with
  | .error e => return .error e
  | .ok resp =>
    match bodyText resp with
    | .error e => return .error e
    | .ok text =>
      match Data.Json.Decode.decode text with
      | .error m => return .error (Error.protocol s!"token response is not JSON: {m}")
      | .ok v =>
        let str (k : String) : Option String :=
          (v.asObject.bind fun fs => (fs.find? (·.1 == k)).map (·.2)).bind
            Data.Json.Value.asString
        let num (k : String) : Option Nat :=
          ((v.asObject.bind fun fs => (fs.find? (·.1 == k)).map (·.2)).bind
            Data.Json.Value.asNumber).map (fun f => f.toUInt64.toNat)
        match str "access_token" with
        | none =>
          return .error (Error.protocol "token response has no 'access_token'")
        | some accessToken =>
          return .ok {
              accessToken
            , expiresAt := issuedAtEpoch + (num "expires_in").getD assertionLifetimeSeconds }

-- ── The whole flow ──────────────────────────────────────────────────────────

/-- Mint an access token from a service-account key, at a given time. -/
def tokenForAt (t : Transport) (sa : ServiceAccount) (issuedAtEpoch : Nat)
    (scope : String := Gcp.cloudPlatformScope) : IO (Except Error Token) := do
  match ← assertion sa scope issuedAtEpoch with
  | .error e => return .error e
  | .ok j    => exchange t sa j issuedAtEpoch

/-- Seconds since the Unix epoch, now. -/
def nowEpochSeconds : IO Nat := do
  return (← Data.Time.getCurrentTime).nanosSinceEpoch / 1000000000

/-- Mint an access token from a service-account key, now. -/
def tokenFor (t : Transport) (sa : ServiceAccount)
    (scope : String := Gcp.cloudPlatformScope) : IO (Except Error Token) := do
  tokenForAt t sa (← nowEpochSeconds) scope

/-- Credentials for GCP, from the key file named by
    `GOOGLE_APPLICATION_CREDENTIALS`.

    `none` when the variable is unset — the source declined, and the chain
    should try `gcloud`, then the keychain, then the environment. This is the
    source `Cloud.Credentials.sourceDescriptions` lists first for GCP, and now
    the one that actually exists. -/
def fromKeyFile (t : Transport) (region : String := "") :
    IO (Except Error (Option Credentials)) := do
  match ← fromKeyFileEnv with
  | .error e     => return .error e
  | .ok none     => return .ok none
  | .ok (some sa) =>
    match ← tokenFor t sa with
    | .error e => return .error e
    | .ok tok  =>
      return .ok (some
        { region
        , projectId := sa.projectId
        , accessToken := some tok.accessToken })

/-- `fromKeyFile` in the shape `Cloud.Credentials.loadWith` expects as its
    key-file source: declines for every provider but GCP, which is the only
    one with a service-account key file to exchange.

    This is what connects the RFC 7523 flow above to the credential chain, so
    that the key file `Cloud.Credentials.sourceDescriptions` names first is
    actually consulted. `Cloud.Credentials.Chain.load` passes it. -/
def keyFileSource (t : Transport) (region : String := "") :
    Provider → IO (Except Error (Option Credentials))
  | .gcp => fromKeyFile t region
  | _    => pure (.ok none)

/-- The GCP chain with the key-file source supplied, skipping the OS
    credential store: key file, then `gcloud`, then the environment.

    For the full chain including the keychain, use
    `Cloud.Credentials.Chain.load`. -/
def loadFrom (t : Transport) (paths : Paths) (region : String := "") :
    IO (Except Error Credentials) :=
  Cloud.loadWith paths .gcp (fun _ => pure none) (keyFileSource t region)

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard jwtBearerGrantType == "urn:ietf:params:oauth:grant-type:jwt-bearer"
#guard assertionLifetimeSeconds == 3600

-- The `kid` appears only when the key file carried one.
#guard ((assertionHeader { clientEmail := "a@b", privateKeyPem := "" }).splitOn "kid").length == 1
#guard ((assertionHeader { clientEmail := "a@b", privateKeyPem := ""
                         , privateKeyId := some "k1" }).splitOn "k1").length == 2

end Cloud.Credentials.Gcp
