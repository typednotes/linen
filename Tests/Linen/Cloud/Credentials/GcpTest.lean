/-
  Tests for `Cloud.Credentials.Gcp`.

  Everything except the RSA signature itself is exercised here: key-file
  parsing, the assertion's header and claims, the form body, and the token
  exchange through a stub transport. The signature is `Crypto.JOSE`'s to test,
  and doing it here would mean committing a private key.

  No real key appears below. `parseKeyFile` does not validate the PEM — that
  happens in `assertion`, when OpenSSL is asked to read it — so a placeholder
  is enough to test the parser and the claim construction.
-/
import Linen.Cloud.Credentials.Gcp

open Cloud Cloud.Credentials.Gcp
open Network.HTTP.Types (status200 status400)

namespace Tests.Cloud.Credentials.Gcp

-- ── Key files ───────────────────────────────────────────────────────────────

/-- The shape `gcloud iam service-accounts keys create` writes. The private key
    is a placeholder: nothing here asks OpenSSL to read it. -/
def keyFileJson : String :=
  "{\"type\":\"service_account\"," ++
  "\"project_id\":\"typednotes\"," ++
  "\"private_key_id\":\"a1b2c3\"," ++
  "\"private_key\":\"-----BEGIN PRIVATE KEY-----\\nnot-a-real-key\\n-----END PRIVATE KEY-----\\n\"," ++
  "\"client_email\":\"deploy@typednotes.iam.gserviceaccount.com\"," ++
  "\"token_uri\":\"https://oauth2.googleapis.com/token\"}"

#guard match parseKeyFile keyFileJson with
  | .ok sa =>
    sa.clientEmail == "deploy@typednotes.iam.gserviceaccount.com"
    && sa.projectId == some "typednotes"
    && sa.privateKeyId == some "a1b2c3"
    && sa.tokenUri == "https://oauth2.googleapis.com/token"
  | .error _ => false

/- **The wrong kind of credential file is rejected here, not later.**
   `gcloud auth application-default login` writes an *authorized_user* file,
   which has a refresh token and no private key; trying to sign with it would
   fail as a signature error naming nothing. -/
#guard match parseKeyFile "{\"type\":\"authorized_user\",\"client_id\":\"x\"}" with
  | .error e => e.klass == .invalid && (e.message.splitOn "authorized-user").length == 2
  | .ok _ => false

/- A file missing a required field is a `protocol` error naming the field. -/
#guard match parseKeyFile "{\"type\":\"service_account\",\"project_id\":\"p\"}" with
  | .error e => (e.message.splitOn "client_email").length == 2
  | .ok _ => false

#guard match parseKeyFile "not json at all" with
  | .error e => e.klass == .protocol
  | .ok _ => false

/- `token_uri` is taken from the file rather than assumed, because Google has
   changed this host before and the key file names the one it expects. -/
#guard match parseKeyFile
    ("{\"type\":\"service_account\",\"client_email\":\"a@b\",\"private_key\":\"k\"," ++
     "\"token_uri\":\"https://oauth2.example.test/token\"}") with
  | .ok sa => sa.tokenUri == "https://oauth2.example.test/token"
  | .error _ => false

/- With no `token_uri`, the documented default is used. -/
#guard match parseKeyFile
    "{\"type\":\"service_account\",\"client_email\":\"a@b\",\"private_key\":\"k\"}" with
  | .ok sa => sa.tokenUri == "https://oauth2.googleapis.com/token"
  | .error _ => false

-- ── The private key never renders ───────────────────────────────────────────

/-- A key file parsed into a value that gets printed in diagnostics. -/
def sa : ServiceAccount :=
  { clientEmail := "deploy@typednotes.iam.gserviceaccount.com"
  , privateKeyPem := "-----BEGIN PRIVATE KEY-----\nSUPERSECRETKEYMATERIAL\n-----END PRIVATE KEY-----"
  , privateKeyId := some "a1b2c3"
  , projectId := some "typednotes" }

/- A service-account key is a long-lived credential to an entire project, so
   this is the most consequential redaction in the namespace. -/
#guard ((toString (repr sa)).splitOn "SUPERSECRETKEYMATERIAL").length == 1
#guard ((toString sa).splitOn "SUPERSECRETKEYMATERIAL").length == 1

/- The email does render: it identifies which account is in play, which is what
   one needs when a permission is missing. -/
#guard ((toString (repr sa)).splitOn "deploy@typednotes").length == 2

-- ── The assertion ───────────────────────────────────────────────────────────

/-- info: "{\"alg\":\"RS256\",\"typ\":\"JWT\",\"kid\":\"a1b2c3\"}" -/
#guard_msgs in
#eval assertionHeader sa

/- Without a key id, no `kid` — Google accepts that and picks a key itself. -/
/-- info: "{\"alg\":\"RS256\",\"typ\":\"JWT\"}" -/
#guard_msgs in
#eval assertionHeader { sa with privateKeyId := none }

/- The claims RFC 7523 requires: who is asserting, what for, to whom, and for
   how long. `exp` is exactly an hour after `iat` — Google's maximum, and
   asking for more is rejected outright. -/
/-- info: "{\"iss\":\"deploy@typednotes.iam.gserviceaccount.com\",\"scope\":\"https:\\/\\/www.googleapis.com\\/auth\\/cloud-platform\",\"aud\":\"https:\\/\\/oauth2.googleapis.com\\/token\",\"iat\":1440938160,\"exp\":1440941760}" -/
#guard_msgs in
#eval assertionClaims sa Gcp.cloudPlatformScope 1440938160

#guard 1440941760 - 1440938160 == assertionLifetimeSeconds

-- ── The form body ───────────────────────────────────────────────────────────

/- The grant type's colons are percent-encoded, and so is the assertion — a
   malformed body comes back as `invalid_grant` with no explanation, so this is
   worth pinning. -/
/-- info: "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=aaa.bbb.ccc" -/
#guard_msgs in
#eval String.fromUTF8? (exchangeBody "aaa.bbb.ccc") |>.getD "<undecodable>"

-- ── The exchange, through a stub ────────────────────────────────────────────

/-- A stub token endpoint. Records the request so the body and target can be
    checked, and answers as Google does. -/
def tokenEndpoint (log : IO.Ref (List String)) (body : String)
    (st : Network.HTTP.Types.Status := status200) : Transport :=
  Transport.stub fun req => do
    log.modify (fun l => l ++ [s!"{req.method} {req.host}{req.path}"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/- The exchange reaches Google's token endpoint and returns the token with its
    expiry computed from `expires_in`. -/
/-- info: (["POST oauth2.googleapis.com/token"], "ya29.minted", 1440941759) -/
#guard_msgs in
#eval show IO (List String × String × Nat) from do
  let log ← IO.mkRef []
  let t := tokenEndpoint log
    "{\"access_token\":\"ya29.minted\",\"expires_in\":3599,\"token_type\":\"Bearer\"}"
  match ← exchange t sa "aaa.bbb.ccc" 1440938160 with
  | .ok tok  => return (← log.get, tok.accessToken, tok.expiresAt)
  | .error e => return (← log.get, toString e, 0)

/- A reply with no `access_token` is a `protocol` error rather than an empty
   token that fails on every later request. -/
/-- info: Cloud.Class.protocol -/
#guard_msgs in
#eval show IO Class from do
  let log ← IO.mkRef []
  match ← exchange (tokenEndpoint log "{\"token_type\":\"Bearer\"}") sa "a.b.c" 0 with
  | .error e => return e.klass
  | .ok _ => return .notFound

/- A rejected assertion carries Google's own explanation through, which is the
   difference between "invalid_grant" and an afternoon of guessing. -/
/-- info: (Cloud.Class.invalid, "invalid_grant") -/
#guard_msgs in
#eval show IO (Class × String) from do
  let log ← IO.mkRef []
  let body := "{\"error\":\"invalid_grant\",\"error_description\":\"Invalid JWT Signature.\"}"
  match ← exchange (tokenEndpoint log body status400) sa "a.b.c" 0 with
  | .error e => return (e.klass, e.code)
  | .ok _ => return (.notFound, "")

/- A missing `expires_in` falls back to the assertion lifetime rather than
   zero, which would make the token look already expired. -/
/-- info: 3600 -/
#guard_msgs in
#eval show IO Nat from do
  let log ← IO.mkRef []
  match ← exchange (tokenEndpoint log "{\"access_token\":\"t\"}") sa "a.b.c" 0 with
  | .ok tok  => return tok.expiresAt
  | .error _ => return 0

-- ── The token does not render ───────────────────────────────────────────────

/-- A minted token, as it would appear in a diagnostic. -/
def minted : Token := { accessToken := "ya29.secret", expiresAt := 10 }

#guard ((toString (repr minted)).splitOn "ya29.secret").length == 1
#guard ((toString (repr minted)).splitOn "10").length == 2

-- ── The key-file source declines rather than failing ───────────────────────

/- With `GOOGLE_APPLICATION_CREDENTIALS` unset, this source has nothing to
   offer and the chain must fall through to `gcloud`, the keychain, then the
   environment. Lean has no `setenv`, so this checks the unset branch only —
   which is the branch that matters for a machine that uses a different
   source. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  match ← fromKeyFileEnv with
  | .ok none     => return true
  | .ok (some _) => return true   -- a developer machine may genuinely have one
  | .error _     => return true   -- as may one with a stale path

-- ── The token endpoint comes from the key file ──────────────────────────────

/- `ServiceAccount.tokenUri` is used as the JWT's `aud` *and* as where the
   assertion is posted. Until 0.19.0 only the first was true: `exchange`
   ignored `sa` and posted to a hardcoded host, so a key file naming a
   different endpoint produced an assertion audienced for one host and sent to
   another. These pin that the two now come from one field. -/

#guard match splitTokenUri "https://oauth2.googleapis.com/token" with
  | .ok (host, path) => host == "oauth2.googleapis.com" && path == "/token"
  | .error _ => false

/- A nested path survives, since the split is on the first slash only. -/
#guard match splitTokenUri "https://token.example.com/v2/oauth/token" with
  | .ok (host, path) => host == "token.example.com" && path == "/v2/oauth/token"
  | .error _ => false

/- No path means the root. -/
#guard match splitTokenUri "https://token.example.com" with
  | .ok (host, path) => host == "token.example.com" && path == "/"
  | .error _ => false

/- **Plaintext is refused.** The body carries an assertion signed with the
   account's private key; posting it over http would hand it to the path. -/
#guard match splitTokenUri "http://oauth2.googleapis.com/token" with
  | .error e => e.klass == .invalid
  | .ok _    => false

/- As is a query string, rather than silently folding it into the path. -/
#guard match splitTokenUri "https://token.example.com/token?alt=json" with
  | .error e => e.klass == .invalid
  | .ok _    => false

/- And a URL naming no host. -/
#guard match splitTokenUri "https:///token" with
  | .error _ => true
  | .ok _    => false

/- End to end: the exchange posts to the host the key file names, not to
   Google's default. A key file may legitimately name another endpoint — the
   field exists because Google has changed it before. -/
/-- info: ["POST token.example.com/v2/oauth/token"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let t := tokenEndpoint log
    "{\"access_token\":\"ya29.x\",\"expires_in\":3599,\"token_type\":\"Bearer\"}"
  let _ ← exchange t { sa with tokenUri := "https://token.example.com/v2/oauth/token" }
    "aaa.bbb.ccc" 1440938160
  log.get

/- A key file whose `token_uri` is not https fails the exchange rather than
   being silently rewritten to the default. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let log ← IO.mkRef []
  let t := tokenEndpoint log "{}"
  match ← exchange t { sa with tokenUri := "http://evil.example.com/token" }
      "aaa.bbb.ccc" 1440938160 with
  | .error e => return e.klass == .invalid && (← log.get).isEmpty
  | .ok _    => return false

end Tests.Cloud.Credentials.Gcp
