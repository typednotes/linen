/-
  `Cloud.Secret.SecretsManager` — AWS Secrets Manager

  ## `ClientRequestToken` is mandatory

  `PutSecretValue` and `CreateSecret` require an idempotency token, and the raw
  API **refuses the call without one** — the official SDKs generate it, so this
  is invisible until one writes a client by hand. A fresh random token is
  generated per call from `Crypto.SecureRandom`.

  Note what that means: because the token is fresh each time, these calls are
  *not* idempotent from this client's point of view. Retrying a `put` after a
  timeout can create two versions. That is the honest behaviour — deduplicating
  would need the caller to supply the token, which the portable interface has
  nowhere to put.

  ## Two value fields, not one

  `GetSecretValue` answers with **either** `SecretString` or `SecretBinary`
  (base64), depending on how the secret was written. Handling only the string
  case — as the sibling `typednotes/infra` does, erroring otherwise — makes
  every binary secret unreadable. `Cloud.Secret.Value` holds bytes, so both
  land in the same place.

  ## Creating on write

  The portable `put` means "store a new version, creating the secret if it does
  not exist", and AWS splits that across two operations. So `put` tries
  `PutSecretValue` and falls back to `CreateSecret` on `notFound`. The
  alternative — requiring the caller to know which — is a distinction the other
  two clouds do not make.
-/
import Linen.Cloud.Secret
import Linen.Cloud.Protocol.AwsJson
import Linen.Crypto.SecureRandom
import Linen.Data.Hex
import Linen.Data.Base64

namespace Cloud.Secret.SecretsManager

open Cloud
open Cloud.Protocol.AwsJson (invoke string? array field?)
-- Not `open Data.Json (Value)`: inside `Cloud.Secret.*` a bare `Value` is
-- `Cloud.Secret.Value`, the secret payload, which is a different thing.
abbrev Json := Data.Json.Value

/-- The `X-Amz-Target` for a Secrets Manager operation. -/
def target (op : String) : String := "secretsmanager." ++ op

/-- A fresh idempotency token. 32 hex characters, within AWS's 32–64 range. -/
def clientRequestToken : IO String := do
  return Data.Hex.encode (← Crypto.SecureRandom.randomBytes 16)

/-- The value from a `GetSecretValue` reply, from whichever field carries it.

    `SecretString` is text; `SecretBinary` is base64. A reply with neither is a
    `protocol` error rather than an empty secret, which would otherwise start a
    service with a blank password. -/
def valueOf (v : Json) (name : String) : Except Error Secret.Value :=
  match string? v "SecretString" with
  | some s => .ok (Secret.Value.ofString s)
  | none   =>
    match string? v "SecretBinary" with
    | some b64 =>
      match Data.Base64.decode b64 with
      | some bytes => .ok (Secret.Value.ofBytes bytes)
      | none       =>
        .error (Error.protocol s!"secret '{name}' has a malformed SecretBinary")
    | none =>
      .error (Error.protocol
        s!"secret '{name}' has neither SecretString nor SecretBinary")

/-- Metadata from a `DescribeSecret` or `ListSecrets` entry.

    The current version is the one AWS labels `AWSCURRENT`; a secret has
    several versions with stage labels rather than a single "latest". -/
def metadataOf (v : Json) : Option Secret.Metadata :=
  (string? v "Name").map fun name =>
    { name
    , version := currentVersion? v
    , updatedAt := string? v "LastChangedDate"
    , labels :=
        (array v "Tags").filterMap fun tag =>
          match string? tag "Key", string? tag "Value" with
          | some k, some val => some (k, val)
          | _, _ => none }
where
  /-- The version id labelled `AWSCURRENT`, from `VersionIdsToStages`. -/
  currentVersion? (v : Json) : Option String :=
    ((field? v "VersionIdsToStages").bind Data.Json.Value.asObject).bind fun fields =>
      (fields.find? fun (_, stages) =>
        match stages.asArray with
        | some a => a.toList.any (fun s => s.asString == some "AWSCURRENT")
        | none   => false).map (·.1)

/-- An AWS Secrets Manager store. -/
def atEndpoint (t : Transport) (creds : Credentials) (ep : Endpoint) : SecretStore :=
  let version := Cloud.SecretsManager.jsonVersion
  { describe := s!"aws secrets manager at {ep.host}"
  , metadata := fun name => do
      match ← invoke t creds ep version (target "DescribeSecret")
          (.object [("SecretId", .string name)]) with
      | .error e => return (if e.klass == .notFound then .ok none else .error e)
      | .ok v    => return .ok (metadataOf v)
  , getValue := fun name => do
      match ← invoke t creds ep version (target "GetSecretValue")
          (.object [("SecretId", .string name)]) with
      | .error e => return .error e
      | .ok v    => return valueOf v name
  , getVersion := fun name versionId => do
      match ← invoke t creds ep version (target "GetSecretValue")
          (.object [("SecretId", .string name), ("VersionId", .string versionId)]) with
      | .error e => return .error e
      | .ok v    => return valueOf v name
  , put := fun name value => do
      let token ← clientRequestToken
      let payload : Json :=
        match value.exposeString? with
        | some s => .string s
        | none   => .string (Data.Base64.encode value.expose)
      let field := if (value.exposeString?).isSome then "SecretString" else "SecretBinary"
      match ← invoke t creds ep version (target "PutSecretValue")
          (.object [ ("SecretId", .string name), (field, payload)
                   , ("ClientRequestToken", .string token) ]) with
      | .ok v => return .ok ((metadataOf v).getD { name, version := string? v "VersionId" })
      | .error e =>
        if e.klass != .notFound then return .error e
        -- The secret does not exist yet; the portable `put` creates it.
        let token' ← clientRequestToken
        match ← invoke t creds ep version (target "CreateSecret")
            (.object [ ("Name", .string name), (field, payload)
                     , ("ClientRequestToken", .string token') ]) with
        | .error e => return .error e
        | .ok v    => return .ok ((metadataOf v).getD { name, version := string? v "VersionId" })
  , list := fun cursor => do
      let body : Json := .object <|
        match cursor with
        | some c => [("NextToken", .string c.token)]
        | none   => []
      match ← invoke t creds ep version (target "ListSecrets") body with
      | .error e => return .error e
      | .ok v =>
        return .ok {
            items := (array v "SecretList").filterMap metadataOf
          , next := (string? v "NextToken").map Cursor.mk } }

/-- An AWS Secrets Manager store for the credentials' region. -/
def of (t : Transport) (creds : Credentials) (region : Option String := none) :
    Except Error SecretStore := do
  let region ← match region with
    | some r => .ok r
    | none   => creds.requireRegion .aws
  .ok (atEndpoint t creds (Cloud.SecretsManager.endpoint region))

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard target "GetSecretValue" == "secretsmanager.GetSecretValue"

-- A string secret and a binary secret both land in `Secret.Value`.
#guard match valueOf (.object [("SecretString", .string "hunter2")]) "s" with
  | .ok v => v.exposeString? == some "hunter2"
  | .error _ => false

#guard match valueOf (.object [("SecretBinary", .string "/w==")]) "s" with
  | .ok v => v.expose.toList == [0xff]
  | .error _ => false

-- Neither field is a `protocol` error, not an empty secret: a service starting
-- with a blank password is the failure this prevents.
#guard match valueOf (.object []) "db-password" with
  | .error e => e.klass == .protocol && (e.message.splitOn "db-password").length == 2
  | .ok _ => false

-- The current version is the one labelled `AWSCURRENT`, not simply the first.
#guard (metadataOf (.object
  [ ("Name", .string "db-password")
  , ("VersionIdsToStages", .object
      [ ("v-old", .array #[.string "AWSPREVIOUS"])
      , ("v-new", .array #[.string "AWSCURRENT"]) ]) ])).bind (·.version) == some "v-new"

end Cloud.Secret.SecretsManager
