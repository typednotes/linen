/-
  `Cloud.Secret.GcpSecretManager` — Google Secret Manager

  ## Reading a value is a `:access` on a *version*

  A secret is a container; its payload lives on a version, and reading one is
  `GET …/versions/latest:access`. The `latest` alias saves a caller from
  tracking version numbers, which are monotonic integers here — unlike AWS's
  opaque ids, so a version string does not travel between clouds.

  ## The payload is nested and base64

  `{"name": …, "payload": {"data": "…"}}`. Reading `data` from the top level —
  the obvious mistake — finds nothing and yields an empty secret, which is the
  failure mode worth avoiding: a service that starts with a blank password
  authenticates to nothing and says why only much later.

  ## Creating requires a replication policy

  `secrets.create` takes the new secret's name as a **query parameter**
  (`secretId`) and requires a `replication` policy in the body; automatic
  replication is the default any caller without a data-residency requirement
  wants. Omitting it is rejected.
-/
import Linen.Cloud.Secret
import Linen.Cloud.Protocol.GoogleRest
import Linen.Data.Base64

namespace Cloud.Secret.GcpSecretManager

open Cloud
open Cloud.Protocol.GoogleRest (invoke string? array field? nextPageToken?)

-- See `Cloud.Secret.SecretsManager` on why `Value` is not opened here.
abbrev Json := Data.Json.Value

/-- The project's secret collection. -/
def secretsPath (project : String) : String := s!"/v1/projects/{project}/secrets"

/-- One secret. -/
def secretPath (project name : String) : String :=
  s!"/v1/projects/{project}/secrets/{name}"

/-- A version's `access` sub-resource, which is what returns the payload. -/
def accessPath (project name version : String) : String :=
  s!"/v1/projects/{project}/secrets/{name}/versions/{version}:access"

/-- The `:addVersion` action, which stores a new payload. -/
def addVersionPath (project name : String) : String :=
  s!"/v1/projects/{project}/secrets/{name}:addVersion"

/-- Metadata from a secret resource.

    Google answers with a fully-qualified `name`, so the short name is what a
    caller recognises. -/
def metadataOf (v : Json) : Option Secret.Metadata :=
  (string? v "name").map fun full =>
    { name := Gcp.shortName full
    , updatedAt := string? v "createTime"
    , labels :=
        match (field? v "labels").bind Data.Json.Value.asObject with
        | none        => []
        | some fields => fields.filterMap fun (k, av) => av.asString.map (fun s => (k, s)) }

/-- The payload from an `:access` reply.

    Nested under `payload`; see the module header on why reading the top level
    silently yields nothing. -/
def payloadOf (v : Json) (name : String) : Except Error Secret.Value :=
  match (field? v "payload").bind (fun p => string? p "data") with
  | none     =>
    .error (Error.protocol s!"secret '{name}' returned no payload.data")
  | some b64 =>
    match Data.Base64.decode b64 with
    | some bytes => .ok (Secret.Value.ofBytes bytes)
    | none       =>
      .error (Error.protocol s!"secret '{name}' has a malformed base64 payload")

/-- A Google Secret Manager store. -/
def atProject (t : Transport) (token project : String) : SecretStore :=
  let host := Gcp.secretManagerHost
  { describe := s!"google secret manager (project {project})"
  , metadata := fun name => do
      match ← invoke t token host "GET" (secretPath project name) with
      | .error e => return (if e.klass == .notFound then .ok none else .error e)
      | .ok v    => return .ok (metadataOf v)
  , getValue := fun name => do
      match ← invoke t token host "GET" (accessPath project name "latest") with
      | .error e => return .error e
      | .ok v    => return payloadOf v name
  , getVersion := fun name version => do
      match ← invoke t token host "GET" (accessPath project name version) with
      | .error e => return .error e
      | .ok v    => return payloadOf v name
  , put := fun name value => do
      let body : Json :=
        .object [("payload", .object [("data", .string (Data.Base64.encode value.expose))])]
      match ← invoke t token host "POST" (addVersionPath project name) [] (some body) with
      | .ok v =>
        return .ok { name, version := (string? v "name").map Gcp.shortName }
      | .error e =>
        if e.klass != .notFound then return .error e
        -- Create the secret first. The name is a query parameter, and the
        -- replication policy is mandatory.
        match ← invoke t token host "POST" (secretsPath project)
            [("secretId", some name)]
            (some (.object [("replication", .object [("automatic", .object [])])])) with
        | .error e => return .error e
        | .ok _ =>
          match ← invoke t token host "POST" (addVersionPath project name) [] (some body) with
          | .error e => return .error e
          | .ok v    =>
            return .ok { name, version := (string? v "name").map Gcp.shortName }
  , list := fun cursor => do
      let query := match cursor with
        | some c => [("pageToken", some c.token)]
        | none   => []
      match ← invoke t token host "GET" (secretsPath project) query with
      | .error e => return .error e
      | .ok v =>
        return .ok {
            items := (array v "secrets").filterMap metadataOf
          , next := nextPageToken? v } }

/-- A Google Secret Manager store, taking the project and token from
    credentials. -/
def of (t : Transport) (creds : Credentials) : Except Error SecretStore := do
  let token ← creds.requireToken .gcp
  let project ← creds.requireProject
  .ok (atProject t token project)

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard secretsPath "typednotes" == "/v1/projects/typednotes/secrets"
#guard secretPath "typednotes" "db-password"
  == "/v1/projects/typednotes/secrets/db-password"
#guard accessPath "typednotes" "db-password" "latest"
  == "/v1/projects/typednotes/secrets/db-password/versions/latest:access"
#guard addVersionPath "typednotes" "db-password"
  == "/v1/projects/typednotes/secrets/db-password:addVersion"

-- The payload is nested. Reading the top level finds nothing, which is the
-- mistake that yields a blank password.
#guard match payloadOf (.object [("payload", .object [("data", .string "aHVudGVyMg==")])])
    "db-password" with
  | .ok v => v.exposeString? == some "hunter2"
  | .error _ => false

#guard match payloadOf (.object [("data", .string "aHVudGVyMg==")]) "db-password" with
  | .error e => e.klass == .protocol
  | .ok _ => false

-- Google answers with a fully-qualified name where the caller asked for a
-- short one.
#guard (metadataOf (.object [("name", .string "projects/p/secrets/db-password")])).map (·.name)
  == some "db-password"

end Cloud.Secret.GcpSecretManager
