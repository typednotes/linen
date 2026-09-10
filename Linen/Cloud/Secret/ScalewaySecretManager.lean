/-
  `Cloud.Secret.ScalewaySecretManager` — Scaleway Secret Manager

  ## Everything is addressed by UUID

  Scaleway's API takes a secret's **id** where a caller knows its **name**, so
  every operation begins by listing secrets and finding the one asked for.
  That is a real extra round trip per call, not an implementation detail — so
  the resolution is memoised per store, and a store created once and reused
  pays it once per secret.

  The cache is not invalidated: a secret deleted and recreated outside this
  process keeps its old id here, and calls will fail with `notFound` until the
  store is recreated. That is preferable to re-listing on every call, and the
  failure at least names the right secret.

  ## Values are base64

  Scaleway stores a secret version's payload base64-encoded, in a `data` field,
  and returns it the same way.

  ## Project-scoped

  Listing and creating are scoped to a project, so the credentials must carry
  one. Without it Scaleway answers with an opaque error, so it is required up
  front instead.
-/
import Linen.Cloud.Secret
import Linen.Cloud.Protocol.ScalewayRest
import Linen.Data.Base64

namespace Cloud.Secret.ScalewaySecretManager

open Cloud
open Cloud.Protocol.ScalewayRest (invoke string? array findId? regionalPath)

-- See `Cloud.Secret.SecretsManager` on why `Value` is not opened here.
abbrev Json := Data.Json.Value

/-- The path of the regional secret collection. -/
def secretsPath (region : String) : String :=
  regionalPath Cloud.Scaleway.secretProduct region "/secrets"

/-- The path of one secret, by id. -/
def secretPath (region id : String) : String :=
  regionalPath Cloud.Scaleway.secretProduct region s!"/secrets/{id}"

/-- The path of a version's `access` sub-resource, which is what returns the
    payload. -/
def accessPath (region id version : String) : String :=
  regionalPath Cloud.Scaleway.secretProduct region
    s!"/secrets/{id}/versions/{version}/access"

/-- Metadata from a listing entry or a single-secret reply. -/
def metadataOf (v : Json) : Option Secret.Metadata :=
  (string? v "name").map fun name =>
    { name
    , version := (Cloud.Protocol.ScalewayRest.nat? v "version_count").map toString
    , updatedAt := string? v "updated_at" }

/-- A Scaleway Secret Manager store.

    `resolve` memoises name-to-id lookups; see the module header on why it is
    not invalidated. -/
def atRegion (t : Transport) (creds : Credentials) (region project : String) :
    IO SecretStore := do
  let cache ← IO.mkRef ([] : List (String × String))
  let resolve (name : String) : IO (Except Error String) := do
    match (← cache.get).find? (·.1 == name) with
    | some (_, id) => return .ok id
    | none =>
      match ← invoke t creds region "GET" (secretsPath region)
          (query := [("project_id", some project), ("name", some name)]) with
      | .error e => return .error e
      | .ok v =>
        match findId? v "secrets" "name" "id" name with
        | some id => do
            cache.modify (fun c => (name, id) :: c)
            return .ok id
        | none =>
          return .error
            { klass := .notFound, status := 404, code := "not_found"
            , message := s!"no secret named '{name}' in project {project}" }
  let readVersion (name version : String) : IO (Except Error Secret.Value) := do
    match ← resolve name with
    | .error e => return .error e
    | .ok id =>
      match ← invoke t creds region "GET" (accessPath region id version) with
      | .error e => return .error e
      | .ok v =>
        match string? v "data" with
        | none     => return .error (Error.protocol s!"secret '{name}' returned no data")
        | some b64 =>
          match Data.Base64.decode b64 with
          | some bytes => return .ok (Secret.Value.ofBytes bytes)
          | none       =>
            return .error (Error.protocol s!"secret '{name}' has a malformed base64 payload")
  return {
      describe := s!"scaleway secret manager in {region} (project {project})"
    , metadata := fun name => do
        match ← resolve name with
        | .error e => return (if e.klass == .notFound then .ok none else .error e)
        | .ok id =>
          match ← invoke t creds region "GET" (secretPath region id) with
          | .error e => return (if e.klass == .notFound then .ok none else .error e)
          | .ok v    => return .ok (metadataOf v)
    , getValue := fun name => readVersion name "latest"
    , getVersion := readVersion
    , put := fun name value => do
        let payload := Data.Base64.encode value.expose
        let store (id : String) : IO (Except Error Secret.Metadata) := do
          match ← invoke t creds region "POST"
              (regionalPath Cloud.Scaleway.secretProduct region s!"/secrets/{id}/versions")
              (payload := some (.object [("data", .string payload)])) with
          | .error e => return .error e
          | .ok v    =>
            return .ok
              { name
              , version := (Cloud.Protocol.ScalewayRest.nat? v "revision").map toString }
        match ← resolve name with
        | .ok id => store id
        | .error e =>
          if e.klass != .notFound then return .error e
          -- Create the secret, then add its first version.
          match ← invoke t creds region "POST" (secretsPath region)
              (payload := some (.object
                [("name", .string name), ("project_id", .string project)])) with
          | .error e => return .error e
          | .ok created =>
            match string? created "id" with
            | none    => return .error (Error.protocol "created secret has no id")
            | some id => do
                cache.modify (fun c => (name, id) :: c)
                store id
    , list := fun cursor => do
        -- Scaleway pages by number rather than by opaque token, so the cursor
        -- carries the page index.
        let page := (cursor.bind (·.token.toNat?)).getD 1
        match ← invoke t creds region "GET" (secretsPath region)
            (query := [ ("project_id", some project)
                      , ("page", some (toString page))
                      , ("page_size", some "50") ]) with
        | .error e => return .error e
        | .ok v =>
          let items := (array v "secrets").filterMap metadataOf
          let total := (Cloud.Protocol.ScalewayRest.nat? v "total_count").getD items.length
          return .ok {
              items
            , next := if page * 50 >= total then none else some ⟨toString (page + 1)⟩ } }

/-- A Scaleway Secret Manager store, taking the region and project from
    credentials. -/
def of (t : Transport) (creds : Credentials) (region : Option String := none) :
    IO (Except Error SecretStore) := do
  match creds.requireProject with
  | .error e => return .error e
  | .ok project =>
    let region' := match region with
      | some r => Except.ok r
      | none   => creds.requireRegion .scaleway
    match region' with
    | .error e => return .error e
    | .ok r    => return .ok (← atRegion t creds r project)

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard secretsPath "fr-par" == "/secret-manager/v1beta1/regions/fr-par/secrets"
#guard secretPath "fr-par" "uuid-1" == "/secret-manager/v1beta1/regions/fr-par/secrets/uuid-1"
#guard accessPath "fr-par" "uuid-1" "latest"
  == "/secret-manager/v1beta1/regions/fr-par/secrets/uuid-1/versions/latest/access"

end Cloud.Secret.ScalewaySecretManager
