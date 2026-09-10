/-
  Tests for `Cloud.Secret` and its four backends.

  The most important assertions here are the *negative* ones: that a secret
  value cannot be printed, and that a missing secret is an error rather than an
  empty value. The second is the failure that otherwise surfaces as a service
  starting up with a blank password and authenticating to nothing, hours after
  the misconfiguration.
-/
import Linen.Cloud.Secret.SecretsManager
import Linen.Cloud.Secret.ScalewaySecretManager
import Linen.Cloud.Secret.GcpSecretManager

open Cloud
open Network.HTTP.Types (status200 status400 status404)

namespace Tests.Cloud.Secret

-- ── The value does not leak ─────────────────────────────────────────────────

/- **Nothing that prints can spill a secret.** `Repr` and `ToString` both
   redact, so a `dbg_trace`, an error message, a panic or an `#eval` all show
   `<redacted>`. -/
#guard toString (Secret.Value.ofString "hunter2") == "<redacted>"
#guard toString (repr (Secret.Value.ofString "hunter2")) == "<redacted>"

/-- info: <redacted> -/
#guard_msgs in
#eval Secret.Value.ofString "hunter2"

/- A length is *not* a secret, and it is often the only thing needed to tell
   "empty" from "wrong". -/
#guard (Secret.Value.ofString "hunter2").size == 7
#guard (Secret.Value.ofString "").isEmpty
#guard !(Secret.Value.ofString "x").isEmpty

/- `expose` is the single named exit, so `grep expose` finds every use. -/
#guard (Secret.Value.ofString "hunter2").exposeString? == some "hunter2"
#guard (Secret.Value.ofString "hunter2").expose.size == 7

/- A binary secret read as text is a value, not a panic — the correction over
   `String.fromUTF8!`. -/
#guard (Secret.Value.ofBytes ⟨#[0xff, 0xfe]⟩).exposeString? == none
#guard (Secret.Value.ofBytes ⟨#[0xff, 0xfe]⟩).size == 2

/- Metadata renders normally, because being able to log "which version is
   deployed" is the first question of every incident involving a secret. -/
/-- info: { name := "db-password", version := some "3", updatedAt := none, labels := [] } -/
#guard_msgs in
#eval ({ name := "db-password", version := some "3" } : Secret.Metadata)

-- ── The in-memory backend ───────────────────────────────────────────────────

/-- info: (some "hunter2", some "1") -/
#guard_msgs in
#eval show IO (Option String × Option String) from do
  let s ← SecretStore.inMemory
  let m ← s.putString "db-password" "hunter2"
  let got ← s.getString "db-password"
  return (got.toOption, (m.toOption).bind (·.version))

/- **A missing secret is `notFound`, not an empty value.** This is the whole
   point of the type: a service that starts with a blank password authenticates
   to nothing and reports why much later. -/
/-- info: (Cloud.Class.notFound, none, false) -/
#guard_msgs in
#eval show IO (Class × Option String × Bool) from do
  let s ← SecretStore.inMemory
  let missing ← s.getValue "nope"
  let opt ← s.getString? "nope"
  let ex ← s.exists? "nope"
  return ( (match missing with | .error e => e.klass | .ok _ => .protocol)
         , opt.toOption.getD none
         , ex.toOption.getD true )

/- **`metadata` never returns a value**, which is what makes the permission
   split in the effect layer meaningful. -/
/-- info: (some "db-password", some "1") -/
#guard_msgs in
#eval show IO (Option String × Option String) from do
  let s ← SecretStore.inMemory
  let _ ← s.putString "db-password" "hunter2"
  match ← s.metadata "db-password" with
  | .ok (some m) => return (some m.name, m.version)
  | _ => return (none, none)

/- Versions accumulate, and an old one stays readable by number. -/
/-- info: (some "second", some "first", some "2") -/
#guard_msgs in
#eval show IO (Option String × Option String × Option String) from do
  let s ← SecretStore.inMemory
  let _ ← s.putString "k" "first"
  let m2 ← s.putString "k" "second"
  let latest ← s.getString "k"
  let first ← s.getVersion "k" "1"
  return (latest.toOption, first.toOption.bind (·.exposeString?), m2.toOption.bind (·.version))

/- A version that does not exist is `notFound`, not the latest. -/
/-- info: Cloud.Class.notFound -/
#guard_msgs in
#eval show IO Class from do
  let s ← SecretStore.inMemory
  let _ ← s.putString "k" "v"
  match ← s.getVersion "k" "99" with
  | .error e => return e.klass
  | .ok _ => return .protocol

/- A binary secret round-trips, and reading it as text is a `protocol` error. -/
/-- info: (#[0, 255], Cloud.Class.protocol) -/
#guard_msgs in
#eval show IO (Array UInt8 × Class) from do
  let s ← SecretStore.inMemory
  let _ ← s.put "bin" (Secret.Value.ofBytes ⟨#[0, 255]⟩)
  let raw ← s.getValue "bin"
  let asText ← s.getString "bin"
  return ( (raw.toOption.map (fun v => v.expose.toList.toArray)).getD #[]
         , (match asText with | .error e => e.klass | .ok _ => .notFound) )

/- The listing is paginated, and returns metadata only. -/
/-- info: (3, false, 5, true) -/
#guard_msgs in
#eval show IO (Nat × Bool × Nat × Bool) from do
  let s ← SecretStore.inMemory
  for i in [0, 1, 2, 3, 4] do
    let _ ← s.putString s!"k{i}" "v"
  let firstPage ← s.list none
  let whole ← s.listAll
  return ( (firstPage.toOption.map (·.items.length)).getD 0
         , (firstPage.toOption.map (·.isLast)).getD true
         , (whole.toOption.map (·.items.length)).getD 0
         , (whole.toOption.map (·.complete)).getD false )

-- ── AWS Secrets Manager ─────────────────────────────────────────────────────

def creds : Credentials :=
  { accessKey := "AKIDEXAMPLE"
  , secretKey := "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
  , region := "eu-west-3" }

def awsEp : Endpoint :=
  { host := "secretsmanager.eu-west-3.amazonaws.com"
  , service := "secretsmanager", region := "eu-west-3" }

def wire (log : IO.Ref (List String)) (body : String)
    (st : Network.HTTP.Types.Status := status200) : Transport :=
  Transport.stub fun req => do
    let target := (req.headers.find? (fun h => h.1 == Data.CI.mk' "X-Amz-Target")).map (·.2)
    log.modify (fun l => l ++ [target.getD "-"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/-- info: (["secretsmanager.GetSecretValue"], some "hunter2") -/
#guard_msgs in
#eval show IO (List String × Option String) from do
  let log ← IO.mkRef []
  let s := Secret.SecretsManager.atEndpoint (wire log "{\"SecretString\":\"hunter2\"}") creds awsEp
  let got ← s.getString "db-password"
  return (← log.get, got.toOption)

/- **A binary secret is readable.** AWS answers with `SecretBinary` instead of
   `SecretString`, and handling only the string case makes every binary secret
   unreachable. -/
/-- info: #[0, 255] -/
#guard_msgs in
#eval show IO (Array UInt8) from do
  let log ← IO.mkRef []
  let s := Secret.SecretsManager.atEndpoint (wire log "{\"SecretBinary\":\"AP8=\"}") creds awsEp
  match ← s.getValue "bin" with
  | .ok v => return v.expose.toList.toArray
  | .error _ => return #[]

/- A reply with neither field is a `protocol` error rather than an empty
   secret. -/
/-- info: Cloud.Class.protocol -/
#guard_msgs in
#eval show IO Class from do
  let log ← IO.mkRef []
  let s := Secret.SecretsManager.atEndpoint (wire log "{}") creds awsEp
  match ← s.getValue "db-password" with
  | .error e => return e.klass
  | .ok _ => return .notFound

/- `metadata` maps a `notFound` to `none`, so "is it configured" needs no
   exception handling. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let log ← IO.mkRef []
  let body := "{\"__type\":\"ResourceNotFoundException\",\"message\":\"not found\"}"
  let s := Secret.SecretsManager.atEndpoint (wire log body status400) creds awsEp
  match ← s.metadata "nope" with
  | .ok none => return true
  | _ => return false

/- **`put` falls back to `CreateSecret` when the secret does not exist**, which
   is what makes the portable "store a new version" work on a cloud that splits
   the two operations. -/
/-- info: ["secretsmanager.PutSecretValue", "secretsmanager.CreateSecret"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let counter ← IO.mkRef 0
  let t := Transport.stub fun req => do
    let target := (req.headers.find? (fun h => h.1 == Data.CI.mk' "X-Amz-Target")).map (·.2)
    log.modify (fun l => l ++ [target.getD "-"])
    let n ← counter.modifyGet (fun n => (n + 1, n + 1))
    if n == 1 then
      pure { statusCode := status400, headers := []
           , body := "{\"__type\":\"ResourceNotFoundException\"}".toUTF8 }
    else
      pure { statusCode := status200, headers := []
           , body := "{\"Name\":\"k\",\"VersionId\":\"v-1\"}".toUTF8 }
  let s := Secret.SecretsManager.atEndpoint t creds awsEp
  let _ ← s.putString "k" "v"
  log.get

/- An idempotency token is generated per call, because the raw API refuses
   `PutSecretValue` without one — the SDKs hide this. -/
/-- info: (32, true) -/
#guard_msgs in
#eval show IO (Nat × Bool) from do
  let a ← Secret.SecretsManager.clientRequestToken
  let b ← Secret.SecretsManager.clientRequestToken
  return (a.length, a != b)

-- ── Scaleway Secret Manager ─────────────────────────────────────────────────

def scwCreds : Credentials :=
  { accessKey := "SCW", secretKey := "scw-secret", region := "fr-par"
  , projectId := some "proj-1" }

/- **Every operation resolves a name to a UUID first**, and the result is
    memoised — so a second read of the same secret makes one call, not two. -/
/--
info: (["GET /secret-manager/v1beta1/regions/fr-par/secrets",
  "GET /secret-manager/v1beta1/regions/fr-par/secrets/uuid-2/versions/latest/access",
  "GET /secret-manager/v1beta1/regions/fr-par/secrets/uuid-2/versions/latest/access"],
 some "hunter2")
-/
#guard_msgs in
#eval show IO (List String × Option String) from do
  let log ← IO.mkRef []
  let t := Transport.stub fun req => do
    log.modify (fun l => l ++ [s!"{req.method} {req.path}"])
    let body :=
      if (req.path.splitOn "access").length == 2 then
        "{\"data\":\"aHVudGVyMg==\"}"
      else
        "{\"secrets\":[{\"id\":\"uuid-2\",\"name\":\"db-password\"}],\"total_count\":1}"
    pure { statusCode := status200, headers := [], body := body.toUTF8 }
  let s ← Secret.ScalewaySecretManager.atRegion t scwCreds "fr-par" "proj-1"
  let _ ← s.getString "db-password"
  let second ← s.getString "db-password"
  return (← log.get, second.toOption)

/- A name that is not in the listing is `notFound`, not the first secret — the
   failure mode that would return one secret's value for another. -/
/-- info: Cloud.Class.notFound -/
#guard_msgs in
#eval show IO Class from do
  let t := Transport.stub fun _ =>
    pure { statusCode := status200, headers := []
         , body := "{\"secrets\":[],\"total_count\":0}".toUTF8 }
  let s ← Secret.ScalewaySecretManager.atRegion t scwCreds "fr-par" "proj-1"
  match ← s.getValue "db-password" with
  | .error e => return e.klass
  | .ok _ => return .protocol

/- Scaleway needs a project, and says so before a request rather than after an
   opaque error. -/
/-- info: Cloud.Class.unbound -/
#guard_msgs in
#eval show IO Class from do
  match ← Secret.ScalewaySecretManager.of Transport.network { region := "fr-par" } with
  | .error e => return e.klass
  | .ok _ => return .protocol

-- ── Google Secret Manager ───────────────────────────────────────────────────

/- **The payload is nested under `payload`**, and base64. Reading `data` from
    the top level finds nothing and yields a blank secret. -/
/-- info: (["GET /v1/projects/typednotes/secrets/db-password/versions/latest:access"], some "hunter2") -/
#guard_msgs in
#eval show IO (List String × Option String) from do
  let log ← IO.mkRef []
  let t := Transport.stub fun req => do
    log.modify (fun l => l ++ [s!"{req.method} {req.path}"])
    pure { statusCode := status200, headers := []
         , body := "{\"name\":\"projects/typednotes/secrets/db-password/versions/3\",\
\"payload\":{\"data\":\"aHVudGVyMg==\"}}".toUTF8 }
  let s := Secret.GcpSecretManager.atProject t "ya29.tok" "typednotes"
  let got ← s.getString "db-password"
  return (← log.get, got.toOption)

/- Storing a version is `:addVersion`, and creating first needs the name as a
   query parameter plus a replication policy. -/
/--
info: ["POST /v1/projects/typednotes/secrets/k:addVersion", "POST /v1/projects/typednotes/secrets?secretId=k",
 "POST /v1/projects/typednotes/secrets/k:addVersion"]
-/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let counter ← IO.mkRef 0
  let t := Transport.stub fun req => do
    log.modify (fun l => l ++ [s!"{req.method} {req.path}{req.queryString}"])
    let n ← counter.modifyGet (fun n => (n + 1, n + 1))
    if n == 1 then
      pure { statusCode := status404, headers := []
           , body := "{\"error\":{\"code\":404,\"status\":\"NOT_FOUND\"}}".toUTF8 }
    else
      pure { statusCode := status200, headers := []
           , body := "{\"name\":\"projects/typednotes/secrets/k/versions/1\"}".toUTF8 }
  let s := Secret.GcpSecretManager.atProject t "ya29.tok" "typednotes"
  let _ ← s.putString "k" "v"
  log.get

/- No token, or no project, is `unbound` before any request. -/
#guard match Secret.GcpSecretManager.of Transport.network {} with
  | .error e => e.klass == .unbound
  | .ok _ => false

#guard match Secret.GcpSecretManager.of Transport.network { accessToken := some "t" } with
  | .error e => e.klass == .unbound
  | .ok _ => false

-- ── All three clouds support binary secrets ─────────────────────────────────

#guard Provider.aws.supports .secretBinary
#guard Provider.gcp.supports .secretBinary
#guard Provider.scaleway.supports .secretBinary

end Tests.Cloud.Secret
