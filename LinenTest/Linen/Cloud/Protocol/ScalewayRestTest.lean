/-
  Tests for `Cloud.Protocol.ScalewayRest`.

  Two facts about Scaleway's own API shape the tests: the region travels in the
  **path** rather than the hostname, and resources are addressed by **UUID**
  where a caller knows a name — so almost every operation needs a
  list-and-find first. That extra round trip is a real cost, not an
  implementation detail, and `findId?` is where it lands.
-/
import Linen.Cloud.Protocol.ScalewayRest

open Cloud Cloud.Protocol.ScalewayRest
open Network.HTTP.Types (status200 status404)

namespace Tests.Cloud.Protocol.ScalewayRest

-- ── One host, region in the path ────────────────────────────────────────────

#guard regionalPath Scaleway.secretProduct "fr-par" "/secrets"
  == "/secret-manager/v1beta1/regions/fr-par/secrets"
#guard regionalPath Scaleway.secretProduct "pl-waw" "/secrets"
  == "/secret-manager/v1beta1/regions/pl-waw/secrets"
#guard regionalPath Scaleway.queuesProduct "fr-par" "/activate-sqs"
  == "/mnq/v1beta1/regions/fr-par/activate-sqs"

/- The product's `(name, version)` pair comes from `Cloud.Scaleway`, so the API
   version cannot drift between call sites. -/
#guard Scaleway.secretProduct.2 == "v1beta1"

-- ── Fixtures ────────────────────────────────────────────────────────────────

def creds : Credentials :=
  { accessKey := "SCWACCESSKEY", secretKey := "scw-secret-key", region := "fr-par"
  , projectId := some "8460bf58-4c44-431e-9df4-8eae3888b1ce" }

def respondWith (log : IO.Ref (List String)) (st : Network.HTTP.Types.Status)
    (body : String) : Transport :=
  Transport.stub fun req => do
    let tok := (req.headers.find? (fun h => h.1 == Data.CI.mk' "X-Auth-Token")).map (·.2)
    log.modify (fun l => l ++
      [s!"{req.method} {req.host}{req.path}{req.queryString} token={tok.getD "-"}"])
    pure { statusCode := st, headers := [], body := body.toUTF8 }

/-- A real `secrets.list` reply. Note that a caller who knows the name
    `db-password` must read the `id` out of here before it can do anything
    else. -/
def secretsList : String :=
  "{\"secrets\":[" ++
    "{\"id\":\"4f1e2d3c-0000-0000-0000-000000000001\",\"name\":\"secrets-master-key\"," ++
     "\"status\":\"ready\",\"version_count\":1}," ++
    "{\"id\":\"4f1e2d3c-0000-0000-0000-000000000002\",\"name\":\"db-password\"," ++
     "\"status\":\"ready\",\"version_count\":3}]," ++
  "\"total_count\":2}"

-- ── The token is a header, and nothing is signed ────────────────────────────

/-- info: ["GET api.scaleway.com/secret-manager/v1beta1/regions/fr-par/secrets?project_id=8460bf58-4c44-431e-9df4-8eae3888b1ce token=scw-secret-key"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let _ ← invoke (respondWith log status200 secretsList) creds "fr-par" "GET"
    (regionalPath Scaleway.secretProduct "fr-par" "/secrets")
    (query := [("project_id", some "8460bf58-4c44-431e-9df4-8eae3888b1ce")])
  log.get

/- Every region reaches the same host — the opposite of AWS, and the reason the
   region has to be in the path. -/
#guard (call creds "fr-par" "GET" "/x").endpoint.host
  == (call creds "pl-waw" "GET" "/x").endpoint.host

/- A body-less request does not claim to carry JSON. -/
#guard (call creds "fr-par" "GET" "/x").headers.length == 0
#guard (call creds "fr-par" "POST" "/x" (payload := some (.object []))).headers
  == [("Content-Type", "application/json")]

-- ── The list-and-find that UUID addressing forces ───────────────────────────

/-- info: (2, some "4f1e2d3c-0000-0000-0000-000000000002") -/
#guard_msgs in
#eval show IO (Nat × Option String) from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 secretsList) creds "fr-par" "GET" "/secrets" with
  | .error _ => return (0, none)
  | .ok v => return ((array v "secrets").length, findId? v "secrets" "name" "id" "db-password")

/- A name that is not in the listing is `none`, not the first entry — the
   failure mode that would have one secret's value returned for another. -/
#guard match Data.Json.Decode.decode secretsList with
  | .ok v => findId? v "secrets" "name" "id" "not-there" == none
  | .error _ => false

#guard match Data.Json.Decode.decode "{\"secrets\":[]}" with
  | .ok v => findId? v "secrets" "name" "id" "anything" == none
  | .error _ => false

-- ── Field accessors ────────────────────────────────────────────────────────

#guard string? (.object [("status", .string "ready")]) "status" == some "ready"
#guard nat? (.object [("total_count", .number 2.0)]) "total_count" == some 2

#guard match requireString (.object []) "id" with
  | .error e => e.klass == .protocol && (e.message.splitOn "'id'").length == 2
  | .ok _ => false

-- ── Errors ──────────────────────────────────────────────────────────────────

/- Scaleway spells the code `type`, in lowercase snake case. -/
/-- info: (Cloud.Class.notFound, "not_found") -/
#guard_msgs in
#eval show IO (Class × String) from do
  let log ← IO.mkRef []
  let body := "{\"message\":\"resource is not found\",\"type\":\"not_found\"," ++
              "\"resource\":\"secret\"}"
  match ← invoke (respondWith log status404 body) creds "fr-par" "GET" "/secrets/x" with
  | .error e => return (e.klass, e.code)
  | .ok _ => return (.protocol, "")

/- A `DELETE` answers 200 with no body. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let log ← IO.mkRef []
  match ← invoke (respondWith log status200 "") creds "fr-par" "DELETE" "/secrets/x" with
  | .ok v => return v.isNull
  | .error _ => return false

end Tests.Cloud.Protocol.ScalewayRest
