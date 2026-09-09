/-
  Tests for `Linen.Control.Monad.Effect.HTTP`.

  Covers both halves of the capability system, as `FileSystemTest` does:

  - **Method permissions** — which operations are allowed. A withheld method
    makes the call fail to elaborate, asserted with `#guard_msgs`.
  - **URL scope** — which arguments those operations may be called on. A URL
    outside the capability's scopes makes the obligation
    `cap.permits m url = true` *unsatisfiable*, which is asserted directly:
    proving it equals `false` shows no proof of `= true` can exist. That is a
    stronger statement than matching an error message, and it does not rot across
    compiler versions the way a message match does.

  Plus a round trip through a stub transport, so no test touches the network.
-/
import Linen.Control.Monad.Effect.HTTP

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.HTTP
open Network.HTTP.Types (StdMethod status200)
open Network.HTTP.Client (Request Response)

namespace Tests.Control.Monad.Effect.HTTP

-- ── The `u!` URL macro ──────────────────────────────────────────────────────

#guard (u!"https://api.example.com/v1/users" : Url)
  == ⟨true, ["api", "example", "com"], 443, ["v1", "users"]⟩
#guard (u!"http://example.test/" : Url) == ⟨false, ["example", "test"], 80, []⟩
#guard (u!"https://example.test" : Url) == ⟨true, ["example", "test"], 443, []⟩

-- An explicit port overrides the scheme's default.
#guard (u!"http://localhost:8080/x" : Url) == ⟨false, ["localhost"], 8080, ["x"]⟩
#guard (u!"https://api.test:8443/" : Url) == ⟨true, ["api", "test"], 8443, []⟩

-- Query strings and fragments are dropped: neither is scope-relevant.
#guard (u!"https://api.test/v1?page=2" : Url) == ⟨true, ["api", "test"], 443, ["v1"]⟩
#guard (u!"https://api.test/v1#top" : Url) == ⟨true, ["api", "test"], 443, ["v1"]⟩

-- Empty segments collapse, as in `p!`.
#guard (u!"https://api.test//v1//users/" : Url)
  == ⟨true, ["api", "test"], 443, ["v1", "users"]⟩

-- Rendering round-trips, with the default port left implicit.
#guard Url.toString u!"https://api.example.com/v1/users"
  == "https://api.example.com/v1/users"
#guard Url.toString u!"http://localhost:8080/x" == "http://localhost:8080/x"
#guard Url.pathString u!"https://api.test/v1/users" == "/v1/users"
#guard Url.pathString u!"https://api.test" == "/"

-- ── Permission obligations ──────────────────────────────────────────────────

-- A granted permission is discharged by `rfl` on a concrete capability.
example : readOnlyWeb.canGet = true := rfl
example : readOnlyWeb.canHead = true := rfl
example : fullWeb.canDelete = true := rfl

-- A withheld permission is provably absent, not merely unproven.
example : readOnlyWeb.canPost = false := rfl
example : readOnlyWeb.canDelete = false := rfl

-- The instances exist exactly when the corresponding bit is set.
example : CanGet readOnlyWeb := inferInstance
example : CanHead readOnlyWeb := inferInstance
example : CanPost fullWeb := inferInstance
example : CanDelete fullWeb := inferInstance

-- The proof carried by an instance really is the field equation — this is why an
-- instance cannot be forged for a capability lacking the bit.
example : CanGet.proof (cap := readOnlyWeb) = (rfl : readOnlyWeb.canGet = true) := rfl

-- `allows` agrees with the bits, and denies the methods this effect never grants.
#guard readOnlyWeb.allows .GET
#guard !readOnlyWeb.allows .POST
#guard !fullWeb.allows .TRACE
#guard !fullWeb.allows .CONNECT
#guard !fullWeb.allows .OPTIONS

-- ── Positive: permitted operations elaborate ────────────────────────────────

-- `cap` is inferred from the row rather than supplied — what the `outParam` on
-- `HasHTTP` buys — and it works inside `do`-notation.
example : Eff [HTTP readOnlyWeb] Response := get u!"https://example.test/"

example : Eff [HTTP fullWeb] Response :=
  post u!"https://example.test/items" (String.toUTF8 "{}")

example : Eff [HTTP fullWeb] Response := delete u!"https://example.test/items/1"

example : Eff [HTTP fullWeb] (Option String) := do
  let _ ← postString u!"https://example.test/items" "{}"
  let s ← getString? u!"https://example.test/items"
  let _ ← delete u!"https://example.test/items/1"
  pure s

-- ── Negative: withheld methods do not elaborate ─────────────────────────────

-- Posting under a read-only capability. `HTTP` *is* in the row, so a name-only
-- effect row would admit this call; the capability value rejects it.
/--
error: failed to synthesize instance of type class
  CanPost readOnlyWeb

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
#check (post (effs := [HTTP readOnlyWeb]) u!"https://example.test/" (String.toUTF8 "x"))

-- Deleting under a read-only capability: read and write are split within a
-- single effect.
/--
error: failed to synthesize instance of type class
  CanDelete readOnlyWeb

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
#check (delete (effs := [HTTP readOnlyWeb]) u!"https://example.test/")

-- A capability that may write but not read. `get` is spelled out here because
-- a bare `#check get` is ambiguous with `MonadState.get`; every other use in
-- this file is disambiguated by its expected type.
abbrev writeOnlyWeb : Capability := { canPost := true }

/--
error: failed to synthesize instance of type class
  CanGet writeOnlyWeb

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
#check (Control.Monad.Effect.HTTP.get (effs := [HTTP writeOnlyWeb]) u!"https://example.test/")

-- ── URL scope ───────────────────────────────────────────────────────────────

abbrev api : Capability := readOnlyUnder u!"https://api.example.com/v1"

-- Inside the scope: the obligation holds, so the operations elaborate.
#guard api.permits .GET u!"https://api.example.com/v1"
#guard api.permits .GET u!"https://api.example.com/v1/users"
#guard api.permits .GET u!"https://api.example.com/v1/users/1/orders"
#guard api.permits .HEAD u!"https://api.example.com/v1/users"

example : Eff [HTTP api] Response := get u!"https://api.example.com/v1/users"
example : Eff [HTTP api] Response := head u!"https://api.example.com/v1"
example : Eff [HTTP api] (Option String) := do
  let _ ← get u!"https://api.example.com/v1/a"
  getString? u!"https://api.example.com/v1/b"

-- Outside the scope the obligation is provably *false*, so no proof of `= true`
-- exists and the call cannot be written at all.
#guard !api.permits .GET u!"https://evil.test/"
example : api.permits .GET u!"https://evil.test/" = false := rfl
theorem no_get_off_origin : api.permits .GET u!"https://evil.test/" ≠ true := by decide

-- The host-suffix trap: `api.example.com.evil.com` *is* a string-prefix
-- extension of `api.example.com`, but is a different host. Label-wise
-- comparison rejects it — which is why `host` is a label list, not a `String`.
#guard !api.permits .GET u!"https://api.example.com.evil.com/v1"
theorem no_host_suffix_escape :
    api.permits .GET u!"https://api.example.com.evil.com/v1" ≠ true := by decide

-- A subdomain is not the host either, in either direction.
#guard !api.permits .GET u!"https://staging.api.example.com/v1"
#guard !api.permits .GET u!"https://example.com/v1"

-- The scheme is part of the scope: an https capability does not admit http.
#guard !api.permits .GET u!"http://api.example.com/v1"
theorem no_scheme_downgrade :
    api.permits .GET u!"http://api.example.com/v1" ≠ true := by decide

-- So is the port.
#guard !api.permits .GET u!"https://api.example.com:8443/v1"
theorem no_port_change :
    api.permits .GET u!"https://api.example.com:8443/v1" ≠ true := by decide

-- The sibling-path trap, the same one `FileSystem` pins: `/v1-admin` is a
-- string-prefix extension of `/v1` but is not under it.
#guard !api.permits .GET u!"https://api.example.com/v1-admin/keys"
theorem no_sibling_path_escape :
    api.permits .GET u!"https://api.example.com/v1-admin/keys" ≠ true := by decide

-- A parent of the scope path is not inside it.
#guard !api.permits .GET u!"https://api.example.com/"

-- An empty `scopes` means unrestricted, so a permission-only capability admits
-- any URL — this is what keeps `readOnlyWeb` usable.
#guard readOnlyWeb.scopes.isEmpty
#guard readOnlyWeb.permits .GET u!"https://anything.test/at/all"
example (m : StdMethod) (u : Url) : readOnlyWeb.permits m u = true := rfl

-- Two capabilities granting the *same methods* but differing in scope: the
-- distinction a type-level effect row cannot draw at all.
example : readOnlyWeb.canGet = api.canGet ∧ readOnlyWeb.canHead = api.canHead :=
  ⟨rfl, rfl⟩
example : readOnlyWeb.permits .GET u!"https://evil.test/" = true := rfl
example : api.permits .GET u!"https://evil.test/" = false := rfl

-- ── Per-scope methods ───────────────────────────────────────────────────────

-- "Read anywhere under /v1, but POST only to /v1/events" — one capability, two
-- scopes over the same origin with different method lists.
abbrev events : Capability :=
  restClient u!"https://api.example.com/v1" u!"https://api.example.com/v1/events"

#guard events.permits .GET u!"https://api.example.com/v1/users"
#guard events.permits .POST u!"https://api.example.com/v1/events"
#guard events.permits .POST u!"https://api.example.com/v1/events/batch"

example : Eff [HTTP events] Response := get u!"https://api.example.com/v1/users"
example : Eff [HTTP events] Response :=
  post u!"https://api.example.com/v1/events" (String.toUTF8 "{}")

-- POST *is* granted by the capability's bits, so `CanPost` resolves — and the
-- request is still rejected, because no scope covers that method at that URL.
-- This is the case a method-level permission alone cannot express.
#guard events.canPost
example : CanPost events := inferInstance
#guard !events.permits .POST u!"https://api.example.com/v1/users"
theorem no_post_outside_events :
    events.permits .POST u!"https://api.example.com/v1/users" ≠ true := by decide

-- And the reverse: GET is not admitted by the write scope's method list, but the
-- read scope covers the same path, so it is permitted anyway.
#guard events.permits .GET u!"https://api.example.com/v1/events"

-- ── Runtime-validated URLs ──────────────────────────────────────────────────

-- A URL known only at runtime cannot be checked by `decide`; `check?` validates
-- it and hands back the evidence.
#guard (ScopedUrl.check? api .GET u!"https://api.example.com/v1/ok").isSome
#guard (ScopedUrl.check? api .GET u!"https://evil.test/").isNone
#guard (ScopedUrl.check? events .POST u!"https://api.example.com/v1/users").isNone

-- The evidence a `ScopedUrl` carries is exactly the operations' obligation, so
-- `getAt` needs no `decide`.
example (su : ScopedUrl api .GET) : Eff [HTTP api] Response := getAt su

-- Validating a URL built at runtime, then using it.
example (segment : String) : Eff [HTTP api] (Option Response) :=
  match ScopedUrl.check? api .GET
      { u!"https://api.example.com/v1" with
        path := u!"https://api.example.com/v1".path ++ [segment] } with
  | some su => some <$> getAt su
  | none    => pure none

-- ── End to end: a round trip through a stub transport ───────────────────────

/-- A transport that records what it was asked to send and answers with a canned
    body — the seam that lets the effect be tested without a network. -/
def stub (log : IO.Ref (List String)) (body : String) (req : Request) :
    IO Response := do
  log.modify (fun l => l ++ [s!"{req.method} {req.host}:{req.port}{req.path}{req.queryString} secure={req.isSecure}"])
  pure { statusCode := status200, headers := [], body := body.toUTF8 }

/-- Read one endpoint, then write to the one the capability opens for writing. -/
def exchange : Eff [HTTP events] (Option String) := do
  let s ← getString? u!"https://api.example.com/v1/users"
  let _ ← postString u!"https://api.example.com/v1/events" "{\"seen\":true}"
  pure s

/-- info: (some "[]", ["GET api.example.com:443/v1/users secure=true", "POST api.example.com:443/v1/events secure=true"]) -/
#guard_msgs in
#eval show IO (Option String × List String) from do
  let log ← IO.mkRef []
  let result ← runHTTPWith events (stub log "[]") exchange
  pure (result, ← log.get)

-- A query string is passed separately and reaches the wire, even though it takes
-- no part in the scope check.
/-- info: ["GET api.example.com:443/v1/users?page=2&full secure=true"] -/
#guard_msgs in
#eval show IO (List String) from do
  let log ← IO.mkRef []
  let _ ← runHTTPWith events (stub log "[]")
    (get u!"https://api.example.com/v1/users" (query := [("page", some "2"), ("full", none)]))
  log.get

-- The handler renders exactly what the capability authorised, port and scheme
-- included.
#guard (toClientRequest .GET u!"http://localhost:8080/health" [] [] none).host == "localhost"
#guard (toClientRequest .GET u!"http://localhost:8080/health" [] [] none).port == 8080
#guard (toClientRequest .GET u!"http://localhost:8080/health" [] [] none).isSecure == false
#guard (toClientRequest .GET u!"https://api.test/v1" [] [] none).isSecure == true

end Tests.Control.Monad.Effect.HTTP
