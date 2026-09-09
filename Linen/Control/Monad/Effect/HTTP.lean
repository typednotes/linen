/-
  `Control.Monad.Effect.HTTP` — a capability-restricted HTTP client effect

  ## Not a Haskell port

  Like `Control.Monad.Effect.FileSystem`, this module is `linen`-original: it has
  no counterpart in `freer-simple` (or in `polysemy`, `effectful`, or
  `fused-effects`), and is not part of the `FreerSimple` import's topological
  checklist. It exists to show that the capability idiom `FileSystem` introduced
  is not filesystem-specific, and is documented as such in
  `docs/imports/FreerSimple/dependencies.md`.

  ## What it adds over an effect row

  `Eff [HTTP cap] α` cannot touch the filesystem, because the row names only
  `HTTP`. That much any Haskell effect library gives. What a row cannot say is
  *which* requests the computation may issue: "GET anything under
  `https://api.example.com/v1`, POST only to `/v1/events`, and never DELETE".
  Saying that with type-level names alone needs one effect type per combination.

  Here the permission is a **value** indexing the effect, exactly as in
  `FileSystem`, at the same two strengths:

  1. **Method permissions** — which *operations* exist (`canGet`, `canPost`, …),
     each demanded as a `Prop`-class instance carrying a real proof.
  2. **URL scope** — which *arguments* those operations may be called on
     (`scopes`), demanded as a proof obligation that `decide` discharges at the
     call site.

  So `delete` under a read-only web capability fails to elaborate, and so does
  `get u!"https://evil.com/"` under a capability scoped to `api.example.com`.

  ## Why `Url` is structured, not a string

  The same two reasons that make `FileSystem.Path` a component list, and they
  again agree:

  - **Decidability.** `String.startsWith`/`String.take` do not reduce under
    `decide` (they get stuck on the slice representation), so a string-prefix
    scope check could not be discharged at elaboration time at all. `List String`
    prefix comparison and `String` *equality* both reduce, so the obligation is
    decidable where it needs to be.
  - **Correctness.** A URL is a hierarchy, and string prefixes get its boundaries
    wrong in two separate places. `https://api.example.com.evil.com/` is a
    string-prefix extension of `https://api.example.com` but is a different host;
    `/v1-admin` is a string-prefix extension of `/v1` but is not under it. Host
    labels are therefore compared as a list, and path segments component-wise.
    Both cases are pinned by tests.

  Write literals with the `u!` macro — `u!"https://api.example.com/v1"` expands at
  macro time to a literal `Url`, so a scope obligation about it still reduces.

  ## Query strings are deliberately outside the scope check

  `Url` carries scheme, host, port and path — the parts that form the hierarchy a
  prefix can meaningfully cut. A query string is not part of that hierarchy
  (`?admin=true` is not "under" anything), so it is passed separately to the
  operations and is not scope-relevant. A capability that must distinguish two
  endpoints should distinguish them by path.

  ## Soundness and the trust boundary

  Every `HTTP` request value carries both proofs, so `runHTTP` re-checks nothing:
  a request that exists was authorised at construction. The handler is the
  trusted boundary — `runHTTPWith` takes the transport as a parameter, and an
  injected transport could of course ignore the request it is given. That is the
  same trust `runFileSystem` places in `IO.FS`; what the capability constrains is
  the *computation*, not the interpreter chosen for it.

  ## Backend

  `linen` already has an HTTP/1.1 client, so per AGENTS.md's reuse precedence the
  handler renders each request into `Network.HTTP.Client.Request` and dispatches
  through `Client.connect` / `Client.performRequest`. No new FFI.
-/
import Linen.Control.Monad.Effect
import Linen.Network.HTTP.Types.Method
import Linen.Network.HTTP.Types.URI
import Linen.Network.HTTP.Client.Types
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Response

namespace Control.Monad.Effect.HTTP

open Data.OpenUnion Control.Monad.Effect
open Network.HTTP.Types (StdMethod Method RequestHeaders Query)

-- ── URLs ────────────────────────────────────────────────────────────────────

/-- A request target, split into the parts a scope can cut on.

    `host` is the DNS labels (`["api", "example", "com"]`) and `path` the URL
    segments, both `List String` so that scope checks are decidable at
    elaboration time and are component-wise; see the module header for why both
    matter. A query string is not part of `Url` — see the header. -/
structure Url where
  /-- `true` for `https`, `false` for `http`. -/
  secure : Bool
  /-- The host as DNS labels: `api.example.com` is `["api", "example", "com"]`. -/
  host : List String
  /-- The port, explicit even when it is the scheme's default. -/
  port : UInt16
  /-- The path as segments: `/v1/users` is `["v1", "users"]`. -/
  path : List String
  deriving DecidableEq, Repr

/-- Render a `Url` back to its usual textual form. -/
def Url.toString (u : Url) : String :=
  let scheme := if u.secure then "https" else "http"
  let host := ".".intercalate u.host
  let defaultPort := if u.secure then 443 else 80
  let port := if u.port == defaultPort then "" else s!":{u.port}"
  s!"{scheme}://{host}{port}/" ++ "/".intercalate u.path

instance : ToString Url := ⟨Url.toString⟩

/-- The path with a leading slash, as `Network.HTTP.Client.Request` wants it. -/
def Url.pathString (u : Url) : String :=
  "/" ++ "/".intercalate u.path

open Lean in
/-- URL literal: `u!"https://api.example.com/v1/users"` expands to
    `⟨true, ["api", "example", "com"], 443, ["v1", "users"]⟩`.

    An explicit `:port` is honoured; otherwise the port is the scheme's default
    (443 for `https`, 80 for `http`). A query string or fragment is dropped, since
    neither is scope-relevant.

    The split happens at macro-expansion time, so the result is a literal
    structure and a scope obligation about it still reduces under `decide`.

    Declared at `max` precedence so it can be passed as a bare function argument
    (`get u!"https://x.test/"`) without parentheses. -/
macro:max "u!" s:str : term => do
  let raw := s.getString
  let (secure, rest) :=
    if raw.startsWith "https://" then (true, (raw.drop 8).toString)
    else if raw.startsWith "http://" then (false, (raw.drop 7).toString)
    else (true, raw)
  -- Drop fragment then query: neither takes part in scoping.
  let rest := (rest.splitOn "#").headD rest
  let rest := (rest.splitOn "?").headD rest
  let (authority, segments) :=
    match rest.splitOn "/" with
    | []            => ("", ([] : List String))
    | a :: segs     => (a, segs.filter (· ≠ ""))
  let (hostStr, portNat) :=
    match authority.splitOn ":" with
    | [h]    => (h, if secure then 443 else 80)
    | [h, p] => (h, p.toNat?.getD (if secure then 443 else 80))
    | _      => (authority, if secure then 443 else 80)
  let hostElems := (hostStr.splitOn ".").filter (· ≠ "")
    |>.map (fun l => Syntax.mkStrLit l) |>.toArray
  let pathElems := segments.map (fun c => Syntax.mkStrLit c) |>.toArray
  let secureLit ← if secure then `(true) else `(false)
  let portLit := Syntax.mkNumLit (toString portNat)
  `(Url.mk $secureLit [$hostElems,*] $portLit [$pathElems,*])

-- ── Capabilities ────────────────────────────────────────────────────────────

/-- One region of the web a capability opens up: an origin, optionally narrowed
    to a path prefix and to a subset of methods.

    `methods := []` means "every method the capability's own bits allow", so a
    scope need only name methods when it is *more* restrictive than the
    capability as a whole. `path := []` is the whole origin. -/
structure Scope where
  /-- Methods allowed in this scope; `[]` means every method the capability has. -/
  methods : List StdMethod := []
  /-- `true` for `https`. A scope never matches across schemes. -/
  secure : Bool := true
  /-- The host as DNS labels; matched exactly, never as a string prefix. -/
  host : List String
  /-- The port; matched exactly. -/
  port : UInt16 := 443
  /-- Path prefix, as segments; `[]` is the whole origin. -/
  path : List String := []
  deriving DecidableEq, Repr

/-- What a computation is permitted to do over HTTP: which methods, and against
    which URLs.

    Method fields default to `false`, so `{ canGet := true }` denies everything
    else by construction — a capability grants only what it names. `scopes`
    defaults to `[]`, meaning *no URL restriction*; a non-empty `scopes` confines
    every request to one of them.

    Declare capability constants with `abbrev`, not `def`: instance resolution
    does not unfold a non-reducible `def`, so `def myCap` leaves `CanGet myCap`
    unresolvable. -/
structure Capability where
  /-- May issue `GET`. -/
  canGet : Bool := false
  /-- May issue `HEAD`. -/
  canHead : Bool := false
  /-- May issue `POST`. -/
  canPost : Bool := false
  /-- May issue `PUT`. -/
  canPut : Bool := false
  /-- May issue `PATCH`. -/
  canPatch : Bool := false
  /-- May issue `DELETE`. -/
  canDelete : Bool := false
  /-- Regions of the web the capability is confined to; `[]` means unrestricted. -/
  scopes : List Scope := []
  deriving DecidableEq, Repr

/-- Does this capability's method set include `m`?

    Methods with no bit of their own (`TRACE`, `CONNECT`, `OPTIONS`) are never
    granted: this effect exposes no operation for them, so there is nothing to
    allow. -/
def Capability.allows (cap : Capability) : StdMethod → Bool
  | .GET    => cap.canGet
  | .HEAD   => cap.canHead
  | .POST   => cap.canPost
  | .PUT    => cap.canPut
  | .PATCH  => cap.canPatch
  | .DELETE => cap.canDelete
  | _       => false

/-- Does `s` cover method `m` at `u`?

    Scheme, host and port must match exactly — the host label-wise, so
    `api.example.com.evil.com` is a different host rather than an extension of
    one — and the scope's path must be a component-wise prefix of the URL's. -/
def Scope.covers (s : Scope) (m : StdMethod) (u : Url) : Bool :=
  (s.methods.isEmpty || s.methods.contains m)
    && s.secure == u.secure && s.host == u.host && s.port == u.port
    && s.path.isPrefixOf u.path

/-- Does this capability allow issuing `m` against `u` at all?

    True when the capability is unscoped (`scopes = []`) or some scope covers the
    pair. This is the *argument* half of the capability; the *operation* half is
    `allows`, demanded separately as a `Prop`-class instance. -/
def Capability.permits (cap : Capability) (m : StdMethod) (u : Url) : Bool :=
  cap.scopes.isEmpty || cap.scopes.any (fun s => s.covers m u)

/-- `cap` grants `GET`. Carries the proof, so it cannot be forged. -/
class CanGet (cap : Capability) : Prop where
  /-- Evidence that the `GET` bit is set. -/
  proof : cap.canGet = true

/-- `cap` grants `HEAD`. Carries the proof, so it cannot be forged. -/
class CanHead (cap : Capability) : Prop where
  /-- Evidence that the `HEAD` bit is set. -/
  proof : cap.canHead = true

/-- `cap` grants `POST`. Carries the proof, so it cannot be forged. -/
class CanPost (cap : Capability) : Prop where
  /-- Evidence that the `POST` bit is set. -/
  proof : cap.canPost = true

/-- `cap` grants `PUT`. Carries the proof, so it cannot be forged. -/
class CanPut (cap : Capability) : Prop where
  /-- Evidence that the `PUT` bit is set. -/
  proof : cap.canPut = true

/-- `cap` grants `PATCH`. Carries the proof, so it cannot be forged. -/
class CanPatch (cap : Capability) : Prop where
  /-- Evidence that the `PATCH` bit is set. -/
  proof : cap.canPatch = true

/-- `cap` grants `DELETE`. Carries the proof, so it cannot be forged. -/
class CanDelete (cap : Capability) : Prop where
  /-- Evidence that the `DELETE` bit is set. -/
  proof : cap.canDelete = true

instance instCanGet {he po pu pa d : Bool} {ss : List Scope} :
    CanGet ⟨true, he, po, pu, pa, d, ss⟩ := ⟨rfl⟩
instance instCanHead {g po pu pa d : Bool} {ss : List Scope} :
    CanHead ⟨g, true, po, pu, pa, d, ss⟩ := ⟨rfl⟩
instance instCanPost {g he pu pa d : Bool} {ss : List Scope} :
    CanPost ⟨g, he, true, pu, pa, d, ss⟩ := ⟨rfl⟩
instance instCanPut {g he po pa d : Bool} {ss : List Scope} :
    CanPut ⟨g, he, po, true, pa, d, ss⟩ := ⟨rfl⟩
instance instCanPatch {g he po pu d : Bool} {ss : List Scope} :
    CanPatch ⟨g, he, po, pu, true, d, ss⟩ := ⟨rfl⟩
instance instCanDelete {g he po pu pa : Bool} {ss : List Scope} :
    CanDelete ⟨g, he, po, pu, pa, true, ss⟩ := ⟨rfl⟩

/-- The method bits really do agree with `allows` — the bridge between the
    `Prop`-class half of the capability and the `Bool` function the request
    constructor stores. -/
theorem allows_get {cap : Capability} (h : cap.canGet = true) :
    cap.allows .GET = true := h
theorem allows_head {cap : Capability} (h : cap.canHead = true) :
    cap.allows .HEAD = true := h
theorem allows_post {cap : Capability} (h : cap.canPost = true) :
    cap.allows .POST = true := h
theorem allows_put {cap : Capability} (h : cap.canPut = true) :
    cap.allows .PUT = true := h
theorem allows_patch {cap : Capability} (h : cap.canPatch = true) :
    cap.allows .PATCH = true := h
theorem allows_delete {cap : Capability} (h : cap.canDelete = true) :
    cap.allows .DELETE = true := h

-- ── Scoped URLs, for targets not known statically ───────────────────────────

/-- A URL together with a proof that `cap` allows `m` against it.

    For URLs known at compile time the obligation on each operation is discharged
    by `decide` and this type is not needed. It exists for URLs computed at
    runtime: `check?` validates one and hands back the evidence, so the operations
    still cannot be reached without it. -/
structure ScopedUrl (cap : Capability) (m : StdMethod) where
  /-- The target. -/
  url : Url
  /-- Evidence that `cap` permits `m` against it. -/
  inScope : cap.permits m url = true

/-- Validate a runtime URL against `cap`, returning the evidence on success. -/
def ScopedUrl.check? (cap : Capability) (m : StdMethod) (u : Url) :
    Option (ScopedUrl cap m) :=
  if h : cap.permits m u = true then some ⟨u, h⟩ else none

-- ── The effect ──────────────────────────────────────────────────────────────

open Network.HTTP.Client in
/-- HTTP requests available under the capability `cap`.

    The single constructor takes a proof that `cap` grants the method *and* a
    proof that `cap` allows that method against the URL, so both the permission
    and the scope are part of what it means for the request to exist. -/
inductive HTTP (cap : Capability) : Type → Type where
  /-- Issue one request. Requires method permission and URL scope. -/
  | request (method : StdMethod) (hp : cap.allows method = true) (url : Url)
      (hs : cap.permits method url = true) (headers : RequestHeaders)
      (query : Query) (body : Option ByteArray) : HTTP cap Response

/-- Locates an `HTTP` effect in the row and recovers *which* capability it
    carries.

    `cap` is an `outParam`: it is an output of resolving against `effs`, not
    something the caller must supply. That is what keeps both the method
    obligations and the URL-scope obligation solvable inside `do`-notation, where
    `cap` would otherwise remain a metavariable. -/
class HasHTTP (effs : List (Type → Type)) (cap : outParam Capability) where
  /-- Inject an HTTP request into the row. -/
  inject : {α : Type} → HTTP cap α → Union effs α

/-- The HTTP effect is the row's head. -/
instance instHasHTTPHere {cap : Capability} {effs : List (Type → Type)} :
    HasHTTP (HTTP cap :: effs) cap where
  inject e := .here e

/-- The HTTP effect is somewhere in the row's tail. -/
instance instHasHTTPThere {cap : Capability} {eff : Type → Type}
    {effs : List (Type → Type)} [HasHTTP effs cap] :
    HasHTTP (eff :: effs) cap where
  inject e := .there (HasHTTP.inject e)

-- ── Operations ──────────────────────────────────────────────────────────────

open Network.HTTP.Client in
/-- `GET` a URL.

    Requires `CanGet cap` and a proof that `cap` allows `GET` against `url`, both
    resolved from the capability the row carries. Under a capability without the
    `GET` bit, or for a URL outside its scopes, this call does not elaborate.

    The scope obligation sits directly after the URL, ahead of the optional
    arguments, so `get u!"https://x.test/"` discharges it by `decide` and no
    positional call can bind a proof to `headers` by accident. -/
def get {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanGet cap] (url : Url)
    (hs : cap.permits .GET url = true := by decide)
    (headers : RequestHeaders := []) (query : Query := []) : Eff effs Response :=
  .impure (h.inject (.request .GET (allows_get perm.proof) url hs headers query none)) .protect

open Network.HTTP.Client in
/-- `HEAD` a URL. Requires `CanHead cap` and URL scope. -/
def head {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanHead cap] (url : Url)
    (hs : cap.permits .HEAD url = true := by decide)
    (headers : RequestHeaders := []) (query : Query := []) : Eff effs Response :=
  .impure (h.inject (.request .HEAD (allows_head perm.proof) url hs headers query none)) .protect

open Network.HTTP.Client in
/-- `POST` a body to a URL. Requires `CanPost cap` and URL scope.

    Under a read-only web capability this call does not elaborate — the
    read/write split within a single effect that a type-level row cannot draw. -/
def post {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanPost cap] (url : Url) (body : ByteArray)
    (hs : cap.permits .POST url = true := by decide)
    (headers : RequestHeaders := []) (query : Query := []) : Eff effs Response :=
  .impure (h.inject (.request .POST (allows_post perm.proof) url hs headers query (some body))) .protect

open Network.HTTP.Client in
/-- `PUT` a body to a URL. Requires `CanPut cap` and URL scope. -/
def put {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanPut cap] (url : Url) (body : ByteArray)
    (hs : cap.permits .PUT url = true := by decide)
    (headers : RequestHeaders := []) (query : Query := []) : Eff effs Response :=
  .impure (h.inject (.request .PUT (allows_put perm.proof) url hs headers query (some body))) .protect

open Network.HTTP.Client in
/-- `PATCH` a body to a URL. Requires `CanPatch cap` and URL scope. -/
def patch {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanPatch cap] (url : Url) (body : ByteArray)
    (hs : cap.permits .PATCH url = true := by decide)
    (headers : RequestHeaders := []) (query : Query := []) : Eff effs Response :=
  .impure (h.inject (.request .PATCH (allows_patch perm.proof) url hs headers query (some body))) .protect

open Network.HTTP.Client in
/-- `DELETE` a URL. Requires `CanDelete cap` and URL scope.

    A capability granting `GET` and `POST` but not `DELETE` makes this call fail
    to elaborate. -/
def delete {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanDelete cap] (url : Url)
    (hs : cap.permits .DELETE url = true := by decide)
    (headers : RequestHeaders := []) (query : Query := []) : Eff effs Response :=
  .impure (h.inject (.request .DELETE (allows_delete perm.proof) url hs headers query none)) .protect

open Network.HTTP.Client in
/-- `GET` a URL and decode the body as UTF-8, or `none` if the bytes are not
    valid UTF-8.

    Total by construction: `String.fromUTF8?` rather than the panicking
    `String.fromUTF8!`, so a malformed body is a value the caller handles. -/
def getString? {effs : List (Type → Type)} {cap : Capability}
    [HasHTTP effs cap] [CanGet cap] (url : Url)
    (hs : cap.permits .GET url = true := by decide) : Eff effs (Option String) :=
  (fun r => String.fromUTF8? r.body) <$> get url hs

open Network.HTTP.Client in
/-- `POST` a UTF-8 string to a URL. -/
def postString {effs : List (Type → Type)} {cap : Capability}
    [HasHTTP effs cap] [CanPost cap] (url : Url) (body : String)
    (hs : cap.permits .POST url = true := by decide) : Eff effs Response :=
  post url body.toUTF8 hs

-- ── Operations on runtime-validated URLs ────────────────────────────────────

open Network.HTTP.Client in
/-- `GET` a URL validated at runtime.

    The `ScopedUrl` supplies the scope evidence, so no `decide` is involved and
    the URL need not be statically known. -/
def getAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanGet cap] (su : ScopedUrl cap .GET)
    (headers : RequestHeaders := []) (query : Query := []) : Eff effs Response :=
  .impure (h.inject (.request .GET (allows_get perm.proof) su.url su.inScope headers query none)) .protect

open Network.HTTP.Client in
/-- `POST` to a URL validated at runtime. -/
def postAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanPost cap] (su : ScopedUrl cap .POST)
    (body : ByteArray) (headers : RequestHeaders := []) (query : Query := []) :
    Eff effs Response :=
  .impure (h.inject (.request .POST (allows_post perm.proof) su.url su.inScope headers query (some body))) .protect

open Network.HTTP.Client in
/-- `DELETE` a URL validated at runtime. -/
def deleteAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasHTTP effs cap] [perm : CanDelete cap] (su : ScopedUrl cap .DELETE)
    (headers : RequestHeaders := []) (query : Query := []) : Eff effs Response :=
  .impure (h.inject (.request .DELETE (allows_delete perm.proof) su.url su.inScope headers query none)) .protect

-- ── Handler ─────────────────────────────────────────────────────────────────

open Network.HTTP.Client in
/-- Render an authorised request into the client's wire-level `Request`. -/
def toClientRequest (m : StdMethod) (u : Url) (headers : RequestHeaders)
    (query : Query) (body : Option ByteArray) : Request :=
  { method := Method.standard m
  , host := ".".intercalate u.host
  , port := u.port
  , path := u.pathString
  , queryString := Network.HTTP.Types.renderQuery query
  , headers := headers
  , body := body
  , isSecure := u.secure }

open Network.HTTP.Client in
/-- Run an HTTP computation against a supplied transport.

    Performs no permission or scope check: every request already carries both
    proofs, so reaching this point means they were established at construction.
    The handler only renders the request and hands it to `send`.

    `send` is a parameter so that tests can drive the effect without a network,
    and so that retries, redirects or connection pooling can be layered in. It is
    the trusted boundary: an injected transport that ignores its argument is not
    something the capability can prevent, any more than `runFileSystem` can
    prevent a different `IO.FS` being called. -/
def runHTTPWith (cap : Capability) {α : Type} (send : Request → IO Response) :
    Eff [HTTP cap] α → IO α :=
  interpretM fun
    | .request m _ url _ headers query body => send (toClientRequest m url headers query body)

open Network.HTTP.Client in
/-- The default transport: connect, perform one request, close.

    A fresh connection per request, which is what `Network.HTTP.Simple` does too;
    `runHTTPWith` is the seam for anything cleverer. -/
def sendOnce (req : Request) : IO Response := do
  let conn ← connect req.host req.port req.isSecure req.timeoutMillis
  try
    performRequest conn req
  finally
    conn.connClose

/-- Run an HTTP computation over the real network. -/
def runHTTP (cap : Capability) {α : Type} : Eff [HTTP cap] α → IO α :=
  runHTTPWith cap sendOnce

-- ── Common capabilities ─────────────────────────────────────────────────────

/-- Read-only web access, anywhere: `post`, `put`, `patch` and `delete` will not
    elaborate. -/
abbrev readOnlyWeb : Capability := { canGet := true, canHead := true }

/-- Every method this effect exposes, anywhere. -/
abbrev fullWeb : Capability :=
  { canGet := true, canHead := true, canPost := true, canPut := true
  , canPatch := true, canDelete := true }

/-- The scope covering everything at `url`'s origin, at or below its path. -/
abbrev under (url : Url) (methods : List StdMethod := []) : Scope :=
  { methods := methods, secure := url.secure, host := url.host, port := url.port
  , path := url.path }

/-- Read-only access confined to one origin and path prefix.

    The interesting shape: this and `readOnlyWeb` grant the same *operations* and
    still differ in which *arguments* they admit. -/
abbrev readOnlyUnder (url : Url) : Capability :=
  { canGet := true, canHead := true, scopes := [under url] }

/-- A typical REST client: read anywhere under `url`, and write only under
    `writeUnder`.

    Two scopes over the same origin with different method lists — the
    per-argument distinction that no type-level effect row can express. -/
abbrev restClient (url : Url) (writeUnder : Url) : Capability :=
  { canGet := true, canHead := true, canPost := true
  , scopes := [under url [.GET, .HEAD], under writeUnder [.POST]] }

end Control.Monad.Effect.HTTP
