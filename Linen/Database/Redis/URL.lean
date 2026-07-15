/-
  Linen.Database.Redis.URL — parsing `redis://` connection-string URLs

  Ported from `hedis`'s `Database.Redis.URL`
  (https://hackage.haskell.org/package/hedis-0.16.1/src/src/Database/Redis/URL.hs),
  module #17 of the `hedis` import (see `docs/imports/hedis/dependencies.md`).

  Exposes `parseConnectInfo`, which turns a Redis connection-string URL into a
  `Database.Redis.Connection.ConnectInfo`. Three schemes are recognised
  (validated, to prevent mixing up configurations):

  - `redis://[[username:]password@]host[:port][/database]` — standalone.
  - `rediss://…` — the same shape, TLS-enabled (see the TLS deviation below).
  - `redis-socket://[[username:]password@]path[?database=N]` — Unix socket.

  Beyond the scheme, every value is optional; omitted values are taken from
  `defaultConnectInfo`.

  ## Substitutions / deviations

  - **`errors` (`Control.Error.Util`) → inline `note`.** Upstream uses exactly
    one function from that package, `note : e → Option a → Except e a`. It is
    written here as a one-line local helper rather than imported.
  - **`HTTP` (`Network.HTTP.Base`) → inline authority parser.** Upstream imports
    this large legacy HTTP-client package purely as a `"user:pass@host:port"`
    authority-string splitter, via `uriToAuthorityString`/`parseURIAuthority`
    and the `user`/`password`/`host`/`port` accessors on its authority type.
    Both are re-implemented here directly: `uriToAuthorityString` rebuilds the
    authority string from a parsed `Network.URI.URI`, and `parseURIAuthority`
    splits it on `@` (userinfo vs. host:port) and then on `:` (user vs.
    password, host vs. port), exactly the shape `dependencies.md` prescribes.
  - **`network-uri` → `Linen.Network.URI`.** The surrounding
    `parseURI`/`uriPath`/`uriScheme`/`uriQuery` come from the already-ported
    `Network.URI` module, not a fresh port.
  - **`http-types`' `parseSimpleQuery` → `Network.HTTP.Types.parseQuery`.** The
    `redis-socket://…?database=N` query string is parsed with the already-ported
    query parser. (Upstream's `parseSimpleQuery` yields
    `[(ByteString, ByteString)]`; `parseQuery` yields `[(String, Option String)]`
    — a key with no `=` becomes `some none`, which we treat as an invalid
    `database` value, matching upstream's empty-value → `readMaybe ""` → error.)
  - **TLS is *not* recorded in the returned `ConnectInfo` — the one place this
    port is not fully faithful, called out explicitly.** Upstream sets
    `connectTLSParams = Just (defaultParamsClient host "")` for a `rediss://`
    URL. This port's `ConnectInfo` has no `connectTLSParams` field: as
    documented in `Database.Redis.Connection`, TLS is modelled as a separate
    explicit `connectTLS` argument (the universe-polymorphism reason given in
    `ConnectionContext`), not a record field. Consequently a `rediss://` URL is
    accepted and parsed, but produces a `ConnectInfo` identical to the
    corresponding `redis://` URL — the secure flag cannot be carried in the
    result. A caller who parses a `rediss://` URL must therefore open the
    connection with `connect*TLS` rather than the plain `connect*`.
  - **Percent-decoding of userinfo.** Upstream's `parseURIAuthority`
    un-escapes (`unEscapeString`) the user/password fields; here they stay in
    the percent-encoded form `Network.URI` produces (that module keeps all
    component values percent-encoded — see its own docstring). Redis
    credentials in practice contain no percent-escapes, so this only affects an
    edge upstream itself handles incidentally via the HTTP package.

  Upstream reproduces the exact (quirky) error text `"Invalid port: <db>"` when
  the `/database` path segment is not a number — even though it is the database,
  not the port — and that text is preserved here for fidelity with the
  upstream doctests.
-/
import Linen.Database.Redis.Connection
import Linen.Database.Redis.ConnectionContext
import Linen.Network.URI
import Linen.Network.HTTP.Types.URI

namespace Database.Redis.URL

open Database.Redis.Connection (ConnectInfo defaultConnectInfo)
open Database.Redis.ConnectionContext (ConnectAddr)

-- ────────────────────────────────────────────────────────────────────
-- `errors`' `note` (inlined — see the module doc-comment)
-- ────────────────────────────────────────────────────────────────────

/-- `note e o` tags a `none` with the error `e`. Mirrors
    `Control.Error.Util.note : e → Maybe a → Either e a`. -/
private def note (e : ε) : Option α → Except ε α
  | none => .error e
  | some a => .ok a

-- ────────────────────────────────────────────────────────────────────
-- Authority parsing (the `HTTP` substitution — see the module doc-comment)
-- ────────────────────────────────────────────────────────────────────

/-- A parsed `"[user[:password]@]host[:port]"` authority. Mirrors
    `Network.HTTP.Base.URIAuthority` (only the four fields `Database.Redis.URL`
    reads). -/
structure URIAuthority where
  /-- Username, if present. -/
  user : Option String
  /-- Password, if present. -/
  password : Option String
  /-- Host (empty if the authority had none). -/
  host : String
  /-- Port number, if present. -/
  port : Option Nat
  deriving Repr, BEq

/-- Rebuild the authority string (`"user:pass@host:port"`) of a parsed URI.
    Mirrors `Network.HTTP.Base.uriToAuthorityString`. The `Network.URI`
    authority already stores the userinfo with its trailing `@` and the port
    with its leading `:`, so concatenation reproduces the original text. -/
def uriToAuthorityString (uri : Network.URI.URI) : String :=
  match uri.uriAuthority with
  | none => ""
  | some a => a.uriUserInfo ++ a.uriRegName ++ a.uriPort

/-- Split a `"host[:port]"` string into its host and optional numeric port,
    breaking at the *last* `:` whose suffix is all digits (so a bracketed IPv6
    literal such as `[::1]:6379` keeps its inner colons in the host). -/
private def splitHostPort (hp : String) : String × Option Nat :=
  match (hp.splitOn ":").reverse with
  | last :: revInit@(_ :: _) =>
    if !last.isEmpty && last.all Char.isDigit then
      (":".intercalate revInit.reverse, last.toNat?)
    else
      (hp, none)
  | _ => (hp, none)

/-- Parse a `"[user[:password]@]host[:port]"` authority string. Mirrors
    `Network.HTTP.Base.parseURIAuthority`: userinfo is split off at `@`, the
    userinfo is split at its first `:` into user/password (a missing or empty
    password becomes `none`), and the remainder is split into host/port. -/
def parseURIAuthority (s : String) : Option URIAuthority :=
  let (rawUser, hostPortStr) :=
    match s.splitOn "@" with
    | [hp] => (none, hp)
    | ui :: rest => (some ui, "@".intercalate rest)
    | [] => (none, "")
  let (user, password) :=
    match rawUser with
    | none => (none, none)
    | some ui =>
      match ui.splitOn ":" with
      | [u] => (some u, none)
      | u :: rest =>
        let p := ":".intercalate rest
        (some u, if p.isEmpty then none else some p)
      | [] => (none, none)
  let (host, port) := splitHostPort hostPortStr
  some { user, password, host, port }

-- ────────────────────────────────────────────────────────────────────
-- parseConnectInfo
-- ────────────────────────────────────────────────────────────────────

/-- The default host and port derived from `defaultConnectInfo.connectAddr`
    (used to fill in an omitted host/port). Mirrors upstream's local
    `finalHost`/`defaultPort` fallbacks (which default to `"localhost"`/`6379`
    should the default address be a Unix socket). -/
private def defaultHostPort : String × UInt16 :=
  match defaultConnectInfo.connectAddr with
  | .hostPort h p => (h, p)
  | .unixSocket _ => ("localhost", 6379)

/-- Parse a `redis://` or `rediss://` URL into a `ConnectInfo`. Mirrors
    upstream's `parseSocket`. The TLS flag of a `rediss://` URL is *not* carried
    into the result (see the module doc-comment). -/
private def parseSocket (uri : Network.URI.URI) : Except String ConnectInfo := do
  let auth ← note "Missing or invalid Authority"
    (parseURIAuthority (uriToAuthorityString uri))
  let dbNumPart := String.ofList (uri.uriPath.toList.dropWhile (· == '/'))
  let db ← if dbNumPart.isEmpty then
      pure defaultConnectInfo.connectDatabase
    else
      note s!"Invalid port: {dbNumPart}" dbNumPart.toInt?
  let (defHost, defPort) := defaultHostPort
  let finalHost := if auth.host.isEmpty then defHost else auth.host
  -- If only one credential is given (in the user slot, no `:`), treat it as the
  -- password; if two are given, user is the username and password the auth.
  let (finalUser, finalAuth) :=
    match auth.user, auth.password with
    | u, none => (none, u)
    | u, some p => if p.all Char.isWhitespace then (none, u) else (u, some p)
  let portVal := match auth.port with
    | none => defPort
    | some n => UInt16.ofNat n
  return { defaultConnectInfo with
    connectAddr := .hostPort finalHost portVal
    connectAuth := finalAuth.map String.toUTF8
    connectUsername := finalUser.map String.toUTF8
    connectDatabase := db }

/-- Parse a `redis-socket://` URL into a `ConnectInfo`. Mirrors upstream's
    `parseUnix`: the socket path is `host ++ uriPath` (with a leading `/`
    ensured), the database comes from an optional `?database=N` query
    parameter, and the auth is taken from the userinfo's user field. -/
private def parseUnix (uri : Network.URI.URI) : Except String ConnectInfo := do
  let auth ← note "Missing or invalid Authority"
    (parseURIAuthority (uriToAuthorityString uri))
  let query := Network.HTTP.Types.parseQuery uri.uriQuery
  let db ← match query.lookup "database" with
    | none => pure defaultConnectInfo.connectDatabase
    | some none => .error "Invalid database"
    | some (some v) => note "Invalid database" v.toInt?
  let combined := auth.host ++ uri.uriPath
  let path := if combined.startsWith "/" then combined else "/" ++ combined
  return { defaultConnectInfo with
    connectAddr := .unixSocket path
    connectAuth := auth.user.map String.toUTF8
    connectDatabase := db }

/-- Parse a `ConnectInfo` from a connection-string URL, following the Redis
    client URL conventions. Returns `Except.error` with a descriptive message
    for an unparseable URI or an unrecognised scheme. Mirrors upstream's
    `parseConnectInfo`.

    Examples (see the test module for `#guard`-checked cases):
    - `redis://username:password@host:42/2`
    - `rediss://` (TLS scheme; secure flag not recorded — see the doc-comment)
    - `redis-socket://password@/tmp/redis.sock?database=2` -/
def parseConnectInfo (url : String) : Except String ConnectInfo := do
  let uri ← note "Invalid URI" (Network.URI.parseURI url)
  match uri.uriScheme with
  | "redis:" => parseSocket uri
  | "rediss:" => parseSocket uri
  | "redis-socket:" => parseUnix uri
  | x => .error s!"Wrong scheme {x}"

end Database.Redis.URL
