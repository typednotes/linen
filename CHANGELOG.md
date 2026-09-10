# Changelog

All notable changes to `linen` are documented here, one entry per released
version (see `version` in `lakefile.lean`). Dates are UTC, in `YYYY-MM-DD`
format.

## [Unreleased]

## [0.16.0] - 2026-09-10

- **`Linen.Cloud`: object stores, queues and secret managers, the same way on
  AWS, GCP and Scaleway.** Three portable interfaces — `ObjectStore`, `Queue`,
  `SecretStore` — each a record of closures with one implementation per cloud,
  following the sibling `typednotes/infra`'s `Backend` shape so that provider
  dispatch is a total function. Underneath: the three-source credential chain
  (CLI config files, OS keychain, environment), RFC 7523 token minting for GCP,
  locality-to-region tables, request signing, the four wire dialects, a
  classified error taxonomy and bounded pagination. `Crypto.SigV4` finally has
  a consumer.

  The reuse is the point: Scaleway's Object Storage speaks the S3 API and its
  Queues speak the SQS API, so `ObjectStore.S3` and `Queue.Sqs` each serve
  **two** clouds and differ in nothing but the host. Where GCP does not fit,
  the types say so — `S3.endpoint?` and `Sqs.endpoint?` answer `none` for it
  rather than returning a host that fails in DNS, and `Cloud.Queue` splits into
  a `Producer` and a `Consumer` because Pub/Sub is a topic-and-subscription
  system rather than a queue. A Pub/Sub `Consumer` cannot be constructed
  without naming a subscription.

- **Every service has a local backend.** `ObjectStore.inMemory`,
  `Queue.inMemory` and `SecretStore.inMemory` behave like the real thing in the
  ways that catch bugs — lexicographic key order, genuine pagination, message
  invisibility until acknowledged, delivery counts that rise, `notFound` for a
  missing secret — and `Transport.stub` replaces the network for any real
  backend. The whole namespace is therefore tested with no credentials, no
  containers and no new FFI.

- **`Secret.Value` cannot be leaked by accident.** It wraps `ByteArray`,
  renders as `<redacted>`, and has **no `ToJSON` instance and no `BEq`** — so a
  secret cannot be serialised into a log line or compared in
  constant-unknown time. `expose` is the single named audit point.

- **`Linen.Control.Monad.Effect.{ObjectStore,Queue,SecretStore}`: the same
  three services, capability-restricted.** The `Effect.FileSystem` pattern at
  its fourth, fifth and sixth instances, and the first over a *remote* backend.
  A capability confines a program to named buckets and key prefixes, or lets a
  worker read one queue and write another without draining either.

  `SecretStore` is the flagship: a capability granting `describe` and not
  `getValue` makes `getValue` **fail to elaborate**. Both operations have the
  same effect type and differ only in a value indexing it, so no type-level
  effect row can draw the distinction — which is the clearest demonstration
  yet of what the ported `freer-simple` mechanism gains from dependent types.
  Each handler takes the backend as a parameter, so one program runs against a
  real cloud or an in-memory double, and each has a pure `dryRun` interpreter.

  Two documented divergences from `Effect.FileSystem`: `scopes := []` grants
  **nothing** here rather than meaning "unrestricted" (a cloud credential's
  blast radius is the whole account, which is not a default anyone wants), and
  `ObjectStore`'s `k!` macro does not filter empty segments the way `p!` does,
  because `a//b`, `a/` and `/a` are three different S3 keys.

- **`Cloud.Error` reads five error dialects, not four.** Added OAuth2's RFC
  6749 shape (`{"error": "invalid_grant", "error_description": …}`), where
  `error` is a *string* — the same field name Google's API errors make an
  object, so the value's type decides which. Without it every token-endpoint
  rejection rendered with an empty code.

- **`Cloud.Transport` sends the path single-encoded even when the signature
  double-encodes it**, which is AWS's actual rule for every service except S3.
  Because `Call.path` is held unencoded, both encodings are derived from one
  value and cannot disagree — so a key containing a space or a `#` signs and
  sends correctly with no work at the call site.

- **`AGENTS.md`: `typednotes` is not external.** Moving code from a sibling in
  the `typednotes` organisation into `linen` is a *move*, not an import: no
  `docs/imports/` entry, no dependency list, no precedence check — and the
  sibling is edited in the same change to delete its copy. Two live copies of
  the same code is the outcome to avoid.

- **`Linen.Control.Monad.Effect.FileSystem`: a permission set per path prefix.**
  The capability's `roots : List Path` becomes `scopes : List Scope`, where each
  `Scope` carries **its own operation list** alongside its root — the shape
  `.HTTP`'s `Scope` already used for URLs. One capability can now say "read,
  write and delete under `/srv/app/releases`, read and write under
  `/srv/app/current`, read only under `/etc/app`, nothing anywhere else", which
  a single uniform `roots` list could not express. Scopes union: `permits` holds
  when *some* scope covers the pair, with no deny rules and no
  most-specific-wins precedence, so adding a scope can only add access.
  Consequent API changes:
  - `Capability.permits` now takes the operation: `cap.permits .read path`
    rather than `cap.permits path`. A new `Op` (`.read`/`.write`/`.delete`) and
    `Capability.allows : Op → Bool` mirror `.PostgreSQL`'s.
  - `ScopedPath` is indexed by the operation (`ScopedPath cap .read`), and
    `ScopedPath.check?` takes it, so evidence for reading a path is not evidence
    for writing it.
  - Added `under` (a scope for a root, as in `.HTTP`), `Capability.union` with
    `permits_union_left`/`_right` and `allows_union_left`/`_right` proving
    neither operand loses access, `Capability.consistent` (do the global bits
    cover every operation the scopes name?), and the `workspace` capability.
  - Added `CanRead.of`/`CanWrite.of`/`CanDelete.of` for capabilities whose bits
    are *computed* rather than written literally — a `union`, say. Instance
    resolution matches the bits syntactically and will not evaluate a fold over
    the scope list, so such a capability declares its instances once
    (`instance : CanRead (a.union b) := .of`); the bit is still discharged by
    `decide`, so it is evidence exactly as much as the literal case.
  - `sandboxed` is unchanged in meaning (`scopes := [under root]`).
- Added the `effects` example (`lake exe examples effects`): the capability
  effects run end-to-end against real scratch files, a loopback HTTP/1.1 server
  and a disposable PostgreSQL container started with Podman, plus the runtime
  `check?` path and one `Eff` over a four-effect row with no `IO` in it.

## [0.15.0] — 2026-09-09

- **Renamed `Linen.Control.Monad.Freer` to `Linen.Control.Monad.Effect`**, with
  every effect module under it (`.Reader`, `.State`, `.Error`, `.Writer`,
  `.NonDet`, `.Coroutine`, `.Fresh`, `.Trace`, `.FileSystem`) and the namespace
  `Control.Monad.Freer` → `Control.Monad.Effect`. `Freer` named the *encoding*
  and the Hackage package the port came from, not what the modules are for; the
  capability effects added below have no `freer-simple` counterpart at all. The
  `Eff` type, and every operation on it, are unchanged. References to upstream's
  own `Control.Monad.Freer.TH` and `Control.Monad.Freer.Internal` keep their
  Haskell names, as does `docs/imports/FreerSimple/`, since both are provenance.
- Added `Linen.Control.Monad.Effect.HTTP` (`linen`-original): the capability
  idiom from `.FileSystem`, applied to HTTP. A `Capability` value gates which
  **methods** exist (`canGet`/`canPost`/…, as `Prop`-class instances) and which
  **URLs** they may be called on (`scopes`, as a `decide`-discharged
  obligation), with per-scope method lists — so one capability can say "GET
  anywhere under `/v1`, POST only to `/v1/events`". `Url` splits the host into
  DNS labels and the path into segments, so `api.example.com.evil.com` is a
  different host rather than a string-prefix extension of one, and `/v1-admin`
  is not under `/v1`; scheme and port are part of the scope too. Literals use
  the `u!` macro; runtime URLs go through `ScopedUrl.check?`. The handler
  dispatches through `Network.HTTP.Client`, with `runHTTPWith` taking the
  transport as a parameter so tests need no network.
- Added `Linen.Control.Monad.Effect.PostgreSQL` (`linen`-original): the same
  idiom over a database, restricting in three parts. The connection target
  (`host`/`port`/`database`/`user`) is **structural** — a term of
  `Eff [PostgreSQL cap] α` names no connection at all, and `runPostgreSQL`
  derives one from the capability alone. Statement kinds (`canSelect`/`canInsert`/
  `canUpdate`/`canDelete`) are `Prop`-class instances, and table scope
  (`tables`) is a `decide`-discharged obligation. Queries are an AST rather
  than strings, for the reason paths are component lists: a `String` of SQL
  cannot be checked by `decide`. Rendering derives from the checked value, so
  the SQL cannot disagree with what was authorised, and every literal is bound
  as a `$n` parameter. There is deliberately no `rawSql` escape hatch. `dryRun`
  interprets a computation into the SQL it would send, so the tests assert real
  statements without a live server.
- `Network.HTTP.Types.StdMethod` now derives `DecidableEq` alongside `BEq`.

## [0.14.0] — 2026-09-06

- Added `Linen.Control.Monad.Freer` and `Linen.Data.OpenUnion`: extensible
  effects ported from Hackage's `freer-simple`. `Eff effs α` carries its
  permitted effects in its type, so a signature is an effect whitelist —
  `send`/`run`/`runM`/`interpret`/`interpretM`/`interpose`/`reinterpret`/`raise`
  over a safe-by-construction open union (`Union`/`Member`), with no
  `unsafeCoerce` and no `Data.FTCQueue`.
- Added the effect modules `Linen.Control.Monad.Freer.{Reader,State,Error,
  Writer,NonDet,Coroutine,Fresh,Trace}` — all ten of `freer-simple`'s
  effect/core modules. `NonDet.msplit` is the one functional omission: it is
  not structurally recursive and diverges on an infinitely-branching
  computation, so porting it would need `partial` or fuel.
- Added `Linen.Control.Monad.Freer.FileSystem` (`linen`-original): a
  dependently-typed capability system over the effect row. A `Capability`
  *value* indexes the effect and gates both which **operations** are allowed
  (`canRead`/`canWrite`/`canDelete`, as `Prop`-class instances) and which
  **arguments** they may be called on (`roots`, as a `decide`-discharged
  obligation; generalised to per-prefix `scopes` in Unreleased above). A read-only capability makes `writeFile` fail to elaborate; a
  sandboxed one rejects `readFile p!"/etc/passwd"`, including the
  string-prefix sibling case `/tmp/sandbox-evil`. Paths are component lists
  written with the `p!` macro; runtime paths go through `ScopedPath.check?`.
- `Eff`'s payload is universe-polymorphic (`Type u` in, `Type (max 1 u)` out)
  rather than pinned to `Type 0`, so that `Coroutine`'s self-referential
  `Status` — which holds an `Eff effs (Status …)` — is expressible as a real
  construction instead of a weakened type. `Eff.bindH` is the matching
  heterogeneous bind.
- Corrected the stale Lean badge in `README.md` (4.31.0 → 4.33.1, the
  toolchain since 0.12.0).

## [0.13.0] — 2026-09-05

- `Linen.Crypto.JOSE` now supports RSA **signing**, not just verification.
- Fixed the FFI shims that call `snprintf` by including `<stdio.h>`, which
  some platforms' headers don't pull in transitively.
- Updated the docs on using FFI-backed modules.

## [0.12.0] — 2026-08-23

- Added `Linen.Data.Ini` and `Linen.Data.Yaml`: INI and YAML parsing/encoding.
- Added `Linen.Data.Float`.
- Bumped the Lean toolchain to `v4.33.1`.

## [0.11.0] — 2026-08-23

- Added `Linen.Crypto.SigV4` (AWS Signature Version 4).
- Added `Linen.Data.Hex` and `Linen.Data.Time.ISO8601`.
- Added `Linen.Network.HTTP.Client.Retry`.
- Added `Linen.Text.XML`.

## [0.10.0] — 2026-07-15

- Bumped the Lean toolchain to `v4.32.0`.

## [0.9.0] — 2026-07-15

- Version bump; no module changes.

## [0.8.0] — 2026-07-15

- Added `Linen.Control.Lens` and its `profunctors`/`indexed-traversable`
  prerequisites: a full `lens`-style profunctor-optics library
  (`Lens`/`Prism`/`Iso`/`Traversal`/`Fold`/`Getter`/`Setter`/`Review`, indexed
  variants, and `Lens` instances scattered across existing `Data.*` modules).
- Added `Linen.Data.Stream` / `Linen.Data.StreamK`: `streamly`-style fused
  streaming, plus `Data.Fold`, `Data.Scanl`, `Data.Unfold`, `Data.Parser`,
  `Data.Producer`, and `Data.Refold`.
- Added `Linen.Database.Redis`: a Redis client (cluster support, pub/sub,
  transactions, sentinel, connection pooling).
- Added `Linen.Text.Pandoc` and `Linen.Text.DocLayout`: document
  readers/writers (HTML, Markdown, Native) built on a layout engine.
- Added `Linen.Data.Array.Unboxed`, `Linen.Data.MutArray`,
  `Linen.Data.MutByteArray`, `Linen.Data.Unbox`.
- Added strict variants `Linen.Data.Either.Strict`, `Linen.Data.Maybe.Strict`,
  `Linen.Data.Tuple.Strict`.
- Added `Linen.System.IO`.

## [0.7.0] — 2026-07-13

- Added `Linen.Time.Calendar.{CalendarDiffDays,Easter,Julian,Month,Quarter}`,
  `Linen.Time.CalendarDiffTime`, `Linen.Time.Clock.TAI`, and
  `Linen.Time.UniversalTime`.

## [0.6.0] — 2026-07-13

- Added `Linen.Network.OAuth2`: OAuth 2.0 client (authorization-code, client-
  credentials, device-authorization, JWT-bearer, and resource-owner-password
  grants, plus PKCE).
- Added `Linen.Crypto.SecureRandom` and `Linen.Crypto.SHA256`.
- Added `Linen.Network.HTTP.Client.Contrib`.

## [0.5.0] — 2026-07-12

- Internal `lakefile.lean` cleanup; no module changes.

## [0.4.0] — 2026-07-12

- Added `Linen.Codec.Picture`: a JuicyPixels-style image codec (Bitmap, GIF,
  HDR, JPEG, PNG, TGA, TIFF) plus `Linen.Graphics.Image`, a `hip`-style image
  processing library (color spaces, convolution, geometric transforms,
  Fourier, Hough transform, interpolation, noise).
- Added `Linen.Database.DuckDB` (FFI bindings and a `sqlite-simple`-style
  high-level API) and `Linen.Database.SQLite` (FFI bindings and simple API).
- Added `Linen.Data.Time.Calendar` and `Linen.Data.Time.LocalTime`.

## [0.3.0] — 2026-07-11

- Added `Linen.Codec.Picture.{BitWriter,InternalHelper,Metadata,Types,
  VectorByteConversion}` (shared JuicyPixels internals ahead of the format
  decoders landing in 0.4.0).
- Added `Linen.Data.Array.Shaped`: a `repa`-style shape-indexed array library
  (delayed/manifest/partitioned/cursored representations, stencils,
  reductions, index-space operators).
- Added `Linen.Data.Colour`: a `colour`-style color library (RGB, sRGB, CIE,
  HSL/HSV color spaces, named colors).
- Added `Linen.Graphics.Netpbm`.

## [0.2.0] — 2026-07-11

- Added `Linen.CDP` (Chrome DevTools Protocol).
- Added `Linen.Data.PDF`: a `pdf-toolbox`-style PDF reader (document/page
  tree, content-stream operators, font descriptors and encodings, xref/
  object parsing, FlateDecode).
- Added `Linen.Network.WebApp`, `Linen.Network.WebSockets`, and
  `Linen.Network.URI`: a WAI/Warp-style web application interface, HTTP
  server (TLS, HTTP/2 via QUIC, gzip, static file serving) with its
  middleware stack, and a WebSockets client/server.
- Added `Linen.Web.Css` and `Linen.Web.Html`.
- Added `Linen.Crypto.AES`, `Linen.Crypto.MD5`, `Linen.Crypto.RC4`, and
  `Linen.Crypto.Zlib.FFI`.
- Added `Linen.System.Console.Ansi` and `Linen.System.Keychain`.
- Added `Linen.Data.Word8`.

## [0.1.0] — 2026-07-02

- First tagged release.
