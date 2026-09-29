# Changelog

All notable changes to `linen` are documented here, one entry per released
version (see `version` in `lakefile.lean`). Dates are UTC, in `YYYY-MM-DD`
format. Entries follow [Keep a Changelog](https://keepachangelog.com):
*Added*, *Changed*, *Deprecated*, *Removed*, *Fixed*, *Security*.

## [Unreleased]

## [1.9.1] - 2026-09-29

### Fixed

- **Deterministic HTTP/2 stream-state regression tests.** The half-closed
  stream test allowed its response to finish before the invalid DATA arrived,
  so a correct connection-level STREAM_CLOSED error failed its stream-reset
  assertion on faster CI runners. A promise now holds the response open;
  a separate test checks the fully closed stream's GOAWAY.

## [1.9.0] - 2026-09-29

The JSON encoding, Scaleway error details, EC2 XML error parsing, and GCP
key-file changes below were first written into the 1.8.0 section, but landed
after the `v1.8.0` tag (`4cb1074`): a consumer pinning `v1.8.0` does not have
them.

### Added

- **Cleartext HTTP/2 (h2c)** in both plain-server modes, and TLS's
  `allowInsecure` branch: prior knowledge (`curl --http2-prior-knowledge`)
  and HTTP/1.1 Upgrade (`curl --http2`). `Settings.settingsHttp2` controls
  it, on by default; TLS ALPN remains independently controlled. Fragmented
  prefaces and buffered frames are preserved. Upgrades validate the single
  HTTP2-Settings header, apply it before responding, and retain the initial
  request as half-closed stream 1. Content-Length and chunked upgrade bodies
  are spooled before 101 with bounded memory; Expect: 100-continue works.
- **Pinned h2spec conformance in CI and release gates**, built from source
  for all runner architectures: 146 cases each for plain/TLS HTTP/2 in
  blocking/event-loop mode. Local tests can opt in with `H2SPEC=/path/to/h2spec`.
- **`Crypto.SHA1`** — FIPS 180-4 SHA-1, pure and structurally recursive (the
  sibling of `Crypto.MD5` from the `cryptohash` import), with
  `hash_size : (hash m).size = 20`. For protocols that fix SHA-1 as a
  function — the WebSocket handshake — not for security.
- **`Network.WebApp.Server.requestFraming`**, `BodyFraming`,
  `parseContentLength`, `parseChunkSize`, `chunkedBodyReader` and
  `drainBody` — the request-body framing decision and chunked decoder, pure or
  over injected readers so they are tested without a socket.
- **`Network.WebApp.Server.ByteSource`** (`ofRecvBuffer`, `buffered`) and
  `parseRequestFrom` / `recvHeadersFrom` — the request parser over any byte
  source, not only the C `RecvBuffer`; **`ResponseSink`** (`ofSocket`,
  `ofSocketEL`) and `sendResponseTo` — response writing over any transport;
  `Network.Sendfile.sendFileWith`. What the TLS server now reads and writes
  through.
- **`Server.TLS.runTLSSocket`** — serve TLS on an already-listening socket
  until a cancellation token fires (what `runTLS` runs, and what the tests
  use).
- **A resumable non-blocking TLS API** in `Network.TLS.Context`:
  `newServerSession` / `newClientSession` create a session once, and
  `handshakeNB` steps its handshake on that same `SSL` object — the design
  of rust-openssl (`MidHandshakeSslStream::handshake`), HsOpenSSL
  (`sslBlock`) and tokio-openssl; `handshake` drives it with `poll`.
- **`Network.TLS.Green`** — `accept`, `connect`, `read`, `write` on green
  threads over the `EventDispatcher`: each operation tried before waiting
  (OpenSSL may already hold decrypted bytes the socket will not signal),
  waiting in whichever direction OpenSSL asks. Tested on non-blocking
  sockets at both ends, with multi-MiB transfers forcing `wantWrite`.
- **HTTP/2 over TLS.** `Network.HTTP2.serve`, a new server connection
  engine: multiplexed streams each handled concurrently, request bodies
  delivered as the handler reads them, responses split to the peer's frame
  size and paced by flow control in both directions, and RFC 9113's
  validation and error handling — stream resets and GOAWAY, with
  rapid-reset (CVE-2023-44487) protection. It passes all 146 cases of the
  h2spec conformance suite, and is tested against curl (nghttp2).
  `Network.WebApp.Server.HTTP2` (`serveHttp2`) serves a WebApp application
  over it, and `Server.TLS` offers `h2` by ALPN (`TLSSettings.http2`, on by
  default) in both of its modes.
- **ALPN in the FFI**: `Network.TLS.setServerAlpn` (a server's protocol
  list, in its order of preference), `setClientAlpn`, `alpnWire`.
- **`HPACK.encodeHeadersStatic`** — encoding with no dynamic table, valid
  whatever SETTINGS_HEADER_TABLE_SIZE the peer advertised.
- **Timers.** Dispatcher deadlines are libuv timers
  (`Std.Internal.UV.Timer`): a timeout is honoured to within about a
  millisecond, and costs nothing while idle — the 100 ms sweep is gone.
  `Green.sleep` suspends a green thread without holding a pool thread.
- **`Server.acceptLoopUntil`** and `forkConnection`.
- **`Server.TLS.runTLSEventLoop`** (`runTLSSocketEL`, `tlsConnectionEL`) —
  HTTPS on green threads: peek, handshake and head reads suspend the green
  thread, so idle and slow TLS connections hold no pool thread.
- **One HTTP/1.1 loop for every transport**: `Server.HttpTransport` and
  `serveHttp`, with `blockingTransport`, `eventLoopTransport`, and
  `Server.TLS.tlsTransport` / `tlsTransportEL`. Plain and TLS, blocking and
  event-loop connections now share parsing, pipelining, draining, timeouts
  and the buffered `responseRaw` handoff.
- **Timeouts on the dispatcher**: `waitReadableFor` / `waitWritableFor`
  (Green), `awaitReadableFor` / `awaitWritableFor` (`IO`, waiting with
  `IO.wait` on the dispatcher's promise — which Lean's task manager
  compensates for, unlike a blocked `poll`), `recvFor` / `recvAwait`,
  `sendAllGreenFor` / `sendAllAwait`; `EventType.oneshot`.
- **TLS with timeouts**: `Network.TLS.readWithin` / `writeWithin` (`poll`
  on a non-blocking socket); `Network.TLS.Green.readFor`, timeouts on
  `handshake` / `accept` / `connect` / `write`, and `readIO` / `writeIO`.
- **`Network.Socket.peek`** (`MSG_PEEK`), and `ByteSource`'s `feed` /
  `unread`, `headComplete`, `maxHeadBytes`, `Server.recvSuspending`,
  `Response.filePartLength` / `fileBodyLength`.

### Changed

- **`Network.WebApp.Server.recvHeaders` returns
  `IO (Option (String × HeaderLines))`**, where `HeaderLines` carries
  `length ≤ maxHeaders` in its type; `none` when the head has more header
  lines. This replaces the axiom `recvHeaders_bounded`, which is removed.
- **A request with no `Content-Length` and no `Transfer-Encoding` has
  `requestBodyLength = .knownLength 0`**, not `.chunkedBody` (RFC 9112 §6.3:
  it has no body). Middleware that treated `.chunkedBody` as "maybe a body",
  such as `requestSizeLimit`, no longer wraps every `GET`.
- **The JSON encoder no longer escapes `/`.** `\/` is legal JSON and not
  legal YAML 1.1, so Kubernetes' server-side apply (`apply-patch+yaml`)
  refused every manifest naming `apps/v1` — "found unknown escape
  character", on `infra`'s first live apply. RFC 8259 does not require the
  escape and Go, Python, Aeson and serde do not emit it. Output bytes change
  wherever a string holds `/` (URLs, `apiVersion`s, JWT claim sets); the
  decoder still accepts `\/`, and `Web.Html` keeps `</script` out of raw text
  by proof, independently of this.
- **A Scaleway error keeps its `details`**: `describeError` appends each
  `argument_name: help_message` to the message, so "invalid argument(s)" says
  which argument and why.
- **`parseXmlError` reads EC2's `<Response><Errors><Error>` envelope**, and
  a request id beside the error element — so an EC2 failure keeps its code
  (and classifies) instead of falling back to the raw body.
- **The GCP key-file source declines a federated or user credential file**
  (`Credentials.Gcp.foreignTypes`, `declaredType`): `external_account`, which
  `google-github-actions/auth` points `GOOGLE_APPLICATION_CREDENTIALS` at, is
  not a service-account key, and failing on it hid the token the next source
  had. Moved from `infra` (`GcpAuth.foreignTypes`).

### Removed

- **The old `Network.HTTP2.Server` internals**: `ConnectionState`,
  `processSettings`, `processWindowUpdateFrame`, `processPing`,
  `sendResponse`, `sendGoaway`, `sendRstStream`. The handler they served
  answered a request as soon as its headers arrived (bodies were never
  delivered), handled streams one at a time, sent a whole body as one DATA
  frame whatever the peer's frame size and windows, never returned receive
  window (an upload stalled after 64 KiB), and checked incoming frame sizes
  against the peer's settings. `runHTTP2Connection` keeps its signature, on
  the new engine.
- **`Network.TLS.acceptSocketNB`, `connectSocketNB`, `connectSocketRaw`.**
  The `*NB` pair created a fresh `SSL` per call and freed it on every
  would-block, so a handshake needing a second read could never complete;
  use `newServerSession`/`newClientSession` with `handshakeNB` (or
  `Network.TLS.Green`). `acceptSocket` and `connectSocket` keep their
  signatures and are now built on the resumable API, so they also work on
  non-blocking sockets.
- **`Network.Socket.FFI.recvBufReadLineNB` / `recvBufReadNNB`** — unused and
  untested, and a partial line longer than the 4 KiB buffer was silently
  dropped on `EAGAIN`.
- **`Server.TLS.TLSSettings.alpn`.** `true` (the default) called
  `Network.TLS.setAlpn`, which *prefers `h2`*, so any browser would
  negotiate HTTP/2 with a server that spoke only HTTP/1.1. Replaced by
  `TLSSettings.http2`: on by default now that the server implements HTTP/2;
  off, it does not answer ALPN and clients fall back to HTTP/1.1.

### Fixed

- **TLS session concurrency.** The HTTP/2 reader and stream handlers could
  call SSL_read/SSL_write on the same OpenSSL object concurrently, causing
  intermittent truncated transfers and resets (reproduced on Linux).
  A per-session mutex protects every I/O/handshake/getter/close operation,
  released before a non-blocking readiness wait. External-class registration
  is once-only. A full-duplex, multi-MiB regression test races both directions
  and getters; TLS and h2c interoperability are checked on macOS and Linux.
- **HTTP/2 teardown also runs on transport exceptions and cancellation**,
  waking waiting handlers and preventing late writes before the caller
  releases the connection.
- **Connection options are parsed as tokens across repeated fields.**
  `Connection: Upgrade, close` now closes an HTTP/1.1 connection as required,
  instead of being treated as keep-alive because the whole field was not
  exactly `close`.
- **HPACK**: table entry sizes are counted in octets, not characters (any
  non-ASCII field desynchronised the table from the peer's); a string that
  does not decode — bad Huffman padding, an encoded EOS, invalid UTF-8 — is
  an error, where its raw bytes or `""` were silently substituted; a
  dynamic table size update is refused above the advertised limit or after
  the block's first field.
- **Blocking-mode servers no longer starve on idle connections.** Each
  connection ran on a pool thread (`forkIO`) and then waited in `poll`,
  which the pool does not compensate for: 64 idle clients kept a new
  request waiting 30 s, until one of them timed out. Each connection now
  has a dedicated thread (`forkConnection`), in `runSettings` and `runTLS`.
- **TLS server contexts use AEAD cipher suites with ephemeral key exchange
  for TLS 1.2** (Mozilla's "intermediate" list, what RFC 9113 §9.2.2
  requires of HTTP/2) and refuse renegotiation.
- **The HTTPS server works.** `Server.TLS.runTLS` performed the handshake and
  then parsed requests from, and wrote responses to, the *raw socket*,
  bypassing the TLS session — no HTTPS request could succeed, and no test
  exercised it. Requests and responses now go through the session (files
  and `responseRaw` included), one thread per connection with a blocking
  handshake — `acceptSocketNB` cannot resume a handshake that would block.
  It is tested end to end with a real TLS client: keep-alive, a chunked
  body, a request split across TLS records, and plaintext on the TLS port.
- **`OnInsecure` works.** It was never implemented: plaintext on the TLS
  port just failed the handshake. The first byte is now peeked (`0x16` opens
  a TLS record, as warp-tls tests): `denyInsecure message` answers
  `426 Upgrade Required` with the message, `allowInsecure` serves plain
  HTTP (with `isSecure = false`).
- **TLS sessions are shut down safely.** The GC finalizer used to call
  `SSL_shutdown`, writing a close_notify to whatever connection the fd
  number belonged to by then; it now only frees. `close` shuts down once,
  and never after a fatal error. Every context sets
  `SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER`, so a retried write whose bytes moved
  is not a "bad write retry"; the OpenSSL error queue is cleared before each
  call; the blocking `read` no longer leaks its buffer at end of input.
- **Event-loop mode serves pipelined requests and slow clients.**
  `runConnectionEL` waited for readability before every request, even
  with the next one already buffered, so pipelined requests hung; and it
  parsed from the C `RecvBuffer`, which gives up after a few `EAGAIN`
  retries, so a head or body arriving in pieces dropped the connection.
  Heads are now buffered on the green thread (no pool thread held while
  waiting) and parsed once complete.
- **The event dispatcher no longer spins.** Registrations were
  level-triggered and never removed, so after one `waitWritable` — or a
  `waitReadable` that left data unread — the shard's `kevent`/`epoll_wait`
  returned at once, forever: an idle process burnt a full core (measured:
  2.0 s of CPU in 2 s; now 0.01 s). They are now one-shot, carry every
  direction awaited on the fd (with epoll a second waiter used to replace
  the first's mask), and are re-armed while waiters remain — libuv's, mio's
  and GHC's scheme. A test checks the CPU an idle dispatcher uses.
- **Every server mode enforces `settingsTimeout`**: waiting for a request
  head (closed quietly, as in Warp), for body bytes (an error the
  application sees), and for a peer to accept writes. Through 1.8.0 the
  blocking server and the TLS server waited forever, so an idle or
  stalled client held its thread; `settingsTimeout` was never read.
- **Event-loop body reads no longer block pool threads**: they wait on the
  dispatcher's promise with `IO.wait` instead of `poll`, as do writes from
  `IO` callbacks (streamed bodies, files, `responseRaw`), which used
  `Blocking.sendAll`.
- **`responseRaw` sees bytes already buffered** in blocking mode too — a
  WebSocket client's first frame sent right behind its upgrade request was
  skipped, since the handler read the socket directly.
- **A `Content-Length` body cut short by the peer is an error**, where the
  empty read at end of input made it look complete.
- **The event-loop accept loops back off on accept errors** (e.g. `EMFILE`)
  instead of spinning on a listener that stays readable.
- **File responses carry `Content-Length`** (of the `FilePart`, when
  given): without it a kept-alive client could not find the end of the
  file.
- **`Sendfile` with `FilePart.count = 0` sends to the end of the file**, as
  documented; it sent nothing.
- **The WebSocket handshake works with real peers.** `computeAcceptKey`
  used a placeholder SHA-1 that ignored its input, *and* `webSocketGUID` was
  garbled (`…-5AB5DC76B45B` for RFC 6455's `…-C5AB0DC85B11`), so every
  `Sec-WebSocket-Accept` was the same wrong constant and browsers refused
  the upgrade; the tests asserted the constant. It now matches RFC 6455
  §1.3's worked example.
- **`WebSockets.Client.runClient` verifies the server's handshake**
  (`checkHandshakeResponse`: status `101`, `Upgrade`, `Connection`, and
  `Sec-WebSocket-Accept` against the key it sent) instead of the status
  alone.
- **Chunked request bodies are decoded, and ambiguous framing is refused
  (request smuggling).** The server read a `Transfer-Encoding: chunked` body
  as empty and did not drain it, so on a kept-alive connection the body's
  bytes were parsed as the *next request* — a smuggling primitive behind any
  proxy that does decode chunked bodies. The body is now decoded (sizes,
  extensions, trailers; truncation and malformed framing are errors, not a
  short body), and a request is refused — the connection closed — when it
  has both `Transfer-Encoding` and `Content-Length`, `Transfer-Encoding` on
  HTTP/1.0, a final coding other than `chunked`, or an invalid or
  conflicting `Content-Length`.
- **A head with more than `maxHeaders` header lines is refused**, where it
  was truncated and the rest read as body or as the next request.
- **Every keep-alive loop drains the unread body** (`drainBody`), including
  chunked bodies and the TLS server's loop, which drained nothing.
- **`ci/consumer/link-helpers.lean` folds whitespace without escape
  sequences** (`ded0fe3`), so the block survives being embedded in a string
  literal — also after the `v1.8.0` tag.

## [1.8.0] - 2026-09-29

The building blocks the sibling `infra` carried copies of — a JSON field
update, switchable terminal colour, the consumer link-flag helpers, a CA
fallback — and the error taxonomy split its move onto `Linen.Cloud` needed.

### Added

- **`Data.Json.Value.setField`** — rewrite one field of an object, keeping
  every other field and their order; appends when absent. Moved from the
  siblings `infra` (`JsonRead.setField`) and `liaison`
  (`Egress/Credential.lean`), which each carried a copy; their halves of the
  move land once they pin a release carrying it.
- **`Data.Json.Value.lookupText` / `lookupNat` / `lookupBool`** — lenient
  scalar reads (a quoted number is a number, an unquoted one is text), moved
  from `infra`'s `JsonRead.stringField` / `natField` / `boolField`.
- **`System.Console.Ansi.style`, `dim`, `wanted`** (and `shouldColor`, its pure
  half; `Intensity.faint`; `Color.fgCode`, `boldCode`, `faintCode`) — a
  switchable SGR wrapper and the `NO_COLOR` / `FORCE_COLOR` / terminal rule,
  moved from `infra` (`Infra/Core/Ansi.lean`); `typednotes-compiler` reads
  `NO_COLOR` too.
- **`ci/consumer/link-helpers.lean` and `ci/consumer/check-link-helpers.sh`**
  — the canonical, versioned block of link-flag helpers an executable that
  requires `linen` needs (`pkgConfigFlags`, `pkgAbsoluteLibs`, `macSdkArgs`,
  `keychainLinkArgs`), and a checker a consumer runs against the tag it pins.
  The CI `consumer` job now splices the block in and checks it, so the
  executable link it performs is what tests the canonical copy. Lake links an
  executable with its own package's `moreLinkArgs` only, so the copy cannot
  go away; six repositories carried a hand-maintained one.
- **A fallback CA bundle for TLS clients.** `createClientContext` also loads
  the first readable of `/etc/ssl/certs/ca-certificates.crt`,
  `/etc/pki/tls/certs/ca-bundle.crt`, `/etc/ssl/ca-bundle.pem` and
  `/etc/ssl/cert.pem` when `SSL_CERT_FILE` is unset and OpenSSL's compiled-in
  default file does not exist — the usual case for the static OpenSSL a Lean
  toolchain links in, whose paths are the build machine's.
  `Network.TLS.fallbackCaBundle` reports which. Consumers can drop their
  "point OpenSSL at the runner's CA bundle" CI steps.
- **`Cloud.Class.unauthenticated` and `Cloud.Class.serviceDisabled`**, split
  out of `denied`; `Class.isAuthFailure` for the old, coarse question;
  `classifyMessage`, and `unauthenticatedCodes` / `serviceDisabledMarkers`.

### Changed

- **The keychain service is a parameter.** `Cloud.Credentials.Keychain`'s
  `fromAccount`, `forProvider`, `storeInAccount`, `store`, `deleteAccount`,
  `loadFrom` and `load`, `Chain.loadFrom`/`load`, and `loadWith`,
  `sourceDescriptions` and `noCredentialsMessage` take the service (default
  `keychainService`, `"linen"`), so a tool that stored credentials under its
  own name keeps finding them — and its not-found message names the right
  service. Asked for by `infra`, whose entries live under `"infra"`.
- **More not-found codes**: `NoSuchEntity` (IAM), `DBInstanceNotFound` (RDS),
  `RepositoryNotFoundException` (ECR) and EC2's `InvalidAMIID.NotFound`,
  `InvalidGroup.NotFound`, `InvalidInstanceID.NotFound` — which EC2 answers
  with a 400, so the status alone would have said `invalid`. `infra`
  recognised all of them and now uses this taxonomy.

- **`Cloud.Class.denied` now means one thing: authenticated, and refused for
  this resource.** A bad signature, an expired or missing token and an unknown
  key classify as `unauthenticated` (including a bare `401`, and the
  incomplete-credentials refusals of `Call.preflight` and
  `Auth.presignedUrlAt`); a Google `PERMISSION_DENIED` whose message says the
  API is not enabled classifies as `serviceDisabled` (`describeError` reads
  the message). A caller that matched `.denied` for "any refusal" should use
  `Class.isAuthFailure`. Asked for by `infra`, whose "a refused marker read
  means not ours" must never widen from one resource to a whole kind — the
  blocker on its move to `Linen.Cloud`.

## [1.7.0] - 2026-09-28

Building blocks moved from the sibling services `lode` and `lun`, which
carried identical copies (and `liaison`, whose warrant-tag check needed the
constant-time comparison neither linen nor it had).

### Added

- **`System.Process`** — run a command to completion with a deadline and an
  abort flag (`IO.Ref Bool`), killing the child's whole process group; stdout
  and stderr read concurrently, optional stdin; `runBytes` for binary output;
  `Result.describe`; `hermeticGit`.
- **`System.LakeLog`** — `lake build`'s text output as `Diagnostic`s
  (`parse`, `splitLocation`, `render`, `isSummary`, `ToJson`).
- **`System.Git.Remote`** — `isBranchName` (what `git check-ref-format
  --branch` accepts) and `Repository.parse` (GitHub, GitLab, other `https`
  hosts, `file://` on request; one canonical clone URL).
- **`Crypto.ConstantTime`** — `eq`/`eqString`, comparisons that read every
  byte whatever they find.
- **`Network.HTTP.Client.parseRetryAfterMillis` and `delayFor`** — the
  `Retry-After` parsing and delay choice of `delayBefore`, usable without a
  `Client.Response` (a relayed response, a loop of one's own that checks a
  cancellation flag between attempts). `retryAfterMillis` and `delayBefore`
  are now defined on top of them, unchanged.
- **`ci/native-deps/apt.txt`** — the Debian/Ubuntu packages linen needs to
  build as a dependency, read by `.github/actions/setup-native-deps` and
  readable by consumers at a tag (a Dockerfile's `ADD`). The action takes a
  `keyring` input (default `true`); a consumer passes `false` to skip the
  Secret Service daemon linen's own tests need.

### Changed

- **The test library is `LinenTest`** (module tree `LinenTest.*`, directory
  `LinenTest/`), no longer `Tests`, following mathlib's `MathlibTest`,
  batteries' `BatteriesTest` and aesop's `AesopTest`; it is the package's
  `testDriver`, so the suite runs with `lake test`. A dependency owning the
  generic top-level module name `Tests` confused Lake's module lookup in every
  consumer that named its own tests `Tests`. Test namespaces are unchanged.
- **`System.GitFn.Build` runs every `git` and `lake` step under a deadline**
  (`Config.timeoutMs`, an hour by default), through `System.Process`: a step
  that outlives it is killed with its process group and the build fails
  saying so. A command that cannot be started is now an `.error`, not an
  exception.

### Security

- **`Crypto.JOSE.JWS.verifySignature` compares HMAC signatures in constant
  time** (`Crypto.ConstantTime.eq`); it used `ByteArray`'s `==`, which returns
  at the first differing byte and so tells an attacker how much of a forged
  MAC is right.

### Fixed

- **Killing a command reaches its descendants.** Lean's runtime (4.34) drops
  the `setsid` flag from the `Child` that `Child.takeStdin` returns, so
  `Child.kill` on it signals the leader only. lode's and lun's runners (now
  `System.Process`) relied on it: at a deadline or an abort, a `lake build`'s
  `lean` workers or a shell's background jobs kept running — and kept the
  output pipes open, so the runner waited for them past its deadline.
  `System.Process` signals the group explicitly (`killGroup`); a test checks
  that a grandchild dies and that the runner returns at the deadline.

## [1.6.2] - 2026-09-28

### Fixed

- **`raw!` made the kernel use gigabytes for a few hundred characters.** It
  proved `RawText.Safe tag "…"` by `decide +kernel` on the string literal, and
  the kernel checks that by expanding the literal to `String.ofList`,
  encoding it to UTF-8 bytes, and decoding them back with
  `ByteArray.utf8Decode?` — well-founded recursion, run by reducing its
  termination proofs, reading each byte by walking the list under the array,
  with every intermediate term kept until the declaration is checked. The cost
  grew faster than quadratically (200 characters: 0.25 GB; 400: 1.8 GB; 620:
  5.5 GB and 15 s). `Graphics.Graphviz.Html`'s 620-character loader alone
  peaked at 6.1 GB, so on GitHub's 7 GB macOS runners it swapped for 18–25
  minutes in the consumer job, which is the one job that compiles it from
  scratch (`precompileModules` builds the whole library there). `raw!` now
  expands the literal to its characters and proves
  `breaksOut tag ['…', …] = false` (new `RawText.safe_ofList`), which the
  kernel checks by comparing `Char` literals: linear, and too small to
  measure — `Graphviz.Html` compiles in 1.6 s at 0.70 GB (its imports' cost),
  down from 19 s and 6.1 GB, and a 1750-character literal in `HtmlTest` is
  free. Still `decide +kernel`, so no new axiom; unsafe literals are still
  refused, with the offending characters in the message. The one cost is at
  run time: a `raw!` value is built from its character list (once, for a
  top-level constant) instead of being a string literal.
- A scan for the same pattern — kernel reduction over string contents
  (`raw!`, `decide`/`rfl` proofs on string functions) — found no other
  instance: no reducing proof in `Linen/` or `Tests/` involves a literal of 80
  or more characters. The slowest remaining modules are large, not
  pathological (`CDP.Domains.DOMPageNetworkEmulationSecurity`: 7644 generated
  lines, 15 s, 1.6 GB).

## [1.6.1] - 2026-09-28

### Fixed

- **`System.GitFn` could still load the same library environment more than
  once.** 1.6.0 cached them per process, but keyed on the request as
  spelled: `import Std` then `import Init` and the reverse, a repeated
  import, or a search-path entry written through `..`, `.`, a duplicate or a
  symbolic link each loaded — and kept, since an import is never released —
  another full environment. The key, and what is imported, is now canonical:
  imports sorted and deduplicated (`canonicalImports`: they load the same
  closure, so the same syntax to parse with); search-path entries made
  absolute and resolved with `realPath` (`lexicalNormalize` for one that does
  not exist) and deduplicated — but **kept in order**, since the first entry
  holding a module wins. Distinct import sets still get one environment
  each, for the life of the process. Tested in `PolicyTest` (the key) and in
  `gitfn-integration`, which requests one environment four ways and asserts a
  single load; with the canonicalisation disabled, three of those four
  checks fail.

## [1.6.0] - 2026-09-28

### Added

- **`lake exe gitfn-remote`**: `System.GitFn` against a real, private GitHub
  repository, `typednotes/test`, at a pinned commit — an authenticated
  `git fetch` of a SHA, as a real descriptor needs. The repository carries
  its expectations (`gitfn.json`): the modules the policy must exclude and
  why (one per reason), the functions that must build and their calls' results
  (over stdio, and HTTP for some), the descriptors that must be rejected, and
  markers its hostile lakefile and `#eval` would leave. CI runs it on the
  Linux x86_64 leg with the repository's read-only deploy key, when the
  `GITFN_DEPLOY_KEY` secret is set, and warns when it is not;
  `GITFN_TEST_REPO`/`GITFN_TEST_COMMIT` point it elsewhere.

### Fixed

- **`System.GitFn` leaked a Lean environment on every build.** The secure
  check parses remote sources with the allowed libraries' environment, and
  `checkProject` imported it afresh on every call — once per `build`,
  `fetchAndCheck` and `vendor`. `importModules` memory is never released when
  an `Environment` is dropped, so each call leaked ~860 MB (`Init`): a host
  building many functions grew without bound. `gitfn-remote`'s 24 checks
  reached over 10 GB, and the 16 GB Linux runner swapped silently for 30
  minutes until the job was cancelled. Library environments are now loaded
  once per process per search path and set of imports
  (`libraryEnvironments`, guarded by a mutex); `libraryEnvironmentsLoaded`
  reports how many. Peak memory of `gitfn-integration` fell from 7.1 GB to
  1.4 GB and of `gitfn-remote` from over 10 GB to 1.4 GB, which is also
  faster. Both integration tests now assert that the count does not grow
  after the first check, and CI bounds both GitFn steps
  (`timeout-minutes`), so a hang fails in minutes rather than hours.

## [1.5.0] - 2026-09-27

### Added

- **`Examples/GitFnGraph.lean`** (`lake exe examples gitfn`): a reactive
  pricing graph whose functions live in a (generated) git repository —
  vendored after the secure check, imported by a program and compiled in as
  ordinary functions, next to local operators; the program checks every
  stream and draws the graph and its run as an offline HTML page.
- `System.GitFn.vendor` takes **several functions** of one project and
  checks each in the one package; `GitFn.definition` binds a vendored
  function at its declared type.
- `Data.Json.Bridge`: Lean core `ToJson`/`FromJson` instances for
  `Data.Json.Value`.

### Changed

- **The worker protocol is JSON-RPC 2.0**, on Lean core's `Lean.JsonRpc`
  types, instead of a bespoke `{"args"}`/`{"ok"}` format: any JSON-RPC client
  can call a worker; argument errors are `invalidParams`, function failures
  `internalError`, unknown methods `methodNotFound`. `request`/`reply` take a
  request id.
- `System.GitFn.JsonValue` is removed: remote calls use Lean core's
  `ToJson`/`FromJson` of the graph's value type.
- The `Codec Lean.Json` instances and `Codec.ofJson` move from
  `System.GitFn.Reactive` to `Control.Reactive.Graph`: they are about graphs
  over Lean core's JSON, not about workers.
- `Data.Name.parse` is Lean core's `Syntax.decodeNameLit` (the elaborator's own
  total name-literal reader) instead of a re-implementation; it is stricter
  (an unescaped space is an error) and its errors are one message.
- `vendor`/`build` cache keys include the generated sources, so a change to
  the worker never reuses a worker built from an older generator.

### Removed

- `System.GitFn.Transport`, which nothing used.

### Fixed

- **Stopping a stdio worker hung while another worker was alive**: each
  process spawned later inherits the write end of the earlier workers' input
  pipes, so closing ours never delivered EOF. `StdioWorker.stop` now sends the
  JSON-RPC notification `exit`; covered by `gitfn-integration`.

## [1.4.0] - 2026-09-27

### Added

- **`System.GitFn`** — Lean functions defined by their git location (a
  repository, a commit SHA, the project directory, the fully qualified name
  and the declared type), fetched, checked, compiled and run **securely**:
  nothing from the repository runs (not its lakefile, build scripts or
  toolchain); its sources are checked with the host's parser before any
  compilation (`System.GitFn.Policy`: allowlisted imports, commands,
  attributes and options; no `unsafe`, `partial`, `axiom`, `sorry`,
  `native_decide`, `#eval`, macros, `include_str`, `extern`/`implemented_by`,
  or side effects outside a monad in the type); they are compiled with the
  host's own toolchain, linking only the libraries you select; and the
  compiled result is checked again (declared type definitionally equal, no
  unsafe/partial/extern/implemented_by remote constant, no initializer,
  standard axioms only). Functions run in a worker speaking JSON (Lean core's
  `ToJson`/`FromJson`) over stdio or as a REST service
  (`System.GitFn.Worker`), or the checked sources are vendored into a package
  you `require` (`System.GitFn.Build.vendor`). `resolve` pins a branch or tag
  to a SHA. A worker is also a node of a reactive graph:
  `Reactive.remote args β r` registers it as an ordinary `FnRef`
  (`System.GitFn.Reactive`, with proven `Codec Lean.Json` instances for
  `Nat`/`Int`/`String`/`Bool` and `Codec.ofJson`). Covered end to end by
  `lake exe gitfn-integration` (including graphs around real stdio and HTTP
  workers), now run by CI on every platform leg.
- **`Control.Reactive.Reactive.fnImpl`** — register an already-erased
  implementation under a declared signature (how remote functions enter a
  graph).
- **`Data.Name`** — a total reader of `Lean.Name`'s dotted syntax
  (`Data.Name.parse`, `Data.Name.roundTrips`), where `String.toName` can reach
  `unreachable!`; `Control.Reactive.Json`'s `parseLabel` now uses it.
- **`Data.Json.Bridge`** — conversions between linen's `Data.Json.Value` and
  Lean core's `Lean.Json`, exact except for numbers, as documented. Writing it
  surfaced an existing limitation, left unchanged in this release because
  fixing it changes output other code may depend on: `Data.Json.Encode`
  writes non-integer numbers with 6 significant digits (`0.123456789` ↦
  `0.123457`), so linen's JSON output loses precision on such numbers.

## [1.3.0] - 2026-09-27

### Added

- **`Control.Reactive`** — typed reactive graphs: DAGs of **observables**.
  Subjects (inputs) are streams of events; every node is a ReactiveX operator
  over earlier nodes (`map`/`mapE`/`mapM`, `filter`, `scan`, `take`, `skip`,
  `distinctUntilChanged`, `merge`, `mergeWith`, `combineLatest`,
  `withLatestFrom`, `zip`, `throttleTime`, `debounceTime`, `delay`), taking
  plain Lean functions (or `FnRef`s from `fn`, for sharing and `rebind`).
  Names are ReactiveX's; semantics are reactive-banana's: instants visited in
  topological order (no glitches), at most one value per node per instant then
  an optional `error`/`complete`, explicit simultaneity, deterministic runs
  over virtual time with a scheduler for the timed operators. A run returns
  every node's stream (`Trace`) or only selected results (`Selection.only`,
  `runFor`, `valuesFor`, `runSelected` — unneeded nodes are not computed), at
  once or incrementally (`Session`, with `pushAll_append`). A graph is itself
  an observable: `Operator.define` turns a builder over typed inputs into an
  operator that splices into other graphs. Every `Graph` carries proofs that
  it is acyclic with correct arities (`WellFormed`) and uniquely labelled
  (`Labelled`); `node x ← e` labels after the source identifier, `scope`
  qualifies reused sub-builders. Split into `Control.Reactive.Graph`,
  `.Builder` and `.Run`.
- **`Control.Reactive.Json`** — graphs (functions bound by label through a
  `Registry`, invariants re-established on reading), logs of occurrences
  (replayable against a newer version of a graph) and traces, as JSON; labels
  round-trip exactly (`parseLabel`, `labelToJSON_string`).
- **`Control.Reactive.Graphviz`** — a reactive graph as typed DOT, optionally
  with a run (values, completion, errors); edges are built from the graph's
  `WellFormed` proof.
- **`Graphics.Graphviz`** — typed Graphviz DOT that cannot be malformed:
  edges are `Fin nodes.size` (no dangling edge), the edge operator follows the
  graph kind, attributes are typed by target with private constructors, and
  `lex_quote` proves no text can escape its quotes.
- **`Graphics.Graphviz.Html`** — self-contained, offline HTML pages rendering
  DOT with Graphviz compiled to WebAssembly (`@hpcc-js/wasm-graphviz` 1.29.1,
  vendored under `vendor/`, script-safety checked at compile time).
- **`Web.Html`** — `script`, `meta_` and `charset`; `RawText tag`, a
  `<script>`/`<style>` body with a proof that it cannot close its element,
  built from `raw!` literals (kernel-checked), `RawText.ofString?`, or
  `RawText.jsonString` (proven safe for any text).
- **`Control.Monad.Effect.Handler`** — `Handler eff m` (an effect's canonical
  handler into `m`), `Handlers effs m` (derived for rows) and `Eff.handle`,
  which runs a whole row in `m`; `Eff.handle_singleton` proves it is
  `interpretM` on a single-effect row. `Handler _ IO` instances for `Trace`
  (stderr), `Error ε` (`IO.userError`, given `ToString ε`), `HTTP cap`
  (`sendOnce`) and `FileSystem cap` (`IO.FS`), with
  `handle_eq_runHTTP`/`handle_eq_runFileSystem`.

### Changed

- **Breaking: `Web.Html.styleSheet` takes a `RawText .style`**, not a
  `String`: a stylesheet containing `</style>` could close its element and
  inject markup. Wrap literals as `styleSheet (raw! "…")`.
- **Breaking: `Linen.Text.Pandoc.Writers.Blaze` is renamed
  `Linen.Text.Pandoc.Writers.HtmlLayout`** (namespace likewise), after what it
  does rather than the Haskell library its upstream walks.

## [1.2.0] - 2026-09-23

### Fixed

- **Consumer executables importing DuckDB no longer fail to link on Linux.**
  The sealed `libduckdb_sealed.so` absorbs the *host's* static
  `libstdc++.a`/`libgcc_eh.a`, which are built against the host glibc and so
  referenced symbols the glibc Lean bundles predates — on Ubuntu 24.04:
  `__isoc23_strtoul` (glibc 2.38), `__libc_single_threaded` (2.32) and
  `_dl_find_object` (2.35). A shared-library link permits undefined
  symbols, so every `lean_lib`/`Tests` build stayed green; `ld.lld` checks
  them only when linking an *executable* (`--no-allow-shlib-undefined`), and
  a consumer's `lean_exe` importing DuckDB failed with `undefined reference:
  __isoc23_strtoul` while this repository's consumer job deliberately kept
  DuckDB out of its test executable. `ffi/duckdb_glibc_compat.c` now links
  hidden-visibility shims into the sealed library — delegations to the
  older-glibc equivalents or conservative constants — so the library behaves
  identically on every host. The CI consumer job now links *and runs* a
  DuckDB-importing executable, the one link shape that ever caught this.

### Added

- **The sealed library's undefined symbols are audited on every build**
  (`auditSealedDuckdbLib` in `lakefile.lean`): every non-weak undefined
  dynamic symbol must be defined by the toolchain's own libraries, else the
  build fails naming the symbols and `ffi/duckdb_glibc_compat.c` — so a
  future host C++ runtime referencing some newer glibc symbol is a loud,
  local build failure, not a consumer's `ld.lld` error.
  `ci/check-sealed-duckdb.sh` asserts the three measured symbols stay
  resolved in the artifact.

## [1.1.0] - 2026-09-23

### Fixed

- **Consumer executables no longer fail to link on Linux.** On Linux,
  `pkgLinkFlags`' explicit `-L<libdir>` named `/usr/lib/<multiarch>`, which
  holds the *system* `libc.so` — preempting the glibc Lean bundles, so any
  consumer's `lean_exe` failed with `undefined symbol: __libc_csu_init`
  (Lean's vendored `Scrt1.o` references compat symbols glibc 2.34 removed).
  `linen`'s own CI never saw it: no target it builds links an executable
  startup object. libpq, zlib and libsecret now link by naming the library
  file outright (`pkgAbsoluteLibs`) on Linux; `pkgLinkFlags` remains for
  macOS, where the keg-only Homebrew directory is safe.

- **No system OpenSSL link flags are emitted, on any platform.** Lean's
  toolchain already ends every link with `-lssl -lcrypto` against its bundled
  static archives, so the flags were redundant — and on Linux actively
  fatal, since the system `libssl.so` needs GLIBC symbol versions newer than
  the glibc Lean bundles.

### Added

- **CI now links a consumer `lean_exe`** (Linux arm64/x86_64 and macOS), with
  `main` referencing `linen_pg_*` and `linen_jose_hmac` symbols. A `lean_lib`
  never links `Scrt1.o`, so until now nothing in CI exercised the shape every
  consumer (`infra`, `ledger`, `liaison`) actually builds. This is the
  regression test for both fixes above.

## [1.0.0] - 2026-09-20

### Changed

- **A consumer now builds only the modules it imports.** `lean_lib Linen` is
  no longer `precompileModules`-enabled. Precompilation forces `Linen:shared`,
  a whole-library artifact that nothing can link against until every module is
  compiled — so importing a single leaf module built all ~770. A package whose
  only import is `Linen.Data.Functor` goes from **2333 build jobs (5m04s) to
  16 (13s)**.

  Compiled code is unaffected. **If you call an `@[extern]` binding from
  `#eval` or `#guard`, set `precompileModules := true` on your own library** —
  that is what makes the bindings reachable through the interpreter.

  This does not change the native layer: any package that links still builds
  every `extern_lib`, so the C shims are compiled and DuckDB's pinned archive
  fetched whatever you import.

- **An unsealable Linux host is now a build failure, not a warning.** When a
  static libstdc++ is missing, `duckdbSealedArchives` warned and fell back to
  dynamic linking — which is the configuration on which *every* DuckDB error
  path aborts the process. There is nothing to fall back to, so the build now
  stops and names both the cause and the `apt-get`/`dnf` line that fixes it.
  `LINEN_ALLOW_UNSEALED_DUCKDB=1` is the explicit opt-out, for someone building
  a subset that never touches a DuckDB error path.

  This stayed a warning for a release because CI could not reach it: GitHub's
  Ubuntu runners ship `g++`, so the branch was unreachable there.

### Added

- **CI covers the axes that actually vary.** Three additions, chosen because
  the ~770 pure-Lean modules are platform-independent and elan pins the
  compiler to one commit — so more distros would re-test identical `.olean`
  semantics, while everything that varies lives at the FFI boundary:

  - **`ubuntu-24.04-arm`** in the build matrix. `duckdbArchiveName`'s
    `static-libs-linux-arm64.zip` branch had **never executed** in CI:
    `ubuntu-latest` is x86_64 and `macos-latest` takes the Darwin branch. The
    asset exists and the code was live but untested.
  - **A consumer build**, on all three platforms, of a throwaway package that
    `require`s `linen` and calls a DuckDB `@[extern]` entry point. This is the
    axis standalone CI cannot test — Lake elaborates a dependency's lakefile
    with the *consumer's* root as the working directory, which is how 0.19.1's
    bug broke every Linux consumer while this repository stayed green.
  - **An unsealable host**, in a `debian:bookworm-slim` container with no
    `g++`, asserting the build fails, that the message names the fix, and that
    the documented opt-out works.

  Alpine/musl is deliberately absent: Lean publishes `linux` and
  `linux_aarch64` only, with no musl build, so it is unsupported rather than
  untested.

## [0.20.0] - 2026-09-19

- **Lean 4.34.0.** `lean-toolchain` moves from `v4.33.1`, and the README badge
  with it. Three deprecated simp lemmas were the only breakage — `if_neg` →
  `ite_eq_right` in `Control.Monad.Effect.FileSystem`, `if_true` → `ite_true`
  in `Text.Pandoc.Builder` — and the replacements were drop-in. The full suite
  passes at 4615 jobs with no warnings.

  Measured rather than assumed: `libleanshared.so` in the Linux v4.34.0 release
  exports the **same ten** `_Unwind_*` symbols as every release before it, with
  `_Unwind_GetIPInfo` still absent. So the toolchain upgrade does not remove the
  need for the sealed DuckDB library described in
  [docs/linking.md](docs/linking.md) §4, and
  [leanprover/lean4#15112](https://github.com/leanprover/lean4/issues/15112)
  remains open.

## [0.19.1] - 2026-09-11

- **A Linux consumer could not build `linen` at all.** The `-L` naming
  `libduckdb_sealed.so` was resolved against `IO.currentDir`, which during
  lakefile elaboration is the **workspace** root — the *consumer's* directory
  when `linen` is a dependency. The library itself is written by the
  `duckdbSealedLib` target to `pkg.buildDir / "ffi"`, which is always `linen`'s
  own directory. Standalone the two coincide; as a dependency they diverge, and
  the consumer's link of `liblinenffi.so` fails with

      ld.lld: error: unable to find library -lduckdb_sealed

  seconds after its own log reports `✔ Built linen/duckdbSealedLib`. The `-L`
  is now derived from this lakefile's own path, so both name `linen`'s build
  directory in either context.

  Affects **0.17.0, 0.18.0 and 0.19.0** — every version since the sealed
  library was introduced — on Linux only, and only when `linen` is consumed as
  a dependency. macOS is unaffected: the sealed path is `isLinuxBuild`-only and
  the dynamic path it uses instead reads the workspace-relative cache
  consistently at both ends.

  **This project's own CI cannot catch this class of bug**, which is why three
  releases shipped with it: building standalone is exactly the case where the
  two paths agree. Catching it needs a build of a *consumer* package on Linux.

## [0.19.0] - 2026-09-11

- **A GCP token exchange now posts to the endpoint the key file names.**
  `ServiceAccount.tokenUri` is parsed from the key file and its doc-comment
  said it was "where to send the assertion… a key file names the one it
  expects". It was used as the JWT's `aud` claim and *nowhere else*:
  `exchange` posted to a hardcoded host and ignored its `sa` parameter
  entirely, which is how this surfaced — as an unused-binding warning.

  Not cosmetic. A key file naming a different endpoint produced an assertion
  audienced for one host and posted to another: rejected as an invalid
  audience at best, and at worst a credential signed for a host it was never
  sent to. Both now read from the one field, so they agree by construction.

  The new `splitTokenUri` refuses a non-`https` `token_uri`, since the body
  carries an assertion signed with the account's private key and posting it in
  clear would hand it to anyone on the path — a tampered key file cannot
  redirect a credential to a plaintext endpoint. It also refuses one carrying a
  query string rather than folding it silently into the path.

- **Warning-free build.** `String.mk` → `String.ofList`, and
  `String.Slice.dropRight` → `dropEnd`, both deprecated upstream.

- **Object versioning is implemented**, closing the last finding from the
  `Linen.Cloud` review. `Provider.Feature.objectVersioning` reported `true` for
  all three clouds with nothing able to use it — no way to read, list or delete
  a specific version.

  `ObjectStore` gains `listVersions`, `getVersion` and `deleteVersion`, plus an
  `ObjectVersion` type carrying the version's metadata, its provider
  identifier, whether it is current, and whether it is a **delete marker**. S3
  records a delete in a versioned bucket by adding a marker rather than
  removing data, so a listing interleaves markers with real versions; they are
  surfaced rather than filtered, because a caller reconstructing history needs
  to see that a key was deleted at a point in it.

  The two dialects differ in ways that are invisible from Lean and so are
  pinned by tests on the wire form. S3 selects the operation with a
  **valueless** `versions` parameter and pages by *two* markers — `key-marker`
  and `version-id-marker` — because a single key can have more versions than
  fit in a page, so a position in the listing is a (key, version) pair; both
  are packed into the one opaque `Cursor` this interface carries. GCS uses
  `versions=true` and a single page token, identifies the current generation by
  the *absence* of `timeDeleted`, and has no delete markers at all.

  `deleteVersion` is deliberately not `delete` with an argument: `delete` adds
  a marker and keeps the data, while this destroys the named version
  irrecoverably. For the same reason GCS's `deleteVersion` does **not**
  normalise a 404 to success the way `delete` does — `delete` is idempotent by
  design, but a caller naming a specific generation that is gone holds a stale
  reference and should be told.

  The three operations are fields with failing defaults rather than required
  ones, so `ObjectStore.inMemory` — which keeps no history — declines instead
  of faking a version store.

## [0.18.0] - 2026-09-11

Correctness fixes in `Linen.Cloud`, where the claims made by docs, feature
tables and diagnostics had drifted from what the code did. In each case the
announcement was implemented rather than withdrawn.

- **GCP key-file credentials work.** `Credentials.Gcp.fromKeyFile` implemented
  the full RFC 7523 JWT-bearer flow and **nothing called it**, while
  `Credentials.sourceDescriptions` advertised the key file as the *first* source
  tried for GCP. A service deployed the ordinary way — a key file named by
  `GOOGLE_APPLICATION_CREDENTIALS`, no `gcloud` CLI, no keychain — could not
  authenticate, and the not-found error named a source that was never consulted.

  The key-file source is now a parameter of `Credentials.loadWith`, tried first,
  as the diagnostics always claimed. It must be a parameter for the same reason
  the keychain source already was: minting a token is an HTTP round-trip, so the
  source needs a `Transport` and `Credentials` stays free of one. New module
  **`Cloud.Credentials.Chain`** is the one place that imports both it and
  `Credentials.Keychain`, so it can assemble the whole chain — key file,
  `gcloud`, keychain, environment for GCP; file, keychain, environment for AWS
  and Scaleway. Prefer it: `Keychain.load` still skips the key file, by design,
  for callers who want exactly one source.

  A key file that is *named but unusable* now fails the lookup rather than
  falling through, so a typo in `GOOGLE_APPLICATION_CREDENTIALS` cannot report
  itself as "no credentials found". Absent sources still decline silently.

- **Local backends really are "just a different `Endpoint`".** Four places in
  the docs promised MinIO, LocalStack and ElasticMQ worked that way, while
  `Endpoint` had no port and no scheme and the request builder hardcoded
  `port := 443` and `isSecure := true`. `Endpoint` gains `port : Option Nat` and
  `secure : Bool`, both defaulting to the cloud case, with `effectivePort`,
  `authority`, and an `Endpoint.localhost` constructor.

  The part that makes this more than a field: SigV4 signs the `Host` header, so
  a request to `localhost:9000` signed over a bare `localhost` is rejected as
  `SignatureDoesNotMatch`. Signing and sending now use the same authority, with
  the port included exactly when RFC 9110 says it should be.

- **`performRaw` validated nothing.** `perform` rejected unusable credentials
  and a pre-encoded path under SigV4; `performRaw` did neither, so the
  operations that use it — those where a non-2xx is the expected answer — sent
  unauthenticated or mis-encoded requests where their siblings returned a clear
  error. Both now share `Call.preflight`.

- **`Auth.native .aws` no longer invents a service name.** It signed for
  `execute-api`, which is API Gateway's: correct for calling API Gateway and
  `SignatureDoesNotMatch` for everything else. There is no single native AWS
  scheme — each service signs under its own name — so `.aws` now returns an
  `invalid` naming the two functions that do work, and the new
  **`Auth.nativeFor`** takes the service for callers who know it but hold no
  `Endpoint`. No capability is lost: `Auth.forEndpoint` was already the correct
  path and cannot disagree with the host it signs for.

- **A partial SQS batch failure no longer discards its successes.**
  `batchOutcome`'s own doc-comment said a partial failure answers the successes
  with the failures folded in; the code returned an error and threw the
  `Successful` list away — and that branch had no test. Told only "3 of 10
  failed", a caller had to either drop 7 delivered messages or resend them and
  duplicate. `send` now names the messages that were delivered; `ack` and
  `extendLease` report nothing on success, so for them a partial failure remains
  an error rather than vanishing. "Everything failed" is unchanged.

- **`System.Keychain` credentials round-trip whole.** The credential-store
  source never wrote `access_token` — the one field that *is* GCP's credential
  — so storing a GCP credential and loading it back produced one with no token:
  the chain found *a* credential, stopped looking, and the first request failed
  a check the skipped sources would have satisfied. It also read blank fields as
  values, ignoring the "set but empty means unset" rule `Cloud.Credentials`
  documents and applies to the environment. The parse half is now the pure
  `parseBody`, so the real round trip is checkable by `#guard`; the previous
  test went `render` → `Data.Ini.parse`, which proves the INI is well-formed but
  never asks whether the module reads its own body back.

- **Scaleway secret listings no longer report a partial result as complete.**
  `next` was derived from the page size that was *requested* rather than from the
  reply: a response with no `total_count` and a full page compared `50 >= 50` and
  answered "no more pages", and any entry that failed to parse shrank the total
  the arithmetic was measured against. Completeness is now observed — a short
  page is the last page — which is what `Cloud.Page` exists to guarantee.
  `resolve` also no longer returns a 404 when the name filter was truncated,
  which would have a caller create a secret that already exists.

- **Presigned URLs work.** `Provider.supports` reported `presignedUrl` for AWS
  and Scaleway while `Crypto.SigV4` listed query-string signing under "what is
  *not* here". `SigV4.presign`/`presignedUrl` implement it — the `X-Amz-*`
  parameters signed as part of the query, `UNSIGNED-PAYLOAD` because the body is
  not known when the URL is minted, and `Host` signed so the grant is bound to
  one endpoint (including its port, so it works against a local MinIO). Exposed
  as `ObjectStore.presign` taking a `PresignedOp` — `download` or `upload` —
  rather than a method string, so a URL cannot be minted for a verb the store
  did not mean to delegate. Over-long and zero expiries are refused at minting
  rather than at use, where AWS complains about the signature instead.

- **HTTP/2 frame decoding is bounds-safe by construction.** Byte access in
  `Network.HTTP2.Frame.Decode` went through `bs[i]!` behind hand-written bounds
  checks — correct as written, but safe only while every future edit kept the
  check and the indexing in agreement, in a parser fed by unauthenticated
  sockets where a panic is a remote denial of service. It now reads through a
  total accessor, so an out-of-range read is `none` by construction. The module
  also gained the doc-comments it entirely lacked, and tests for every
  truncation, over-long padding, and malformed SETTINGS length.

- **`urlEncode` percent-encodes UTF-8 bytes.** It encoded *code points*: `é`
  became `%E9` rather than `%C3%A9`, and anything above U+00FF emitted arbitrary
  characters — `€` indexed a 16-entry hex table with 522. It now delegates to
  the already-correct `Network.URI.escapeURIString`, and `Text.Pandoc.URI`
  inherits the fix. A test had asserted the wrong answer, which is why this
  lasted.

- **New: [docs/rfcs.md](docs/rfcs.md).** The specifications `linen` implements,
  mapped to their modules; the foundational ones it rests on but does not
  implement (791, 793/9293, 1122, 8200, 8446); those it deliberately does not
  (MPLS, BGP-4); and several worth reading for their own sake. Citations were
  missing at the source too — `Network.TLS.Types` now names RFC 8446 while
  stating that OpenSSL implements it rather than `linen`, `Network.Socket` names
  RFC 9293 and 1122, and `Data.IP` names RFC 791, 8200 and 4632. Obsoleted
  documents carry both numbers, since RFC 793 is still the number everyone uses.

## [0.17.0] - 2026-09-11

- **The test suite now actually runs in CI, on both platforms.** `Tests` is not
  a default Lake target, so `lean-action`'s `lake build` never reached it: every
  green run before this had been green because nothing was tested. Building it
  explicitly surfaced eleven pre-existing Linux failures across five unrelated
  causes. None were regressions; macOS passed throughout, which is why they went
  unnoticed. The matrix also gains `fail-fast: false`, because a flaky failure on
  one platform had been cancelling the other leg and discarding the result the
  run existed to produce.

- **`System.Keychain` on Linux round-trips arbitrary bytes.** The libsecret
  backend used `secret_password_store_sync`/`secret_password_lookup_sync`, which
  carry a secret as a NUL-terminated string: a secret containing a NUL byte was
  silently truncated at it, and its length recovered with `strlen`.
  `setSecret`/`getSecret` promise the bytes back verbatim, as the macOS and
  Windows branches always delivered. The branch now uses the length-explicit
  `secret_value_new` with `secret_service_{store,lookup}_sync`. If you stored
  binary secrets on Linux with an earlier version, the stored values were
  truncated when written and cannot be recovered by this fix.

- **DuckDB is linked as a sealed shared library on Linux.** Lean's
  `libleanshared.so` exports 10 of the 11 `_Unwind_*` symbols the system
  `libstdc++` imports and precedes it in the global lookup scope, so a
  dynamically linked libduckdb unwound through two incompatible unwinders and
  aborted the process on **every** DuckDB error path — `terminate called after
  throwing an instance of 'duckdb::…Exception'`, with DuckDB's own `catch (...)`
  in the frame being unwound. DuckDB, its C++ runtime and its unwinder are now
  linked into one shared object with those symbols localized, so nothing can
  interpose them. Reported upstream as leanprover/lean4#15112. macOS is
  unchanged and needs none of this: Mach-O's two-level namespace makes its dylib
  immune.

  **New Linux build requirement:** producing that library needs `g++` and a
  static `libstdc++.a`/`libgcc.a` (Ubuntu: `g++`, which pulls in
  `libstdc++-*-dev`). If they are absent the build prints a `[linen] WARNING`
  naming the missing piece and falls back to dynamic linking — which builds and
  works on success paths but aborts on DuckDB errors. `ci/check-sealed-duckdb.sh`
  asserts the sealed library's properties on the linked artifact, since a symbol
  table is not observable from a `#guard`.

  On Linux the pinned DuckDB archive is now `static-libs-linux-<arch>.zip`
  rather than `libduckdb-linux-<arch>.zip`: the `libduckdb_static.a` in the
  latter is a core-only build and not equivalent to the `libduckdb.so` beside it
  in the same zip, missing the whole `core_functions` extension — `sum`, `avg`,
  `abs` and `round` among others.

- **Two tests fixed rather than papered over.** `Network.SendfileTest` closed
  the listening socket before joining the task that accepts on it; a TCP
  `connect` completes out of the listen backlog without anyone calling `accept`,
  so the foreground could reach `close` first and the task then failed with
  `EBADF`. `System.TimeManagerTest` tickled a handle every 15ms against a 50ms
  deadline, so a single overrunning `IO.sleep` expired the handle and the test
  blamed `tickle`; it now uses a 10x per-gap margin and measures the gaps rather
  than assuming them, reporting an inconclusive run instead of failing one.

- **New: [docs/linking.md](docs/linking.md).** Static versus dynamic linking,
  position-independent code, how C++ exceptions interact with dynamic symbol
  resolution, the sealing design and the alternatives weighed against it,
  per-release measurements of Lean's unwinder exports, and an inventory of every
  FFI dependency — which are vendored, which are pinned, which come from the
  host, and which are C versus C++. Worth reading before adding a native
  dependency, especially a C++ one.

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
