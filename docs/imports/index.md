# Hackage import index

This file indexes every Hackage package imported into `linen`, per
[AGENTS.md](../../AGENTS.md)'s Hackage-import convention: each package gets its own
`docs/imports/<package>/dependencies.md` topologically-ordered module list. Packages
below are listed in **topological order** (a package appears only after every package
it depends on).

## Packages

1. [`Aeson`](Aeson/dependencies.md) (done) — 5 module(s)
2. [`AnsiTerminal`](AnsiTerminal/dependencies.md) (done) — 2 module(s)
3. [`AutoUpdate`](AutoUpdate/dependencies.md) (done) — 2 module(s)
4. [`Base`](Base/dependencies.md) (done) — 45 module(s)
5. [`Base64`](Base64/dependencies.md) (done) — 2 module(s)
6. [`BsbHttpChunked`](BsbHttpChunked/dependencies.md) (done) — 2 module(s)
7. [`ByteString`](ByteString/dependencies.md) (done) — 9 module(s)
8. [`CaseInsensitive`](CaseInsensitive/dependencies.md) (done) — 2 module(s)
9. [`ConfiguratorPg`](ConfiguratorPg/dependencies.md) (done) — 3 module(s)
10. [`Containers`](Containers/dependencies.md) (done) — 5 module(s)
11. [`Cookie`](Cookie/dependencies.md) (done) — 2 module(s)
12. [`DataDefault`](DataDefault/dependencies.md) (done) — 2 module(s)
13. [`DataFrame`](DataFrame/dependencies.md) (done) — 12 module(s)
14. [`FastLogger`](FastLogger/dependencies.md) (done) — 2 module(s)
15. [`Hasql`](Hasql/dependencies.md) (done) — 9 module(s)
16. [`Http2`](Http2/dependencies.md) (done) — 12 module(s)
17. [`HttpDate`](HttpDate/dependencies.md) (done) — 2 module(s)
18. [`HttpTypes`](HttpTypes/dependencies.md) (done) — 6 module(s)
19. [`IpRoute`](IpRoute/dependencies.md) (done) — 2 module(s)
20. [`Jose`](Jose/dependencies.md) (done) — 6 module(s)
21. [`MimeTypes`](MimeTypes/dependencies.md) (done) — 2 module(s)
22. [`Mtl`](Mtl/dependencies.md) (done) — 5 module(s)
23. [`Network`](Network/dependencies.md) (done) — 7 module(s)
24. [`OptParse`](OptParse/dependencies.md) (done) — 5 module(s)
25. [`PostgREST`](PostgREST/dependencies.md) (done) — 45 module(s)
26. [`QUIC`](QUIC/dependencies.md) (done) — 7 module(s)
27. [`Http3`](Http3/dependencies.md) (done) — 7 module(s)
28. [`Recv`](Recv/dependencies.md) (done) — 2 module(s)
29. [`ResourceT`](ResourceT/dependencies.md) (done) — 2 module(s)
30. [`Conduit`](Conduit/dependencies.md) (done) — 5 module(s)
31. [`STM`](STM/dependencies.md) (done) — 5 module(s)
32. [`Scientific`](Scientific/dependencies.md) (done) — 2 module(s)
33. [`SimpleSendfile`](SimpleSendfile/dependencies.md) (done) — 2 module(s)
34. [`StreamingCommons`](StreamingCommons/dependencies.md) (done) — 2 module(s)
35. [`TLS`](TLS/dependencies.md) (done) — 3 module(s)
36. [`HttpClient`](HttpClient/dependencies.md) (done) — 6 module(s)
37. [`HttpConduit`](HttpConduit/dependencies.md) (done) — 3 module(s)
38. [`Req`](Req/dependencies.md) (done) — 2 module(s)
39. [`Text`](Text/dependencies.md) (done) — 3 module(s)
40. [`Time`](Time/dependencies.md) (done) — 11 module(s): 3 reconciled onto
    `Std.Time` (`Linen.Data.Time.Clock`/`.Calendar`/`.LocalTime`, ported ad
    hoc before this codebase's import process had `Std.Time` in its
    precedence analysis, now rebuilt on it — the reconciliation also fixed
    `getCurrentTime`, which had read a monotonic clock rather than
    wall-clock time; `Linen.System.Time`, the ad hoc FFI wall-clock shim this
    bug had motivated, is deleted, subsumed by
    `Std.Time.DateTime.Timestamp.now`) plus 8 new modules
    (`Linen.Time.Calendar.CalendarDiffDays`/`.Month`/`.Quarter`/`.Julian`/
    `.Easter`, `Linen.Time.CalendarDiffTime`, `Linen.Time.UniversalTime`,
    `Linen.Time.Clock.TAI`). Full Hackage-import pass against
    [`time`](https://hackage.haskell.org/package/time) v1.15 and Lean's own
    `Std.Time` (already covers almost everything else — Gregorian/ISO-week/
    ordinal calendar arithmetic, clocks, durations, IANA-tzdata timezones,
    locale-aware `strftime`-style formatting); see the `dependencies.md`'s
    status note for details.
41. [`TimeManager`](TimeManager/dependencies.md) (done) — 2 module(s)
42. [`UnixCompat`](UnixCompat/dependencies.md) (done) — 2 module(s)
43. [`UnliftIO`](UnliftIO/dependencies.md) (done) — 2 module(s)
44. [`Vault`](Vault/dependencies.md) (done) — 2 module(s)
45. [`Vector`](Vector/dependencies.md) (done) — 2 module(s)
46. [`WAI`](WAI/dependencies.md) (done) — 3 module(s)
47. [`WaiAppStatic`](WaiAppStatic/dependencies.md) (done) — 4 module(s)
48. [`WaiExtra`](WaiExtra/dependencies.md) (done) — 37 module(s)
49. [`WaiHttp2Extra`](WaiHttp2Extra/dependencies.md) (done) — 6 module(s)
50. [`WaiLogger`](WaiLogger/dependencies.md) (done) — 2 module(s)
51. [`Warp`](Warp/dependencies.md) (done) — 18 module(s)
52. [`WarpQUIC`](WarpQUIC/dependencies.md) (done) — 2 module(s)
53. [`WarpTLS`](WarpTLS/dependencies.md) (done) — 3 module(s)
54. [`WebSockets`](WebSockets/dependencies.md) (done) — 6 module(s)
55. [`WaiWebSockets`](WaiWebSockets/dependencies.md) (done) — 2 module(s)
56. [`Word8`](Word8/dependencies.md) (done) — 2 module(s)
57. [`network-uri`](network-uri/dependencies.md) (done) — RFC 3986 URI parsing
    ([source](https://github.com/haskell/network-uri)), a prerequisite of `cdp`.
58. [`cdp`](cdp/dependencies.md) (done) — Chrome DevTools Protocol client
    ([source](https://github.com/arsalan0c/cdp-hs)), 45 module(s).
59. [`zlib`](Zlib/dependencies.md) (done) — raw zlib/RFC 1950 inflate (FFI), a
    prerequisite of `io-streams`.
60. [`io-streams`](IoStreams/dependencies.md) (done) — scoped
    `InputStream`/`OutputStream` plumbing, a prerequisite of
    `pdf-toolbox-core`.
61. [`cryptohash`](Cryptohash/dependencies.md) (done) — MD5 only, a
    prerequisite of `pdf-toolbox-core`'s encryption support.
62. [`cipher-rc4`](CipherRc4/dependencies.md) (done) — RC4 stream cipher, a
    prerequisite of `pdf-toolbox-core`'s encryption support.
63. [`cipher-aes`](CipherAes/dependencies.md) (done) — AES-128-CBC block
    cipher, a prerequisite of `pdf-toolbox-core`'s encryption support.
64. [`crypto-api`](CryptoApi/dependencies.md) (done) — PKCS5 unpadding only,
    folded into the `cipher-aes` port.
65. [`pdf-toolbox-core`](PdfToolboxCore/dependencies.md) (done) — PDF object
    model, parser, xref, and encryption
    ([source](https://github.com/Yuras/pdf-toolbox)), 19 module(s).
66. [`pdf-toolbox-content`](PdfToolboxContent/dependencies.md) (done) — PDF
    content-stream operators, fonts, and text encoding
    ([source](https://github.com/Yuras/pdf-toolbox)), 13 module(s).
67. [`pdf-toolbox-document`](PdfToolboxDocument/dependencies.md) (done) — PDF
    document/page-tree API and text extraction
    ([source](https://github.com/Yuras/pdf-toolbox)), 11 module(s).
68. [`colour`](colour/dependencies.md) (done) — device-independent color
    space math (`Data.Colour.*`), a prerequisite of `hip`, 14 module(s).
69. [`repa`](repa/dependencies.md) (done) — shape-indexed regular
    parallel arrays (`Data.Array.Repa.*`), a prerequisite of `hip`, 21
    module(s), ported as `Linen.Data.Array.Shaped.*`.
70. [`netpbm`](netpbm/dependencies.md) (done) — PNM/PGM/PPM image
    decoder (`Graphics.Netpbm`), a prerequisite of `hip`, 1 module,
    ported as `Linen.Graphics.Netpbm`.
71. [`JuicyPixels`](JuicyPixels/dependencies.md) (done) — PNG/JPEG/GIF/
    TIFF/BMP/TGA/HDR image codec suite (`Codec.Picture.*`), a prerequisite
    of `hip`, 29 module(s), ported as `Linen.Codec.Picture.*`.
72. [`hip`](hip/dependencies.md) (done) — Haskell Image Processing
    library ([source](https://hackage.haskell.org/package/hip))
    (`Graphics.Image.*`), 30 module(s) (21 new-port, 8 covered by the
    already-ported `repa`, plus 2 of the 21 glue-only and reusing
    `Linen.Codec.Picture.*`/`Linen.Graphics.Netpbm`). **Scope note:**
    `Graphics.Image.IO.Histogram` is excluded, and with it the
    `Chart`/`Chart-diagrams` dependency — those pull in the entire
    `diagrams-lib`/`diagrams-svg`/`SVGFonts` 2D vector-graphics EDSL merely
    to plot one histogram, a subsystem unrelated to image processing itself
    and roughly as large as `JuicyPixels` on its own. Decided with the user
    2026-07-11.
73. [`sqlite-simple`](sqlite-simple/dependencies.md) (done) — mid-level
    SQLite client library ([source](https://github.com/nurpax/sqlite-simple)),
    16 module(s) (12 from `sqlite-simple` itself, plus its raw-FFI
    dependency `direct-sqlite` folded directly into the same
    `dependencies.md`, 4 modules — the same "raw C binding folded into the
    one wrapper package that uses it" treatment `Hasql` gives
    `postgresql-libpq`/`LibPQ`). Needs a new native `sqlite3` C-library
    link (via `pkg-config`, following the existing `libpq`/`openssl`
    pattern in `lakefile.lean`) — see the `dependencies.md`'s "Native C
    library" section.
74. [`duckdb-ffi`](duckdb-ffi/dependencies.md) (done) — low-level FFI
    bindings to the DuckDB C API
    ([source](https://github.com/Tritlo/duckdb-haskell)), 18 module(s).
    A prerequisite of `duckdb-simple`; kept as its own entry rather than
    folded in (unlike `direct-sqlite`/`sqlite-simple`) because of its full
    upstream size — see the `dependencies.md`'s size-decision note.
    **Scope note:** scoped down from the full 44-module upstream surface to
    the 18 modules `duckdb-simple` actually imports; the other 26 —
    aggregate/table-function registration, the Arrow interop layer,
    streaming results, and the whole `Deprecated.*` legacy pre-1.0 shim
    tree (plus the top-level re-export facades) — are excluded as
    deprecated/unused C-API surface with no consumer in `duckdb-simple`,
    the same treatment the `hip` entry above gives
    `Graphics.Image.IO.Histogram`. Decided with the user 2026-07-11. Needs
    a new native `duckdb` C-library link; **DuckDB ships no `pkg-config`
    file on either macOS/Homebrew or Ubuntu's default apt repos**, unlike
    every other native dependency in this repo — resolved via a new CI step
    to download a pinned DuckDB release archive plus a bespoke
    non-`pkg-config` discovery path in `lakefile.lean`, see the
    `dependencies.md`'s "Native C library" section. **Correction
    (2026-07-12):** one binding originally filed under the excluded
    `StreamingResult` module, `duckdb_fetch_chunk`, turned out to be
    load-bearing for `duckdb-simple`'s own facade after all (it walks a
    materialized, non-streaming result) — added directly to the kept
    `QueryExecution` module rather than reopening this scope split; see
    `dependencies.md`'s own correction note.
75. [`duckdb-simple`](duckdb-simple/dependencies.md) (done) — mid-level
    DuckDB client library modeled after `sqlite-simple`'s API
    ([source](https://github.com/Tritlo/duckdb-haskell)), 17 module(s).
    Confirmed independent of `sqlite-simple`/`direct-sqlite` (checked its
    `.cabal` directly) — listed after `sqlite-simple` only because that is
    the requested import order, not a real dependency edge. Completes the
    `sqlite-simple` → `duckdb-ffi` → `duckdb-simple` import chain.
76. [`hoauth2`](hoauth2/dependencies.md) (done) — OAuth2 authorization
    client ([source](https://github.com/freizl/hoauth2)), 22 module(s)
    (18 upstream `exposed-modules` + 4 `other-modules`), ported as
    `Linen.Network.OAuth2.*`. **Scope note:** `binary`/`binary-instances`,
    `microlens`, `uri-bytestring`/`uri-bytestring-aeson`, `exceptions`, and
    `memory` are all substituted with existing `linen` ports or Lean stdlib
    per the precedence rule rather than freshly imported; `crypton` is
    scoped down to two new small OpenSSL-backed FFI primitives
    (`Linen.Crypto.SHA256`, `Linen.Crypto.SecureRandom`, reusing the
    already-linked `ffi/jose.c` OpenSSL dependency) rather than importing
    the whole package. See the `dependencies.md`'s "External dependencies"
    section for the full rationale on each.
77. [`profunctors`](profunctors/dependencies.md) (done) — `Profunctor`/
    `Strong`/`Choice`/`Traversing`/… classes (`Data.Profunctor.*`), a
    prerequisite of `lens` (modern `lens`'s optics are defined as
    constrained profunctor transformations), 16 module(s).
78. [`indexed-traversable`](indexed-traversable/dependencies.md) (done) —
    `FunctorWithIndex`/`FoldableWithIndex`/`TraversableWithIndex` classes, a
    prerequisite of `lens` (`Control.Lens.Indexed` and most per-container
    instance modules), 4 module(s) (folds in
    `indexed-traversable-instances`, no separate entry — see that file's own
    note).
79. [`lens`](lens/dependencies.md) (done) — van Laarhoven/profunctor optics
    (`Control.Lens.*`) ([source](https://hackage.haskell.org/package/lens)),
    66 new-port module(s) (the plan's 64 plus `Control.Lens.Zoom`/
    `.Reified`, gap-filled after batch B) against 84 upstream
    `exposed-modules` + 1 `other-modules`; built on `profunctors` (#77) and
    `indexed-traversable` (#78). **Template Haskell note:** `Control.Lens.TH`
    (`makeLenses`/`makePrisms`) has no Lean 4 equivalent (no TH); the
    substitution is hand-written per-field/constructor lens/prism
    definitions, the same treatment `Linen.Database.DuckDB.Simple.Generic`
    already gives GHC-`Generic`-derived code — see the `dependencies.md`'s
    "Template Haskell substitution strategy" section. **Scope note:** GHC/TH
    -specific modules with no Lean analogue (`Data.Data.Lens`,
    `.Dynamic.Lens`, `.Typeable.Lens`, `GHC.Generics.Lens`,
    `Language.Haskell.TH.Lens`, `Control.Lens.TH` and its support modules,
    `Control.Parallel.Strategies.Lens`, `Control.Seq.Lens`) are dropped, and
    four per-container instance modules (`Data.IntSet.Lens`,
    `Data.Sequence.Lens`, `Data.Tree.Lens`, `Data.Text.Lazy.Lens`) are
    deferred pending their own (not yet ported) container — see the
    `dependencies.md`'s "dropped"/"deferred" sections.

### `hip` dependencies covered by the Lean stdlib or an existing port (no separate Hackage import needed)

- `array`, `primitive` — Lean's native `Array`/`ByteArray`/`FloatArray`.
- `base`, `bytestring`, `containers`, `mtl`, `vector`, `zlib` — already
  ported (`Base`, `ByteString`, `Containers`, `Mtl`, `Vector`, `Zlib`).
- `deepseq` — controls GHC's laziness, which Lean (eager by default) has no
  equivalent notion of; genuinely out of scope, not a simplification of
  in-scope behavior.
- `directory`, `filepath`, `process`, `temporary` — `System.FilePath`,
  `IO.FS.*` (incl. `IO.FS.createTempFile`/`createTempDir`), `IO.Process`.
- `random` — `Init.Data.Random` (`RandomGen`, `StdGen`, `randNat`,
  `randBool`) is already a direct port of this same Haskell library.
- `transformers` — Lean's own `ExceptT`/`ReaderT`/`StateT` (already relied
  on by the `Mtl` port).
- `unordered-containers` — `Std.HashMap`/`Std.HashSet`.
- `attoparsec`, `attoparsec-binary`, `binary` — `Std.Internal.Parsec` /
  `Std.Internal.Parsec.ByteArray` cover byte-level parsing for both
  `netpbm` and `JuicyPixels`'s binary decoding.
- `storable-record` — describes C-struct memory layout for GHC FFI;
  `netpbm` only uses it internally, and Lean structures don't need a
  separate memory-layout descriptor for this.
- `vector-th-unbox`, `ghc-prim`, `template-haskell`, `QuickCheck` — GHC
  metaprogramming/internals and a testing library with no Lean analogue;
  `repa`'s use of these is an implementation detail we reimplement directly
  without them.

80. [`hedis`](hedis/dependencies.md) (done) — Redis
    client (`Database.Redis.*`)
    ([source](https://hackage.haskell.org/package/hedis)), 19 module(s)
    (the plan's 18 plus one structural split, `Database.Redis.PubSub.Types` —
    a small dependency-free module holding the pure Pub/Sub value types
    (`Message`/`PubSub`/`Cmd`) and the subscription-change algebra, carved out
    of `Database.Redis.PubSub` to break the `Hooks`↔`PubSub` import cycle that
    upstream breaks with a `{-# SOURCE #-}` boot import; see that module's own
    doc-comment for the full rationale), ported as `Linen.Database.Redis.*`.
    **Headline finding:** unlike
    `lens` (#77–79), no separate Hackage-import package is needed first —
    every `build-depends` entry resolves against the Lean stdlib, an
    already-ported `linen` module, or a narrow (1–7-function) slice
    substituted with directly-inlined code (`scanner`, `bytestring-lexing`,
    `errors`, `exceptions`, `async`, `resource-pool`, `HTTP`) — see the
    `dependencies.md`'s "External dependencies" section for the
    function-by-function resolution of each. RESP2 wire-protocol framing
    (`Database.Redis.Protocol`) is the one genuinely new port, built on
    `Std.Internal.Parsec` over the already-ported `Linen.Network.Socket`/
    `Linen.Network.TLS`, the same "frame parser over an existing socket
    abstraction" shape as `Linen.Network.HTTP2.Frame.*`. **Scope note:**
    upstream `hedis` 0.16.1 itself only implements RESP2, not RESP3 — this
    plan ports exactly that upstream scope, nothing deferred.

81. [`streamly`](streamly/dependencies.md) (done) —
    high-performance stream-fusion / streaming library, scoped to its
    foundational [`streamly-core`](https://hackage.haskell.org/package/streamly-core)
    package (v0.3.1), 36 module(s) ported (the plan projected 39 in-scope
    modules of ~95 upstream library modules; a few of the plan's separately
    counted facades/`.Type` splits were consolidated during porting),
    ported as `Linen.Data.Stream.*` and sibling `Linen.Data.*` families
    (`StreamK`/`Fold`/`Scanl`/`Unfold`/`Parser`/`Producer`/`Refold`, plus
    unboxed `Array`/`MutArray`/`MutByteArray`/`Unbox`). **Headline finding:**
    like `hedis` (#80) and unlike `lens` (#77–79), no separate not-yet-ported
    Hackage prerequisite is needed first — every `build-depends` entry
    resolves against the Lean stdlib, an already-ported `linen` module, a
    narrow inline (`exceptions` → `Linen.Control.Exception`), or a drop
    (`fusion-plugin-types`/`ghc-bignum`/`integer-gmp`/`ghc-prim`/
    `template-haskell`/`heaps`/`monad-control`/`filepath`/`Win32` — GHC
    toolchain, laziness-fusion, and TH-codegen shims with no Lean analogue).
    This is a genuinely new streaming *paradigm* — stream-fusion via the
    `Step`/`skip`/`stop` state machine — distinct from the coroutine-pipeline
    `Conduit` port (#30) already in `linen`; the port reproduces streamly's
    fused *data encoding* faithfully but not the GHC `fusion-plugin` that
    optimizes it (eager Lean has no such rewrite-rule pass). **Scope note:**
    bounded to `streamly-core`, deferring the full `streamly` package's
    concurrent `SVar` scheduler (the same way `hedis`'s doc bounds itself to
    upstream's RESP2 scope); and within `streamly-core`, deferring the
    `FileSystem.*`/`Path`/Posix/Windows, `Unicode.*`, `Serialize`/`Unbox.TH`
    (Template-Haskell codegen), `Time.*`, `Console`/`ForkIO`, and secondary
    combinator/container/`Generic`-array/deprecated subtrees — see the
    `dependencies.md`'s "Scope note" section.

82. [`doclayout`](doclayout/dependencies.md) (done) — a small, self-contained
    Wadler/Leijen-style pretty-printer
    ([source](https://hackage.haskell.org/package/doclayout)), v0.5.0.3, all 4
    library modules ported (`Text.DocLayout` + `HasChars`/`ANSIFont`/
    `Attributed`) as `Linen.Text.DocLayout.*` (the `Text.` domain prefix maps
    to `Linen.Text.`, sitting beside its consumer `Linen.Text.Pandoc`;
    `DocLayout` is a descriptive name, not GHC/Haskell branding, so no
    Lean-ify rename). **Headline finding:** a genuine new port — `linen` had
    no Wadler/Leijen pretty-printer (`Linen.Data.PDF.*` and the ad-hoc
    `ShowS`/builder emitters are not it) — and a **blocking prerequisite of
    `pandoc` (#83)**, imported first as its own entry the way `profunctors`
    (#77)/`indexed-traversable` (#78) preceded `lens` (#79): every pandoc
    writer renders through its `Doc`/`render`/`literal`/`charWidth`. All
    `build-depends` resolved against the Lean stdlib or existing ports
    (`base`/`text`/`containers`/`mtl` → `Base`/`Text` (#39)/`Containers`
    (#10)/`Mtl` (#22)), a narrow inline (`safe` → two one-liners; the
    `emojis` `baseEmojis` width table folded in), or a drop (`GHC.Generics`/
    `Data.Data` generic-deriving — no Lean analogue). The render engine
    (`render`/`offset`/`height` and transitive callers, 17 `unsafe def`) uses
    `unsafe` per the `Data.Conduit`/`StreamK`/`Stream` precedent (structural
    but non-well-founded reassociation); `flatten`/`normalize` are total. The
    multi-thousand-entry `baseEmojis` grapheme-cluster table is out of scope —
    `charWidth` uses a wcwidth-style range approximation instead. See the
    `dependencies.md` for the full resolution.
83. [`pandoc`](pandoc/dependencies.md) (done) — the
    universal document converter
    ([source](https://hackage.haskell.org/package/pandoc)), v3.10, scoped to a
    focused core of 34 in-scope modules (out of `pandoc`'s ~170 library modules
    plus the separate [`pandoc-types`](https://hackage.haskell.org/package/pandoc-types)
    AST package's 6, whose 5 library modules —
    `Definition`/`Builder`/`Walk`/`Generic`/`JSON` — are folded in as the
    foundation tier, the same "raw dependency folded into the one wrapper that
    uses it" treatment `sqlite-simple` gives `direct-sqlite`), planned as
    `Linen.Text.Pandoc.*` (`Pandoc` kept as a proper-noun tool name, not
    GHC/Haskell branding — the same reasoning `hedis` keeps `Redis` and `lens`
    keeps `Lens`). **Headline finding:** unlike `hedis` (#80)/`streamly` (#81)
    and like `lens` (#77–79), there is a **blocking not-yet-ported
    prerequisite** — [`doclayout`](doclayout/dependencies.md) (#82),
    the `Doc` pretty-printing algebra every writer renders through, which must
    be imported first as its own entry (the way `profunctors` preceded `lens`);
    plus `tagsoup` (HTML tokenizer) and a bounded YAML front-matter parser as
    two reader-side prerequisites. `blaze-html`/`blaze-markup` substitute onto
    the existing `Linen.Web.Html`, and `aeson`/`containers`/`text`/`mtl`/
    `network-uri`/`mime-types`/`parsec`/`scientific` all resolve against the
    Lean stdlib or existing ports (`syb`/`ghc-prim`/`template-haskell`/
    `file-embed`/`QuickCheck`/`semigroups` dropped). **Scope note:** bounded to
    the AST + shared reader/writer infrastructure + the two most central
    formats (**Markdown** and **HTML**, both read and write) plus the
    AST-native `Native`/`JSON` round-trip — deferring the long tail of exotic
    format readers/writers, the binary/zip formats (DOCX/ODT/EPUB/Pptx/Xlsx),
    the Lua-scripting/filter system, syntax highlighting (`skylighting`), math
    typesetting (`texmath`), citations (`citeproc`), templating
    (`doctemplates`), and the App/CLI/PDF/`IO`-monad outer shell — the same
    scoping discipline `streamly` (scoped to `streamly-core`) and `hedis`
    (RESP2 only) applied. See the `dependencies.md`'s "Scope note" section.
    **Result:** all 34 modules ported as `Linen.Text.Pandoc.*` plus the
    top-level `Linen.Text.Pandoc` facade (`getReader`/`getWriter`/`convert`
    format dispatch), giving a working Markdown↔AST↔HTML round-trip plus the
    AST-native `Native`/`JSON` formats. Both reader-side prerequisites
    flagged above were resolved as bounded inline folds rather than separate
    entries: a permissive HTML tokenizer (`TagTok`/`tokenize`) inside
    `Readers/HTML.lean` substitutes for `tagsoup`, and a YAML-subset block-
    mapping/sequence/scalar parser over `Std.Internal.Parsec` inside
    `Readers/Markdown.lean` substitutes for the `HsYAML`-backed
    `Readers.Metadata` front-matter machinery — both citing the
    `Emoji.lean`/`MIME.lean` fold-in precedent. `blaze-html` substitutes onto
    `Linen.Web.Html` as planned. `lake build Linen Tests` passes at 4334 jobs.

## Crates (crates.io)

Same convention as above, applied to Rust crates per AGENTS.md's
crates.io-import section.

1. [`keyring`](Keyring/dependencies.md) (done) — cross-platform OS
   credential-store client (macOS Keychain / Linux Secret Service / Windows
   Credential Manager)
   ([source](https://github.com/open-source-cooperative/keyring-rs)),
   1 module.
