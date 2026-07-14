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

## Crates (crates.io)

Same convention as above, applied to Rust crates per AGENTS.md's
crates.io-import section.

1. [`keyring`](Keyring/dependencies.md) (done) — cross-platform OS
   credential-store client (macOS Keychain / Linux Secret Service / Windows
   Credential Manager)
   ([source](https://github.com/open-source-cooperative/keyring-rs)),
   1 module.
