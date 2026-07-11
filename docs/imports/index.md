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
40. [`Time`](Time/dependencies.md) (done) — 2 module(s)
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
71. [`JuicyPixels`](JuicyPixels/dependencies.md) (in progress) — PNG/JPEG/GIF/
    TIFF/BMP/TGA/HDR image codec suite (`Codec.Picture.*`), a prerequisite
    of `hip`, 29 module(s), ported as `Linen.Codec.Picture.*`.
72. [`hip`](hip/dependencies.md) (planned) — Haskell Image Processing
    library ([source](https://hackage.haskell.org/package/hip))
    (`Graphics.Image.*`). **Scope note:** `Graphics.Image.IO.Histogram` is
    excluded, and with it the `Chart`/`Chart-diagrams` dependency — those
    pull in the entire `diagrams-lib`/`diagrams-svg`/`SVGFonts` 2D
    vector-graphics EDSL merely to plot one histogram, a subsystem
    unrelated to image processing itself and roughly as large as
    `JuicyPixels` on its own. Decided with the user 2026-07-11.

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
