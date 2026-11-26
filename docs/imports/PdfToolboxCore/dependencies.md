# `pdf-toolbox-core` module dependencies

Topological order of every module of the
[`pdf-toolbox-core`](https://hackage.haskell.org/package/pdf-toolbox-core)
Hackage package, source at [Yuras/pdf-toolbox](https://github.com/Yuras/pdf-toolbox)
(`core/` subdirectory), imported into `linen` per
[AGENTS.md](../../../AGENTS.md)'s Hackage-import convention, as a prerequisite of
[`pdf-toolbox-content`](../PdfToolboxContent/dependencies.md) and
[`pdf-toolbox-document`](../PdfToolboxDocument/dependencies.md).

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

`pdf-toolbox-core`'s Cabal file gates `Pdf.Core.Stream.Filter.FlateDecode`
behind a `zlib` build flag (default enabled), with two source-tree variants:
`core/zlib/...` (real `FlateDecode` via zlib inflate) and `core/no-zlib/...`
(`flateDecode = Nothing`, a no-op). The `zlib` variant is ported — PDFs
overwhelmingly use `FlateDecode` compression, and per the user's explicit
choice this import gets real, working crypto/compression rather than stubs.

The package's `Prelude`-shim `other-modules` entry (`core/compat/Prelude.hs`,
a CPP compatibility re-export for old GHCs) has no content to port.

Two upstream debug/defensive artifacts are intentionally *not* faithfully
reproduced (not simplifications of behavior, just dropping incidental
side-effects/dead code that add nothing): `Pdf.Core.XRef.lookupTableEntry`'s
stray `print (index, gen, off, gen', free)` immediately before throwing on a
generation mismatch (upstream debug residue, not part of the documented
contract), and the handful of `error "impossible"` dead-branch guards in
`Pdf.Core.Util.readCompressedObject`/`Pdf.Core.XRef.lookupStreamEntry`/
`Pdf.Core.Writer` (each reachable per GHC's checker but not per the
surrounding invariant) — these get real termination/totality treatment in
Lean (a proof or a type that makes the case unrepresentable) rather than a
literal `panic`/`sorry`, per AGENTS.md's "don't dodge the proof" rule.

`Pdf.Core.Object`'s recursive type (`Object` containing `Dict = HashMap Name
Object` and `Array = Vector Object`) is ported faithfully — no flattening or
un-decoding of the recursive structure, per AGENTS.md's explicit warning
against that exact shortcut.

## External dependencies

Per the library's Cabal `build-depends`, beyond what `linen` already covers
(`base`, `bytestring`, `vector`, `hashable`→`Std.HashMap`/`Std.Hashable`,
`scientific`→[`Scientific`](../Scientific/dependencies.md),
`attoparsec`→Lean's own `Std.Internal.Parsec`,
`base16-bytestring`→existing hex helpers in `Linen/Data/ByteString/Builder.lean`,
`containers`→[`Containers`](../Containers/dependencies.md)):

- [`io-streams`](../IoStreams/dependencies.md) (transitively [`zlib`](../Zlib/dependencies.md))
- [`cryptohash`](../Cryptohash/dependencies.md) (MD5 only)
- [`cipher-rc4`](../CipherRc4/dependencies.md)
- [`cipher-aes`](../CipherAes/dependencies.md)
- [`crypto-api`](../CryptoApi/dependencies.md) (PKCS5 unpadding only, folded into the AES module)

`Text.Printf`'s `printf "%f"` (used once, in `Pdf.Core.Object.Builder`'s
number rendering) needs a fixed-point (non-exponent) `Double`/`Float`
formatter — a small helper alongside the port, not a new package.

## Topologically sorted modules

<!-- 1. `Pdf.Core.Name` — ported as `Linen/Data/PDF/Core/Name.lean`. -->
<!-- 2. `Pdf.Core.Exception` — ported as `Linen/Data/PDF/Core/Exception.lean`. -->
<!-- 3. `Pdf.Core.Parsers.Util` — ported as `Linen/Data/PDF/Core/Parsers/Util.lean`. -->
<!-- 4. `Pdf.Core.IO.Buffer` — ported as `Linen/Data/PDF/Core/IO/Buffer.lean`. -->
<!-- 5. `Pdf.Core.Object` — ported as `Linen/Data/PDF/Core/Object.lean`. -->
<!-- 6. `Pdf.Core.Object.Util` — ported as `Linen/Data/PDF/Core/Object/Util.lean`. -->
<!-- 7. `Pdf.Core.Object.Builder` — ported as `Linen/Data/PDF/Core/Object/Builder.lean`. -->
<!-- 8. `Pdf.Core.Parsers.Object` — ported as `Linen/Data/PDF/Core/Parsers/Object.lean`. -->
<!-- 9. `Pdf.Core.Parsers.XRef` — ported as `Linen/Data/PDF/Core/Parsers/XRef.lean`. -->
<!-- 10. `Pdf.Core.Util` — ported as `Linen/Data/PDF/Core/Util.lean`. -->
<!-- 11. `Pdf.Core.Stream.Filter.Type` — ported as `Linen/Data/PDF/Core/Stream/Filter/Type.lean`. -->
<!-- 12. `Pdf.Core.Stream.Filter.FlateDecode` (zlib variant) — ported as
   `Linen/Data/PDF/Core/Stream/Filter/FlateDecode.lean`. -->
<!-- 13. `Pdf.Core.Stream` — ported as `Linen/Data/PDF/Core/Stream.lean`. -->
<!-- 14. `Pdf.Core.XRef` — ported as `Linen/Data/PDF/Core/XRef.lean`. -->
<!-- 15. `Pdf.Core.Encryption` — ported as `Linen/Data/PDF/Core/Encryption.lean`. -->
<!-- 16. `Pdf.Core.File` — ported as `Linen/Data/PDF/Core/File.lean`. -->
<!-- 17. `Pdf.Core` — package aggregator (re-exports `File`, `Object`, `Encryption`) —
   ported as `Linen/Data/PDF/Core.lean`. -->
<!-- 18. `Pdf.Core.Types` — ported as `Linen/Data/PDF/Core/Types.lean`. -->
<!-- 19. `Pdf.Core.Writer` — ported as `Linen/Data/PDF/Core/Writer.lean`. -->
