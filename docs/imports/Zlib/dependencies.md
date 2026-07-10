# `zlib` module dependencies

Topological order of every module of the [`zlib`](https://hackage.haskell.org/package/zlib)
Hackage package imported into `linen`, per [AGENTS.md](../../AGENTS.md)'s
Hackage-import convention. A prerequisite of [`io-streams`](../IoStreams/dependencies.md)
and, transitively, [`pdf-toolbox-core`](../PdfToolboxCore/dependencies.md) (PDF
stream data is compressed with `FlateDecode`, the zlib/RFC 1950 format).

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

Upstream `zlib` exposes `Codec.Compression.Zlib`, `Codec.Compression.Zlib.Internal`,
`Codec.Compression.GZip`, `Codec.Compression.Zlib.Raw`, and the low-level FFI
binding `Codec.Zlib` used by `io-streams`'s own `System.IO.Streams.Zlib`. Only
the **inflate (decompress) direction of the raw zlib/RFC 1950 format** is
needed here:

- `pdf-toolbox-core`'s `Pdf.Core.Stream.Filter.FlateDecode` calls
  `io-streams`'s `Streams.decompress`, which is built on `zlib`'s
  `Codec.Zlib.initInflate`/`feedInflate`/`flushInflate`/`finishInflate` with
  `WindowBits 15` (raw zlib format, *not* gzip — gzip uses `WindowBits 31`).
- No call site anywhere in `pdf-toolbox-core`/`-content`/`-document` ever
  compresses/deflates or reads a gzip stream — verified by grepping the whole
  upstream source tree for `compress`/`deflate`/`GZip`/`gunzip`: zero hits.

So this port is one FFI-backed function, `decompress`, driving a persistent
`z_stream` (via a C shim linked against the system `libz`, mirroring the
`libpq`/OpenSSL pkg-config pattern already used for
[`Jose`](../Jose/dependencies.md)/`Hasql`), not a full port of `Codec.Compression.*`'s
pure-Haskell high-level wrapper API (which upstream itself only offers as a
convenience layer over the same FFI calls).

`zlib`'s own `build-depends` (`bytestring`, `base`) are already covered by
`linen` (see [`../index.md`](../index.md)).

## Topologically sorted modules

<!-- 1. `Codec.Zlib` — ported as `Linen/Crypto/Zlib/FFI.lean` (namespace
   `Crypto.Zlib`): an opaque `Inflate` handle (`z_stream*`, mirroring
   `Linen/Network/TLS/Context.lean`'s OpenSSL-handle pattern) with
   `initInflate`, `feed`, `finish`, wired to a new `ffi/zlib.c` shim and a
   `zlib` `pkgConfig` entry in the root `lakefile.lean`. -->

