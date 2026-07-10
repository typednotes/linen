# `io-streams` module dependencies

Topological order of every module of the [`io-streams`](https://hackage.haskell.org/package/io-streams)
Hackage package imported into `linen`, per [AGENTS.md](../../AGENTS.md)'s
Hackage-import convention. A prerequisite of
[`pdf-toolbox-core`](../PdfToolboxCore/dependencies.md), the only consumer of
`io-streams` anywhere in `pdf-toolbox-*`/`linen`.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

`io-streams` exposes ~19 modules (`System.IO.Streams.{Core,Internal,Combinators,
List,ByteString,Text,Builder,Attoparsec,Attoparsec.ByteString,Zlib,Handle,File,
Network,Process,Vector,Concurrent,Debug,Tutorial,...}`). Grepping every actual
call site across `pdf-toolbox-core` and `pdf-toolbox-content` (`Streams.*`
usages in `Pdf.Core.{Stream,Util,XRef,Encryption,Writer,IO.Buffer}` and
`Pdf.Content.Parser`) turns up exactly:

`countInput`, `countOutput`, `takeBytes`, `readExactly`, `parseFromStream`,
`decompress`, `fromList`, `toList`, `read`, `write`, `writeLazyByteString`,
`makeInputStream`, `fromByteString`, plus the raw `InputStream(..)`/`OutputStream(..)`
constructor fields (`_read`/`_unRead`/`_write`) from `System.IO.Streams.Internal`,
used directly by `Pdf.Core.IO.Buffer.toInputStream` to adapt its own
`Buffer` abstraction.

None of `Handle.hs`/`File.hs`/`Network.hs`/`Process.hs`/`Concurrent.hs`/
`Vector.hs`/`Text.hs`/`Builder.hs`/`Debug.hs`/`Tutorial.hs` are called by
anything in `pdf-toolbox-*` (every `InputStream` pdf-toolbox constructs
originates from `Pdf.Core.IO.Buffer`, backed by either a `Handle` opened via
plain `base`/`System.IO` — not `io-streams`'s own `Handle.hs` helpers — or an
in-memory `ByteString`, never a network/process source) — not ported.

Because it has exactly one consumer in `linen` and every real call site is a
buffered, random-access read over a PDF file (never a genuinely unbounded/
incremental network source — PDF parsing is offset/seek-driven by
construction), this is ported as internal plumbing folded directly into the
PDF port rather than a standalone top-level `Linen.Network.*` module, per
AGENTS.md's "place modules the way the Lean stdlib would" rule (a generic
mutable-stream abstraction with one caller doesn't warrant its own namespace):
`Linen/Data/PDF/Stream.lean` (namespace `Data.PDF.Stream`), a `ByteArray`-backed,
cursor-plus-pushback structure that satisfies every call site above without
genuine incremental/lazy IO. `parseFromStream` is implemented by running a
`Std.Internal.Parsec.ByteArray.Parser` directly against the resident buffer
from the current cursor and advancing the cursor to the parser's resulting
position, rather than incrementally feeding attoparsec chunk-by-chunk.
`decompress` delegates to [`zlib`](../Zlib/dependencies.md)'s `Inflate` FFI handle.

`io-streams`'s own `build-depends` beyond `zlib` (`bytestring`, `base`,
`network`, `process`, `text`, `time`, `transformers`, `primitive`,
`vector`) are either already covered by `linen` or belong only to the
unported modules above.

## Topologically sorted modules

<!-- 1. `System.IO.Streams.Internal` — ported as `Linen/Data/PDF/Stream.lean`'s
   core structure (cursor + pushback over a `ByteArray`). -->
<!-- 2. `System.IO.Streams.Core` / `.Combinators` / `.List` / `.ByteString` —
   ported as the rest of `Linen/Data/PDF/Stream.lean`'s API
   (`fromList`/`toList`/`fromByteString`/`read`/`write`/`writeLazyByteString`/
   `makeInputStream`/`countInput`/`countOutput`/`takeBytes`/`readExactly`). -->
<!-- 3. `System.IO.Streams.Attoparsec` / `.Attoparsec.ByteString` — ported as
   `Linen/Data/PDF/Stream.lean`'s `parseFromStream` (buffer-resident
   `Std.Internal.Parsec.ByteArray` run, no incremental feeding). -->
<!-- 4. `System.IO.Streams.Zlib` — ported as `Linen/Data/PDF/Stream.lean`'s
   `decompress`, delegating to [`Crypto.Zlib.Inflate`](../Zlib/dependencies.md). -->
