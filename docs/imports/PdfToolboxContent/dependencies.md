# `pdf-toolbox-content` module dependencies

Topological order of every module of the
[`pdf-toolbox-content`](https://hackage.haskell.org/package/pdf-toolbox-content)
Hackage package, source at [Yuras/pdf-toolbox](https://github.com/Yuras/pdf-toolbox)
(`content/` subdirectory), imported into `linen` per
[AGENTS.md](../../AGENTS.md)'s Hackage-import convention, as a prerequisite of
[`pdf-toolbox-document`](../PdfToolboxDocument/dependencies.md). Depends on
[`pdf-toolbox-core`](../PdfToolboxCore/dependencies.md).

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

All 13 exposed modules are ported — this package is small and every module is
genuinely used by `pdf-toolbox-document`'s font/text-extraction layer.

`Pdf.Content.GlyphList` (~4280 entries, Adobe Glyph List) and
`Pdf.Content.TexGlyphList` (~284 entries) are large literal lookup tables with
no logic — ported as data, not transcribed by hand into this document.

`Pdf.Content.UnicodeCMap.fromHex`'s upstream use of a generic `MonadFail`
(flagged `-- XXX: wtf?!` in its own source comment — upstream itself
considers it code smell) is committed to a concrete `Except String` in the
port rather than reproduced as a polymorphic-monad signature; this is a
legitimate simplification of an acknowledged-smelly generic-typeclass detail,
not a weakening of behavior (the sole caller already instantiates it at
`Parser`/`Either String` anyway).

`Pdf.Content.Encoding.PdfDoc`'s table has an apparent upstream typo (byte
codes `22` and `23` both map to `"\x0017"`) — ported byte-for-byte as-is
(faithful port of upstream data), not silently corrected.

## External dependencies

Beyond what `linen`/`pdf-toolbox-core` already cover (`base`, `bytestring`,
`text`→[`Text`](../Text/dependencies.md), `containers`→[`Containers`](../Containers/dependencies.md),
`attoparsec`→`Std.Internal.Parsec`, `base16-bytestring`→existing hex helpers,
`io-streams`→[`IoStreams`](../IoStreams/dependencies.md)): none — no further
new Hackage prerequisites.

## Topologically sorted modules

<!-- 1. `Pdf.Content.Transform` -->
<!-- 2. `Pdf.Content.Ops` -->
<!-- 3. `Pdf.Content.FontDescriptor` -->
<!-- 4. `Pdf.Content.GlyphList` -->
<!-- 5. `Pdf.Content.TexGlyphList` -->
<!-- 6. `Pdf.Content.Encoding.WinAnsi` -->
<!-- 7. `Pdf.Content.Encoding.MacRoman` -->
<!-- 8. `Pdf.Content.Encoding.PdfDoc` -->
<!-- 9. `Pdf.Content.UnicodeCMap` -->
<!-- 10. `Pdf.Content.Parser` -->
<!-- 11. `Pdf.Content.Processor` -->
<!-- 12. `Pdf.Content.FontInfo` -->
<!-- 13. `Pdf.Content` — package aggregator -->
