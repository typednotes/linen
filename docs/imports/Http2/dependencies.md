# `Http2` module dependencies

Topological order of every module of the `Http2` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Network.HTTP2.Frame.Types` -->
<!-- 2. `Network.HTTP2.Frame.Decode` -->
<!-- 3. `Network.HTTP2.Frame.Encode` -->
<!-- 4. `Network.HTTP2.HPACK.Huffman` -->
<!-- 5. `Network.HTTP2.HPACK.Table` -->
<!-- 6. `Network.HTTP2.HPACK.Decode` -->
<!-- 7. `Network.HTTP2.HPACK.Encode` -->
<!-- 8. `Network.HTTP2.Types` -->
<!-- 9. `Network.HTTP2.Stream` -->
<!-- 10. `Network.HTTP2.FlowControl` -->
<!-- 11. `Network.HTTP2.Server` -->
<!-- 12. *(`Http2` package root — no upstream module; covered by `linen`'s own root)* -->

