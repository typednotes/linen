# `Http3` module dependencies

Topological order of every module of the `Http3` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Network.HTTP3.Error` -->
<!-- 2. `Network.HTTP3.Frame` -->
<!-- 3. `Network.HTTP3.QPACK.Table` -->
<!-- 4. `Network.HTTP3.QPACK.Decode` -->
<!-- 5. `Network.HTTP3.QPACK.Encode` -->
<!-- 6. `Network.HTTP3.Server` -->
<!-- 7. *(`Http3` package root — no upstream module; covered by `linen`'s own root)* -->

