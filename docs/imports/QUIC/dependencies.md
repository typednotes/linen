# `QUIC` module dependencies

Topological order of every module of the `QUIC` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Network.QUIC.Types` -->
<!-- 2. `Network.QUIC.Config` -->
<!-- 3. `Network.QUIC.Connection` -->
<!-- 4. `Network.QUIC.Client` -->
<!-- 5. `Network.QUIC.Server` -->
<!-- 6. `Network.QUIC.Stream` -->
<!-- 7. *(`QUIC` package root — no upstream module; covered by `linen`'s own root)* -->

