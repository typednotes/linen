# `WebSockets` module dependencies

Topological order of every module of the `WebSockets` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Network.WebSockets.Types` -->
<!-- 2. `Network.WebSockets.Frame` -->
<!-- 3. `Network.WebSockets.Connection` -->
<!-- 4. `Network.WebSockets.Handshake` -->
<!-- 5. *(`WebSockets` package root — no upstream module; covered by `linen`'s own root)* -->
<!-- 6. `Network.WebSockets.Client` (client-side `runClient`; added for `CDP.Runtime`, see `../cdp/dependencies.md`) -->

