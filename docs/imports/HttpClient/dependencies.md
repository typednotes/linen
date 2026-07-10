# `HttpClient` module dependencies

Topological order of every module of the `HttpClient` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Network.HTTP.Client.Types` -->
<!-- 2. `Network.HTTP.Client.Request` -->
<!-- 3. `Network.HTTP.Client.Response` -->
<!-- 4. `Network.HTTP.Client.Connection` -->
<!-- 5. `Network.HTTP.Client.Redirect` -->
<!-- 6. *(`HttpClient` package root — no upstream module; covered by `linen`'s own root)* -->

