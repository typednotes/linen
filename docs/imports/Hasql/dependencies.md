# `Hasql` module dependencies

Topological order of every module of the `Hasql` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Database.PostgreSQL.LibPQ.Types` -->
<!-- 2. `Database.PostgreSQL.LibPQ` -->
<!-- 3. `Hasql.Connection` -->
<!-- 4. `Hasql.Encoders` -->
<!-- 5. `Hasql.Session` -->
<!-- 6. `Hasql.Decoders` -->
<!-- 7. `Hasql.Pool` -->
<!-- 8. `Hasql.Statement` -->
<!-- 9. *(`Hasql` package root — no upstream module; covered by `linen`'s own root)* -->

