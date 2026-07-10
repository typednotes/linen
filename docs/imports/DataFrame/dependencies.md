# `DataFrame` module dependencies

Topological order of every module of the `DataFrame` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `DataFrame.Internal.Types` -->
<!-- 2. `DataFrame.IO.CSV` -->
<!-- 3. `DataFrame.Internal.Column` -->
<!-- 4. `DataFrame.Display` -->
<!-- 5. `DataFrame.Operations.Join` -->
<!-- 6. `DataFrame.Operations.Sort` -->
<!-- 7. `DataFrame.Operations.Statistics` -->
<!-- 8. `DataFrame.Operations.Aggregation` -->
<!-- 9. `DataFrame.Operations.Subset` -->
<!-- 10. `DataFrame.Operations.Transform` -->
<!-- 11. `DataFrame` -->
<!-- 12. *(`DataFrame` package root — no upstream module; covered by `linen`'s own root)* -->

