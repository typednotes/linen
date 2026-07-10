# `STM` module dependencies

Topological order of every module of the `STM` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Control.Monad.STM` -->
<!-- 2. `Control.Concurrent.STM.TVar` -->
<!-- 3. `Control.Concurrent.STM.TMVar` -->
<!-- 4. `Control.Concurrent.STM.TQueue` -->
<!-- 5. *(`STM` package root — no upstream module; covered by `linen`'s own root)* -->

