# `ByteString` module dependencies

Topological order of every module of the `ByteString` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Data.ByteString.Internal` -->
<!-- 2. `Data.ByteString` -->
<!-- 3. `Data.ByteString.Char8` -->
<!-- 4. `Data.ByteString.Lazy.Internal` -->
<!-- 5. `Data.ByteString.Lazy` -->
<!-- 6. `Data.ByteString.Lazy.Char8` -->
<!-- 7. `Data.ByteString.Short` -->
<!-- 8. `Data.ByteString.Builder` -->
<!-- 9. *(`ByteString` package root — no upstream module; covered by `linen`'s own root)* -->

