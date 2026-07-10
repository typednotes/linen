# `Network` module dependencies

Topological order of every module of the `Network` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Network.Socket.Types` -->
<!-- 2. `Network.Socket.FFI` -->
<!-- 3. `Network.Socket` -->
<!-- 4. `Network.Socket.Blocking` -->
<!-- 5. `Network.Socket.ByteString` -->
<!-- 6. `Network.Socket.EventDispatcher` -->
<!-- 7. *(`Network` package root — no upstream module; covered by `linen`'s own root)* -->

