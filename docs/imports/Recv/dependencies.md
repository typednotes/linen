# `Recv` module dependencies

Topological order of every module of the `Recv` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Network.Socket.Recv` --> — `recv`/`recvString` are thin wrappers around a
blocking-style socket `recv`, functionally identical to the already-ported
`Network.Socket.Blocking.recv` (see `Examples/Recv.lean`); duplicate, not re-ported.
<!-- 2. *(`Recv` package root — no upstream module; covered by `linen`'s own root)* -->

