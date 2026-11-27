# Linen documentation

Start with the [Quick Start](../README.md#quick-start) to add linen as a Lake
dependency. Import a specific module if you do not want everything in `import Linen`.

- [Module reference](modules.md) — find APIs by subsystem.
- [Cookbook: read-only POST](read-only-post.md) — allow a typed read without exposing raw HTTP.
- [Byte strings and builders](byte-strings.md) — choose slices, chunks, or contiguous output.
- [Scoped resource lifecycles](resource-lifecycles.md) — resource state at compile time and cleanup at scope exit.
- [Indexed monads by example](indexed-monad-examples.md) — work through each scoped interpreter and its state transitions.
- [Precompiled Lean cells](cell-packages.md) — signed native workers and typed calls.
- [Native linking](linking.md) — which imports pull in a native library and why Linux DuckDB is sealed.
- [Upstream imports](imports/index.md) — provenance and dependency order for ported code.
- [Standards and RFCs](rfcs.md) — protocol reference material.
- [GitHub branch rules](github/main-branch-ruleset.json) and [tag rules](github/tag-ruleset.json).

Capability contracts live with their modules; see the
[HTTP capability contract](../Linen/Control/Monad/Effect/HTTP/SPEC.md) and the
[POSIX socket contract](../Linen/Network/Socket/SPEC.md).
