# `cdp` module dependencies

Topological order of every module of the [`cdp`](https://hackage.haskell.org/package/cdp)
Hackage package (a Chrome DevTools Protocol client), source at
[arsalan0c/cdp-hs](https://github.com/arsalan0c/cdp-hs), derived from `import CDP.*`
statements in each source file.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

`cdp-hs`'s `library` component mixes two things under one Cabal target:

- **the runtime library** (`hs-source-dirs: src`, plus `CDP.Definition` from `gen/`,
  which `CDP.Endpoints` reuses to decode a live browser's `/json/protocol` response) —
  this is what a consumer of the package actually imports, and is what gets ported here.
- **a build-time code generator** (`CDP.Gen.Program`, `CDP.Gen.Snippets`,
  `CDP.Gen.Deprecated`, `gen/Main.hs`, the `cdp-gen` executable) that parses the
  [`devtools-protocol`](https://github.com/ChromeDevTools/devtools-protocol) JSON spec
  and *emits* the 39 `CDP.Domains.*` Haskell source files below. It has no Lean
  counterpart — the `Linen.CDP.*` domain modules are ported directly from the
  generated Haskell, so there is nothing to regenerate them from at build time.
  **Excluded from this list and from porting.**

## External (non-`CDP`) dependencies

Per the library's Cabal `build-depends`: `aeson`, `base`, `bytestring`, `containers`,
`data-default`, `directory`, `extra`, `filepath`, `http-conduit`, `monad-loops`, `mtl`,
`network-uri`, `process`, `random`, `text`, `vector`, `websockets`.

Already covered by `linen`: `aeson`, `base`, `bytestring`, `containers`,
`data-default`, `http-conduit`, `mtl`, `text`, `vector`, `websockets` (see
[`../index.md`](../index.md)). `directory`, `extra`, `filepath`, and `process` are
listed in the package's Cabal `build-depends` but are only actually used by the
excluded code generator (`gen/Main.hs`) — not by anything under `src/` or
`CDP.Definition` — so none of the four need importing. `filepath` would in any case
be covered directly by Lean's `System.FilePath` core module (no import needed, per
AGENTS.md's stdlib-substitution rule).

`monad-loops` and `random` are imported by nearly every module in `src/` (leftover
from the code generator's shared per-domain template) but **not one function from
either is ever called** — verified by grepping the whole `src/` tree for every
public name each exports (`whileM`/`untilM`/`iterateUntilM`/… and
`randomRIO`/`getStdGen`/`StdGen`/…): zero non-import hits. Both are dead
dependencies of the upstream library and are skipped entirely (nothing to import,
per AGENTS.md's "check whether it already exists"/only-port-what's-needed spirit —
this also matches how `random`'s functionality is separately already covered by
Lean's own `Init.Data.Random`, so it would have been redundant regardless).

**Now imported** (was the one real prerequisite, per AGENTS.md's recursive rule —
see [`../network-uri/dependencies.md`](../network-uri/dependencies.md)):

- `network-uri` (`Network.URI`, ported to `Linen/Network/URI.lean`) — genuinely used by `CDP.Endpoints`
  (`parseURI`/`uriAuthority`/`uriPort`/`uriRegName`/`uriPath`, to pull host/port/path
  out of the browser's debugger URL). Distinct from the already-ported
  `HttpTypes.Network.HTTP.Types.URI` (query-string/URL-encoding only, not full
  RFC 3986 URI parsing).

## Topologically sorted modules

<!-- 1. `CDP.Definition` -->
<!-- 2. `CDP.Internal.Utils` -->
<!-- 3. `CDP.Domains.CacheStorage` -->
<!-- 4. `CDP.Domains.Cast` -->
<!-- 5. `CDP.Domains.DOMStorage` -->
<!-- 6. `CDP.Domains.Database` -->
<!-- 7. `CDP.Domains.DeviceOrientation` -->
<!-- 8. `CDP.Domains.EventBreakpoints` -->
<!-- 9. `CDP.Domains.HeadlessExperimental` -->
<!-- 10. `CDP.Domains.Input` -->
<!-- 11. `CDP.Domains.Inspector` -->
<!-- 12. `CDP.Domains.Media` -->
<!-- 13. `CDP.Domains.Memory` -->
<!-- 14. `CDP.Domains.Performance` -->
<!-- 15. `CDP.Domains.Runtime` -->
16. `CDP.Domains.Debugger`
17. `CDP.Domains.HeapProfiler`
18. `CDP.Domains.IO`
19. `CDP.Domains.DOMPageNetworkEmulationSecurity`
20. `CDP.Domains.Accessibility`
21. `CDP.Domains.Animation`
22. `CDP.Domains.Audits`
23. `CDP.Domains.BrowserTarget`
24. `CDP.Domains.CSS`
25. `CDP.Domains.DOMDebugger`
26. `CDP.Domains.DOMSnapshot`
27. `CDP.Domains.Fetch`
28. `CDP.Domains.IndexedDB`
29. `CDP.Domains.LayerTree`
30. `CDP.Domains.Log`
31. `CDP.Domains.Overlay`
32. `CDP.Domains.PerformanceTimeline`
33. `CDP.Domains.Profiler`
34. `CDP.Domains.ServiceWorker`
35. `CDP.Domains.BackgroundService`
36. `CDP.Domains.Storage`
37. `CDP.Domains.SystemInfo`
38. `CDP.Domains.Tethering`
39. `CDP.Domains.Tracing`
40. `CDP.Domains.WebAudio`
41. `CDP.Domains.WebAuthn`
42. `CDP.Domains` — aggregator: re-exports all 39 `CDP.Domains.*` modules above
43. `CDP.Endpoints`
44. `CDP.Runtime`
45. `CDP` — package aggregator: re-exports `CDP.Domains` + `CDP.Runtime`
