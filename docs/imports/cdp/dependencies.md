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

**Not yet imported — need their own `docs/imports/<library>/dependencies.md` first,
per AGENTS.md's recursive rule, before the `CDP` modules that use them can be
ported:**

- `monad-loops` (`Control.Monad.Loops`)
- `network-uri` (`Network.URI`) — distinct from the already-ported
  `HttpTypes.Network.HTTP.Types.URI` (query-string/URL-encoding only, not full
  RFC 3986 URI parsing)
- `random` (`System.Random`)

## Topologically sorted modules

1. `CDP.Definition`
2. `CDP.Internal.Utils`
3. `CDP.Domains.CacheStorage`
4. `CDP.Domains.Cast`
5. `CDP.Domains.DOMStorage`
6. `CDP.Domains.Database`
7. `CDP.Domains.DeviceOrientation`
8. `CDP.Domains.EventBreakpoints`
9. `CDP.Domains.HeadlessExperimental`
10. `CDP.Domains.Input`
11. `CDP.Domains.Inspector`
12. `CDP.Domains.Media`
13. `CDP.Domains.Memory`
14. `CDP.Domains.Performance`
15. `CDP.Domains.Runtime`
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
