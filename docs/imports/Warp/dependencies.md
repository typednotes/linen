# `Warp` module dependencies

Topological order of every module of the `Warp` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Network.Wai.Handler.Warp.Counter` -->
<!-- 2. `Network.Wai.Handler.Warp.Date` -->
<!-- 3. `Network.Wai.Handler.Warp.HashMap` -->
<!-- 4. `Network.Wai.Handler.Warp.Header` -->
<!-- 5. `Network.Wai.Handler.Warp.PackInt` -->
<!-- 6. `Network.Wai.Handler.Warp.ReadInt` -->
<!-- 7. `Network.Wai.Handler.Warp.Request` -->
<!-- 8. `Network.Wai.Handler.Warp.Settings` -->
<!-- 9. `Network.Wai.Handler.Warp.Response` -->
<!-- 10. `Network.Wai.Handler.Warp.Run` -->
<!-- 11. `Network.Wai.Handler.Warp.Types` -->
<!-- 12. `Network.Wai.Handler.Warp.Conduit` -->
<!-- 13. `Network.Wai.Handler.Warp.IO` -->
<!-- 14. `Network.Wai.Handler.Warp.SendFile` -->
<!-- 15. `Network.Wai.Handler.Warp.Internal` -->
<!-- 16. `Network.Wai.Handler.Warp.WithApplication` -->
<!-- 17. `Network.Wai.Handler.Warp` -->
<!-- 18. *(`Warp` package root — no upstream module; covered by `linen`'s own root)* -->

