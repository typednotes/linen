# `PostgREST` module dependencies

Topological order of every module of the `PostgREST` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `PostgREST.ApiRequest.Preferences` -->
<!-- 2. `PostgREST.Auth.Types` -->
<!-- 3. `PostgREST.Auth` -->
<!-- 4. `PostgREST.Cache.Sieve` -->
<!-- 5. `PostgREST.Config.JSPath` -->
<!-- 6. `PostgREST.Config.PgVersion` -->
<!-- 7. `PostgREST.Config.Proxy` -->
<!-- 8. `PostgREST.Cors` -->
<!-- 9. `PostgREST.Debounce` -->
<!-- 10. `PostgREST.Listener` -->
<!-- 11. `PostgREST.Logger` -->
<!-- 12. `PostgREST.MediaType` -->
<!-- 13. `PostgREST.Network` -->
<!-- 14. `PostgREST.RangeQuery` -->
<!-- 15. `PostgREST.Response` -->
<!-- 16. `PostgREST.Response.GucHeader` -->
<!-- 17. `PostgREST.Response.Performance` -->
<!-- 18. `PostgREST.SchemaCache.Identifiers` -->
<!-- 19. `PostgREST.ApiRequest.Types` -->
<!-- 20. `PostgREST.Config` -->
<!-- 21. `PostgREST.Config.Database` -->
<!-- 22. `PostgREST.Error.Types` -->
<!-- 23. `PostgREST.Error` -->
<!-- 24. `PostgREST.MainTx` -->
<!-- 25. `PostgREST.Plan.Types` -->
<!-- 26. `PostgREST.Query.SqlFragment` -->
<!-- 27. `PostgREST.SchemaCache.Relationship` -->
<!-- 28. `PostgREST.Plan.ReadPlan` -->
<!-- 29. `PostgREST.Plan.MutatePlan` -->
<!-- 30. `PostgREST.SchemaCache.Representations` -->
<!-- 31. `PostgREST.SchemaCache.Routine` -->
<!-- 32. `PostgREST.Plan.CallPlan` -->
<!-- 33. `PostgREST.SchemaCache.Table` -->
<!-- 34. `PostgREST.SchemaCache` -->
<!-- 35. `PostgREST.AppState` -->
<!-- 36. `PostgREST.Metrics` -->
<!-- 37. `PostgREST.Admin` -->
<!-- 38. `PostgREST.Observation` -->
<!-- 39. `PostgREST.TimeIt` -->
<!-- 40. `PostgREST.Unix` -->
<!-- 41. `PostgREST.Version` -->
<!-- 42. `PostgREST.App` -->
<!-- 43. `PostgREST.CLI` -->
<!-- 44. `PostgREST.Response.OpenAPI` -->
<!-- 45. *(`PostgREST` package root — no upstream module; covered by `linen`'s own root)* -->

