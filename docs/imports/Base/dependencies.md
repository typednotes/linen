# `Base` module dependencies

Topological order of every module of the `Base` Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or covered by the stdlib) — kept commented out as a completed checklist.

<!-- 1. `Control.Applicative` -->
<!-- 2. `Control.Category` -->
<!-- 3. `Control.Concurrent.MVar` -->
<!-- 4. `Control.Concurrent.Chan` -->
<!-- 5. `Control.Concurrent.QSem` -->
<!-- 6. `Control.Concurrent.QSemN` -->
<!-- 7. `Control.Concurrent.Green` -->
<!-- 8. `Control.Concurrent.Scheduler` -->
<!-- 9. `Control.Concurrent` -->
<!-- 10. `Control.Monad` -->
<!-- 11. `Data.Bifunctor` -->
<!-- 12. `Data.Bits` -->
<!-- 13. `Data.Bool` -->
<!-- 14. `Data.Char` -->
<!-- 15. `Data.Complex` -->
<!-- 16. `Data.Either` -->
<!-- 17. `Control.Arrow` -->
<!-- 18. `Control.Exception` -->
<!-- 19. `Data.Function` -->
<!-- 20. `Data.Functor.Compose` -->
<!-- 21. `Data.Functor.Const` -->
<!-- 22. `Data.Functor.Contravariant` -->
<!-- 23. `Data.Functor.Identity` -->
<!-- 24. `Data.Functor.Product` -->
<!-- 25. `Data.Functor.Sum` -->
<!-- 26. `Data.IORef` -->
<!-- 27. `Data.Ix` -->
<!-- 28. `Data.List.NonEmpty` -->
<!-- 29. `Data.Foldable` -->
<!-- 30. `Data.List` -->
<!-- 31. `Data.Maybe` -->
<!-- 32. `Data.Newtype` -->
<!-- 33. `Data.Ord` -->
<!-- 34. `Data.Proxy` -->
<!-- 35. `Data.Ratio` -->
<!-- 36. `Data.Fixed` -->
<!-- 37. `Data.String` -->
<!-- 38. `Data.Traversable` -->
<!-- 39. `Data.Tuple` -->
<!-- 40. `Data.Unique` -->
<!-- 41. `Data.Void` -->
<!-- 42. `System.Environment` -->
<!-- 43. `System.Exit` -->
<!-- 44. `System.IO` -->
<!-- 45. *(`Base` package root — no upstream module; covered by `linen`'s own root)* -->

