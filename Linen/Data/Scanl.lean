/-
  Linen.Data.Scanl — the public facade for the stateful left-scan `Scanl`.

  ## Haskell source
  `Streamly.Data.Scanl` from
  [`streamly-core`](https://hackage.haskell.org/package/streamly-core)
  (module #34 of the `streamly` import, see
  `docs/imports/streamly/dependencies.md`), the public `Streamly.Data.*` facade
  over `Streamly.Internal.Data.Scanl.Type` (#13). Upstream this module carries
  no logic of its own: it re-exports the internal `Scanl` type and its
  combinators under a single public namespace.

  In this port the `Scanl` type and every combinator already live in the
  `Data.Scanl` namespace, so re-exporting `Linen.Data.Scanl.Type` here makes the
  whole scan surface reachable via the clean top-level `Linen.Data.Scanl` import
  — matching an `import Streamly.Data.Scanl` — without a user needing to know the
  internal `.Type` module split.
-/

-- ── Re-exported module ──────────────────────────────────────────────────────
import Linen.Data.Scanl.Type
