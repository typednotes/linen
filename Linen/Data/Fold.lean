/-
  Linen.Data.Fold — the public facade for the terminating left-fold `Fold`.

  ## Haskell source
  `Streamly.Data.Fold` from
  [`streamly-core`](https://hackage.haskell.org/package/streamly-core)
  (module #33 of the `streamly` import, see
  `docs/imports/streamly/dependencies.md`), the public `Streamly.Data.*` facade
  over `Streamly.Internal.Data.Fold.Type` (#14). Upstream this module carries no
  logic of its own: it re-exports the internal `Fold` type and its combinators
  under a single public namespace.

  In this port the `Fold` type and every combinator already live in the
  `Data.Fold` namespace, so re-exporting `Linen.Data.Fold.Type` here makes the
  whole fold surface reachable via the clean top-level `Linen.Data.Fold` import
  — matching an `import Streamly.Data.Fold` — without a user needing to know the
  internal `.Type` module split.
-/

-- ── Re-exported module ──────────────────────────────────────────────────────
import Linen.Data.Fold.Type
