/-
  Linen.Data.Unfold — the public facade for the seed→stream generator `Unfold`.

  ## Haskell source
  `Streamly.Data.Unfold` from
  [`streamly-core`](https://hackage.haskell.org/package/streamly-core)
  (module #35 of the `streamly` import, see
  `docs/imports/streamly/dependencies.md`), the public `Streamly.Data.*` facade
  over `Streamly.Internal.Data.Unfold.Type` (#15) and
  `Streamly.Internal.Data.Unfold.Enumeration` (#16). Upstream this module
  carries no logic of its own: it re-exports the internal `Unfold` type together
  with its combinators and the `Enum`-range unfolds under a single public
  namespace.

  In this port the `Unfold` type and every combinator (including the
  enumeration unfolds) already live in the `Data.Unfold` namespace, so
  re-exporting the two internal modules here makes the whole unfold surface
  reachable via the clean top-level `Linen.Data.Unfold` import — matching an
  `import Streamly.Data.Unfold` — without a user needing to know the internal
  `.Type`/`.Enumeration` module split.
-/

-- ── Re-exported modules ─────────────────────────────────────────────────────
import Linen.Data.Unfold.Type
import Linen.Data.Unfold.Enumeration
