/-
  Linen.Data.StreamK — the public facade for the CPS-encoded stream `StreamK`.

  ## Haskell source
  `Streamly.Data.StreamK` from
  [`streamly-core`](https://hackage.haskell.org/package/streamly-core)
  (module #31 of the `streamly` import, see
  `docs/imports/streamly/dependencies.md`), the public `Streamly.Data.*` facade
  over `Streamly.Internal.Data.StreamK.Type` (#12). Upstream this module carries
  no logic of its own: it re-exports the internal `StreamK` type and its
  construction/elimination combinators under a single public namespace.

  This port does the same, using Lean's `export` command inside
  `namespace Data.StreamK` so that the combinators defined on the `StreamK`
  type (which live in the nested `Data.StreamK.StreamK` namespace) are also
  reachable directly as `Data.StreamK.<name>` — matching an
  `import Streamly.Data.StreamK` used qualified as `StreamK`.
-/

-- ── Re-exported module ──────────────────────────────────────────────────────
import Linen.Data.StreamK.Type

namespace Data.StreamK

-- The `StreamK` type itself already lives in `Data.StreamK`; lift its
-- combinators out of the nested `Data.StreamK.StreamK` namespace.
export Data.StreamK.StreamK (
  nil cons fromPure fromEffect consM concatEffect foldStreamShared foldStream
  uncons foldl' foldlM' foldrM foldr drain null toList head tail fromList
  unfoldr unfoldrM map mapM append interleave reverse bindWith concatMapWith
  concatMap crossApply crossWith cross crossApplySnd crossApplyFst)

end Data.StreamK
