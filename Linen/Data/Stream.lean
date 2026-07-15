/-
  Linen.Data.Stream — the public facade for the fused direct-style stream
  `Stream` (`StreamD`).

  ## Haskell source
  `Streamly.Data.Stream` from
  [`streamly-core`](https://hackage.haskell.org/package/streamly-core)
  (module #32 of the `streamly` import, see
  `docs/imports/streamly/dependencies.md`), the public `Streamly.Data.*` facade
  over the internal fused-stream tree: `Stream.Type` (#19) and the four
  operation modules `Stream.Generate` (#20), `Stream.Eliminate` (#21),
  `Stream.Transform` (#22), `Stream.Lift` (#23) and `Stream.Nesting` (#24).
  Upstream this module carries no logic of its own: it assembles the fused
  `Stream` type together with its generators, eliminators, transforms, monad
  lifters and nesting combinators under a single public namespace.

  This port does the same, using Lean's `export` command inside
  `namespace Data.Stream` so that every combinator defined on the `Stream` type
  (which live in the nested `Data.Stream.Stream` namespace across the operation
  modules) is also reachable directly as `Data.Stream.<name>` — matching an
  `import Streamly.Data.Stream` used qualified as `Stream`.
-/

-- ── Re-exported modules ─────────────────────────────────────────────────────
import Linen.Data.Stream.Type
import Linen.Data.Stream.Generate
import Linen.Data.Stream.Eliminate
import Linen.Data.Stream.Transform
import Linen.Data.Stream.Lift
import Linen.Data.Stream.Nesting

namespace Data.Stream

-- The `Stream` type itself already lives in `Data.Stream`; lift its
-- combinators out of the nested `Data.Stream.Stream` namespace.
export Data.Stream.Stream (
  all any append catEithers catLefts catMaybes catRights concatFor concatMap
  cons consM cross crossApply crossApplyFst crossApplySnd crossWith drain drop
  dropWhile dropWhileM elem elemIndices enumerateFromStepIntegral
  enumerateFromToIntegral eqBy evalStateT filter filterM find findIndices findM
  fold foldToScanl foldl' foldlM' foldlMx' foldlT foldlx' foldr foldr1 foldrM
  fromEffect fromFoldable fromIndices fromIndicesM fromList fromListM fromPure
  fromStreamK generalizeInner generate generateM head index indexed indexedR
  interleave interleaveMin intersperse intersperseM iterateM iterateValue last
  liftInner liftInnerWith lookup map mapM mapMaybe mapMaybeM maximum maximumBy
  mergeBy mergeByM minimum minimumBy morphInner nil nilM notElem null postscan
  postscanl repeatM repeatValue replicate replicateM reverse rollingMap
  rollingMapM runInnerWith runInnerWithState runReaderT runStateT scan scanl
  scanl' scanlM sequence take takeWhile takeWhileM the toList toListRev
  toStreamK unfold unfoldCross unfoldEach unfoldr unfoldrM uniq uniqBy
  usingReaderT usingStateT withReaderT zipWith zipWithM)

end Data.Stream
