/-
  Linen.Data.Tuple.Strict — strict tuple accumulators for folds and scans

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Tuple.Strict`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Tuple/Strict.hs),
  module #2 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Strict data types used as the accumulator of strict left folds and scans.
  Upstream's rationale: strict accumulators let the compiler unbox and optimize
  tight loops. The trailing prime distinguishes these from Lean's lazy tuples.

  ## Substitutions / deviations

  - **Strictness is automatic.** Upstream marks every field `!` (bang
    patterns); Lean is call-by-value so its `structure` fields are already
    strict — the primed types are plain `structure`s here.
  - **`Tuple3Fused'` merged with `Tuple3'`.** Upstream keeps two identical
    three-field types apart solely so a `Fusion.Plugin` `Fuse` annotation can
    be attached to one; the annotation is a GHC-plugin no-op in Lean (see the
    plan's `fusion-plugin-types` drop), so only `Tuple3'` is provided.
-/

namespace Data.Tuple

-- ── Strict tuples ───────────────────────────────────────────────────────────

/-- A strict 2-tuple. -/
structure Tuple' (a b : Type u) where
  fst : a
  snd : b
  deriving Repr, DecidableEq, Inhabited, BEq

/-- A strict 3-tuple. -/
structure Tuple3' (a b c : Type u) where
  fst : a
  snd : b
  thd : c
  deriving Repr, DecidableEq, Inhabited, BEq

/-- A strict 4-tuple. -/
structure Tuple4' (a b c d : Type u) where
  fst : a
  snd : b
  thd : c
  fth : d
  deriving Repr, DecidableEq, Inhabited, BEq

end Data.Tuple
