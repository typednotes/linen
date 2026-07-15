/-
  Linen.Data.Stream.Step — the stream-fusion `Step` state machine

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Stream.Step`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Stream/Step.hs),
  module #7 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A stream is a succession of `Step`s produced by a stepper `s → m (Step s a)`.
  `Yield` produces one value and the next state, `Skip` moves to the next state
  producing nothing, and `Stop` ends the stream. This is the encoding streamly
  hands to GHC's fusion plugin.

  ## Substitutions / deviations

  - **`Fuse` annotation dropped.** Upstream's `{-# ANN type Step Fuse #-}` is a
    GHC-plugin marker with no Lean analogue (see the plan's
    `fusion-plugin-types` drop) — the *data encoding* is reproduced faithfully,
    the plugin that optimizes it is out of scope.
  - **`fromPure`/`skip`/`stop` helpers** are commented out upstream; not
    ported.
-/

namespace Data.Stream

-- ── The Step state machine ──────────────────────────────────────────────────

/-- One step of a fused stream: `Yield x s'` emits `x` and continues at state
    `s'`; `Skip s'` continues at `s'` emitting nothing; `Stop` ends. -/
inductive Step (s a : Type u) where
  | Yield : a → s → Step s a
  | Skip : s → Step s a
  | Stop : Step s a
  deriving Repr, Inhabited

namespace Step

/-- Map over the yielded value, leaving the state machine's shape unchanged. -/
@[inline] def map (f : a → b) : Step s a → Step s b
  | .Yield x s => .Yield (f x) s
  | .Skip s => .Skip s
  | .Stop => .Stop

/-- `Functor` over the yielded value (the second parameter). -/
instance : Functor (Step s) where
  map := Step.map

end Step
end Data.Stream
