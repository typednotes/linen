/-
  Linen.Data.Unfold.Enumeration — `Enum`-range unfolds

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Unfold.Enumeration`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Unfold/Enumeration.hs),
  module #16 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  `Unfold`-based enumerators (`enumerateFrom…`), the generator counterparts of
  the `Enum`/`Enumerable` range operations, built on `Data.Unfold` (#15).

  ## Substitutions / deviations

  - **Haskell numeric type classes → Lean classes.** Upstream is parameterized
    over `Num`/`Integral`/`Bounded`/`Fractional`. Lean has no single such
    hierarchy, so each enumerator is generalized over exactly the Lean classes
    it uses (`Add`/`Sub`/`Mul`/`OfNat`/`LE`+`DecidableLE`). This covers `Int`,
    `Nat`, `Float`, etc. as the concrete instances the Haskell classes had.
  - **`Enumerable` class dropped.** It just packages the methods below over
    `Enum`; the concrete `enumerateFrom…` functions (the useful, reusable part,
    per upstream's own module note) are ported directly.
  - **Bounded / small-`Int` / `Fractional`-specific variants deferred.** The
    `enumerateFrom…{IntegralBounded,SmallBounded,Small,Fractional}` families are
    overflow-aware specializations (needing `maxBound`/`minBound` or floating
    step counting); the unbounded `Num`/`Integral` core below subsumes their
    behavior for the in-scope use and the rest is out of this batch.
  - **The `…Step`/`…From`/`…FromThen` (open) enumerators are infinite streams.**
    As `Unfold` *values* they are fine (just `step`/`inject` closures); only
    running one to completion diverges (Lean is eager). Use a bounded
    (`…To`) enumerator or take a finite prefix.
-/

import Linen.Data.Unfold.Type

namespace Data.Unfold

open Data.Stream (Step)

universe u v
variable {m : Type u → Type v} {a : Type u}

-- ── Enumeration of Num types ──────────────────────────────────────────────────

/-- Enumerate from `from` incrementing by `stride` each step: yields
    `from + i*stride`. Numerically stable for floating point. -/
@[inline] def enumerateFromStepNum [Add a] [Mul a] [OfNat a 0] [OfNat a 1] [Monad m] :
    Unfold m (a × a) a where
  s := a × a × a
  inject := fun (from_, stride) => pure (from_, stride, 0)
  step := fun (from_, stride, i) => pure (.Yield (from_ + i * stride) (from_, stride, i + 1))

/-- Enumerate from `from` with stride `next - from`. -/
@[inline] def enumerateFromThenNum [Add a] [Sub a] [Mul a] [OfNat a 0] [OfNat a 1] [Monad m] :
    Unfold m (a × a) a :=
  lmap (fun (from_, next) => (from_, next - from_)) enumerateFromStepNum

/-- Enumerate from `from` with stride `1`. -/
@[inline] def enumerateFromNum [Add a] [Mul a] [OfNat a 0] [OfNat a 1] [Monad m] :
    Unfold m a a :=
  lmap (fun from_ => (from_, (1 : a))) enumerateFromStepNum

-- ── Enumeration of unbounded Integrals ────────────────────────────────────────

/-- Enumerate integrals from `x` by `stride`: yields `x`, then `x+stride`, …
    (no overflow checks). -/
@[inline] def enumerateFromStepIntegral [Add a] [Monad m] : Unfold m (a × a) a where
  s := a × a
  inject := pure
  step := fun (x, stride) => pure (.Yield x (x + stride, stride))

/-- Enumerate integrals from `from` by `1`. -/
@[inline] def enumerateFromIntegral [Add a] [OfNat a 1] [Monad m] : Unfold m a a :=
  lmap (fun from_ => (from_, (1 : a))) enumerateFromStepIntegral

/-- Enumerate integrals from `from` with stride `next - from`. -/
@[inline] def enumerateFromThenIntegral [Add a] [Sub a] [Monad m] : Unfold m (a × a) a :=
  lmap (fun (from_, next) => (from_, next - from_)) enumerateFromStepIntegral

/-- Enumerate integrals from `from` up to (and including) `to`, by `1`. -/
@[inline] def enumerateFromToIntegral [Add a] [OfNat a 1] [LE a] [DecidableLE a] [Monad m] :
    Unfold m (a × a) a where
  s := a × a
  inject := pure
  step := fun (x, to) => pure (if x ≤ to then .Yield x (x + 1, to) else .Stop)

/-- Enumerate integrals from `from`, stepping towards `to` by `next - from`,
    stopping when `to` is passed (in the stride's direction). -/
@[inline] def enumerateFromThenToIntegral
    [Add a] [Sub a] [LE a] [DecidableLE a] [Monad m] : Unfold m (a × a × a) a where
  s := a × a × a × Bool
  inject := fun (from_, next, to) => pure (from_, next - from_, to, decide (from_ ≤ next))
  step := fun (x, stride, to, up) =>
    pure (if (if up then x ≤ to else to ≤ x) then .Yield x (x + stride, stride, to, up) else .Stop)

end Data.Unfold
