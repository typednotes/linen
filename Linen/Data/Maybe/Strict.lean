/-
  Linen.Data.Maybe.Strict — a strict `Maybe` accumulator

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Maybe.Strict`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Maybe/Strict.hs),
  module #3 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A strict version of `Option`, used as a fold/scan accumulator so the
  compiler can unbox it. `Just'` holds a strict field.

  ## Substitutions / deviations

  - **Strictness is automatic** — Lean is eager, so `Just'`'s field is already
    strict without upstream's `!` bang.
  - **`fromJust'` is total.** Upstream's `fromJust' Nothing' = error "…"` is a
    partial function. AGENTS.md forbids leaving such a landmine; here
    `fromJust'` takes an `[Inhabited a]` and returns `default` on `Nothing'`,
    the standard Lean rendering of a partial extractor. `toMaybe`/`isJust'`
    are total as upstream.
-/

namespace Data.Maybe

-- ── Strict Maybe ────────────────────────────────────────────────────────────

/-- A strict `Option`: `Just'` forces its payload; `Nothing'` is empty. -/
inductive Maybe' (a : Type u) where
  | Just' : a → Maybe' a
  | Nothing' : Maybe' a
  deriving Repr, DecidableEq, Inhabited, BEq

namespace Maybe'

/-- Convert the strict `Maybe'` back to a lazy `Option`. -/
@[inline] def toMaybe : Maybe' a → Option a
  | .Just' a => some a
  | .Nothing' => none

/-- Is this a `Just'`? -/
@[inline] def isJust' : Maybe' a → Bool
  | .Just' _ => true
  | .Nothing' => false

/-- Extract the payload of a `Just'`; returns `default` on `Nothing'`
    (upstream `error`s — made total per AGENTS.md). -/
@[inline] def fromJust' [Inhabited a] : Maybe' a → a
  | .Just' a => a
  | .Nothing' => default

end Maybe'
end Data.Maybe
