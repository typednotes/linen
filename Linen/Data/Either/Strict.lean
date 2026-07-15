/-
  Linen.Data.Either.Strict — a strict `Either` accumulator

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Either.Strict`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Either/Strict.hs),
  module #4 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A strict version of `Either` used as a fold/scan accumulator. Both `Left'`
  and `Right'` hold strict fields.

  ## Substitutions / deviations

  - **Strictness is automatic** — Lean is eager, so the constructor fields are
    already strict without upstream's `!` bangs.
  - **`fromLeft'`/`fromRight'` are total.** Upstream `error`s on the wrong
    constructor; per AGENTS.md they take an `[Inhabited _]` and return
    `default` instead. `isLeft'`/`isRight'` are total as upstream.
-/

namespace Data.Either

-- ── Strict Either ───────────────────────────────────────────────────────────

/-- A strict `Either`: both branches force their payloads. -/
inductive Either' (a b : Type u) where
  | Left' : a → Either' a b
  | Right' : b → Either' a b
  deriving Repr, DecidableEq, Inhabited, BEq

namespace Either'

/-- Is this a `Left'`? -/
@[inline] def isLeft' : Either' a b → Bool
  | .Left' _ => true
  | .Right' _ => false

/-- Is this a `Right'`? -/
@[inline] def isRight' : Either' a b → Bool
  | .Left' _ => false
  | .Right' _ => true

/-- Extract the payload of a `Left'`; returns `default` otherwise
    (upstream `error`s — made total per AGENTS.md). -/
@[inline] def fromLeft' [Inhabited a] : Either' a b → a
  | .Left' a => a
  | .Right' _ => default

/-- Extract the payload of a `Right'`; returns `default` otherwise
    (upstream `error`s — made total per AGENTS.md). -/
@[inline] def fromRight' [Inhabited b] : Either' a b → b
  | .Left' _ => default
  | .Right' b => b

end Either'
end Data.Either
