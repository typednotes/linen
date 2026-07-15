/-
  Linen.Data.Stream.BaseCompat — `base`-compatibility helpers for the
  stream-fusion core

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.BaseCompat`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/BaseCompat.hs),
  module #1 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`). Re-rooted under
  `Linen.Data.Stream.*` per the plan's namespace decision.

  ## Substitutions / deviations

  - **`(#.)` → ordinary composition.** Upstream's `(#.) _f = coerce` is a
    zero-cost coercion-based composition: it discards the first function and
    `coerce`s, relying on GHC's `Coercible` (representational equality of
    `newtype`s). Lean has no general `Coercible`, so the operator degrades to
    plain function composition `f ∘ g`. This is semantically identical for the
    intended use (composing with a coercion/`newtype` wrapper that is the
    identity at runtime); only the "free" optimization is lost — exactly the
    kind of GHC-specific performance shim the plan's "External dependencies"
    note treats as a no-op for the port.
  - **`unsafeWithForeignPtr` dropped.** It bridges GHC's `ForeignPtr`/`Ptr`
    machinery, which has no Lean analogue (Lean's `ByteArray`/`Array` are
    managed, not pointer-based). No in-scope module needs it.
-/

namespace Data.Stream.BaseCompat

-- ── Coercion-style composition ──────────────────────────────────────────────

/-- Coercion-style composition. Upstream's `(#.)` is `coerce`-based zero-cost
    composition through a `Coercible` first argument; with no `Coercible` in
    Lean this is ordinary composition `f ∘ g` (semantically identical when the
    first function is a runtime-identity coercion). -/
@[inline] def coerceComp (f : β → γ) (g : α → β) : α → γ := fun x => f (g x)

@[inherit_doc] scoped infixr:9 " #. " => coerceComp

end Data.Stream.BaseCompat
