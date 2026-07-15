/-
  Linen.Data.Producer — `Producer` combinators

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Producer`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Producer.hs),
  module #18 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Facade over `Data.Producer.Type` (#17) adding the `Producer`↔`Unfold` bridge.

  ## Substitutions / deviations

  - **`fromStreamD` deferred.** Upstream also provides
    `fromStreamD :: Producer m (Stream m a) a`, which needs the fused direct
    `Stream` type (`Streamly.Internal.Data.Stream.Type`, Tier 2 / #19 — not yet
    ported). It is deferred until that module lands.
  - **`Producer.Source` re-export dropped.** Upstream re-exports
    `Streamly.Internal.Data.Producer.Source`, a separate module outside this
    batch's in-scope set (see the plan's scope note); not re-exported here.
  - So the in-scope content of this module is `simplify` (plus the re-exported
    `Producer.Type`).
-/

import Linen.Data.Producer.Type
import Linen.Data.Unfold.Type

namespace Data.Producer

open Data.Unfold (Unfold)

-- ── Converting to unfolds ─────────────────────────────────────────────────────

/-- Simplify a producer to an unfold (forgetting the seed-`extract`). -/
@[inline] def simplify (p : Producer m a b) : Unfold m a b :=
  { step := p.step, inject := p.inject }

end Data.Producer
