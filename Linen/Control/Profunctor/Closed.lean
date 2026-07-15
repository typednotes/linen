/-
  Linen.Control.Profunctor.Closed — the `Closed` typeclass

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Closed` (module #7
  of `docs/imports/profunctors/dependencies.md`). Where `Strong` lets the
  product structure of `Type` pass through a profunctor, `Closed` lets the
  *closed* (function-space) structure pass through. Backs
  `Control.Lens.Internal.Zoom`'s function-space handling.

  **Scope note.** Upstream's `Closure`/`Environment` (free/cofree-adjoin
  closedness) are left unported for the same reason as `Strong`'s
  `Tambara`/`Pastro` — see that module's scope note.
-/

import Linen.Control.Profunctor.Unsafe
import Linen.Control.Profunctor.Types

open Control

-- The profunctor `p`'s two argument universes are independent by design,
-- but always co-occur syntactically in `curry'`, so the linter can't tell
-- they need to stay free.
set_option linter.checkUnivs false

namespace Control.Profunctor

/-- A **closed profunctor** lets the closed (function-space) structure of
    `Type` pass through it: given `p a b`, produce a profunctor value over
    functions into/out of `a`/`b`.

    Laws:
    $$\text{lmap}\;(\cdot \circ f) \circ \text{closed} = \text{rmap}\;(\cdot \circ f) \circ \text{closed}$$
    $$\text{dimap}\;\text{const}\;(\cdot\,()) \circ \text{closed} = \text{id}$$ -/
class Closed (P : Type u → Type u → Type v) extends Profunctor P where
  /-- $\text{closed} : P\,a\,b \to P\,(x \to a)\,(x \to b)$. -/
  closed : ∀ {X : Type u}, P α β → P (X → α) (X → β)

/-- $\text{curry}' : P\,(a, b)\,c \to P\,a\,(b \to c)$, via `closed`. -/
def curry' [Closed P] (p : P (α × β) γ) : P α (β → γ) :=
  Profunctor.lmap (fun (a : α) (b : β) => (a, b)) (Closed.closed p)

/-- `Tagged` is `Closed`: the phantom side is discarded either way. -/
instance : Closed Tagged where
  closed t := ⟨fun _ => t.unTagged⟩

/-- Ordinary functions are `Closed`: $\text{closed} = (\cdot \circ \cdot)$, i.e. composition. -/
instance : Closed Control.Fun where
  closed f := ⟨fun g => f.apply ∘ g⟩

/-- `Costar F` is `Closed` for any `Functor F`. -/
instance [Functor F] : Closed (Costar F) where
  closed f := ⟨fun fxa x => f.runCostar ((·  x) <$> fxa)⟩

end Control.Profunctor
