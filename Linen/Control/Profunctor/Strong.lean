/-
  Linen.Control.Profunctor.Strong — the `Strong`/`Costrong` typeclasses

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Strong` (module #3
  of `docs/imports/profunctors/dependencies.md`). `Strong` generalizes
  `Star` of a strong `Functor`: profunctor strength with respect to `Prod`.
  This is the class that backs `Control.Lens.Lens` (a lens is, in the
  van Laarhoven encoding, a natural transformation constrained to be
  `Strong`).

  **Scope note.** Upstream's module also defines `Tambara`/`Pastro`
  (freely/cofreely adjoining strength to an arbitrary profunctor via a
  rank-2 field / an existential `GADT`) and their `Costrong` duals
  `Cotambara`/`Copastro`. These are category-theoretic scaffolding with no
  call site in `lens` itself (`lens` only ever uses the `Strong`/`Costrong`
  classes and this module's concrete instances) — they are Kan-extension-style
  free/cofree constructions, not something a *lawful* value of `Strong p` is
  ever required to inhabit, and they add no further class the rest of this
  port depends on. They are left unported, matching the plan's scope note
  that extras beyond what `lens` calls into may be deferred.
-/

import Linen.Control.Profunctor.Unsafe
import Linen.Control.Profunctor.Types

open Control

-- The profunctor `p`'s two argument universes are independent by design,
-- but always co-occur syntactically in `uncurry'`/`strong`, so the linter
-- can't tell they need to stay free.
set_option linter.checkUnivs false

namespace Control.Profunctor

-- ── Strong ─────────────────────────────────────

/-- A **strong profunctor** lets the product structure of `Type` pass
    through it. Every `Functor` in Lean is strong with respect to `Prod`, so
    `Strong` generalizes `Star` of a functor.

    Upstream's `MINIMAL first' | second'` mutual default (as with
    `Profunctor`'s `dimap`/`lmap`/`rmap`, see that module's note) is not
    something Lean's class elaborator can instantiate a default for; every
    instance here already supplies both concretely, so `first'` is made the
    required primitive and `second'` keeps its upstream default, now
    non-circular.

    Laws:
    $$\text{first}' = \text{dimap}\;\text{swap}\;\text{swap} \circ \text{second}'$$
    $$\text{lmap}\;\text{fst} = \text{rmap}\;\text{fst} \circ \text{first}'$$ -/
class Strong (P : Type u → Type u → Type v) extends Profunctor P where
  /-- Thread an extra component `γ` through on the left: $\text{first}' : P\,a\,b \to P\,(a,γ)\,(b,γ)$. -/
  first' : P α β → P (α × γ) (β × γ)
  /-- Thread an extra component `γ` through on the right: $\text{second}' : P\,a\,b \to P\,(γ,a)\,(γ,b)$. -/
  second' : P α β → P (γ × α) (γ × β) := fun p => dimap Prod.swap Prod.swap (first' p)

/-- $\text{uncurry}' : P\,a\,(b \to c) \to P\,(a,b)\,c$, via `first'`. -/
def uncurry' [Strong P] (p : P α (β → γ)) : P (α × β) γ :=
  Profunctor.rmap (fun (f, x) => f x) (Strong.first' p)

/-- Lift a binary function into a `Strong` profunctor: $\text{strong}\;f\;x
    : P\,α\,γ$ from $f : α \to β \to γ$ and $x : P\,α\,β$. -/
def strong [Strong P] (f : α → β → γ) (x : P α β) : P α γ :=
  Profunctor.dimap (fun a => (a, a)) (fun (b, a) => f a b) (Strong.first' x)

/-- Ordinary functions are `Strong`:
    $\text{first}'\;f\,(a, c) = (f\,a, c)$. -/
instance : Strong Control.Fun where
  first' f := ⟨fun (a, c) => (f.apply a, c)⟩
  second' f := ⟨fun (c, a) => (c, f.apply a)⟩

/-- `Star F` is `Strong` for any `Functor F`. -/
instance [Functor F] : Strong (Star F) where
  first' f := ⟨fun (a, c) => (fun b => (b, c)) <$> f.runStar a⟩
  second' f := ⟨fun (c, a) => (fun b => (c, b)) <$> f.runStar a⟩

/-- `WrappedArrow P` is `Strong` for any `Arrow P`, via `Arrow.first`/`Arrow.second`. -/
instance [Arrow P] : Strong (WrappedArrow P) where
  first' k := ⟨Arrow.first k.unwrapArrow⟩
  second' k := ⟨Arrow.second k.unwrapArrow⟩

/-- `Forget R` is `Strong`: project out the extra component before running. -/
instance : Strong (Forget R) where
  first' k := ⟨k.runForget ∘ Prod.fst⟩
  second' k := ⟨k.runForget ∘ Prod.snd⟩

-- ── Costrong ───────────────────────────────────

/-- The dual of `Strong`: costrength with respect to `Prod`, analogous to
    `ArrowLoop` (`unfirst` is `loop`).

    Same mutual-default note as `Strong`: `unfirst` is made the required
    primitive, `unsecond` keeps its upstream default non-circularly.

    Laws:
    $$\text{unfirst} = \text{unsecond} \circ \text{dimap}\;\text{swap}\;\text{swap}$$ -/
class Costrong (P : Type u → Type u → Type v) extends Profunctor P where
  /-- Discharge an extra threaded component `δ` from the left. -/
  unfirst : P (α × δ) (β × δ) → P α β
  /-- Discharge an extra threaded component `δ` from the right. -/
  unsecond : P (δ × α) (δ × β) → P α β := fun p => unfirst (dimap Prod.swap Prod.swap p)

/-- `Tagged` is `Costrong`: project the retained half of the phantom pair. -/
instance : Costrong Tagged where
  unfirst t := ⟨t.unTagged.fst⟩
  unsecond t := ⟨t.unTagged.snd⟩

-- Note: upstream also gives `(->)` a `Costrong` instance —
-- `unfirst f a = b where (b, d) = f (a, d)` — which ties a knot through
-- Haskell's laziness: `d` is produced by the very call it feeds. That has no
-- total, strict Lean translation (nothing guarantees `f` doesn't force `d`
-- before producing it), so `Costrong Control.Fun` is intentionally not
-- ported.

end Control.Profunctor
