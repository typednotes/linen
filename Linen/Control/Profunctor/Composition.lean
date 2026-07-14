/-
  Linen.Control.Profunctor.Composition — `Procompose` and `Rift`

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Composition`
  (module #10 of `docs/imports/profunctors/dependencies.md`). `Procompose p
  q` is the composition of profunctors `p` and `q` in the bicategory `Prof`
  — the profunctor analogue of composing two functors; `Rift p q` is the
  right Kan lift of `q` along `p` in the same bicategory.

  **Scope note.** Upstream also provides:
  - `Sieve`/`Cosieve`/`Representable`/`Corepresentable`/`Costrong` instances
    for `Procompose`, built from `Data.Functor.Compose` (functor
    composition matching the profunctor composition). These need careful
    universe/associativity bookkeeping between `Data.Functor.Compose` and
    `Procompose` for comparatively little further payoff here (`lens`
    itself only reaches into `Procompose`'s `Profunctor`/`Strong`/`Choice`
    structure and the `ProfunctorAdjunction` with `Rift`), so they are left
    unported.
  - The unitor/associator isomorphisms `idl`/`idr`/`assoc` and the
    functor-composition witnesses `stars`/`costars`/`kleislis`/`cokleislis`,
    all stated as van-Laarhoven `Iso`s (`lens`'s own `Iso` type, not yet
    ported here) — left unported for the same reason as other `lens`-only
    helpers throughout this package.
-/

import Linen.Control.Category
import Linen.Control.Profunctor.Choice
import Linen.Control.Profunctor.Closed
import Linen.Control.Profunctor.Monad
import Linen.Control.Profunctor.Strong

open Control

namespace Control.Profunctor

-- ── Procompose ─────────────────────────────────

/-- `Procompose p q d c` is the profunctor composition of `p` and `q`:
    $\exists x.\; p\,x\,c \times q\,d\,x$, i.e. the coend
    $\int^{x} p\,x\,c \times q\,d\,x$. -/
structure Procompose (P Q : Type u → Type u → Type v) (D C : Type u) where
  {X : Type u}
  /-- The `p`-half of the composite, over the hidden intermediate `X`. -/
  outer : P X C
  /-- The `q`-half of the composite, over the hidden intermediate `X`. -/
  inner : Q D X

/-- If `p` is a `Category`, a self-composed `Procompose p p a b` collapses
    back down to a single `p a b` via composition. -/
def procomposed [Category P] (pq : Procompose P P α β) : P α β :=
  Category.comp pq.inner pq.outer

instance [Profunctor P] [Profunctor Q] : Profunctor (Procompose P Q) where
  dimap l r pq := ⟨Profunctor.rmap r pq.outer, Profunctor.lmap l pq.inner⟩
  lmap l pq := ⟨pq.outer, Profunctor.lmap l pq.inner⟩
  rmap r pq := ⟨Profunctor.rmap r pq.outer, pq.inner⟩

instance [Strong P] [Strong Q] : Strong (Procompose P Q) where
  first' pq := ⟨Strong.first' pq.outer, Strong.first' pq.inner⟩
  second' pq := ⟨Strong.second' pq.outer, Strong.second' pq.inner⟩

instance [Choice P] [Choice Q] : Choice (Procompose P Q) where
  left' pq := ⟨Choice.left' pq.outer, Choice.left' pq.inner⟩
  right' pq := ⟨Choice.right' pq.outer, Choice.right' pq.inner⟩

instance [Closed P] [Closed Q] : Closed (Procompose P Q) where
  closed pq := ⟨Closed.closed pq.outer, Closed.closed pq.inner⟩

-- Note: upstream also gives `Procompose` `Traversing`/`Mapping` instances,
-- both defined component-wise on `traverse'`/`map'`
-- (`traverse' (Procompose p q) = Procompose (traverse' p) (traverse' q)`).
-- Since this port makes `wander`/`roam` the *primitive* methods of
-- `Traversing`/`Mapping` (see the scope note in
-- `Linen.Control.Profunctor.Traversing`) precisely to avoid the free-applicative
-- `Bazaar` encoding, and deriving `Procompose`'s `wander`/`roam` generically
-- from its components' `wander`/`roam` genuinely needs that same `Bazaar`
-- machinery (there is no way to thread one arbitrary rank-2 effect `F`
-- through both composed layers independently without it), these two
-- instances are left unported here.

instance : ProfunctorFunctor (Procompose P) where
  promap f := fun pq => ⟨pq.outer, f pq.inner⟩

-- Note: upstream also gives `Procompose p` a `ProfunctorMonad` instance
-- (`proreturn q = Procompose id q`, `projoin (Procompose f (Procompose g h)) =
-- Procompose (f . g) h`). It cannot be ported here: `Procompose`'s hidden
-- existential field `{X : Type u}` puts `Procompose P Q d c` one universe
-- above `Type v` (at `Type (max (u + 1) v)`), so `Procompose P` only fits
-- `ProfunctorFunctor`'s signature (whose result universe `w` is free) but not
-- `ProfunctorMonad`'s (whose `projoin : T (T P) :→ T P` forces `T`'s result
-- universe to equal its argument universe `v`, for `T` to be applicable to
-- its own output). This is a genuine consequence of Lean's predicative
-- universe stratification — Haskell's flat, unstratified `Type` has no
-- analogous restriction — not a proof shortcut.

-- ── Rift ───────────────────────────────────────

/-- `Rift p q a b` is the right Kan lift of `q` along `p`:
    $\forall x.\; p\,b\,x \to q\,a\,x$. -/
structure Rift (P Q : Type u → Type u → Type v) (A B : Type u) where
  runRift : ∀ {X : Type u}, P B X → Q A X

instance [Profunctor P] [Profunctor Q] : Profunctor (Rift P Q) where
  dimap ca bd f := ⟨fun p => Profunctor.lmap ca (f.runRift (Profunctor.lmap bd p))⟩
  lmap ca f := ⟨fun p => Profunctor.lmap ca (f.runRift p)⟩
  rmap bd f := ⟨fun p => f.runRift (Profunctor.lmap bd p)⟩

/-- `Rift p p` forms a `Category`, isomorphic to `p` composing with itself. -/
instance [Profunctor P] : Category (Rift P P) where
  id := ⟨fun p => p⟩
  comp f g := ⟨fun p => f.runRift (g.runRift p)⟩

instance : ProfunctorFunctor (Rift P) where
  promap f := fun r => ⟨fun p => f (r.runRift p)⟩

-- Note: upstream also gives `Rift p` a `ProfunctorComonad` instance
-- (`proextract f = runRift f id`, `produplicate (Rift f) = Rift (Rift . (f
-- .))`). It is unportable for exactly the same reason as `Procompose p`'s
-- `ProfunctorMonad` instance just above: `Rift`'s field `runRift : ∀ {X :
-- Type u}, ...` likewise puts `Rift P Q a b` one universe above `Type v`,
-- so `Rift P` fits `ProfunctorFunctor` but not the self-composable
-- `ProfunctorComonad`. See the note on `Procompose`'s `ProfunctorMonad`
-- instance above.

/-- The 2-morphism defining the right Kan lift: discharge a `Procompose p
    (Rift p q)` down to a plain `q`. -/
def decomposeRift [Profunctor Q] (pr : Procompose P (Rift P Q) α β) : Q α β :=
  pr.inner.runRift pr.outer

end Control.Profunctor
