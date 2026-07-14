/-
  Linen.Control.Profunctor.Yoneda — `Yoneda` and `Coyoneda`

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Yoneda` (module
  #15 of `docs/imports/profunctors/dependencies.md`, the last module of the
  original 16-module plan proper — the facade is #16). `Yoneda p` is the
  cofree profunctor on `p`: the profunctor version of the Yoneda lemma,
  $\text{Yoneda}\;p\;a\;b \cong p\,a\,b$, packaged as a rank-2 field so that
  every `Profunctor`-polymorphic operation on it is free (no need for a
  `Profunctor p` constraint to build one). `Coyoneda p` is its free/existential
  dual.

  **Scope note.** Upstream gives both `Yoneda` and `Coyoneda` *both* a
  `ProfunctorMonad` and a `ProfunctorComonad` instance (they are mutually
  adjoint on `Prof`). Neither self-composing instance ports here: `Yoneda`'s
  `runYoneda : ∀ {X Y : Type u}, ...` and `Coyoneda`'s hidden `{X Y : Type
  u}` field both put `Yoneda P a b`/`Coyoneda P a b` one universe above
  `Type v`, so (exactly as for `Procompose`/`Rift` in
  `Linen.Control.Profunctor.Composition` and `Ran` in
  `Linen.Control.Profunctor.Ran`) they satisfy the free-result-universe
  `ProfunctorFunctor` but not the self-composable `ProfunctorMonad`/
  `ProfunctorComonad` (both of which need `T (T P)` to type-check, forcing
  `T`'s result universe to equal its argument universe). The plain
  `returnYoneda`/`extractCoyoneda` helpers below take over the role
  `proreturn`/`proextract` would have played for the other instances in this
  file.
-/

import Linen.Control.Category
import Linen.Control.Profunctor.Closed
import Linen.Control.Profunctor.Mapping
import Linen.Control.Profunctor.Monad

open Control

namespace Control.Profunctor

-- ── Yoneda ─────────────────────────────────────

/-- `Yoneda p a b := ∀ x y, (x → a) → (b → y) → p x y`, the cofree
    profunctor on `p`. -/
structure Yoneda (P : Type u → Type u → Type v) (A B : Type u) where
  runYoneda : ∀ {X Y : Type u}, (X → A) → (B → Y) → P X Y

/-- Recover the underlying `p a b` from a `Yoneda p a b`; no `Profunctor p`
    constraint is needed, since the rank-2 field already carries the whole
    profunctor structure of `p`. -/
def extractYoneda (y : Yoneda P A B) : P A B := y.runYoneda id id

/-- Lift a plain `p a b` into `Yoneda p a b`, via `dimap`. Takes over the
    role `ProfunctorMonad.proreturn` would have played — see the module's
    scope note on why `Yoneda` cannot be given a `ProfunctorMonad` instance. -/
def returnYoneda [Profunctor P] (p : P A B) : Yoneda P A B :=
  ⟨fun l r => Profunctor.dimap l r p⟩

/-- `Yoneda p` needs no constraint on `p` at all to be a `Profunctor`. -/
instance : Profunctor (Yoneda P) where
  dimap l r y := ⟨fun l' r' => y.runYoneda (l ∘ l') (r' ∘ r)⟩
  lmap l y := ⟨fun l' r => y.runYoneda (l ∘ l') r⟩
  rmap r y := ⟨fun l r' => y.runYoneda l (r' ∘ r)⟩

instance : Functor (Yoneda P A) where
  map f y := ⟨fun l r => y.runYoneda l (r ∘ f)⟩

instance : ProfunctorFunctor Yoneda where
  promap f := fun y => ⟨fun l r => f (y.runYoneda l r)⟩

instance [Category P] [Profunctor P] : Category (Yoneda P) where
  id := ⟨fun l r => Profunctor.dimap l r Category.id⟩
  comp f g := ⟨fun l r => Category.comp (f.runYoneda l id) (g.runYoneda id r)⟩

instance [Strong P] : Strong (Yoneda P) where
  first' y := returnYoneda (Strong.first' (extractYoneda y))
  second' y := returnYoneda (Strong.second' (extractYoneda y))

instance [Choice P] : Choice (Yoneda P) where
  left' y := returnYoneda (Choice.left' (extractYoneda y))
  right' y := returnYoneda (Choice.right' (extractYoneda y))

instance [Costrong P] : Costrong (Yoneda P) where
  unfirst y := returnYoneda (Costrong.unfirst (extractYoneda y))
  unsecond y := returnYoneda (Costrong.unsecond (extractYoneda y))

instance [Cochoice P] : Cochoice (Yoneda P) where
  unleft y := returnYoneda (Cochoice.unleft (extractYoneda y))
  unright y := returnYoneda (Cochoice.unright (extractYoneda y))

instance [Closed P] : Closed (Yoneda P) where
  closed y := returnYoneda (Closed.closed (extractYoneda y))

instance [Mapping P] : Mapping (Yoneda P) where
  roam f y := returnYoneda (Mapping.roam f (extractYoneda y))
  map' y := returnYoneda (Mapping.map' (extractYoneda y))
  wander f y := returnYoneda (Traversing.wander f (extractYoneda y))
  traverse' y := returnYoneda (Traversing.traverse' (extractYoneda y))

instance [Traversing P] : Traversing (Yoneda P) where
  wander f y := returnYoneda (Traversing.wander f (extractYoneda y))
  traverse' y := returnYoneda (Traversing.traverse' (extractYoneda y))

-- ── Coyoneda ───────────────────────────────────

/-- `Coyoneda p a b := ∃ x y,\, (a \to x) \times (y \to b) \times p\,x\,y`,
    the free profunctor on `p`. -/
structure Coyoneda (P : Type u → Type u → Type v) (A B : Type u) where
  {X Y : Type u}
  intoX : A → X
  outOfY : Y → B
  run : P X Y

/-- Lift a plain `p a b` into `Coyoneda p a b`. -/
def returnCoyoneda (p : P A B) : Coyoneda P A B := ⟨id, id, p⟩

/-- Recover the underlying `p a b` from a `Coyoneda p a b`, given a
    `Profunctor p`. Takes over the role `ProfunctorComonad.proextract` would
    have played — see the module's scope note on why `Coyoneda` cannot be
    given a `ProfunctorComonad` instance. -/
def extractCoyoneda [Profunctor P] (c : Coyoneda P A B) : P A B :=
  Profunctor.dimap c.intoX c.outOfY c.run

instance : Profunctor (Coyoneda P) where
  dimap l r c := ⟨c.intoX ∘ l, r ∘ c.outOfY, c.run⟩
  lmap l c := ⟨c.intoX ∘ l, c.outOfY, c.run⟩
  rmap r c := ⟨c.intoX, r ∘ c.outOfY, c.run⟩

instance : Functor (Coyoneda P A) where
  map f c := ⟨c.intoX, f ∘ c.outOfY, c.run⟩

instance : ProfunctorFunctor Coyoneda where
  promap f := fun c => ⟨c.intoX, c.outOfY, f c.run⟩

instance [Category P] [Profunctor P] : Category (Coyoneda P) where
  id := ⟨id, id, Category.id⟩
  comp c d :=
    ⟨c.intoX, d.outOfY, Category.comp (Profunctor.rmap (d.intoX ∘ c.outOfY) c.run) d.run⟩

instance [Strong P] : Strong (Coyoneda P) where
  first' c := returnCoyoneda (Strong.first' (extractCoyoneda c))
  second' c := returnCoyoneda (Strong.second' (extractCoyoneda c))

instance [Choice P] : Choice (Coyoneda P) where
  left' c := returnCoyoneda (Choice.left' (extractCoyoneda c))
  right' c := returnCoyoneda (Choice.right' (extractCoyoneda c))

instance [Costrong P] : Costrong (Coyoneda P) where
  unfirst c := returnCoyoneda (Costrong.unfirst (extractCoyoneda c))
  unsecond c := returnCoyoneda (Costrong.unsecond (extractCoyoneda c))

instance [Cochoice P] : Cochoice (Coyoneda P) where
  unleft c := returnCoyoneda (Cochoice.unleft (extractCoyoneda c))
  unright c := returnCoyoneda (Cochoice.unright (extractCoyoneda c))

instance [Closed P] : Closed (Coyoneda P) where
  closed c := returnCoyoneda (Closed.closed (extractCoyoneda c))

instance [Mapping P] : Mapping (Coyoneda P) where
  roam f c := returnCoyoneda (Mapping.roam f (extractCoyoneda c))
  map' c := returnCoyoneda (Mapping.map' (extractCoyoneda c))
  wander f c := returnCoyoneda (Traversing.wander f (extractCoyoneda c))
  traverse' c := returnCoyoneda (Traversing.traverse' (extractCoyoneda c))

instance [Traversing P] : Traversing (Coyoneda P) where
  wander f c := returnCoyoneda (Traversing.wander f (extractCoyoneda c))
  traverse' c := returnCoyoneda (Traversing.traverse' (extractCoyoneda c))

end Control.Profunctor
