/-
  Linen.Control.Lens.Lens — `Lens`, `Lens'`, `lens`, `(%%~)`, `(<%~)`,
  `(<<%~)`, `united`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Lens` (fetched and read via
  Hackage's rendered Haddock and source). A `Lens s t a b` is the "has
  exactly one" optic: unlike a `Setter`, it can also read the focused value
  back out (constrained to `Functor`, not merely `Settable`), and unlike a
  `Traversal` it always focuses exactly one `a`, never zero or many.

  **Scope note (`cloneLens`/`ALens`/`ALens'`).** Upstream's `ALens s t a b`
  (`Pretext (->) a b` specialized) and `cloneLens` exist to let a `Lens`
  already instantiated at one functor be safely replayed at another,
  reusing `Control.Lens.Internal.Context`'s `Pretext`/`IndexedComonadStore`
  machinery. `Linen.Control.Lens.Internal.Context`'s own scope note already
  explains why that comonadic replay machinery (`IndexedFunctor`,
  `IndexedComonad`, `Sellable`) was not ported — its only real consumer is
  exactly `cloneLens`, here. With no `ALens` representation to build one
  from, `cloneLens`/`ALens`/`ALens'` are skipped in this port too, following
  that module's precedent; every use in this batch's scope instead threads a
  `Lens` directly through `Functor`-polymorphic combinators (`view`, `over`,
  `set`, …), which needs no replaying at all. -/

import Linen.Control.Lens.Getter
import Linen.Control.Lens.Setter

open Data.Functor

namespace Control.Lens

-- ── lens ────────────────────────────────────────

/-- `lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b`: build a `Lens` out
    of a getter and a setter — `lens sa sbt afb s = sbt s <$> afb (sa s)`. -/
@[inline] def lens {S T A B : Type u} (sa : S → A) (sbt : S → B → T) : Lens S T A B :=
  fun {F} [Functor F] afb s => (sbt s) <$> afb (sa s)

-- ── (%%~) ───────────────────────────────────────

/-- `(%%~) :: LensLike f s t a b -> (a -> f b) -> s -> f t`: upstream notes
    `(%%~) ≡ id` — running any `LensLike`-shaped optic (in particular a
    `Lens`) is already exactly application, so this is a named synonym for
    that application, useful for section/pipeline-style call sites. -/
@[inline] def overF {F : Type u → Type u} {S T A B : Type u}
    (l : LensLike F S T A B) (afb : A → F B) (s : S) : F T :=
  l afb s
@[inherit_doc overF] infixr:75 " %%~ " => overF

-- ── (<%~) / (<<%~) ──────────────────────────────

/-- `(<%~) :: LensLike ((,) b) s t a b -> (a -> b) -> s -> (b, t)`: like
    `(%~)`, but additionally pairs the result with the new value written. -/
@[inline] def setAndGetNew {S T A B : Type u} (l : LensLike (Prod B) S T A B) (f : A → B)
    (s : S) : B × T :=
  l (fun a => (f a, f a)) s
@[inherit_doc setAndGetNew] infixr:75 " <%~ " => setAndGetNew

/-- `(<<%~) :: LensLike ((,) a) s t a b -> (a -> b) -> s -> (a, t)`: like
    `(%~)`, but additionally pairs the result with the *old* value read out. -/
@[inline] def setAndGetOld {S T A B : Type u} (l : LensLike (Prod A) S T A B) (f : A → B)
    (s : S) : A × T :=
  l (fun a => (a, f a)) s
@[inherit_doc setAndGetOld] infixr:75 " <<%~ " => setAndGetOld

-- ── united ──────────────────────────────────────

/-- `united :: Lens' a ()`: every value has a `()` inside it — the terminal
    lens, which reads out `()` and, since there is only one inhabitant of
    `Unit`, can only ever write back the same `a` it started with.

    Fixed at a concrete `Type` (rather than the ambient `Type u` used
    elsewhere in this module), since `Unit` itself is a concrete `Type 0`
    type and `Lens'` requires both of its indices to share one universe —
    the same accommodation `Linen.Control.Lens.Equality` already makes, for
    the same reason. -/
@[inline] def united {A : Type} : Lens' A Unit :=
  fun {F} [Functor F] (f : Unit → F Unit) (a : A) => Functor.map (fun _ => a) (f ())

end Control.Lens
