/-
  Linen.Control.Lens.Setter — `Setter`, `sets`, `over`, `set`, `set'`,
  `(.~)`, `(%~)`, `(?~)`, `(<.~)`, `(<?~)`, `mapped`, `contramapped`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Setter` (fetched and read
  via Hackage's rendered Haddock and source). A `Setter s t a b` is a
  write-only optic: it can transform every `a` inside an `s` into a `b`,
  producing a `t`, but (being restricted to `Settable`, rather than
  arbitrary `Applicative`, functors — see `Linen.Control.Lens.Internal.
  Setter`) can never be used to *read* the `a`s back out, only rewrite them.

  **Scope note (`Profunctor`-generalized `sets`).** Upstream states `sets ::
  (Profunctor p, Profunctor q, Settable f) => (p a b -> q s t) -> Optical p
  q f s t a b`, generalizing over an arbitrary pair of profunctors so that
  `sets` can also build index-preserving setters. `linen`'s concrete optic
  aliases (`Setter`, `Lens`, `Traversal`, …, `Linen.Control.Lens.Type`) are,
  by deliberate design, plain `LensLike`-shaped functions of `→` rather than
  `Profunctor`-parameterized ones (see `Linen.Control.Lens.Getter`'s `to`
  doc comment for why: `linen`'s function-arrow `Profunctor` instance lives
  on the nominal wrapper `Control.Fun`, not on bare `→`, so a
  `p`-polymorphic combinator can never land in a bare-arrow optic without an
  explicit, unhelpful wrapping/unwrapping step at every call site). This
  port therefore specializes `sets` to build a `Setter` directly from a
  plain `(a -> b) -> s -> t` function, matching the "no profunctor
  parameter" choice already made for every other concrete optic alias.

  **Scope note (`MonadState`-dependent combinators).** Upstream's
  `assign`/`(.=)`, `modifying`/`(%=)`, `(?=)`, `(<.=)`, `(<?=)`,
  `(<%=)`/`(<<%=)`/`(<<.=)`, and their `Lens.At`/`Lens.Indexed` counterparts
  all need a `MonadState` constraint. `linen` has ported no mtl-style
  `MonadState` class (only the concrete `Control.Monad.State` transformer),
  so there is no class to dispatch these against; they are skipped here,
  matching `Linen.Control.Lens.Getter`'s identical scope note for
  `use`/`uses`. -/

import Linen.Control.Lens.Type
import Linen.Control.Lens.Internal.Setter

open Control.Lens.Internal Data.Functor

namespace Control.Lens

-- ── sets ────────────────────────────────────────

/-- `sets :: ((a -> b) -> s -> t) -> Setter s t a b`: build a `Setter` out of
    an ordinary "modify everywhere" function, by running it under
    `untainted`/`pure` so it type-checks against an arbitrary `Settable f` —
    `sets f afb s = pure (f (untainted ∘ afb) s)`. See the module's scope
    note for why this is specialized away from upstream's `Profunctor`-
    generalized form. -/
@[inline] def sets {S T A B : Type u} (f : (A → B) → S → T) : Setter S T A B :=
  fun {F} [Settable F] afb s => (pure (f (fun a => Settable.untainted (afb a)) s) : F T)

-- ── over / set / set' ───────────────────────────

/-- `over :: ASetter s t a b -> (a -> b) -> s -> t`: run a `Setter`
    concretely at `Id`, recovering an ordinary "modify everywhere" function —
    `over l f = runIdentity ∘ l (Identity ∘ f)`, simplified here since `Id`
    is already transparently its own carrier. -/
@[inline] def over {S T A B : Type u} (l : Setter S T A B) (f : A → B) (s : S) : T :=
  l (F := Id) f s

/-- `set :: ASetter s t a b -> b -> s -> t`: replace every focused `a` with a
    constant `b` — `set l b = over l (const b)`. -/
@[inline] def set {S T A B : Type u} (l : Setter S T A B) (b : B) (s : S) : T :=
  over l (fun _ => b) s

/-- `set' :: ASetter' s a -> a -> s -> s`: `set` specialized to a
    non-type-changing `Setter'`. -/
@[inline] def set' {S A : Type u} (l : Setter' S A) (a : A) (s : S) : S :=
  set l a s

-- ── infix operators ─────────────────────────────

/-- `(.~) :: ASetter s t a b -> b -> s -> t`: infix `set`. -/
@[inline] def setTo {S T A B : Type u} (l : Setter S T A B) (b : B) : S → T := set l b
@[inherit_doc setTo] infixr:75 " .~ " => setTo

/-- `(%~) :: ASetter s t a b -> (a -> b) -> s -> t`: infix `over`. -/
infixr:75 " %~ " => over

/-- `(?~) :: ASetter s t a (Maybe b) -> b -> s -> t`: set the focused
    `Option`-valued field to `some b` — `l ?~ b = set l (some b)`. -/
@[inline] def setSomeTo {S T A B : Type u} (l : Setter S T A (Option B)) (b : B) : S → T :=
  set l (some b)
@[inherit_doc setSomeTo] infixr:75 " ?~ " => setSomeTo

/-- `(<.~) :: ASetter s t a b -> b -> s -> (b, t)`: `set`, additionally
    pairing the result with the value written. -/
@[inline] def setAndPair {S T A B : Type u} (l : Setter S T A B) (b : B) (s : S) : B × T :=
  (b, set l b s)
@[inherit_doc setAndPair] infixr:75 " <.~ " => setAndPair

/-- `(<?~) :: ASetter s t a (Maybe b) -> b -> s -> (b, t)`: `(?~)`,
    additionally pairing the result with the value written. -/
@[inline] def setSomeAndPair {S T A B : Type u} (l : Setter S T A (Option B)) (b : B) (s : S) :
    B × T :=
  (b, set l (some b) s)
@[inherit_doc setSomeAndPair] infixr:75 " <?~ " => setSomeAndPair

-- ── mapped / contramapped ───────────────────────

/-- `mapped :: Functor f => Setter (f a) (f b) a b`: every `Functor` gives
    rise to a `Setter` on its contents, via `fmap` — `mapped = sets fmap`. -/
@[inline] def mapped {F : Type u → Type u} [Functor F] {A B : Type u} : Setter (F A) (F B) A B :=
  sets Functor.map

/-- `contramapped :: Contravariant f => Setter (f b) (f a) a b`: every
    `Contravariant` functor gives rise to a `Setter` on its contents (with
    the type-change direction flipped, matching `contramap`'s variance) —
    `contramapped = sets contramap`. -/
@[inline] def contramapped {F : Type u → Type u} [Contravariant F] {A B : Type u} :
    Setter (F B) (F A) A B :=
  sets Contravariant.contramap

end Control.Lens
