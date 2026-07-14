/-
  Linen.Control.Lens.Iso — `Iso`, `Iso'`, `iso`, `withIso`, `cloneIso`,
  `«from»`, `au`, `auf`, `under`, `mapping`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Iso` (fetched and read via
  Hackage's rendered Haddock and source: the signatures below were pulled
  from the real source, not recalled from memory). Upstream's own
  description: "an `Iso` is a `Lens` that is bidirectional — you can run it
  forwards or backwards." Unlike `Lens`/`Traversal`/`Setter`/`Getter`
  (`Linen.Control.Lens.{Lens,Type,Setter,Getter}`), which this codebase
  deliberately keeps as bare `LensLike`-shaped functions of `→` rather than
  `Profunctor`-parameterized ones (see `Getter.to`'s doc comment for why),
  `Iso` genuinely needs the profunctor generalization: `withIso` has to be
  able to run an `Iso` at `Exchange a b` (`Linen.Control.Lens.Internal.Iso`)
  to recover *both* directions of the isomorphism it packages, and `Exchange`
  is a concrete `Profunctor` with no bare-arrow shape to fall back to. This
  module therefore matches upstream's real, profunctor-generalized signature
  verbatim: `type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a
  (f b) -> p s (f t)`.

  Every combinator below reuses `Linen.Control.Lens.Type`'s `Optic`, fixing
  its profunctor's codomain at the same universe `u` as everything else — the
  same accommodation `Linen.Control.Lens.Type`'s own `IndexedLensLike` already
  makes, for the same reason (this file's only consumer, `Exchange`, lives at
  a single universe throughout).

  A pleasant simplification falls out of Lean's `Id` being *transparently*
  `Id α := α` (unlike Haskell's `newtype Identity a = Identity a`, which is
  representationally but not definitionally the identity): upstream's
  `withIso` has to thread `runIdentity` through explicitly to strip the
  `Identity` wrapper it manufactures; here, instantiating `F := Id` already
  *is* the identity, with nothing further to strip — the exact same
  simplification `Linen.Control.Lens.Setter`'s `over` already relies on for
  the analogous `Id`-instantiation trick.

  **Scope note (`non`/`non'`/`anon`).** Upstream's `non`/`non'` need `only`
  (`Control.Lens.Prism`) and `review` (`Control.Lens.Review`) — both ported
  *after* this module in this batch (`Iso` before `Prism` before `Review`,
  matching how `Linen.Control.Lens.Internal.{Iso,Prism,Review}` were already
  ordered). Porting `non`/`non'` here would invert that dependency direction
  (this module would need to import modules that in turn import it). They are
  therefore left for whichever later batch can place them without a cycle —
  `anon` needs no `Prism`/`Review` machinery itself but exists upstream
  purely to generalize `non`, so it is deferred alongside it for the same
  reason (no call site without `non'`/`only` nearby to justify it alone).

  **Scope note (`enum`).** Upstream's `enum :: Enum a => Iso' Int a` is keyed
  to Haskell's `Enum` class (`toEnum`/`fromEnum`); Lean's standard library has
  no direct counterpart (no single class with both directions for an
  arbitrary "has a canonical `Int` numbering" type), and manufacturing one
  with no other call site in this batch's scope is not worth it. Skipped. -/

import Linen.Control.Lens.Type
import Linen.Control.Lens.Internal.Iso

open Control Control.Profunctor Control.Lens.Internal

namespace Control.Lens

-- ── Iso ─────────────────────────────────────────

/-- `Iso s t a b := ∀ p f, (Profunctor p, Functor f) => p a (f b) -> p s (f
    t)`: a `Lens` that is bidirectional — it can be run forwards (`s -> a`)
    or backwards (`b -> t`). See the module doc comment for why this, alone
    among this codebase's concrete optic aliases, is genuinely
    profunctor-generalized. -/
abbrev Iso (S T A B : Type u) :=
  ∀ {P : Type u → Type u → Type u} [Profunctor P] {F : Type u → Type u} [Functor F],
    Optic P F S T A B

/-- `Iso' s a := Iso s s a a`. -/
abbrev Iso' (S A : Type u) := Iso S S A A

-- ── iso ─────────────────────────────────────────

/-- `iso :: (s -> a) -> (b -> t) -> Iso s t a b`: build an `Iso` out of a pair
    of mutually-inverse functions — `iso sa bt = dimap sa (fmap bt)`. -/
@[inline] def iso {S T A B : Type u} (sa : S → A) (bt : B → T) : Iso S T A B :=
  fun {P} [Profunctor P] {F} [Functor F] p => Profunctor.dimap sa (Functor.map bt) p

-- ── withIso / cloneIso ──────────────────────────

/-- `withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r`: run an
    `Iso` at the concrete profunctor `Exchange a b`, recovering both
    directions of the isomorphism it packages — `withIso ai k = case ai
    (Exchange id Identity) of Exchange sa bt -> k sa (runIdentity . bt)`,
    simplified here since Lean's `Id` needs no `runIdentity` to strip (see
    the module doc comment). -/
@[inline] def withIso {S T A B R : Type u} (l : Iso S T A B) (k : (S → A) → (B → T) → R) : R :=
  let e : Exchange A B S T := l (P := Exchange A B) (F := Id) ⟨id, id⟩
  k e.sa e.bt

/-- `cloneIso :: AnIso s t a b -> Iso s t a b`: rebuild a fresh, fully
    polymorphic `Iso` out of one already run at a concrete profunctor —
    `cloneIso k = withIso k iso`. -/
@[inline] def cloneIso {S T A B : Type u} (l : Iso S T A B) : Iso S T A B :=
  withIso l (fun sa bt => iso sa bt)

-- ── «from» ──────────────────────────────────────

/-- `from :: AnIso s t a b -> Iso b a t s`: reverse an `Iso`, swapping its two
    directions — `from l = withIso l $ \sa bt -> iso bt sa`. Named `«from»`
    (escaping the keyword) since `from` is reserved in Lean 4. -/
@[inline] def «from» {S T A B : Type u} (l : Iso S T A B) : Iso B A T S :=
  withIso l (fun sa bt => iso bt sa)

-- ── au / auf / under ────────────────────────────

/-- `au :: Functor f => AnIso s t a b -> ((b -> t) -> f s) -> f a`: run an
    action that builds an `f s` out of the isomorphism's backward direction,
    then map the forward direction over the result — `au k = withIso k $ \sa
    bt f -> fmap sa (f bt)`. -/
@[inline] def au {S T A B : Type u} {F : Type u → Type u} [Functor F]
    (l : Iso S T A B) (f : (B → T) → F S) : F A :=
  withIso l (fun sa bt => sa <$> f bt)

/-- `auf :: (Functor f, Functor g) => AnIso s t a b -> (f t -> g s) -> f b ->
    g a`: like `au`, but additionally maps the backward direction over an `f
    b` before handing it to the supplied function — `auf k ftgs fb = withIso
    k $ \sa bt -> sa <$> ftgs (bt <$> fb)`. -/
@[inline] def auf {S T A B : Type u} {F G : Type u → Type u} [Functor F] [Functor G]
    (l : Iso S T A B) (ftgs : F T → G S) (fb : F B) : G A :=
  withIso l (fun sa bt => sa <$> ftgs (bt <$> fb))

/-- `under :: AnIso s t a b -> (t -> s) -> b -> a`: conjugate a function `t ->
    s` by an `Iso`, transporting it to a function `b -> a` — `under k = withIso
    k $ \sa bt ts -> sa . ts . bt`. -/
@[inline] def under {S T A B : Type u} (l : Iso S T A B) (ts : T → S) (b : B) : A :=
  withIso l (fun sa bt => sa (ts (bt b)))

-- ── mapping ─────────────────────────────────────

/-- `mapping :: (Functor f, Functor g) => AnIso s t a b -> Iso (f s) (g t) (f
    a) (g b)`: lift an `Iso` to act underneath any pair of functors —
    `mapping k = withIso k $ \sa bt -> iso (fmap sa) (fmap bt)`. -/
@[inline] def mapping {S T A B : Type u} {F G : Type u → Type u} [Functor F] [Functor G]
    (l : Iso S T A B) : Iso (F S) (G T) (F A) (G B) :=
  withIso l (fun sa bt => iso (Functor.map sa) (Functor.map bt))

end Control.Lens
