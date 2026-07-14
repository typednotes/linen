/-
  Linen.Control.Lens.Profunctor — `OpticP`, `fromLens`, `fromIso`,
  `fromPrism`, `fromSetter`, `toLens`, `toIso`, `toPrism`, `toSetter`,
  `toTraversal`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Profunctor` (fetched and
  read via the real source, not recalled from memory — the plan
  accompanying this batch described this module's content as `Choicy`/
  `Bizarre1` combinators, but the real upstream source is something
  different: a small interoperability layer between this library's van
  Laarhoven optics and genuinely `Profunctor`-based optics, per its own doc
  comment: "provide[s] conversion functions between the optics defined in
  this library and `Profunctor`-based optics… to provide an interoperability
  layer between the two styles of optics, and not to reimplement all the
  library in terms of `Profunctor` optics." What is ported below follows
  the real source, not the plan's description.)

  Upstream's real core:

  ```
  type OpticP p s t a b = p a b -> p s t

  fromLens      :: Strong p    => LensLike (Context a b) s t a b -> OpticP p s t a b
  fromIso       :: Profunctor p => Optic p Identity s t a b -> OpticP p s t a b
  fromPrism     :: Choice p     => Optic p Identity s t a b -> OpticP p s t a b
  fromSetter    :: Mapping p    => ASetter s t a b -> OpticP p s t a b
  fromTraversal :: Traversing p => ATraversal s t a b -> OpticP p s t a b

  toPrism      :: (Choice p, Applicative f)     => OpticP (WrappedPafb f p) s t a b -> Optic p f s t a b
  toIso        :: (Profunctor p, Functor f)     => OpticP (WrappedPafb f p) s t a b -> Optic p f s t a b
  toLens       :: Functor f                     => OpticP (Star f) s t a b -> LensLike f s t a b
  toSetter     :: Settable f                    => OpticP (Star f) s t a b -> LensLike f s t a b
  toTraversal  :: Applicative f                 => OpticP (Star f) s t a b -> LensLike f s t a b
  ```

  i.e. the "from" family turns one of this codebase's own concrete optic
  aliases (`Lens`/`Iso`/`Prism`/`Setter`/`Traversal`, `Linen.Control.Lens.
  {Lens,Iso,Prism,Setter,Traversal}`) into a genuinely `Profunctor`-
  polymorphic `OpticP`, and the "to" family runs the reverse conversion.

  **A pleasant simplification (`fromIso`/`fromPrism`, `Id` transparency).**
  Upstream's `fromIso`/`fromPrism` both run their argument at `F := Identity`
  and then thread `runIdentity`/`Identity` explicitly through `rmap` to strip
  the wrapper back off (`fromIso p pab = rmap runIdentity (p (rmap Identity
  pab))`). As `Linen.Control.Lens.Iso`'s own doc comment already notes for
  `withIso`, Lean's `Id` is *transparently* `Id α := α` (unlike Haskell's
  representationally-real `newtype Identity`), so instantiating an `Iso`/
  `Prism` at `F := Id` already *is* `OpticP`'s `p a b -> p s t` shape, with
  nothing left to strip — `fromIso`/`fromPrism` collapse to `l (F := Id)`
  outright.

  **Deviation (`fromSetter`, no explicit `Identity` wrapping either).** The
  same `Id`-transparency trick applies to `fromSetter`'s inner `s' f =
  runIdentity . s (Identity . f)`: instantiating the `Setter` (`Linen.
  Control.Lens.Setter`, whose `F` ranges over `Settable`, and `Id` is
  `Settable`) at `F := Id` already gives a plain `(A → B) → S → T` function,
  which is exactly `Mapping.roam`'s (`Linen.Control.Profunctor.Mapping`)
  expected argument shape with no further wrapping.

  **Scope note (`fromTraversal`, skipped).** Upstream's `fromTraversal l =
  wander (cloneTraversal l)` needs `ATraversal` (an already-"run" reified
  traversal) and `cloneTraversal`, built from the `Bazaar`/`Pretext`
  machinery that `Linen.Control.Lens.Internal.{Bazaar,Context}` and
  `Linen.Control.Lens.Traversal`'s own scope notes already document as not
  fully ported (no way to safely "replay" an already-applied optic at a
  different functor). `fromTraversal` is skipped here for the same reason
  `Linen.Control.Lens.Traversal`'s `cloneTraversal` is skipped; `toTraversal`
  (the reverse direction, needing none of that machinery) is ported below.
-/

import Linen.Control.Lens.Type
import Linen.Control.Lens.Lens
import Linen.Control.Lens.Iso
import Linen.Control.Lens.Prism
import Linen.Control.Lens.Setter
import Linen.Control.Lens.Internal.Context
import Linen.Control.Lens.Internal.Profunctor
import Linen.Control.Profunctor.Strong
import Linen.Control.Profunctor.Choice
import Linen.Control.Profunctor.Mapping
import Linen.Control.Profunctor.Types

open Control Control.Profunctor Control.Lens.Internal Data.Functor

namespace Control.Lens

-- ── OpticP ──────────────────────────────────────

/-- `OpticP p s t a b := p a b -> p s t`: a genuinely `Profunctor`-based
    optic, as opposed to the van Laarhoven `Optic`/`LensLike` shapes every
    other concrete optic alias in this codebase is built from. -/
abbrev OpticP (P : Type u → Type u → Type v) (S T A B : Type u) := P A B → P S T

-- ── from* : van Laarhoven → Profunctor ──────────

/-- `fromLens :: Strong p => LensLike (Context a b) s t a b -> OpticP p s t
    a b`: turn a `Lens` into a `Profunctor`-based one, by running it
    concretely at `F := Context A B` to recover the "peek"/"pos" pair a
    `Strong` profunctor's `second'` can thread a replacement through —
    `fromLens l p = dimap (\s => let ⟨f, a⟩ := l sell s; (f, a)) (fun x =>
    x.1 x.2) (second' p)`. -/
@[inline] def fromLens {P : Type u → Type u → Type u} [Strong P] {S T A B : Type u}
    (l : Lens S T A B) : OpticP P S T A B :=
  fun p =>
    Profunctor.dimap
      (fun s => let c := l (F := Context A B) Context.sell s; (c.peek, c.pos))
      (fun x => x.1 x.2)
      (Strong.second' p)

/-- `fromIso :: Profunctor p => Optic p Identity s t a b -> OpticP p s t a
    b`: run an `Iso` concretely at `F := Id`, which already lands exactly on
    `OpticP`'s shape (see the module doc comment's note on `Id`
    transparency). -/
@[inline] def fromIso {P : Type u → Type u → Type u} [Profunctor P] {S T A B : Type u}
    (l : Iso S T A B) : OpticP P S T A B :=
  l (F := Id)

/-- `fromPrism :: Choice p => Optic p Identity s t a b -> OpticP p s t a b`:
    run a `Prism` concretely at `F := Id`, the `Choice`-flavoured analogue of
    `fromIso`. -/
@[inline] def fromPrism {P : Type u → Type u → Type u} [Choice P] {S T A B : Type u}
    (l : Prism S T A B) : OpticP P S T A B :=
  l (F := Id)

/-- `fromSetter :: Mapping p => ASetter s t a b -> OpticP p s t a b`: run a
    `Setter` concretely at `F := Id` (`Id` is `Settable`) to recover a plain
    "modify everywhere" function, then lift it through `Mapping.roam`. -/
@[inline] def fromSetter {P : Type u → Type u → Type u} [Mapping P] {S T A B : Type u}
    (l : Setter S T A B) : OpticP P S T A B :=
  Mapping.roam (fun f s => l (F := Id) f s)

-- ── to* : Profunctor → van Laarhoven ────────────

/-- `toPrism :: (Choice p, Applicative f) => OpticP (WrappedPafb f p) s t a b
    -> Optic p f s t a b`: recover a `Prism`-shaped van Laarhoven optic from
    a `Profunctor`-based one, by running it at the wrapper `WrappedPafb F P`
    (`Linen.Control.Lens.Internal.Profunctor`) and stripping the wrapper back
    off. -/
@[inline] def toPrism {P : Type u → Type u → Type u} [Choice P] {F : Type u → Type u}
    [Applicative F] {S T A B : Type u} (p : OpticP (WrappedPafb F P) S T A B) :
    Optic P F S T A B :=
  fun pab => (p ⟨pab⟩).unwrapPafb

/-- `toIso :: (Profunctor p, Functor f) => OpticP (WrappedPafb f p) s t a b
    -> Optic p f s t a b`: the `Profunctor`-only (no `Choice` needed)
    analogue of `toPrism`. -/
@[inline] def toIso {P : Type u → Type u → Type u} [Profunctor P] {F : Type u → Type u}
    [Functor F] {S T A B : Type u} (p : OpticP (WrappedPafb F P) S T A B) :
    Optic P F S T A B :=
  fun pab => (p ⟨pab⟩).unwrapPafb

/-- `toLens :: Functor f => OpticP (Star f) s t a b -> LensLike f s t a b`:
    recover a bare van Laarhoven `LensLike` from a `Profunctor`-based optic,
    by running it at the concrete profunctor `Star F`. -/
@[inline] def toLens {F : Type u → Type u} [Functor F] {S T A B : Type u}
    (p : OpticP (Star F) S T A B) : LensLike F S T A B :=
  fun afb s => (p ⟨afb⟩).runStar s

/-- `toSetter :: Settable f => OpticP (Star f) s t a b -> LensLike f s t a
    b`: the `Settable`-constrained instance of `toLens`'s construction. -/
@[inline] def toSetter {F : Type u → Type u} [Settable F] {S T A B : Type u}
    (p : OpticP (Star F) S T A B) : LensLike F S T A B :=
  fun afb s => (p ⟨afb⟩).runStar s

/-- `toTraversal :: Applicative f => OpticP (Star f) s t a b -> LensLike f s
    t a b`: the `Applicative`-constrained instance of `toLens`'s
    construction. -/
@[inline] def toTraversal {F : Type u → Type u} [Applicative F] {S T A B : Type u}
    (p : OpticP (Star F) S T A B) : LensLike F S T A B :=
  fun afb s => (p ⟨afb⟩).runStar s

end Control.Lens
