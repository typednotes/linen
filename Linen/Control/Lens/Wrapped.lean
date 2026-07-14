/-
  Linen.Control.Lens.Wrapped — `Wrapped`, `_Wrapped'`, `_Unwrapped'`,
  `_Wrapped`, `_Unwrapped`, `_Wrapping'`, `_Unwrapping'`, `_Wrapping`,
  `_Unwrapping`, `op`, `ala`, `alaf`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Wrapped` (fetched and read
  via the real source, not recalled from memory). Upstream's own
  description: `Wrapped` "provides similar functionality as
  `Control.Newtype`, from the `newtype` package, but in a more convenient
  and efficient form" — an `Iso` between a single-constructor wrapper type
  and the value it wraps.

  Upstream's real core:

  ```
  class Wrapped s where
    type Unwrapped s :: Type
    _Wrapped' :: Iso' s (Unwrapped s)
  ```

  translated here as `Linen.Control.Lens.Tuple`'s (`Field1`..`Field9`) and a
  not-yet-ported `Linen.Control.Lens.At`'s (`Ixed`/`At`) settled `outParam`
  idiom for a Haskell associated type: `Unwrapped` becomes a second,
  `outParam`-marked class parameter rather than a genuine `Type`-family
  member, since Lean has no associated-type-family construct to translate
  it to directly.

  **Deviation (`Rewrapped`/`Rewrapping`, replaced by two plain `Wrapped`
  constraints).** Upstream additionally declares a two-parameter, no-method
  marker class `class Wrapped s => Rewrapped (s :: Type) (t :: Type)` (with
  `Rewrapping s t := (Rewrapped s t, Rewrapped t s)`), used to constrain
  every combinator below that changes the wrapped type
  (`_Wrapped`/`_Unwrapped`/`_Wrapping`/`_Unwrapping`/`ala`/`alaf`). In GHC,
  `Rewrapped`'s entire job is to let the *functional dependencies* on
  `Wrapped`'s associated type drive inference of `t` from `s` (and vice
  versa) at each of those combinators' call sites — `Rewrapped`/`Rewrapping`
  carry no information beyond "`s` and `t` are each individually `Wrapped`"
  once that inference problem is solved another way. Lean's `outParam`
  already *is* that "another way": `[Wrapped S US] [Wrapped T UT]` pins down
  `US`/`UT` from `S`/`T` exactly as upstream's functional dependencies pin
  down `Unwrapped s`/`Unwrapped t`, with no separate marker class needed to
  drive it. Every combinator below that upstream states with a `Rewrapping
  s t` constraint is therefore stated here with `[Wrapped S US] [Wrapped T
  UT]` directly — the same substitution, not a loss of any call site (there
  is no use of `Rewrapped`/`Rewrapping` upstream beyond exactly this).

  **Scope note (pattern synonyms `Wrapped`/`Unwrapped`, `_GWrapped'`).**
  Upstream also exports two `PatternSynonyms`-based pattern synonyms (`pattern
  Wrapped a <- (view _Wrapped -> a) where Wrapped a = review _Wrapped a`, and
  its `Unwrapped` counterpart) letting a value be pattern-matched as though
  its wrapper were a real constructor, and a `Generic`-based default
  implementation `_GWrapped'` for `_Wrapped'` derived structurally from a
  type's `Generic` instance. Lean has no view-pattern-backed pattern-synonym
  construct to translate the former to, and no runtime-`Generic`-derivation
  machinery backing the latter (`linen` never ports GHC-generics-based
  defaults elsewhere either); both are skipped.

  **Deviation (`ala`/`alaf`, routed through `au`/`auf` rather than
  `xplat`/`xplatf`).** Upstream defines `ala f = xplat $ _Unwrapping f` and
  `alaf f = xplatf $ _Unwrapping f`, where `xplat`/`xplatf`
  (`Control.Lens.Iso`) are in turn defined as `xplat = au . from` /
  `xplat f g = xplatf f g id` — i.e. `xplat`/`xplatf` are themselves just
  `au`/`auf` precomposed with `from` on their `Iso` argument. `linen`'s
  `Linen.Control.Lens.Iso` ports `au`/`auf` but not `xplat`/`xplatf`
  (unneeded there, with no call site until this module). Rather than adding
  `xplat`/`xplatf` to a module this batch does not touch, `ala`/`alaf` are
  defined directly against `au`/`auf` composed with `from (_Unwrapping f) =
  _Wrapping f` (`from` is involutive on an `Iso`), which is definitionally
  the same computation upstream's `xplat`/`xplatf` route through: `ala f =
  au (_Wrapping f)`, `alaf f = auf (_Wrapping f)`. -/

import Linen.Control.Lens.Iso
import Linen.Data.Newtype

namespace Control.Lens

-- ── Wrapped ─────────────────────────────────────

/-- `class Wrapped s where type Unwrapped s; _Wrapped' :: Iso' s (Unwrapped
    s)`: an isomorphism between a single-constructor wrapper type `S` and
    the value `Unwrapped` it wraps. `Unwrapped` is an `outParam`, modeling
    upstream's associated type (see the module doc comment). -/
class Wrapped (S : Type u) (Unwrapped : outParam (Type u)) where
  _Wrapped' : Iso' S Unwrapped

export Wrapped (_Wrapped')

-- ── _Unwrapped' / _Wrapping' / _Unwrapping' ─────

/-- `_Unwrapped' :: Wrapped s => Iso' (Unwrapped s) s`: the reverse of
    `_Wrapped'` — `_Unwrapped' = from _Wrapped'`. -/
@[inline] def _Unwrapped' {S US : Type u} [Wrapped S US] : Iso' US S :=
  «from» _Wrapped'

/-- `_Wrapping' :: Wrapped s => (Unwrapped s -> s) -> Iso' s (Unwrapped s)`:
    a convenience version of `_Wrapped'` taking a constructor argument whose
    only role is to pin down `s` (its value is otherwise ignored) —
    `_Wrapping' _ = _Wrapped'`. -/
@[inline] def _Wrapping' {S US : Type u} [Wrapped S US] (_ : US → S) : Iso' S US :=
  _Wrapped'

/-- `_Unwrapping' :: Wrapped s => (Unwrapped s -> s) -> Iso' (Unwrapped s)
    s`: the `_Unwrapped'` counterpart of `_Wrapping'` —
    `_Unwrapping' _ = from _Wrapped'`. -/
@[inline] def _Unwrapping' {S US : Type u} [Wrapped S US] (_ : US → S) : Iso' US S :=
  «from» _Wrapped'

-- ── _Wrapped / _Unwrapped ────────────────────────

/-- `_Wrapped :: Rewrapping s t => Iso s t (Unwrapped s) (Unwrapped t)`: work
    under a newtype wrapper, allowing the wrapped type to change —
    `_Wrapped = withIso _Wrapped' $ \sa _ -> withIso _Wrapped' $ \_ bt -> iso
    sa bt` (see the module doc comment for `Rewrapping s t`'s replacement by
    `[Wrapped S US] [Wrapped T UT]`). -/
@[inline] def _Wrapped {S T US UT : Type u} [Wrapped S US] [Wrapped T UT] :
    Iso S T US UT :=
  withIso (_Wrapped' (S := S)) (fun sa _ =>
    withIso (_Wrapped' (S := T)) (fun _ bt => iso sa bt))

/-- `_Unwrapped :: Rewrapping s t => Iso (Unwrapped t) (Unwrapped s) t s`:
    the reverse of `_Wrapped` — `_Unwrapped = from _Wrapped`. -/
@[inline] def _Unwrapped {S T US UT : Type u} [Wrapped S US] [Wrapped T UT] :
    Iso UT US T S :=
  «from» (_Wrapped (S := S) (T := T))

/-- `_Wrapping :: Rewrapping s t => (Unwrapped s -> s) -> Iso s t (Unwrapped
    s) (Unwrapped t)`: the `_Wrapped` counterpart of `_Wrapping'` —
    `_Wrapping _ = _Wrapped`. -/
@[inline] def _Wrapping {S T US UT : Type u} [Wrapped S US] [Wrapped T UT]
    (_ : US → S) : Iso S T US UT :=
  _Wrapped

/-- `_Unwrapping :: Rewrapping s t => (Unwrapped s -> s) -> Iso (Unwrapped t)
    (Unwrapped s) t s`: the `_Unwrapped` counterpart of `_Wrapping` —
    `_Unwrapping _ = from _Wrapped`. -/
@[inline] def _Unwrapping {S T US UT : Type u} [Wrapped S US] [Wrapped T UT]
    (_ : US → S) : Iso UT US T S :=
  «from» (_Wrapped (S := S) (T := T))

-- ── op ───────────────────────────────────────────

/-- `op :: Wrapped s => (Unwrapped s -> s) -> s -> Unwrapped s`: given the
    constructor for a `Wrapped` type, return a deconstructor that is its
    inverse — `op _ = view _Wrapped'`, implemented here directly via
    `withIso` (extracting `_Wrapped'`'s forward direction) rather than
    routing through a `Getting`-shaped `view`, matching how `_Wrapped'`
    itself is always run in this module. Laws: `op f . f = id`, `f . op f =
    id`. -/
@[inline] def op {S US : Type u} [Wrapped S US] (_ : US → S) (s : S) : US :=
  withIso _Wrapped' (fun sa _ => sa s)

-- ── ala / alaf ───────────────────────────────────

/-- `ala :: (Functor f, Rewrapping s t) => (Unwrapped s -> s) -> ((Unwrapped
    t -> t) -> f s) -> f (Unwrapped s)`: McBride's `ala` combinator — run a
    `Foldable`/`Functor`-shaped action `e` polymorphically in a wrapper
    constructor, then unwrap the result; e.g. `ala Sum foldMap [1,2,3,4] =
    10`. `ala f = xplat $ _Unwrapping f` upstream; here `xplat = au . from`
    unfolds directly against `_Wrapping f = from (_Unwrapping f)` (`from` is
    involutive on an `Iso`), giving `ala f = au (_Wrapping f)` — see the
    module doc comment. -/
@[inline] def ala {S T US UT : Type u} {F : Type u → Type u} [Functor F]
    [Wrapped S US] [Wrapped T UT] (f : US → S) (g : (UT → T) → F S) : F US :=
  au (_Wrapping (S := S) (T := T) f) g

/-- `alaf :: (Functor f, Functor g, Rewrapping s t) => (Unwrapped s -> s) ->
    (f t -> g s) -> f (Unwrapped t) -> g (Unwrapped s)`: the `auf`-flavoured
    counterpart of `ala`, additionally re-wrapping an `f`-shaped argument
    before handing it to the supplied function; e.g. `alaf Sum foldMap
    Prelude.length ["hello","world"] = 10`. `alaf f = xplatf $ _Unwrapping
    f` upstream, unfolding the same way as `ala` to `alaf f = auf (_Wrapping
    f)`. -/
@[inline] def alaf {S T US UT : Type u} {F G : Type u → Type u} [Functor F] [Functor G]
    [Wrapped S US] [Wrapped T UT] (f : US → S) (g : F T → G S) (fb : F UT) : G US :=
  auf (_Wrapping (S := S) (T := T) f) g fb

-- ── Instances: Linen.Data.Newtype's monoid wrappers ─

/-- `instance Wrapped (Dual a) where type Unwrapped (Dual a) = a; _Wrapped'
    = iso getDual Dual`. -/
instance {A : Type u} : Wrapped (Data.Dual A) A where
  _Wrapped' := iso Data.Dual.getDual Data.Dual.mk

/-- `instance Wrapped (Sum a) where type Unwrapped (Sum a) = a; _Wrapped' =
    iso getSum Sum`. -/
instance {A : Type u} : Wrapped (Data.Sum A) A where
  _Wrapped' := iso Data.Sum.getSum Data.Sum.mk

/-- `instance Wrapped (Product a) where type Unwrapped (Product a) = a;
    _Wrapped' = iso getProduct Product`. -/
instance {A : Type u} : Wrapped (Data.Product A) A where
  _Wrapped' := iso Data.Product.getProduct Data.Product.mk

/-- `instance Wrapped All where type Unwrapped All = Bool; _Wrapped' = iso
    getAll All`. -/
instance : Wrapped Data.All Bool where
  _Wrapped' := iso Data.All.getAll Data.All.mk

/-- `instance Wrapped Any where type Unwrapped Any = Bool; _Wrapped' = iso
    getAny Any`. -/
instance : Wrapped Data.Any Bool where
  _Wrapped' := iso Data.Any.getAny Data.Any.mk

end Control.Lens
