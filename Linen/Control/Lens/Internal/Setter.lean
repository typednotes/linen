/-
  Linen.Control.Lens.Internal.Setter — `Settable`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Setter` (fetched
  and read via Hackage's rendered source). `Settable` is the class every
  functor `f` must satisfy to be safely threaded through a write-only
  traversal (`Control.Lens.Setter`'s `over`/`set`, `Control.Lens.Fold`'s
  `backwards`, …): `f` has to be *isomorphic to the `Identity` functor*, so
  that "running" a setter through it can't observe anything beyond the
  single value it carries. Upstream's own instances are `Identity`
  (obviously isomorphic to itself), `Backwards f` (reverses effect order but
  is still carrier-isomorphic), and `Compose f g` of two `Settable`s.
  `Data.Functor.Const`, by contrast, deliberately has **no** `Settable`
  instance: it discards its covariant slot entirely, so it cannot recover
  the value a setter would have written — exactly the failure mode
  `Settable` exists to rule out.

  **Scope note (`Mutator`).** Real upstream does not use GHC's own
  `Data.Functor.Identity` for the `Identity` instance directly; it
  re-wraps it as a local `newtype Mutator a = Mutator { runMutator :: a }`
  purely so that a `Settable` instance doesn't have to live at
  `Data.Functor.Identity`'s definition site (an orphan-instance concern that
  doesn't exist in Lean). Structurally `Mutator` *is* `Identity` — same
  single-field carrier, same `Functor`/`Applicative` behaviour — so per this
  codebase's existing convention (`Linen.Control.Lens.Internal.Instances`'s
  `Data.Traversable Id` instance, itself commented on there) this port
  gives `Settable` an instance for Lean core's `Id` directly rather than
  manufacturing a `Mutator` wrapper with nothing to distinguish it.

  **Scope note (superclasses).** Upstream's real class head is
  `class (Applicative f, Distributive f, Traversable f) => Settable f`.
  `linen` has no `Distributive` class at all (see the scope notes in
  `Linen.Control.Profunctor.{Mapping,Rep,Types,Cayley}` and
  `Linen.Control.Lens.Internal.Indexed`, none of which port one either), and
  no method below — `untainted`/`untaintedDot`/`taintedDot` — calls into
  `Traversable` or `Distributive` at all; they only need `Applicative`. This
  port keeps just the superclass every method actually uses, matching the
  precedent set by `Conjoined` in `Linen.Control.Lens.Internal.Indexed`.

  **Scope note (`Backwards`).** `linen` has not ported `Control.Applicative.
  Backwards` (its sole consumer, the `backwards` fold combinator, lives in
  `Control.Lens.Fold`, out of scope until a later batch), so no `Settable
  Backwards` instance is given here. -/

import Linen.Control.Profunctor.Unsafe
import Linen.Data.Functor

open Control Control.Profunctor Data.Functor

namespace Control.Lens.Internal

/-- A functor safe to thread through a write-only traversal: it must be
    isomorphic to the identity functor, so running a setter through it can
    only ever recover the single value that was written — never observe,
    duplicate, or drop anything else. See the module docstring for how this
    simplifies upstream's real superclass list. -/
class Settable (F : Type u → Type u) extends Applicative F where
  /-- Recover the underlying value, witnessing that `F` is isomorphic to the
      identity functor. -/
  untainted : F α → α

namespace Settable

/-- Strip a `Settable` layer off a profunctor's second argument: given
    `p a (f b)`, recover `p a b` by post-composing with `untainted`. -/
@[inline] def untaintedDot {F : Type u → Type u} [Settable F] {P : Type u → Type u → Type v}
    [Profunctor P] (p : P α (F β)) : P α β :=
  Profunctor.rmap Settable.untainted p

/-- Add a `Settable` layer to a profunctor's second argument: given
    `p a b`, recover `p a (f b)` by post-composing with `pure`. -/
@[inline] def taintedDot {F : Type u → Type u} [Settable F] {P : Type u → Type u → Type v}
    [Profunctor P] (p : P α β) : P α (F β) :=
  Profunctor.rmap (Pure.pure : β → F β) p

end Settable

/-- `Id` is `Settable`: it already *is* the identity functor, so
    `untainted` is the identity function (see the module's `Mutator` scope
    note for why no separate wrapper type is introduced here). -/
instance : Settable Id where
  untainted a := a

/-- The composition of two `Settable` functors is `Settable`: unwrap both
    layers in turn. -/
instance {F : Type u → Type u} {G : Type u → Type u} [Settable F] [Settable G] :
    Settable (Compose F G) where
  untainted c := Settable.untainted (Settable.untainted c.getCompose)

end Control.Lens.Internal
