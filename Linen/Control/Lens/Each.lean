/-
  Linen.Control.Lens.Each — `Each`, `each`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Each` (fetched and read via
  Hackage's rendered Haddock and source). Upstream's real class:

  ```
  class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
    each :: Traversal s t a b
    default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
    each = traverse
  ```

  `each` is a `Traversal` visiting *every* element of a container at once —
  the same shape `traversed` (`Linen.Control.Lens.Traversal`) already gives
  any `Data.Traversable`, but packaged as a class so it can be dispatched by
  the container's *type* alone (useful for containers, like tuples, whose
  `traverse` isn't itself `Data.Traversable.traverse`).

  **Representation (`outParam`).** As with `Linen.Control.Lens.Tuple`'s
  `Field1`..`Field9`, upstream's functional dependencies (`s -> a`, `t -> b`,
  `s b -> t`, `t a -> s`) are modeled with `S`/`B` as the class's real
  inputs and `A`/`T` as `outParam`s computed from them.

  **Deviation (no conditional `default` method).** Unlike a Haskell class
  method, a Lean `class` field's default value cannot demand an *extra*
  hypothesis (`Traversable g`) beyond the class's own parameters, so
  upstream's `default each = traverse` cannot be written once and inherited;
  each `Data.Traversable`-backed instance below instead calls
  `Data.Traversable.traverse` directly in its own `each` — observably the
  exact same body the default would have produced.

  **Scope note (containers).** Upstream also gives `each` for `Map`,
  `IntMap`, `HashMap`, `Seq`, `Text`, `ByteString`, `Vector` (several
  flavours), `Array`/`UArray`, `Complex`, `Tree`, `These`, and the strict
  variants of `Either`/`Pair`/`Maybe`/`These` — `linen` has ported none of
  those container types. Only the containers `linen` already has a
  `Data.Traversable` instance for (`List`, `Option`) plus Lean's native
  `Array` (traversed via a `List` round-trip, since `Data.Traversable` has no
  `Array` instance of its own) are ported here.

  **Scope note (tuple arity).** Upstream gives one instance per GHC tuple
  arity, 2-tuple through 9-tuple, each requiring every component (and every
  replacement) to share one uniform type — e.g. `instance Each (a,a) (a',a')
  a a' where each f ~(x,y) = (,) <$> f x <*> f y`. This port keeps the same
  uniform-type shape but, per this batch's guidance, stops at the 4-tuple
  (`Prod`'s only native arities before nesting becomes indistinguishable from
  reusing `Each` on the tail) rather than restating the same pattern out to
  9; `Field1`..`Field9`-style recursive derivation does not apply here, since
  (unlike `Field`, which reads *one* position) `each` must visit *every*
  position of the tuple at once and there is no single "recursive case"
  shared by every arity below the cap. -/

import Linen.Control.Lens.Type
import Linen.Data.Traversable

namespace Control.Lens

-- ── Each ────────────────────────────────────────

/-- `class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where each ::
    Traversal s t a b`: a `Traversal` visiting every element of a container
    at once, dispatched on the container's type. `S`/`B` are the class's real
    inputs; `A`/`T` are `outParam`s computed from them, modeling upstream's
    functional dependencies (see `Linen.Control.Lens.Tuple`'s `Field1` for
    the identical pattern). -/
class Each (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  each : Traversal S T A B

export Each (each)

-- ── List / Option / Array ───────────────────────

/-- `instance Traversable f => Each [a] [b] a b where each = traverse`. -/
instance instEachList {A B : Type u} : Each (List A) B A (List B) where
  each := fun {F} [Applicative F] afb l => Data.Traversable.traverse afb l

/-- `instance Each (Maybe a) (Maybe b) a b where each = traverse`. -/
instance instEachOption {A B : Type u} : Each (Option A) B A (Option B) where
  each := fun {F} [Applicative F] afb o => Data.Traversable.traverse afb o

/-- Lean's native `Array`, traversed by round-tripping through `List` (the
    container `Data.Traversable` actually provides an instance for), then
    rebuilding an `Array` from the result — same elements, same order, as
    traversing the array directly. -/
instance instEachArray {A B : Type u} : Each (Array A) B A (Array B) where
  each := fun {F} [Applicative F] afb arr =>
    (fun l => l.toArray) <$> Data.Traversable.traverse afb arr.toList

-- ── tuples ───────────────────────────────────────

/-- `instance Each (a,a) (a',a') a a' where each f ~(x,y) = (,) <$> f x <*> f
    y`. -/
instance instEachPair {A B : Type u} : Each (A × A) B A (B × B) where
  each := fun {F} [Applicative F] afb p => Prod.mk <$> afb p.1 <*> afb p.2

/-- `instance Each (a,a,a) (a',a',a') a a' where each f ~(x,y,z) = (,,) <$> f
    x <*> f y <*> f z`. -/
instance instEachTriple {A B : Type u} : Each (A × A × A) B A (B × B × B) where
  each := fun {F} [Applicative F] afb p =>
    (fun x y z => (x, y, z)) <$> afb p.1 <*> afb p.2.1 <*> afb p.2.2

/-- `instance Each (a,a,a,a) (a',a',a',a') a a' where each f ~(w,x,y,z) =
    (,,,) <$> f w <*> f x <*> f y <*> f z`. -/
instance instEachQuadruple {A B : Type u} : Each (A × A × A × A) B A (B × B × B × B) where
  each := fun {F} [Applicative F] afb p =>
    (fun w x y z => (w, x, y, z)) <$> afb p.1 <*> afb p.2.1 <*> afb p.2.2.1 <*> afb p.2.2.2

end Control.Lens
