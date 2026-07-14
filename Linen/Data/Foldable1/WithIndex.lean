/-
  Linen.Data.Foldable1.WithIndex — indexed folds over non-empty containers

  Port of Haskell's `Data.Foldable1.WithIndex` from `indexed-traversable`
  (v0.1.5). `Foldable1WithIndex i f` is the non-empty-witnessing variant of
  `Linen.Data.Foldable.WithIndex`: for a container statically known to hold
  at least one element, elements can be combined into a `Semigroup` directly,
  with no identity element (`mempty`) required.

  ## Design

  `linen` has no standalone `Semigroup` class (mirroring the lack of a
  `Monoid` class — see `Linen.Data.Foldable.foldMap`), so `Semigroup m` is
  represented the same way `Data.Foldable.WithIndex.foldMapWithIndex`
  represents `Monoid m`, minus the `Inhabited`/identity requirement: just
  `Append m`.

  ## Scope

  Upstream declares `class (Foldable1 f, FoldableWithIndex i f) =>
  Foldable1WithIndex i f`. `linen` has no standalone `Foldable1` class (no
  Haskell-style "non-empty `Foldable`" hierarchy exists here yet — the
  concept is only witnessed by concrete non-empty types like
  `Linen.Data.List.NonEmpty`), so that superclass is dropped; the
  `Data.Foldable.WithIndex i f` superclass is kept, since `List.NonEmpty`
  already has one (`Linen.Data.Foldable.WithIndex`).

  Reference: https://hackage.haskell.org/package/indexed-traversable-0.1.5/docs/Data-Foldable1-WithIndex.html
-/

import Linen.Data.Foldable.WithIndex

namespace Data.Foldable1

-- ── Class ────────────────────────────────────────────────────────────

/-- A non-empty `Foldable`-like container with an additional index `i`
    (Haskell's `Foldable1WithIndex i f`, functional dependency `f -> i`
    modeled by `outParam`).

    Unlike `Data.Foldable.WithIndex.foldMapWithIndex`, no identity element is
    required: $$\text{foldMap1WithIndex}(f, [(i_1,x_1),\ldots,(i_n,x_n)])
      = f(i_1,x_1) \mathbin{\ast} \cdots \mathbin{\ast} f(i_n,x_n)$$
    for $n \geq 1$, where $\ast$ is the `Append` (semigroup) operation. -/
class WithIndex (i : outParam (Type u)) (f : Type u → Type u)
    [Data.Foldable.WithIndex i f] where
  /-- Map each element (with its index) into a semigroup (`Append`) and
      combine — total because `f` is statically non-empty, so no identity
      element is needed (Haskell's `ifoldMap1`). -/
  foldMap1WithIndex {m : Type v} [Append m] : (i → α → m) → f α → m

end Data.Foldable1

-- ── Instances ────────────────────────────────────────────────────────

namespace Data.Foldable1.WithIndex

/-- A non-empty list is indexed by its `Nat` position, head at `0` (Haskell's
    `Foldable1WithIndex Int NonEmpty`). -/
instance : Data.Foldable1.WithIndex Nat List.NonEmpty where
  foldMap1WithIndex f ne :=
    (ne.tail.zipIdx 1).foldl (fun acc (a, i) => acc ++ f i a) (f 0 ne.head)

end Data.Foldable1.WithIndex
