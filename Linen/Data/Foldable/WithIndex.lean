/-
  Linen.Data.Foldable.WithIndex — indexed folds

  Port of Haskell's `Data.Foldable.WithIndex` from `indexed-traversable`
  (v0.1.5). `FoldableWithIndex i f` generalizes `Foldable` with access to a
  position/key index `i` while folding.

  ## Design

  Mirrors `Linen.Data.Foldable`'s own shape: the class's primitive is a right
  fold (`foldrWithIndex`, upstream's `ifoldr`), and `foldMapWithIndex`
  (upstream's `ifoldMap`) is a derived function built from it using
  `Append`/`Inhabited` in place of Haskell's `Monoid` (`linen` has no
  standalone `Monoid` class; see `Linen.Data.Foldable.foldMap` for the same
  substitution).

  ## Scope

  Upstream declares `class Foldable f => FoldableWithIndex i f`. `linen` has
  no `Foldable` instances yet for `Prod k`, `Std.HashMap k`, `Data.Map k`, or
  `Data.IntMap` (out of scope here, same reasoning as
  `Linen.Data.Functor.WithIndex`), so the `Foldable f` superclass constraint
  is dropped.

  `indexed-traversable-instances`' `Std.HashMap`/`Data.Map`/`Data.IntMap`
  instances are folded in here (see `docs/imports/indexed-traversable/dependencies.md`).

  Reference: https://hackage.haskell.org/package/indexed-traversable-0.1.5/docs/Data-Foldable-WithIndex.html
-/

import Linen.Data.Map
import Linen.Data.IntMap
import Linen.Data.List.NonEmpty
import Std.Data.HashMap

namespace Data.Foldable

-- ── Class ────────────────────────────────────────────────────────────

/-- A `Foldable`-like container with an additional index `i` (Haskell's
    `FoldableWithIndex i f`, functional dependency `f -> i` modeled by
    `outParam`).

    $$\text{foldrWithIndex}(f, z, [(i_1,x_1), \ldots, (i_n,x_n)])
      = f(i_1, x_1, f(i_2, x_2, \ldots f(i_n, x_n, z)))$$ -/
class WithIndex (i : outParam (Type u)) (f : Type u → Type u) where
  /-- Right fold over an indexed container with access to the index `i`. -/
  foldrWithIndex : (i → α → β → β) → β → f α → β

namespace WithIndex

/-- Map each element (with its index) into a semigroup (`Append`) and combine,
    starting from `default` (Haskell's `ifoldMap`, with `Monoid` replaced by
    `Append`/`Inhabited` — see `Linen.Data.Foldable.foldMap`). -/
@[inline] def foldMapWithIndex [WithIndex i f] [Append m] [Inhabited m]
    (g : i → α → m) (t : f α) : m :=
  foldrWithIndex (fun i a b => g i a ++ b) default t

end WithIndex
end Data.Foldable

-- ── Instances ────────────────────────────────────────────────────────

namespace Data.Foldable.WithIndex

/-- Lists are indexed by their `Nat` position (Haskell's `FoldableWithIndex Int []`). -/
instance : Data.Foldable.WithIndex Nat List where
  foldrWithIndex f z l := l.zipIdx.foldr (fun (a, i) acc => f i a acc) z

/-- `Option` carries at most one element, indexed by `Unit` (Haskell's
    `FoldableWithIndex () Maybe`). -/
instance : Data.Foldable.WithIndex Unit Option where
  foldrWithIndex f z
    | some a => f () a z
    | none => z

/-- A pair `k × α` is indexed by its (fixed) first component (Haskell's
    `FoldableWithIndex k ((,) k)`). -/
instance {k : Type u} : Data.Foldable.WithIndex k (Prod k) where
  foldrWithIndex f z p := f p.1 p.2 z

/-- A hash map is indexed by its keys, folded in an unspecified order (the
    underlying `Std.HashMap` is unordered). Also covers `Data.IntMap`. -/
instance {k : Type u} [BEq k] [Hashable k] :
    Data.Foldable.WithIndex k (fun v => Std.HashMap k v) where
  foldrWithIndex f z m := (Std.HashMap.toList m).foldr (fun (key, val) acc => f key val acc) z

/-- An ordered map is indexed by its keys, folded in ascending key order
    (Haskell's `FoldableWithIndex k (Map k)`). -/
instance {k : Type u} [Ord k] : Data.Foldable.WithIndex k (fun v => Data.Map k v) where
  foldrWithIndex := Data.Map.foldrWithKey

/-- A non-empty list is indexed by its `Nat` position, head at `0` (Haskell's
    `FoldableWithIndex Int NonEmpty`). -/
instance : Data.Foldable.WithIndex Nat List.NonEmpty where
  foldrWithIndex f z ne := f 0 ne.head ((ne.tail.zipIdx 1).foldr (fun (a, i) acc => f i a acc) z)

end Data.Foldable.WithIndex
