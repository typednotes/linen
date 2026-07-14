/-
  Linen.Data.Traversable.WithIndex — indexed traversals

  Port of Haskell's `Data.Traversable.WithIndex` from `indexed-traversable`
  (v0.1.5). `TraversableWithIndex i t` generalizes `Traversable` with access
  to a position/key index `i` while traversing — the class
  `Control.Lens.Indexed`/`Control.Lens.Traversal` (in `lens`) actually build
  indexed optics on top of.

  ## Scope

  Upstream declares `class (FunctorWithIndex i t, FoldableWithIndex i t,
  Traversable t) => TraversableWithIndex i t`. `linen`'s own
  `Linen.Data.Traversable.Traversable` has no instances yet for `Prod k`,
  `Std.HashMap k`, `Data.Map k`, or `Data.IntMap` (out of scope here, same
  reasoning as `Linen.Data.Functor.WithIndex`/`Linen.Data.Foldable.WithIndex`),
  so the `Traversable t` superclass is dropped. The
  `Data.Functor.WithIndex i t`/`Data.Foldable.WithIndex i t` superclasses are
  kept — every container instantiated below already has both.

  Each instance here implements `traverseWithIndex` directly against its
  container's own API, rather than porting upstream's generic `Int`-counting
  `Indexing` newtype default (an implementation detail for deriving
  `itraverse` from a plain `traverse`); the observable behavior — traverse
  left-to-right, threading the index alongside each element — is the same.

  `indexed-traversable-instances`' `Std.HashMap`/`Data.Map`/`Data.IntMap`
  instances are folded in here (see `docs/imports/indexed-traversable/dependencies.md`).

  Reference: https://hackage.haskell.org/package/indexed-traversable-0.1.5/docs/Data-Traversable-WithIndex.html
-/

import Linen.Data.Functor.WithIndex
import Linen.Data.Foldable.WithIndex

namespace Data.Traversable

-- ── Class ────────────────────────────────────────────────────────────

/-- A `Traversable`-like structure with an additional index `i` (Haskell's
    `TraversableWithIndex i t`, functional dependency `t -> i` modeled by
    `outParam`).

    $$\text{traverseWithIndex} : (i \to \alpha \to G\,\beta) \to t\,\alpha \to G\,(t\,\beta)$$ -/
class WithIndex (i : outParam (Type u)) (t : Type u → Type u)
    [Data.Functor.WithIndex i t] [Data.Foldable.WithIndex i t] where
  /-- Traverse an indexed structure, applying an effectful function to each
      element (with its index) and collecting results left to right. -/
  traverseWithIndex {G : Type u → Type u} [Applicative G] : (i → α → G β) → t α → G (t β)

end Data.Traversable

-- ── Instances ────────────────────────────────────────────────────────

namespace Data.Traversable.WithIndex

/-- Lists are indexed by their `Nat` position (Haskell's `TraversableWithIndex Int []`). -/
instance : Data.Traversable.WithIndex Nat List where
  traverseWithIndex f l :=
    l.zipIdx.foldr (fun (a, i) acc => (· :: ·) <$> f i a <*> acc) (pure [])

/-- `Option` carries at most one element, indexed by `Unit` (Haskell's
    `TraversableWithIndex () Maybe`). -/
instance : Data.Traversable.WithIndex Unit Option where
  traverseWithIndex f
    | some a => some <$> f () a
    | none => pure none

/-- A pair `k × α` is indexed by its (fixed) first component (Haskell's
    `TraversableWithIndex k ((,) k)`). -/
instance {k : Type u} : Data.Traversable.WithIndex k (Prod k) where
  traverseWithIndex f p := (fun b => (p.1, b)) <$> f p.1 p.2

/-- A hash map is indexed by its keys, traversed in an unspecified order.
    Also covers `Data.IntMap`. -/
instance {k : Type u} [BEq k] [Hashable k] :
    Data.Traversable.WithIndex k (fun v => Std.HashMap k v) where
  traverseWithIndex f m :=
    (Std.HashMap.toList m).foldr
      (fun (key, val) acc => (fun val' m' => Std.HashMap.insert m' key val') <$> f key val <*> acc)
      (pure ∅)

/-- An ordered map is indexed by its keys, traversed in ascending key order
    (Haskell's `TraversableWithIndex k (Map k)`). -/
instance {k : Type u} [Ord k] : Data.Traversable.WithIndex k (fun v => Data.Map k v) where
  traverseWithIndex f m :=
    Data.Map.foldrWithKey
      (fun key val acc => (fun val' m' => Data.Map.insert' key val' m') <$> f key val <*> acc)
      (pure Data.Map.empty) m

/-- A non-empty list is indexed by its `Nat` position, head at `0` (Haskell's
    `TraversableWithIndex Int NonEmpty`). -/
instance : Data.Traversable.WithIndex Nat List.NonEmpty where
  traverseWithIndex f ne :=
    let hd := f 0 ne.head
    let tl := (ne.tail.zipIdx 1).foldr (fun (a, i) acc => (· :: ·) <$> f i a <*> acc) (pure [])
    Data.List.NonEmpty.mk <$> hd <*> tl

end Data.Traversable.WithIndex
