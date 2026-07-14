/-
  Linen.Data.Functor.WithIndex — indexed functors

  Port of Haskell's `Data.Functor.WithIndex` from `indexed-traversable`
  (v0.1.5). `FunctorWithIndex i f` generalizes `Functor` with access to a
  position/key index `i` while mapping. Upstream declares the class with a
  functional dependency `f -> i` (the index type is determined by the
  container); this is modeled with an `outParam`, the same pattern
  `Linen.Codec.Picture.Types.Pixel` uses for its `Component` parameter.

  ## Scope

  Upstream declares `class Functor f => FunctorWithIndex i f`. `linen` has no
  `Functor` instances yet for `Prod k`, `Std.HashMap k`, `Data.Map k`, or
  `Data.IntMap` — adding those belongs to a `containers`-style `Functor`
  instance port, out of scope for `indexed-traversable` itself — so the
  `Functor f` superclass constraint is dropped here; `WithIndex` stands
  alone. This does not weaken `mapWithIndex` itself, only the unrelated
  requirement that `f` already be a plain `Functor`.

  `indexed-traversable-instances`' `Std.HashMap`/`Data.Map`/`Data.IntMap`
  instances are folded in here (see `docs/imports/indexed-traversable/dependencies.md`).

  Reference: https://hackage.haskell.org/package/indexed-traversable-0.1.5/docs/Data-Functor-WithIndex.html
-/

import Linen.Data.Map
import Linen.Data.IntMap
import Linen.Data.List.NonEmpty
import Std.Data.HashMap

namespace Data.Functor

-- ── Class ────────────────────────────────────────────────────────────

/-- A functor with an additional index `i` (Haskell's `FunctorWithIndex i f`,
    functional dependency `f -> i` modeled by `outParam`).

    Instances should satisfy the modified functor laws:
    $$\text{mapWithIndex}\;f \circ \text{mapWithIndex}\;g = \text{mapWithIndex}\;(\lambda i.\;f\,i \circ g\,i)$$
    $$\text{mapWithIndex}\;(\lambda \_\,a.\,a) = \text{id}$$ -/
class WithIndex (i : outParam (Type u)) (f : Type u → Type u) where
  /-- Map with access to the index. -/
  mapWithIndex : (i → α → β) → f α → f β

end Data.Functor

-- ── Instances ────────────────────────────────────────────────────────

namespace Data.Functor.WithIndex

/-- Lists are indexed by their `Nat` position (Haskell's `FunctorWithIndex Int []`). -/
instance : Data.Functor.WithIndex Nat List where
  mapWithIndex := List.mapIdx

/-- `Option` carries at most one element, indexed by `Unit` (Haskell's
    `FunctorWithIndex () Maybe`). -/
instance : Data.Functor.WithIndex Unit Option where
  mapWithIndex f
    | some a => some (f () a)
    | none => none

/-- A pair `k × α` is indexed by its (fixed) first component (Haskell's
    `FunctorWithIndex k ((,) k)`). -/
instance {k : Type u} : Data.Functor.WithIndex k (Prod k) where
  mapWithIndex f p := (p.1, f p.1 p.2)

/-- A hash map is indexed by its keys. Also covers `Data.IntMap`, which is
    `Std.HashMap Nat` (Haskell's `FunctorWithIndex Int IntMap` is thus the
    `k := Nat` case of this instance, not a separate declaration). -/
instance {k : Type u} [BEq k] [Hashable k] :
    Data.Functor.WithIndex k (fun v => Std.HashMap k v) where
  mapWithIndex := Std.HashMap.map

/-- An ordered map is indexed by its keys (Haskell's `FunctorWithIndex k (Map k)`). -/
instance {k : Type u} [Ord k] : Data.Functor.WithIndex k (fun v => Data.Map k v) where
  mapWithIndex := Data.Map.mapWithKey

/-- A non-empty list is indexed by its `Nat` position, head at `0` (Haskell's
    `FunctorWithIndex Int NonEmpty`). -/
instance : Data.Functor.WithIndex Nat List.NonEmpty where
  mapWithIndex f ne := ⟨f 0 ne.head, ne.tail.mapIdx (fun i a => f (i + 1) a)⟩

end Data.Functor.WithIndex
