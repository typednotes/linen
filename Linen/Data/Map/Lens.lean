/-
  Linen.Data.Map.Lens — `Ixed`/`At` for `Linen.Data.Map`, `toMapOf`

  Port of Hackage's `lens-5.3.6`'s `Data.Map.Lens` (fetched and read via
  Hackage's rendered source). Upstream's real content:

  ```
  instance Ord k => Ixed (Map k v) where
    ix k f m = case Map.lookup k m of
      Just v  -> f v <&> \v' -> Map.insert k v' m
      Nothing -> pure m

  instance Ord k => At (Map k v) where
    at k f m = f mv <&> \r -> case r of
      Nothing -> maybe m (const (Map.delete k m)) mv
      Just v' -> Map.insert k v' m
      where mv = Map.lookup k m

  toMapOf :: Ord k => Getting (Map k v) s (k, v) -> s -> Map k v
  toMapOf l = views l (uncurry Map.singleton) -- (simplified: folds over pairs)
  ```

  translated against `Linen.Data.Map`'s `Map` (a `Lean.RBMap`-backed
  `abbrev`, whose `find?`/`insert`/`erase` are used directly via dot
  notation). -/

import Linen.Control.Lens.At
import Linen.Control.Lens.Indexed
import Linen.Data.Map

namespace Control.Lens

open Data (Map)

/-- `instance Ord k => Ixed (Map k v) where …`: reading/writing the value (if
    any) at key `k`. -/
instance instIxedMap {K V : Type u} [Ord K] : Ixed (Map K V) K V where
  ix k := fun {F} [Applicative F] (f : V → F V) (m : Map K V) =>
    match m.find? k with
    | some v => (fun v' => m.insert k v') <$> f v
    | none => pure m

/-- `instance Ord k => At (Map k v) where …`: writing `some v'` inserts/
    replaces the value at `k`; writing `none` deletes it. -/
instance instAtMap {K V : Type u} [Ord K] : At (Map K V) K V where
  «at» k := fun {F} [Functor F] (f : Option V → F (Option V)) (m : Map K V) =>
    (fun
      | some v' => m.insert k v'
      | none => m.erase k) <$> f (m.find? k)

/-- `toMapOf :: Ord k => Getting (Map k v) s (k, v) -> s -> Map k v`: collect
    every `(k, v)` pair an `IndexedFold` visits into a `Map`, later entries
    (in visitation order) overriding earlier ones with the same key —
    implemented via `itoListOf` (`Linen.Control.Lens.Indexed`) followed by
    `Data.Map.fromList`'s own left-to-right insertion order. -/
@[inline] def toMapOf {S K V : Type u} [Ord K] (l : IndexedFold K S V) (s : S) : Map K V :=
  (itoListOf l s).foldl (fun m (k, v) => m.insert k v) ∅

end Control.Lens
