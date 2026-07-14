/-
  Linen.Data.Set.Lens — `Ixed`/`At`/`AsEmpty` for `Linen.Data.Set`,
  `setmapped`, `setOf`

  Port of Hackage's `lens-5.3.6`'s `Data.Set.Lens` (fetched and read via
  Hackage's rendered source). Upstream's real content:

  ```
  setmapped :: Ord j => IndexPreservingSetter (Set i) (Set j) i j
  setmapped = sets Set.map

  setOf :: Ord a => Getting (Set a) s a -> s -> Set a
  setOf l = views l Set.singleton  -- (simplified: folds every focused element)
  ```

  translated against `Linen.Data.Set`'s `Set'` (a `Lean.RBMap`-backed
  `abbrev`, named `Set'` in `linen` to avoid clashing with Lean's own
  `Set`). `Ixed`/`At` are additionally given here (upstream places its own
  `Set`/`IntSet`/`HashSet` "presence" access behind the separate `Contains`
  class, which `linen`'s `Linen.Control.Lens.At` explicitly defers — see
  `Linen.Data.HashSet.Lens`'s identical note for why `At` stands in for it
  here, with `IxValue = Unit`). -/

import Linen.Control.Lens.At
import Linen.Control.Lens.Empty
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Setter
import Linen.Data.Set

namespace Control.Lens

open Data (Set')

/-- `Ixed (Set' K) K Unit`: an element is present (focusable as `Unit`) or
    entirely absent, mirroring `Linen.Data.HashSet.Lens`'s identical
    `Ixed`. Narrowed to `Type` (rather than a universe-polymorphic `Type
    u`), since `Ixed`'s `IxValue` out-param must share `Set' K`'s own
    universe, and `Unit` is fixed at `Type`. -/
instance instIxedSet {K : Type} [Ord K] : Ixed (Set' K) K Unit where
  ix k := fun {F} [Applicative F] (f : Unit → F Unit) (s : Set' K) =>
    if s.contains k then (fun _ => s) <$> f () else pure s

/-- `At (Set' K) K Unit`: writing `some ()` inserts `k`; writing `none`
    deletes it — see the module doc comment. -/
instance instAtSet {K : Type} [Ord K] : At (Set' K) K Unit where
  «at» k := fun {F} [Functor F] (f : Option Unit → F (Option Unit)) (s : Set' K) =>
    (fun
      | some _ => Data.Set'.insert' k s
      | none => s.erase k) <$> f (if s.contains k then some () else none)

/-- `AsEmpty (Set' K)` — `nearly ∅ isEmpty`, using `Data.Set'.null`. -/
instance instAsEmptySet {K : Type} [Ord K] : AsEmpty (Set' K) where
  _Empty := nearly (∅ : Set' K) Data.Set'.null

/-- `setmapped :: Ord j => IndexPreservingSetter (Set i) (Set j) i j` —
    `sets Set.map`, over `Linen.Data.Set`'s `mapSet`. -/
@[inline] def setmapped {I J : Type} [Ord I] [Ord J] : Setter (Set' I) (Set' J) I J :=
  sets Data.Set'.mapSet

/-- `setOf :: Ord a => Getting (Set a) s a -> s -> Set a`: collect every
    element an `IndexedFold` visits into a `Set'` — implemented via
    `itoListOf` (`Linen.Control.Lens.Indexed`), discarding the index. -/
@[inline] def setOf {S A : Type} [Ord A] (l : IndexedFold Unit S A) (s : S) : Set' A :=
  let pairs : List (Unit × A) := itoListOf l s
  Data.Set'.fromList (pairs.map Prod.snd)

end Control.Lens
