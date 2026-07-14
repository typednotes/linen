/-
  Linen.Control.Lens.Empty — `AsEmpty`, `_Empty`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Empty` (fetched and read via
  Hackage's rendered Haddock and source). Upstream's real class:

  ```
  class AsEmpty a where
    _Empty :: Prism' a ()
    default _Empty :: (Monoid a, Eq a) => Prism' a ()
    _Empty = only mempty
  ```

  together with a `PatternSynonym`, `pattern Empty <- (has _Empty -> True)
  where Empty = review _Empty ()`, letting `Empty` be matched/constructed
  like an ordinary data constructor for any `AsEmpty` type.

  **Deviation (no conditional `default` method).** As with
  `Linen.Control.Lens.Each`'s identical note, a Lean `class` field's default
  cannot demand an extra `(Monoid a, Eq a)` hypothesis beyond the class's own
  parameter, so upstream's `default _Empty = only mempty` has no direct
  counterpart here; every instance below instead gives `_Empty` directly via
  `Linen.Control.Lens.Prism.nearly`, matching what upstream's own hand-written
  instances (`nearly [] null`, `nearly Map.empty Map.null`, …) already do in
  preference to the `Monoid`-based default for every container-shaped type.

  **Scope note (`pattern Empty`).** Lean has no `PatternSynonyms`-style
  mechanism to turn an arbitrary `Prism`-backed predicate into something
  usable directly in `match`/pattern position; `linen` has no existing
  precedent for this either (no other ported module manufactures a `match`-
  usable pattern from an optic). Skipped; `_Empty` together with
  `Linen.Control.Lens.Fold.has`/`Linen.Control.Lens.Review.review` already
  gives the same test/build capability the pattern synonym packages, just
  spelled `has _Empty s`/`review _Empty ()` instead of `s is Empty`/`Empty`.

  **Scope note (containers).** Upstream also gives `AsEmpty` for `Ordering`,
  `()`, `Any`/`All`/`Sum`/`Product`/`Dual`/`ZipList` (from `Data.Monoid`),
  `GHC.Event.Event`, tuples up to some arity, `Map`/`IntMap`/`HashMap`/`Set`/
  `HashSet`/`IntSet`, `Seq`, `Text`/`ByteString` (strict and lazy), several
  `Vector` flavours, and the `Data.Strict`/`Data.These` variants added in
  4.18/4.20 — `linen` has ported none of the `Data.Monoid` newtypes, `Map`/
  `Set`-family containers, `Text`/`ByteString`, or `Vector`. This port gives
  instances for every container-shaped type `linen` already has a natural
  "empty"/`null` pair for: `List`, `Option`, `String`, and Lean's native
  `Array`. -/

import Linen.Control.Lens.Prism

namespace Control.Lens

-- ── AsEmpty ─────────────────────────────────────

/-- `class AsEmpty a where _Empty :: Prism' a ()`: a type with a
    distinguished "empty" value that a `Prism'` can recognize/build. Fixed at
    a concrete `Type` (rather than the ambient `Type u` used elsewhere in
    this batch), since `Prism' a Unit` needs `Unit` and `a` to share one
    universe — the same accommodation `Linen.Control.Lens.Prism`'s `only`/
    `nearly`/`_Nothing` already make, for the same reason. -/
class AsEmpty (A : Type) where
  _Empty : Prism' A Unit

export AsEmpty (_Empty)

-- ── List / Option / String / Array ──────────────

/-- `instance AsEmpty [a] where _Empty = nearly [] Prelude.null`. -/
instance instAsEmptyList {A : Type} : AsEmpty (List A) where
  _Empty := nearly [] List.isEmpty

/-- `instance AsEmpty (Maybe a) where _Empty = _Nothing`. -/
instance instAsEmptyOption {A : Type} : AsEmpty (Option A) where
  _Empty := _Nothing

/-- Not one of upstream's own instances (upstream's `Text`/`ByteString`
    instances have no `String` counterpart to port, `linen` having ported
    neither), but the same `nearly empty null`-shaped pattern applied to
    Lean's native `String`, matching this module's own scope note on
    covering every container `linen` already has a natural "empty" test
    for. -/
instance instAsEmptyString : AsEmpty String where
  _Empty := nearly "" String.isEmpty

/-- Lean's native `Array`, the same `nearly empty null` shape as every other
    instance in this module. -/
instance instAsEmptyArray {A : Type} : AsEmpty (Array A) where
  _Empty := nearly #[] Array.isEmpty

end Control.Lens
