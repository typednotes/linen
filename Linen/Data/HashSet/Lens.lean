/-
  Linen.Data.HashSet.Lens — `Ixed`/`At`/`AsEmpty`/`Each` for `Std.HashSet`

  Port of Hackage's `lens-5.3.6`'s `Data.HashSet.Lens` (fetched and read via
  Hackage's rendered source). Upstream's real content:

  ```
  instance Eq a => Ixed (HashSet a) where
    ix k f s
      | S.member k s = f () <&> \() -> s
      | otherwise    = pure s
  ```

  i.e. `Ixed` alone, over `Data.HashSet`'s membership-testing `Set`-shaped
  container (`IxValue (HashSet a) = ()`: an element is either "present" —
  focusable as `()` — or entirely absent). `linen` has no ported
  `Data.HashSet`; per the batch plan this targets Lean's own `Std.HashSet`
  (from Lean's core `Std` library) instead.

  **Deviation (`At`, added beyond upstream).** Upstream's own
  `Data.HashSet.Lens` gives only `Ixed`, deferring insert/delete-capable
  access to `Contains` (`Control.Lens.At`, itself deferred by `linen`'s own
  `Linen.Control.Lens.At` to this later batch — see that module's scope
  note). Since `Std.HashSet.insert`/`.erase` already give `linen` everything
  `At`'s `Lens' M (Option Unit)` needs (report/insert/delete presence, the
  same job `Contains`'s `Lens' M Bool` does with `Bool` instead of `Option
  Unit`), an `At` instance is given here directly rather than adding a
  separate `Contains` class purely for `HashSet` alone — matching this
  module's `Ixed` in spirit (`IxValue = Unit`) while giving genuine
  insert/delete capability through the already-ported `At` class instead of
  a new one.

  **Scope note (`AsEmpty`, `each`).** A `Std.HashSet` also naturally has an
  "empty" test (`Linen.Control.Lens.Empty`'s `AsEmpty`) and an "every
  element" traversal (`Linen.Control.Lens.Each`'s `Each`); both are given
  here too, round-tripping through `toList`/`ofList` for `each` exactly as
  `Linen.Control.Lens.Each`'s own `instEachArray` already does for `Array`
  (`Std.HashSet` has no in-place element-rewriting primitive of its own to
  traverse against directly). -/

import Linen.Control.Lens.At
import Linen.Control.Lens.Each
import Linen.Control.Lens.Empty
import Linen.Data.Traversable
import Std.Data.HashSet

namespace Control.Lens

/-- `instance Eq a => Ixed (HashSet a) where ix k f s | member k s = f () <&>
    \() -> s | otherwise = pure s`: an element is present (focusable as
    `Unit`) or entirely absent — writing through `ix` when present leaves
    `s` unchanged (there is nothing to overwrite but presence itself). -/
instance instIxedHashSet {A : Type} [BEq A] [Hashable A] :
    Ixed (Std.HashSet A) A Unit where
  ix k := fun {F} [Applicative F] (f : Unit → F Unit) (s : Std.HashSet A) =>
    if s.contains k then (fun _ => s) <$> f () else pure s

/-- `At (HashSet a)`: writing `some ()` through `at k` inserts `k`; writing
    `none` deletes it — see the module doc comment for why this stands in
    for upstream's separate `Contains` class here. -/
instance instAtHashSet {A : Type} [BEq A] [Hashable A] :
    At (Std.HashSet A) A Unit where
  «at» k := fun {F} [Functor F] (f : Option Unit → F (Option Unit)) (s : Std.HashSet A) =>
    (fun
      | some _ => s.insert k
      | none => s.erase k) <$> f (if s.contains k then some () else none)

/-- `AsEmpty (HashSet a)` — `nearly ∅ isEmpty`. -/
instance instAsEmptyHashSet {A : Type} [BEq A] [Hashable A] : AsEmpty (Std.HashSet A) where
  _Empty := nearly ∅ Std.HashSet.isEmpty

/-- `Each (HashSet a) (HashSet b) a b` — round-trips through `toList`/
    `ofList`, matching `Linen.Control.Lens.Each`'s `instEachArray`. -/
instance instEachHashSet {A B : Type} [BEq A] [Hashable A] [BEq B] [Hashable B] :
    Each (Std.HashSet A) B A (Std.HashSet B) where
  each := fun {F} [Applicative F] (afb : A → F B) (s : Std.HashSet A) =>
    (fun l => Std.HashSet.ofList l) <$> Data.Traversable.traverse afb s.toList

end Control.Lens
