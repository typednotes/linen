/-
  Tests for `Linen.Control.Lens.Indexed`.
-/
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Fold

open Control.Lens Control.Lens.Internal

namespace Tests.Linen.Control.Lens.Indexed

-- `imap`/`ifoldr`/`ifoldl`/`ifoldMap` — plain wrappers over
-- `Data.{Functor,Foldable}.WithIndex`, indexed by list position.
#guard imap (fun i a => a + i) [10, 20, 30] = [10, 21, 32]
#guard ifoldr (fun i a acc => i + a + acc) 0 [10, 20, 30] = 63
#guard ifoldl (fun i acc a => i + a + acc) 0 [10, 20, 30] = 63
#guard ifoldMap (M := List Nat) (fun i a => [i * a]) [1, 2, 3] = [0, 2, 6]

-- `itraverse` — direct, non-optic form.
#guard itraverse (fun i a => if a > 0 then some (a + i) else none) [1, 2, 3] = some [1, 3, 5]
#guard itraverse (fun i a => if a > 0 then some (a + i) else none) [1, 0, 3] = none

-- `itraversed`, run as an `IndexedTraversal` at `P := Indexed Nat`.
#guard itraversed (F := Option) (P := Indexed Nat)
    (Indexed.mk (fun i a => if a > 0 then some (a + i) else none)) [1, 2, 3] = some [1, 3, 5]

-- `imapped`, run as an `IndexedSetter` at `F := Id`, `P := Indexed Nat`.
#guard Id.run (imapped (F := Id) (P := Indexed Nat) (Indexed.mk (fun i a => a + i)) [10, 20, 30])
  = [10, 21, 32]

-- `ifolded`/`itoListOf` — collecting `(index, value)` pairs.
#guard itoListOf ifolded [10, 20, 30] = [(0, 10), (1, 20), (2, 30)]

-- `withIndex`/`asIndex` — turning an `IndexedFold` into an ordinary `Fold`.
#guard toListOf (withIndex ifolded) [10, 20, 30] = [(0, 10), (1, 20), (2, 30)]
#guard toListOf (asIndex ifolded) [10, 20, 30] = [0, 1, 2]

-- `indexing` — turning the plain `traversed` `Traversal` into an indexed one,
-- numbering elements in traversal order from `0`; run directly at `F := Id`
-- (adding each element's index to itself) without going through the
-- universally-`F`-quantified `IndexedTraversal` abbreviation.
#guard Id.run (indexing traversed (F := Id) (Indexed.mk (fun i a => a + i)) [10, 20, 30])
  = [10, 21, 32]

-- `icompose`/`(<.>)` — composing two `IndexedTraversal`s, pairing indices.
#guard itoListOf
    ((itraversed : IndexedTraversal Nat (List (List Nat)) (List (List Nat)) (List Nat) (List Nat))
      <.> (itraversed : IndexedTraversal Nat (List Nat) (List Nat) Nat Nat))
    [[1, 2], [3]]
  = [((0, 0), 1), ((0, 1), 2), ((1, 0), 3)]

-- `(.>)` — composing an `IndexedTraversal` with a plain `Traversal`, keeping
-- only the outer index.
#guard itoListOf
    ((itraversed : IndexedTraversal Nat (List (List Nat)) (List (List Nat)) (List Nat) (List Nat))
      .> (traversed : Traversal (List Nat) (List Nat) Nat Nat))
    [[1, 2], [3]]
  = [(0, 1), (0, 2), (1, 3)]

end Tests.Linen.Control.Lens.Indexed
