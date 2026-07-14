/-
  Linen.Data.Bits.Lens — `bitAt`, `bits`

  Port of Hackage's `lens-5.3.6`'s `Data.Bits.Lens` (fetched and read via
  Hackage's rendered source). Upstream's real content:

  ```
  bitAt :: Bits b => Int -> Lens' b Bool
  bitAt n = lens (`testBit` n) (\x b -> if b then x `setBit` n else x `clearBit` n)

  bits :: (Bits b, Bits b') => IndexPreservingSetter b b' Bool Bool
  bits = ...
  ```

  translated against `Linen.Data.Bits`'s `Bits`/`FiniteBits` classes (which
  already give `testBit`/`setBit`/`clearBit`, in `Bits.setBit`/`clearBit`).

  **Deviation (`Int` → `Nat`).** Upstream's `bitAt :: Int -> Lens' b Bool`
  can be called with a negative index, which every real `Bits` instance
  treats as simply "never set" (`testBit`/`setBit`/`clearBit` all silently
  no-op or return `False` outside the representable range on GHC's own
  instances). `Nat` cannot be negative in the first place, matching this
  batch's identical `Ixed (List A) Nat A` narrowing in
  `Linen.Control.Lens.At`.

  **Deviation (`bits`, `IndexedTraversal'` in place of `IndexPreservingSetter`
  over two possibly-different `Bits` types).** Upstream's `bits` is
  polymorphic in *both* the source and target `Bits` type (`b`/`b'`),
  letting a caller convert between, say, an `Int` and a `Word` while
  setting/clearing bits. `linen` has no cross-type `Bits`-to-`Bits`
  conversion machinery, and every real use of `bits` in practice sets `b =
  b'`. This port gives `bits` as an `IndexedTraversal' Nat B Bool` over a
  single `FiniteBits` type instead (visiting every bit position from `0` to
  `finiteBitSize - 1`), built by iterating `List.range finiteBitSize` and
  reusing `Data.Traversable.traverse` (`Linen.Data.Traversable`) exactly as
  `Linen.Control.Lens.Each`'s own `instEachArray` already does for a
  container with no bespoke recursion of its own — needing no new
  termination proof. -/

import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Internal.Indexed
import Linen.Control.Lens.Lens
import Linen.Data.Bits
import Linen.Data.Traversable

open Control.Lens.Internal

namespace Control.Lens

open Data (Bits FiniteBits)

/-- `bitAt :: Bits b => Int -> Lens' b Bool`: read/write whether bit `n` is
    set — `bitAt n = lens (testBit n) (\x b -> if b then setBit x n else
    clearBit x n)`, narrowed to a `Nat` index (see the module doc comment).
    Narrowed to `Type` (rather than a universe-polymorphic `Type u`), since
    `Lens' B Bool` forces `B` and `Bool` into the same universe. -/
@[inline] def bitAt {B : Type} [Bits B] (n : Nat) : Lens' B Bool :=
  lens (fun b => Bits.testBit b n)
       (fun b set => if set then Data.Bits.setBit b n else Data.Bits.clearBit b n)

/-- `bits :: (Bits b, Bits b') => IndexPreservingSetter b b' Bool Bool`: an
    `IndexedTraversal'` visiting every bit position of a `FiniteBits` value,
    indexed by that position — see the module doc comment for why a single
    `FiniteBits` type (rather than upstream's two, possibly different,
    `Bits` types) is used here. -/
@[inline] def bits {B : Type} [FiniteBits B] : IndexedTraversal' Nat B Bool :=
  fun {F} [Applicative F] {P} [Indexable Nat P] (pab : P Bool (F Bool)) (b : B) =>
    let positions := List.range (FiniteBits.finiteBitSize (α := B))
    (fun (results : List Bool) =>
        (positions.zip results).foldl
          (fun acc (p, set) => if set then Data.Bits.setBit acc p else Data.Bits.clearBit acc p)
          b)
      <$> Data.Traversable.traverse (fun p => Indexable.indexed pab p (Bits.testBit b p)) positions

end Control.Lens
