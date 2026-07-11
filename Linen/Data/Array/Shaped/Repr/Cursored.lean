/-
  Linen.Data.Array.Shaped.Repr.Cursored — the `Cursored` array representation

  Ported from Haskell's `Data.Array.Repa.Repr.Cursored` (package `repa`).
  Cursored arrays are produced by `repa`'s stencil functions to help share
  index computations between neighbouring elements: instead of recomputing
  a full index from scratch for every read, a "cursor" to one element can be
  *shifted* to reach a nearby element more cheaply.

  Upstream's `Array C sh a` existentially quantifies over the cursor type
  (`forall cursor. ACursored { ... }`), so consumers of a `Cursored` array
  never need to know what the cursor type actually is. Lean has no
  existential-type syntax, but the same effect is achieved by storing the
  cursor type itself as a `Type`-valued field — `Cursored sh e` then lives
  one universe higher than `sh`/`e`, which is why `Source` (see `Base.lean`)
  is universe-polymorphic in its result.

  The `Load`/`LoadRange` instances (`Eval.Cursored`'s
  `fillCursoredBlock2S`/`fillCursoredBlock2P`) are dropped along with every
  other `Load` instance — materializing a `Cursored` array into a `Manifest`
  is just `computeS`, which only needs the `Source` instance below.
-/

import Linen.Data.Array.Shaped.Base

namespace Data.Array.Shaped

/-- A cursored array: a shape, together with a way to make a cursor to an
    index, shift a cursor by an offset, and read the element at a cursor. -/
structure Cursored (sh e : Type) where
  /-- The (existentially hidden) cursor type. -/
  cursor : Type
  extent : sh
  /-- Make a cursor to a particular element. -/
  makeCursor : sh → cursor
  /-- Shift a cursor by an offset, to get to another element. -/
  shiftCursor : sh → cursor → cursor
  /-- Load/compute the element at the given cursor. -/
  loadCursor : cursor → e

instance : Source Cursored where
  extent a := a.extent
  linearIndex a i := a.loadCursor (a.makeCursor (Shape.fromIndex a.extent i))

/-- Define a new cursored array. -/
def makeCursored {sh e} (cursor : Type) (sh' : sh)
    (makeCursor : sh → cursor) (shiftCursor : sh → cursor → cursor)
    (loadCursor : cursor → e) : Cursored sh e :=
  ⟨cursor, sh', makeCursor, shiftCursor, loadCursor⟩

end Data.Array.Shaped
