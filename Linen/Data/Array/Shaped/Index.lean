/-
  Linen.Data.Array.Shaped.Index — index/shape types and their `Shape` instances

  Ported from Haskell's `Data.Array.Repa.Index` (package `repa`). Haskell's
  infix type/data operator `:.` becomes the `Snoc` structure here (Lean
  identifiers can't be built from `:` and `.`), re-exposed through the `:.`
  notation below so that shape/index expressions still read exactly as they
  do upstream, e.g. `Z.Z :. y :. x`.
-/

import Linen.Data.Array.Shaped.Shape

namespace Data.Array.Shaped

-- ── Index types ──────────────────────────────────────────────────

/-- An index of dimension zero. -/
inductive Z : Type where
  | Z : Z
deriving BEq, Repr, Inhabited

/-- Extend a shape/index by one more dimension. -/
structure Snoc (tail head : Type) where
  tail : tail
  head : head
deriving BEq, Repr, Inhabited

/-- Extend a shape/index by one more dimension: `sh :. n`. -/
infixl:65 " :. " => Snoc.mk

-- Common dimensions
abbrev DIM0 := Z
abbrev DIM1 := Snoc DIM0 Int
abbrev DIM2 := Snoc DIM1 Int
abbrev DIM3 := Snoc DIM2 Int
abbrev DIM4 := Snoc DIM3 Int
abbrev DIM5 := Snoc DIM4 Int

/-- Helper for index construction, constraining the coordinate to `Int`. -/
def ix1 (x : Int) : DIM1 := Z.Z :. x

def ix2 (y x : Int) : DIM2 := Z.Z :. y :. x

def ix3 (z y x : Int) : DIM3 := Z.Z :. z :. y :. x

def ix4 (a z y x : Int) : DIM4 := Z.Z :. a :. z :. y :. x

def ix5 (b a z y x : Int) : DIM5 := Z.Z :. b :. a :. z :. y :. x

-- ── Shape instances ──────────────────────────────────────────────

instance : Shape Z where
  rank _ := 0
  zeroDim := Z.Z
  unitDim := Z.Z
  intersectDim _ _ := Z.Z
  addDim _ _ := Z.Z
  size _ := 1
  toIndex _ _ := 0
  fromIndex _ _ := Z.Z
  inShapeRange _ _ _ := true
  listOfShape _ := []
  shapeOfList
    | [] => Z.Z
    | _ => panic! "Data.Array.Shaped.Index.shapeOfList: non-empty list when converting to Z"

instance [Shape sh] [Inhabited sh] : Shape (Snoc sh Int) where
  rank | sh :. _ => Shape.rank sh + 1
  zeroDim := Shape.zeroDim :. 0
  unitDim := Shape.unitDim :. 1
  intersectDim
    | sh1 :. n1, sh2 :. n2 => Shape.intersectDim sh1 sh2 :. min n1 n2
  addDim
    | sh1 :. n1, sh2 :. n2 => Shape.addDim sh1 sh2 :. (n1 + n2)
  size | sh1 :. n => Shape.size sh1 * n
  toIndex
    | sh1 :. sh2, sh1' :. sh2' => Shape.toIndex sh1 sh1' * sh2 + sh2'
  fromIndex
    | ds :. d, n =>
      let r := if Shape.rank ds == 0 then n else Int.tmod n d
      Shape.fromIndex ds (Int.tdiv n d) :. r
  inShapeRange
    | zs :. z, sh1 :. n1, sh2 :. n2 =>
      n2 >= z && n2 < n1 && Shape.inShapeRange zs sh1 sh2
  listOfShape | sh :. n => n :: Shape.listOfShape sh
  shapeOfList
    | [] => panic! "Data.Array.Shaped.Index.shapeOfList: empty list when converting to (_ :. Int)"
    | x :: xs => Shape.shapeOfList xs :. x

end Data.Array.Shaped
