/-
  Linen.Data.Array.Shaped.Shape — class of types usable as array shapes/indices

  Ported from Haskell's `Data.Array.Repa.Shape` (package `repa`). `sizeIsValid`
  is dropped: upstream describes it as "mostly used for writing QuickCheck
  tests" and it exists to guard against `Int` overflow when multiplying
  dimension sizes together, a concern that does not arise for Lean's
  unbounded `Int`. `deepSeq` is dropped too: it exists to force strict
  evaluation under GHC's laziness, which has no counterpart in Lean's
  call-by-value semantics.
-/

namespace Data.Array.Shaped

/-- Class of types that can be used as array shapes and indices. -/
class Shape (sh : Type) extends BEq sh where
  /-- Get the number of dimensions in a shape. -/
  rank : sh → Int
  /-- The shape of an array of size zero, with a particular dimensionality. -/
  zeroDim : sh
  /-- The shape of an array with size one, with a particular dimensionality. -/
  unitDim : sh
  /-- Compute the intersection of two shapes. -/
  intersectDim : sh → sh → sh
  /-- Add the coordinates of two shapes componentwise. -/
  addDim : sh → sh → sh
  /-- Get the total number of elements in an array with this shape. -/
  size : sh → Int
  /-- Convert an index into its equivalent flat, linear, row-major version. -/
  toIndex : sh → sh → Int
  /-- Inverse of `toIndex`. -/
  fromIndex : sh → Int → sh
  /-- Check whether an index is within a given range `[lo, hi)`. -/
  inShapeRange : sh → sh → sh → Bool
  /-- Convert a shape into its list of dimensions. -/
  listOfShape : sh → List Int
  /-- Convert a list of dimensions to a shape. -/
  shapeOfList : List Int → sh

/-- Check whether an index is a part of a given shape. -/
def inShape [Shape sh] (extent index : sh) : Bool :=
  Shape.inShapeRange Shape.zeroDim extent index

/-- Nicely format a shape as a string. -/
def showShape [Shape sh] (sh : sh) : String :=
  (Shape.listOfShape sh).foldr (fun n str => str ++ " :. " ++ toString n) "Z"

end Data.Array.Shaped
