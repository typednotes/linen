/-
  Linen.Data.Array.Shaped.Stencil.Base — basic definitions for stencil
  handling

  Ported from Haskell's `Data.Array.Repa.Stencil.Base` (package `repa`).
-/

import Linen.Data.Array.Shaped.Index

namespace Data.Array.Shaped

/-- How to handle the case when the stencil lies partly outside the array. -/
inductive Boundary (a : Type) where
  /-- Use a fixed value for border regions. -/
  | fixed (c : a)
  /-- Treat points outside the array as having a constant value. -/
  | const (c : a)
  /-- Clamp points outside to the same value as the edge pixel. -/
  | clamp

/-- Represents a convolution stencil that we can apply to an array. Only
    statically known stencils are supported, matching upstream's single
    `StencilStatic` constructor. -/
structure Stencil (sh a : Type) where
  /-- Extent of the stencil. -/
  extent : sh
  /-- The zero/identity value to start accumulating from. -/
  zero : a
  /-- Accumulate the coefficient at an index (relative to the stencil's
      focus) against the corresponding array value and the running total. -/
  acc : sh → a → a → a

/-- Make a stencil from a function yielding coefficients at each index. -/
def makeStencil {sh a} [Add a] [Mul a] [OfNat a 0]
    (ex : sh) (getCoeff : sh → Option a) : Stencil sh a :=
  ⟨ex, 0, fun ix val acc =>
    match getCoeff ix with
    | none => acc
    | some coeff => acc + val * coeff⟩

/-- Wrapper for `makeStencil` that requires a `DIM2` stencil. -/
def makeStencil2 {a} [Add a] [Mul a] [OfNat a 0]
    (height width : Int) (getCoeff : DIM2 → Option a) : Stencil DIM2 a :=
  makeStencil (ix2 height width) getCoeff

end Data.Array.Shaped
