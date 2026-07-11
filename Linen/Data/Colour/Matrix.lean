/-
  Linen.Data.Colour.Matrix — dense 3×3 matrices and 3-vectors

  ## Haskell equivalent
  `Data.Colour.Matrix` (internal to
  https://hackage.haskell.org/package/colour, not itself exported)

  ## Design
  Upstream represents matrices and vectors as plain lists (`[[a,b,c],...]`
  and `[a,b,c]`) and pattern-matches on exactly three rows/columns —
  `colour` only ever forms 3×3 matrices, for RGB↔XYZ colour-space
  transforms. A list-based port would have to either leave those matches
  non-exhaustive or fail loudly on the wrong shape, neither of which is
  available under this repo's no-`partial`/no-`sorry` rule, so a fixed-size
  `Vec3`/`Matrix3` structure is used instead: every match is total by
  construction.

  As with `Chan`, upstream is generic over any `Fractional`/`Real` numeric
  representation, and every module that uses it instantiates that at a
  floating-point type in practice, so both structures are specialized to
  `Float` directly.
-/

namespace Data.Colour

/-- A 3-dimensional coordinate triple (an XYZ or RGB colour-space
    coordinate, or a row/column of a `Matrix3`). -/
structure Vec3 where
  x : Float
  y : Float
  z : Float
  deriving Repr, BEq

namespace Vec3

/-- The dot product of two vectors. -/
def dot (u v : Vec3) : Float := u.x * v.x + u.y * v.y + u.z * v.z

/-- Scale a vector by a scalar. -/
def scale (s : Float) (v : Vec3) : Vec3 := ⟨s * v.x, s * v.y, s * v.z⟩

end Vec3

/-- A dense, row-major 3×3 matrix. -/
structure Matrix3 where
  r0 : Vec3
  r1 : Vec3
  r2 : Vec3
  deriving Repr, BEq

namespace Matrix3

/-- Transpose a matrix, swapping rows and columns. -/
def transpose (m : Matrix3) : Matrix3 :=
  ⟨⟨m.r0.x, m.r1.x, m.r2.x⟩, ⟨m.r0.y, m.r1.y, m.r2.y⟩, ⟨m.r0.z, m.r1.z, m.r2.z⟩⟩

/-- Multiply a matrix by a column vector. -/
def mult (l : Matrix3) (x : Vec3) : Vec3 :=
  ⟨Vec3.dot l.r0 x, Vec3.dot l.r1 x, Vec3.dot l.r2 x⟩

/-- Multiply two matrices. -/
def matrixMult (l m : Matrix3) : Matrix3 :=
  let mt := transpose m
  transpose ⟨mult l mt.r0, mult l mt.r1, mult l mt.r2⟩

/-- The determinant of a matrix. -/
def determinant (m : Matrix3) : Float :=
  let a := m.r0.x; let b := m.r0.y; let c := m.r0.z
  let d := m.r1.x; let e := m.r1.y; let f := m.r1.z
  let g := m.r2.x; let h := m.r2.y; let i := m.r2.z
  a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)

/-- The inverse of a matrix. -/
def inverse (m : Matrix3) : Matrix3 :=
  let a := m.r0.x; let b := m.r0.y; let c := m.r0.z
  let d := m.r1.x; let e := m.r1.y; let f := m.r1.z
  let g := m.r2.x; let h := m.r2.y; let i := m.r2.z
  let det := determinant m
  ⟨⟨(e * i - f * h) / det, -(b * i - c * h) / det, (b * f - c * e) / det⟩,
   ⟨-(d * i - f * g) / det, (a * i - c * g) / det, -(a * f - c * d) / det⟩,
   ⟨(d * h - e * g) / det, -(a * h - b * g) / det, (a * e - b * d) / det⟩⟩

end Matrix3
end Data.Colour
