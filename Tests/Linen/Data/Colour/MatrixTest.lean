/-
  Tests for `Linen.Data.Colour.Matrix` — dense 3×3 matrices and 3-vectors.
-/
import Linen.Data.Colour.Matrix

open Data.Colour

namespace Tests.Data.Colour.Matrix

def identity : Matrix3 := ⟨⟨1, 0, 0⟩, ⟨0, 1, 0⟩, ⟨0, 0, 1⟩⟩

#guard Matrix3.mult identity ⟨1, 2, 3⟩ == (⟨1, 2, 3⟩ : Vec3)
#guard Matrix3.determinant identity == 1
#guard Matrix3.inverse identity == identity
#guard Matrix3.matrixMult identity ⟨⟨2, 0, 0⟩, ⟨0, 2, 0⟩, ⟨0, 0, 2⟩⟩ ==
  (⟨⟨2, 0, 0⟩, ⟨0, 2, 0⟩, ⟨0, 0, 2⟩⟩ : Matrix3)
#guard Matrix3.transpose ⟨⟨1, 2, 3⟩, ⟨4, 5, 6⟩, ⟨7, 8, 9⟩⟩ ==
  (⟨⟨1, 4, 7⟩, ⟨2, 5, 8⟩, ⟨3, 6, 9⟩⟩ : Matrix3)

end Tests.Data.Colour.Matrix
