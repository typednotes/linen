/-
  Tests for `Linen.Data.Complex`.

  Algebraic operations over `Complex Int`, plus the conjugation-involution and
  addition-commutativity proofs.
-/
import Linen.Data.Complex

open Data

namespace Tests.Data.Complex

def z1 : Complex Int := ⟨1, 2⟩
def z2 : Complex Int := ⟨3, 4⟩

/-! ### Components / constructors -/

#guard z1.re == 1 && z1.im == 2
#guard Complex.ofReal (5 : Int) == ⟨5, 0⟩
#guard (Complex.i : Complex Int) == ⟨0, 1⟩

/-! ### Arithmetic -/

#guard (z1 + z2) == ⟨4, 6⟩
#guard (-z1) == ⟨-1, -2⟩
#guard (z2 - z1) == ⟨2, 2⟩
#guard (z1 * z2) == ⟨-5, 10⟩                      -- (1·3 − 2·4, 1·4 + 2·3)

/-! ### Conjugate / magnitude -/

#guard Complex.conjugate z1 == ⟨1, -2⟩
#guard Complex.magnitudeSquared (⟨3, 4⟩ : Complex Int) == 25       -- 3² + 4²
#guard z1 * Complex.conjugate z1 == ⟨Complex.magnitudeSquared z1, 0⟩   -- z·z̄ = (|z|², 0)

/-! ### Instances -/

#guard toString z1 == "1 + 2i"

/-! ### Proofs (compile-time) -/

example (z : Complex Int) : Complex.conjugate (Complex.conjugate z) = z :=
  Complex.conjugate_conjugate (fun x => by omega) z
example (z w : Complex Int) : z + w = w + z :=
  Complex.add_comm' (fun a b => by omega) z w

end Tests.Data.Complex
