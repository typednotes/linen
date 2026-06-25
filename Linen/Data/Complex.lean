/-
  Linen.Data.Complex — Complex numbers

  Complex numbers over an arbitrary type, with algebraic operations and proofs.
  Lean's core library has no generic complex type, so this ports Haskell's
  `Data.Complex` shape, parameterised over any type with the relevant numeric
  structure.

  $$z = a + bi \quad \text{where } a, b : \alpha$$
-/

namespace Data

/-- A complex number $z = \text{re} + \text{im} \cdot i$ over a type `α`.

    $$\mathbb{C}(\alpha) = \{ a + bi \mid a, b \in \alpha \}$$

    Provides addition, multiplication, conjugation, and magnitude squared.
-/
structure Complex (α : Type u) where
  /-- The real part: $\text{Re}(z)$. -/
  re : α
  /-- The imaginary part: $\text{Im}(z)$. -/
  im : α
deriving BEq, Ord, Repr, Hashable

namespace Complex

instance [Inhabited α] : Inhabited (Complex α) where
  default := ⟨default, default⟩

instance [ToString α] : ToString (Complex α) where
  toString z := s!"{z.re} + {z.im}i"

/-- Construct a purely real complex number: $a + 0i$. -/
@[inline] def ofReal [OfNat α 0] (a : α) : Complex α := ⟨a, 0⟩

/-- The imaginary unit: $i = 0 + 1i$. -/
@[inline] def i [OfNat α 0] [OfNat α 1] : Complex α := ⟨0, 1⟩

/-- Addition of complex numbers:
    $$(a + bi) + (c + di) = (a + c) + (b + d)i$$
-/
instance [Add α] : Add (Complex α) where
  add z w := ⟨z.re + w.re, z.im + w.im⟩

/-- Negation: $-(a + bi) = -a + (-b)i$. -/
instance [Neg α] : Neg (Complex α) where
  neg z := ⟨-z.re, -z.im⟩

/-- Subtraction: $(a + bi) - (c + di) = (a - c) + (b - d)i$. -/
instance [Sub α] : Sub (Complex α) where
  sub z w := ⟨z.re - w.re, z.im - w.im⟩

/-- Multiplication of complex numbers:
    $$(a + bi)(c + di) = (ac - bd) + (ad + bc)i$$
-/
instance [Add α] [Sub α] [Mul α] : Mul (Complex α) where
  mul z w := ⟨z.re * w.re - z.im * w.im, z.re * w.im + z.im * w.re⟩

/-- Complex conjugate: $\overline{a + bi} = a - bi$.

    Satisfies $z \cdot \overline{z} = |z|^2$. -/
def conjugate [Neg α] (z : Complex α) : Complex α := ⟨z.re, -z.im⟩

/-- Magnitude squared (norm squared):
    $$|z|^2 = \text{Re}(z)^2 + \text{Im}(z)^2 = z \cdot \overline{z}$$

    Returns a real value. Avoids square roots for exact arithmetic. -/
def magnitudeSquared [Add α] [Mul α] (z : Complex α) : α :=
  z.re * z.re + z.im * z.im

-- ── Proofs ─────────────────────────────────────

/-- Conjugation is an involution: $\overline{\overline{z}} = z$.
    $$\overline{\overline{a + bi}} = \overline{a - bi} = a + bi$$

    Requires that negation is involutive: $-(-x) = x$.
-/
theorem conjugate_conjugate [Neg α] (hnn : ∀ x : α, -(-x) = x) (z : Complex α) :
    conjugate (conjugate z) = z := by
  simp [conjugate, hnn]

/-- Addition is commutative: $(z_1 + z_2) = (z_2 + z_1)$.

    Requires that the underlying type has commutative addition.
-/
theorem add_comm' [Add α] (hc : ∀ a b : α, a + b = b + a) (z w : Complex α) :
    z + w = w + z := by
  simp [HAdd.hAdd, Add.add]
  exact ⟨hc z.re w.re, hc z.im w.im⟩

end Complex
end Data
