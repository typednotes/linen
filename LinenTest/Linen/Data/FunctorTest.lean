/-
  Tests for `Linen.Data.Functor`.

  Covers the functor constructions that are not in the Lean standard library:
  `Compose`, `Const`, `Product`, `FunctorSum`, and `Contravariant` (with its
  `Predicate`/`Equivalence` instances).

  Decidable checks use `#guard`; the `Prop`-valued contravariant laws are
  illustrated with `example ... := rfl`.
-/
import Linen.Data.Functor

open Data.Functor

namespace Tests.Functor

-- ── Compose ───────────────────────────────────────────────────────────

-- `Compose Option List` maps through both layers.
#guard ((· + 1) <$> (⟨some [1, 2, 3]⟩ : Compose Option List Nat)).getCompose
        == some [2, 3, 4]
#guard ((· + 1) <$> (⟨none⟩ : Compose Option List Nat)).getCompose == none

-- `pure` wraps in both layers; `<*>` applies through both.
-- (`List` has no core `Applicative`, so the applicative tests use `Option`.)
#guard (pure 7 : Compose Option Option Nat).getCompose == some (some 7)
#guard (Seq.seq (⟨some (some (· + 1))⟩ : Compose Option Option (Nat → Nat))
          (fun () => (⟨some (some 10)⟩ : Compose Option Option Nat))).getCompose
        == some (some 11)

-- ── Const ─────────────────────────────────────────────────────────────

-- Mapping over the phantom parameter is a no-op on the carried value.
#guard ((fun b => !b) <$> (⟨42⟩ : Const Nat Bool)).getConst == 42
#guard ((⟨5⟩ : Const Nat Bool) == ⟨5⟩)
#guard !((⟨5⟩ : Const Nat Bool) == ⟨6⟩)

-- `pure` yields the monoidal identity (`default`).
#guard (pure 99 : Const String Nat).getConst == ""

-- ── Product ───────────────────────────────────────────────────────────

-- Mapping a product maps both components independently.
#guard ((· + 1) <$> (⟨(some 1, [2, 3])⟩ : Product Option List Nat)).runProduct
        == (some 2, [3, 4])
#guard ((⟨(some 1, [2])⟩ : Product Option List Nat) == ⟨(some 1, [2])⟩)

-- ── FunctorSum ────────────────────────────────────────────────────────

-- Mapping a sum maps whichever branch is present.
#guard (match ((· + 1) <$> (.inl (some 1) : FunctorSum Option List Nat)) with
        | .inl a => a == some 2
        | .inr _ => false)
#guard (match ((· + 1) <$> (.inr [1, 2] : FunctorSum Option List Nat)) with
        | .inl _ => false
        | .inr b => b == [2, 3])

-- ── Contravariant ─────────────────────────────────────────────────────

-- `contramap` precomposes a `Predicate`.
example :
    (Contravariant.contramap (· + 1) (⟨fun n => n = 0⟩ : Predicate Nat)).getPredicate 5
      = ((6 : Nat) = 0) := rfl

-- `Predicate` satisfies the contravariant identity law.
example (p : Predicate Nat) : Contravariant.contramap id p = p := rfl

-- `contramap` on an `Equivalence` relates pre-images.
example (e : Equivalence Nat) (a b : Nat) :
    (Contravariant.contramap (· + 1) e).getEquivalence a b
      = e.getEquivalence (a + 1) (b + 1) := rfl

end Tests.Functor
