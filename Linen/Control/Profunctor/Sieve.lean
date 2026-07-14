/-
  Linen.Control.Profunctor.Sieve — the `Sieve`/`Cosieve` typeclasses

  Port of Hackage's `profunctors-5.6.3`'s `Data.Profunctor.Sieve` (module #5
  of `docs/imports/profunctors/dependencies.md`). A profunctor `P` is a
  `Sieve` on `F` if it is a subprofunctor of `Star F` — i.e. `sieve : P a b →
  a → F b`. Upstream expresses "`F` is determined by `P`" with a functional
  dependency (`p -> f`); this port uses Lean's `outParam` for the same
  effect, so `[Sieve P F]` resolves `F` from `P` alone during instance
  search.
-/

import Linen.Control.Profunctor.Unsafe
import Linen.Control.Profunctor.Types
import Linen.Data.Functor

open Control
open Data.Functor (Const)

namespace Control.Profunctor

/-- `P` is a `Sieve` **on** `F` if it is a subprofunctor of `Star F`: a
    subset of $\text{Hom}(-, F=)$ closed under `lmap`/`rmap`. -/
class Sieve (P : Type u → Type u → Type v) (F : outParam (Type u → Type v)) [Profunctor P] [Functor F] where
  /-- Run the sieve: $\text{sieve} : P\,a\,b \to a \to F\,b$. -/
  sieve : P α β → α → F β

/-- Ordinary functions are a `Sieve` on `Id`: $\text{sieve}\;f = \text{pure} \circ f$. -/
instance : Sieve Control.Fun Id where
  sieve f a := f.apply a

/-- `Star F` is a `Sieve` on `F` itself, by definition. -/
instance [Functor F] : Sieve (Star F) F where
  sieve := Star.runStar

/-- `Forget R` is a `Sieve` on the constant functor `Const R`. -/
instance : Sieve (Forget R) (Const R) where
  sieve k a := ⟨k.runForget a⟩

/-- `P` is a `Cosieve` **on** `F` if it is a subprofunctor of `Costar F`: a
    subset of $\text{Hom}(F-, =)$ closed under `lmap`/`rmap`. -/
class Cosieve (P : Type u → Type u → Type v) (F : outParam (Type u → Type v)) [Profunctor P] [Functor F] where
  /-- Run the cosieve: $\text{cosieve} : P\,a\,b \to F\,a \to b$. -/
  cosieve : P α β → F α → β

/-- Ordinary functions are a `Cosieve` on `Id`: $\text{cosieve}\;f\;(\text{Id}\;d) = f\,d$. -/
instance : Cosieve Control.Fun Id where
  cosieve f a := f.apply a

/-- `Costar F` is a `Cosieve` on `F` itself, by definition. -/
instance [Functor F] : Cosieve (Costar F) F where
  cosieve := Costar.runCostar

end Control.Profunctor
