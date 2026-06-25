/-
  Tests for `Linen.Data.Bifunctor`.

  `bimap`/`mapFst`/`mapSnd` over `Prod`, `Sum`, and `Except`, plus the identity
  law via the `LawfulBifunctor` instances. (`Sum`/`Except` have no core `BEq`,
  so those results are inspected with `match`.)
-/
import Linen.Data.Bifunctor

open Data

namespace Tests.Data.Bifunctor

/-! ### Prod -/

#guard Bifunctor.bimap (· + 1) (· ++ "!") (1, "a") == (2, "a!")
#guard Bifunctor.mapFst (· + 1) (1, "a") == (2, "a")
#guard Bifunctor.mapSnd (· ++ "!") (1, "a") == (1, "a!")

/-! ### Sum -/

#guard (match Bifunctor.bimap (· + 1) (· ++ "!") (Sum.inl 1 : Nat ⊕ String) with
        | .inl n => n == 2  | .inr _ => false)
#guard (match Bifunctor.bimap (· + 1) (· ++ "!") (Sum.inr "a" : Nat ⊕ String) with
        | .inr s => s == "a!" | .inl _ => false)

/-! ### Except -/

#guard (match Bifunctor.bimap (· + 1) (· ++ "!") (Except.error 1 : Except Nat String) with
        | .error n => n == 2  | .ok _ => false)
#guard (match Bifunctor.bimap (· + 1) (· ++ "!") (Except.ok "a" : Except Nat String) with
        | .ok s => s == "a!" | .error _ => false)

/-! ### Identity law (compile-time) -/

example (p : Nat × String)        : Bifunctor.bimap id id p = p := LawfulBifunctor.bimap_id p
example (x : Nat ⊕ String)        : Bifunctor.bimap id id x = x := LawfulBifunctor.bimap_id x
example (x : Except Nat String)   : Bifunctor.bimap id id x = x := LawfulBifunctor.bimap_id x

end Tests.Data.Bifunctor
