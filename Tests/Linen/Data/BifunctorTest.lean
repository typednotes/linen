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

/-! ### bitraverse -/

#guard Bitraverse.bitraverse (G := Option) (fun n => if n > 0 then some (n + 1) else none)
  (fun s => if s.length > 0 then some (s ++ "!") else none) (1, "a") == some (2, "a!")
#guard Bitraverse.bitraverse (G := Option) (fun n => if n > 0 then some (n + 1) else none)
  (fun s => if s.length > 0 then some (s ++ "!") else none) (0, "a") == none

#guard (match Bitraverse.bitraverse (G := Option) (fun n => some (n + 1)) (fun s => some (s ++ "!"))
    (Sum.inl 1 : Nat ⊕ String) with
  | some (.inl n) => n == 2 | _ => false)
#guard (match Bitraverse.bitraverse (G := Option) (fun n => some (n + 1)) (fun s => some (s ++ "!"))
    (Sum.inr "a" : Nat ⊕ String) with
  | some (.inr s) => s == "a!" | _ => false)

#guard (match Bitraverse.bitraverse (G := Option) (fun n => some (n + 1)) (fun s => some (s ++ "!"))
    (Except.error 1 : Except Nat String) with
  | some (.error n) => n == 2 | _ => false)
#guard (match Bitraverse.bitraverse (G := Option) (fun n => some (n + 1)) (fun s => some (s ++ "!"))
    (Except.ok "a" : Except Nat String) with
  | some (.ok s) => s == "a!" | _ => false)

end Tests.Data.Bifunctor
