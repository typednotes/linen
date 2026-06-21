/-
  Tests for `Linen.Control.Category`.

  `Fun` is the category of functions. Morphisms have no `BEq`, so behaviour is
  checked by applying them; the laws (which hold definitionally) are checked
  with `example ... := rfl`.
-/
import Linen.Control.Category

open Control

namespace Tests.Control.Category

-- identity morphism is the identity function
#guard (Category.id : Fun Nat Nat).apply 5 == 5

-- diagrammatic composition: first `+ 1`, then `* 2`
#guard (Category.comp (⟨(· + 1)⟩ : Fun Nat Nat) ⟨(· * 2)⟩).apply 3 == 8

-- the scoped `≫` operator agrees with `comp`
#guard ((⟨(· + 1)⟩ : Fun Nat Nat) ≫ ⟨(· * 2)⟩).apply 3 == 8

-- lawful category: identity and associativity laws hold definitionally
example (f : Fun Nat Nat) : Category.comp Category.id f = f := rfl
example (f : Fun Nat Nat) : Category.comp f Category.id = f := rfl
example (f g h : Fun Nat Nat) :
    Category.comp (Category.comp f g) h = Category.comp f (Category.comp g h) := rfl

end Tests.Control.Category
