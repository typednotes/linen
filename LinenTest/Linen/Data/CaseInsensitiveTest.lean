/-
  Tests for `Linen.Data.CaseInsensitive` — the case-insensitive `CI` wrapper.
-/
import Linen.Data.CaseInsensitive

open Data

namespace Tests.Data.CaseInsensitive

/-! ### FoldCase -/

#guard FoldCase.foldCase "HeLLo" == "hello"
#guard FoldCase.foldCase 'A' == 'a'

/-! ### mk' preserves original, computes folded -/

#guard (CI.mk' "Hello").original == "Hello"
#guard (CI.mk' "Hello").foldedCase == "hello"

/-! ### equality / ordering / hashing use the folded form -/

#guard (CI.mk' "Hello") == (CI.mk' "HELLO")
#guard (CI.mk' "Hello") == (CI.mk' "hello")
#guard ((CI.mk' "Hello") == (CI.mk' "World")) == false
#guard ((CI.mk' 'A') == (CI.mk' 'a'))
#guard compare (CI.mk' "abc") (CI.mk' "ABD") == Ordering.lt
#guard compare (CI.mk' "ABC") (CI.mk' "abc") == Ordering.eq
-- hashing is consistent with case-insensitive equality
#guard hash (CI.mk' "Hello") == hash (CI.mk' "HELLO")

/-! ### ToString / Repr preserve the original casing -/

#guard toString (CI.mk' "HeLLo") == "HeLLo"

/-! ### map (recomputes the folded form) -/

#guard ((CI.mk' "ab").map (· ++ "C")).original == "abC"
#guard ((CI.mk' "ab").map (· ++ "C")).foldedCase == "abc"
#guard ((CI.mk' "ab").map (· ++ "C")) == (CI.mk' "ABC")

/-! ### `==` is exactly folded-case comparison (compile-time) -/

example [FoldCase α] [BEq α] (a b : CI α) :
    (a == b) = (a.foldedCase == b.foldedCase) := CI.ci_eq_iff a b

end Tests.Data.CaseInsensitive
