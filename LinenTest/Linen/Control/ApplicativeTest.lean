/-
  Tests for `Linen.Control.Applicative`.

  Covers `asum`, the only `Alternative` combinator not already in Lean core.
-/
import Linen.Control.Applicative

open Control.Applicative

namespace Tests.Control.Applicative

-- `asum` returns the first successful alternative.
#guard asum [none, some 1, some 2] == some 1
#guard asum [some 7] == some 7

-- An empty list (and a list of all failures) folds to `failure`.
#guard asum ([] : List (Option Nat)) == none
#guard asum [none, none] == (none : Option Nat)

end Tests.Control.Applicative
