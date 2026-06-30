/-
  Tests for `Linen.Network.HTTP.Types.Version`.

  `HttpVersion` and its ordering are pure, so behaviour is checked with `#guard`.
-/
import Linen.Network.HTTP.Types.Version

open Network.HTTP.Types

namespace Tests.Network.HTTP.Types.Version

/-! ### Constants / fields / ToString -/

#guard http11.major == 1
#guard http11.minor == 1
#guard toString http09 == "HTTP/0.9"
#guard toString http11 == "HTTP/1.1"
#guard toString http20 == "HTTP/2.0"

/-! ### BEq -/

#guard http11 == ⟨1, 1⟩
#guard (http10 == http11) == false

/-! ### Ord — lexicographic (major, then minor) -/

#guard compare http10 http11 == Ordering.lt          -- same major, minor 0 < 1
#guard compare http11 http10 == Ordering.gt
#guard compare http11 http11 == Ordering.eq
#guard compare http11 http20 == Ordering.lt          -- major 1 < 2
#guard compare http20 http09 == Ordering.gt          -- major dominates minor

/-! ### Laws -/

example : http11.major = 1 ∧ http11.minor = 1 := http11_valid
example : http20.major = 2 ∧ http20.minor = 0 := http20_valid

end Tests.Network.HTTP.Types.Version
