/-
  Tests for `Linen.PostgREST.Version`.
-/
import Linen.PostgREST.Version

open PostgREST.Version

namespace Tests.PostgREST.Version

#guard version == "12.2.0-linen"
#guard prettyVersion == "PostgREST 12.2.0-linen (Linen/Lean 4 port)"

end Tests.PostgREST.Version
