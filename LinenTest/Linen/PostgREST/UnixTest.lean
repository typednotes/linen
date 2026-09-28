/-
  Tests for `Linen.PostgREST.Unix`.
-/
import Linen.PostgREST.Unix

open PostgREST.Unix

namespace Tests.PostgREST.Unix

#guard defaultSocketMode == 0o660
#guard defaultSocketMode == 432

end Tests.PostgREST.Unix
