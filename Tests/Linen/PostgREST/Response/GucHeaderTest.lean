/-
  Tests for `Linen.PostgREST.Response.GucHeader`.
-/
import Linen.PostgREST.Response.GucHeader

open PostgREST.Response.GucHeader

namespace Tests.PostgREST.Response.GucHeader

/-! ### Constants -/

#guard gucHeaderPrefix == "response.headers"
#guard gucStatusVar == "response.status"

/-! ### `parseGucHeaders` -/

-- The upstream parser is a documented stub (real JSON parsing is not yet
-- wired in): both well-formed and malformed input currently yield `[]`.
#guard parseGucHeaders "[{\"X-Foo\": \"bar\"}]" == []
#guard parseGucHeaders "not json" == []
#guard parseGucHeaders "" == []

/-! ### `parseGucStatus` -/

#guard parseGucStatus "200" == some 200
#guard parseGucStatus "  404  " == some 404
#guard parseGucStatus "abc" == none
#guard parseGucStatus "" == none

end Tests.PostgREST.Response.GucHeader
