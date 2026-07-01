/-
  Tests for `Linen.PostgREST.Config.JSPath`.
-/
import Linen.PostgREST.Config.JSPath

open PostgREST.Config

namespace Tests.PostgREST.Config.JSPath

/-! ### `ToString` -/

#guard toString (JSPathSegment.key "role") == "role"
#guard toString (JSPathSegment.index 3) == "[3]"
#guard toString ({ segments := [.key "user", .key "role"] } : JSPath) == ".user.role"
#guard toString ({ segments := [] } : JSPath) == "."

/-! ### `isEmpty` / `depth` -/

#guard ({ segments := [] } : JSPath).isEmpty == true
#guard ({ segments := [.key "role"] } : JSPath).isEmpty == false
#guard ({ segments := [.key "user", .index 0, .key "name"] } : JSPath).depth == 3

/-! ### `parse` -/

#guard JSPath.parse ".role" == { segments := [.key "role"] }
#guard JSPath.parse ".user.permissions" == { segments := [.key "user", .key "permissions"] }
#guard JSPath.parse ".items.0.name" ==
  { segments := [.key "items", .index 0, .key "name"] }
#guard JSPath.parse "role" == { segments := [.key "role"] }
#guard JSPath.parse "." == { segments := [] }
#guard JSPath.parse "" == { segments := [] }

/-! ### `follow` -/

#guard (JSPath.parse ".role").follow [("role", "admin")] == some "admin"
#guard (JSPath.parse ".role").follow [] == none
#guard (JSPath.parse ".user.role").follow [("role", "admin")] == none

/-! ### `followNested` -/

def userClaims : String → List (String × String)
  | "user" => [("role", "editor")]
  | _ => []

#guard (JSPath.parse ".role").followNested [("role", "admin")] userClaims == some "admin"
#guard (JSPath.parse ".user.role").followNested [("user", "1")] userClaims == some "editor"
#guard (JSPath.parse ".user.role").followNested [] userClaims == none
#guard (JSPath.parse ".user.permissions").followNested [("user", "1")] userClaims == none

/-! ### `defaultRoleClaimPath` -/

#guard defaultRoleClaimPath == { segments := [.key "role"] }
#guard toString defaultRoleClaimPath == ".role"

end Tests.PostgREST.Config.JSPath
