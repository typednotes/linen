/-
  Tests for `Linen.PostgREST.Auth.Types`.
-/
import Linen.PostgREST.Auth.Types

open PostgREST.Auth

namespace Tests.PostgREST.Auth.Types

def anon : AuthResult :=
  { authRole := "anon", authRole_nonempty := by decide, authClaims := [("sub", "anon")] }

def admin : AuthResult :=
  { authRole := "admin", authRole_nonempty := by decide,
    authClaims := [("sub", "1"), ("role", "admin")] }

#guard anon.authRole == "anon"
#guard anon.lookupClaim "sub" == some "anon"
#guard anon.lookupClaim "missing" == none

#guard admin.lookupClaim "sub" == some "1"
#guard admin.lookupClaim "role" == some "admin"
#guard admin.authClaims.length == 2

example : anon.authRole.length > 0 := anon.authRole_nonempty

end Tests.PostgREST.Auth.Types
