/-
  `PostgREST.Auth` — authentication result types

  Mirrors PostgREST's `PostgREST.Auth` module.
-/

namespace PostgREST.Auth

/-- The result of authenticating a request.
    $$\text{AuthResult} = \{ \text{role} : \{s : \text{String} \mid s.\text{length} > 0\},\;
      \text{claims} \}$$
    The role is proven non-empty: PostgreSQL requires a non-empty role name
    for `SET ROLE`. -/
structure AuthResult where
  /-- The PostgreSQL role to assume for this request. -/
  authRole : String
  /-- Proof that the role name is non-empty (PostgreSQL rejects empty role names). -/
  authRole_nonempty : authRole.length > 0
  /-- All JWT claims as key-value pairs (string representation). -/
  authClaims : List (String × String)
  deriving Repr

/-- Look up a specific claim value. -/
def AuthResult.lookupClaim (ar : AuthResult) (key : String) : Option String :=
  ar.authClaims.lookup key

end PostgREST.Auth
