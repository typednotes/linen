/-
  PostgREST.Unix — Unix socket and signal handling

  ## Haskell source
  - `PostgREST.Unix` (postgrest package)
-/

namespace PostgREST.Unix

/-- Unix socket file permissions. -/
def defaultSocketMode : Nat := 0o660

end PostgREST.Unix
