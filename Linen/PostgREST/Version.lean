/-
  PostgREST.Version — PostgREST version constant

  ## Haskell source
  - `PostgREST.Version` (postgrest package)
-/

namespace PostgREST.Version

/-- The version of this PostgREST port. -/
def version : String := "12.2.0-linen"

/-- The version string for display. -/
def prettyVersion : String := s!"PostgREST {version} (Linen/Lean 4 port)"

end PostgREST.Version
