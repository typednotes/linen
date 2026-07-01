/-
  PostgREST.SchemaCache.Representations — Type representations

  Types for PostgreSQL type representations (casts) used for output
  formatting and input parsing.

  ## Haskell source
  - `PostgREST.SchemaCache.Representations` (postgrest package)
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.SchemaCache

open PostgREST.SchemaCache.Identifiers

/-- A representation (output cast) for a PostgreSQL type. -/
structure Representation where
  /-- The source type OID. -/
  repSourceType : String
  /-- The target type name. -/
  repTargetType : String
  /-- The function that performs the conversion. -/
  repFunction : QualifiedIdentifier
  deriving BEq, Repr

/-- A media handler for custom content-type output. -/
structure MediaHandler where
  /-- The qualified name of the handler function. -/
  mhFunction : QualifiedIdentifier
  /-- The media type this handler produces. -/
  mhMediaType : String
  /-- The source type. -/
  mhSourceType : String
  deriving BEq, Repr

end PostgREST.SchemaCache
