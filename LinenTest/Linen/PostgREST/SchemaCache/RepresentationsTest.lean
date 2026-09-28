/-
  Tests for `Linen.PostgREST.SchemaCache.Representations`.
-/
import Linen.PostgREST.SchemaCache.Representations

open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.SchemaCache.Representations

/-! ### `Representation` -/

def intToText : Representation :=
  { repSourceType := "int4"
    repTargetType := "text"
    repFunction := { qiSchema := "public", qiName := "int4_to_text" } }

#guard intToText.repSourceType == "int4"
#guard intToText.repTargetType == "text"
#guard intToText.repFunction == ({ qiSchema := "public", qiName := "int4_to_text" } : QualifiedIdentifier)

#guard intToText == intToText
#guard intToText != { intToText with repTargetType := "varchar" }

/-! ### `MediaHandler` -/

def csvHandler : MediaHandler :=
  { mhFunction := { qiSchema := "public", qiName := "to_csv" }
    mhMediaType := "text/csv"
    mhSourceType := "record" }

#guard csvHandler.mhMediaType == "text/csv"
#guard csvHandler.mhSourceType == "record"

#guard csvHandler == csvHandler
#guard csvHandler != { csvHandler with mhMediaType := "application/xml" }

end Tests.PostgREST.SchemaCache.Representations
