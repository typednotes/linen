/-
  Tests for `Linen.PostgREST.Config.PgVersion`.
-/
import Linen.PostgREST.Config.PgVersion

open PostgREST.Config

namespace Tests.PostgREST.Config.PgVersion

/-! ### `ToString` -/

#guard toString ({ pgvMajor := 15, pgvMinor := 0, pgvPatch := 4 } : PGVersion) == "15.0.4"
#guard toString (default : PGVersion) == "0.0.0"

/-! ### `Ord` -/

#guard compare ({ pgvMajor := 15, pgvMinor := 0 } : PGVersion) { pgvMajor := 14, pgvMinor := 9 } == .gt
#guard compare ({ pgvMajor := 14, pgvMinor := 0 } : PGVersion) { pgvMajor := 14, pgvMinor := 1 } == .lt
#guard compare ({ pgvMajor := 14, pgvMinor := 0, pgvPatch := 4 } : PGVersion)
  { pgvMajor := 14, pgvMinor := 0, pgvPatch := 4 } == .eq
#guard compare ({ pgvMajor := 14, pgvMinor := 0, pgvPatch := 1 } : PGVersion)
  { pgvMajor := 14, pgvMinor := 0, pgvPatch := 4 } == .lt

/-! ### `fromVersionNum` / `toVersionNum` -/

#guard PGVersion.fromVersionNum 150004 == { pgvMajor := 15, pgvMinor := 0, pgvPatch := 4 }
#guard PGVersion.fromVersionNum 140009 == { pgvMajor := 14, pgvMinor := 0, pgvPatch := 9 }
#guard PGVersion.fromVersionNum 90600 == { pgvMajor := 9, pgvMinor := 6, pgvPatch := 0 }

#guard ({ pgvMajor := 15, pgvMinor := 0, pgvPatch := 4 } : PGVersion).toVersionNum == 150004
#guard (PGVersion.fromVersionNum 90600).toVersionNum == 90600

/-! ### `parse` -/

#guard PGVersion.parse "15.0.4" == some { pgvMajor := 15, pgvMinor := 0, pgvPatch := 4 }
#guard PGVersion.parse "14.9" == some { pgvMajor := 14, pgvMinor := 9 }
#guard PGVersion.parse "13" == some { pgvMajor := 13, pgvMinor := 0 }
#guard PGVersion.parse "" == none
#guard PGVersion.parse "1.2.3.4" == none
#guard PGVersion.parse "a.b.c" == none

/-! ### Version checks -/

#guard pgVersionMin == { pgvMajor := 9, pgvMinor := 6 }
#guard ({ pgvMajor := 15, pgvMinor := 0 } : PGVersion).isSupported == true
#guard ({ pgvMajor := 9, pgvMinor := 6 } : PGVersion).isSupported == true
#guard ({ pgvMajor := 9, pgvMinor := 5 } : PGVersion).isSupported == false
#guard ({ pgvMajor := 8, pgvMinor := 0 } : PGVersion).isSupported == false

#guard ({ pgvMajor := 15, pgvMinor := 0 } : PGVersion).isAtLeastMajor 14 == true
#guard ({ pgvMajor := 15, pgvMinor := 0 } : PGVersion).isAtLeastMajor 16 == false

#guard ({ pgvMajor := 15, pgvMinor := 2 } : PGVersion).isAtLeast 15 1 == true
#guard ({ pgvMajor := 15, pgvMinor := 0 } : PGVersion).isAtLeast 15 1 == false
#guard ({ pgvMajor := 16, pgvMinor := 0 } : PGVersion).isAtLeast 15 1 == true

end Tests.PostgREST.Config.PgVersion
