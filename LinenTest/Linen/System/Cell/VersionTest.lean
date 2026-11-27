/- SemVer parsing, precedence, and release selection examples. -/
import Linen.System.Cell.Version

namespace Tests.System.Cell.Version

open _root_.System.Cell
open Lean (toJson fromJson?)

-- S6: Accept canonical SemVer, preserving the exact identifiers.
#guard (["0.0.0", "123456789012345678901.2.3", "1.0.0-alpha.0+build.007",
    "1.2.3-x-y-z.--", "1.0.0+007.RC"].all fun s =>
  ((Version.parse s).toOption.map toString) == some s)
#guard (["", "v1.0.0", "1.2", "1.2.3.4", "01.2.3", "1.02.3", "1.2.03", " 1.2.3",
    "1.2.3 ", "1.2.3-", "1.2.3+", "1.2.3-alpha..1", "1.2.3-01", "1.2.3+build..0",
    "1.2.3+hello+world", "1.2.3-β", "1.2.3-some_value"].all fun s => !(Version.parse s).isOk)
#guard ((Version.parse "1.0.0-beta.1+001" >>= fun v => fromJson? (toJson v)).toOption : Option Version) ==
  (Version.parse "1.0.0-beta.1+001").toOption

example (_version : Version) : True := by
  fail_if_success
    have _ : Version := { _version with buildMetadata := ["invalid metadata"] }
  trivial

private def precedence (left right : String) : Option Ordering := do
  return (← (Version.parse left).toOption).precedence (← (Version.parse right).toOption)

-- S6: Numeric, string, and list-length precedence follow SemVer.
private def sequence := ["1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha.beta",
  "1.0.0-beta", "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1", "1.0.0", "1.0.1", "1.1.0", "2.0.0"]

#guard (sequence.zip (sequence.drop 1)).all fun (a, b) =>
  precedence a b == some .lt && precedence b a == some .gt
#guard precedence "1.0.0-12" "1.0.0--" == some .lt
#guard precedence "1.0.0-Beta" "1.0.0-beta" == some .lt
#guard precedence "1.0.0+build.1" "1.0.0+build.2" == some .eq
#guard (Version.parse "1.0.0+build.1").toOption != (Version.parse "1.0.0+build.2").toOption

#guard (ReleaseName.parse "math.addition-v2").isOk
#guard (["", "Math.addition", "math..addition", "math.addition@", "math/addition",
    "1add", ".math", "math.", String.ofList (List.replicate 129 'a')].all fun s =>
  !(ReleaseName.parse s).isOk)

private def satisfies (req version : String) : Bool :=
  ((VersionRequirement.parse req).toOption.any fun r => (Version.parse version).toOption.any r.matches)

-- S7: A stable compatible range uses numeric bounds and excludes prereleases.
#guard satisfies "^1.2.3" "1.10.0+build.001"
#guard satisfies "^1.2.3" "1.2.3"
#guard !satisfies "^1.2.3" "1.2.2"
#guard !satisfies "^1.2.3" "2.0.0"
#guard !satisfies "^1.2.3" "1.3.0-alpha"
#guard satisfies "^0.2.3" "0.2.4"
#guard !satisfies "^0.2.3" "0.3.0"
#guard satisfies "^0.0.3" "0.0.3"
#guard !satisfies "^0.0.3" "0.0.4"
#guard satisfies "1.0.0-rc.1+build.01" "1.0.0-rc.1+build.01"
#guard !satisfies "1.0.0-rc.1+build.01" "1.0.0-rc.1+build.02"
#guard (["^1.2", "^1.2.3-rc.1", "^1.2.3+meta"].all fun s => !(VersionRequirement.parse s).isOk)

end Tests.System.Cell.Version
