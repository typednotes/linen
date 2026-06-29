/-
  Tests for `Linen.Data.Configurator` — the `key = value` config parser/query.

  IO `load` is not exercised here; `parseConfig` (its pure core) is.
-/
import Linen.Data.Configurator

open Data.Configurator

namespace Tests.Data.Configurator

/-- Parse `content`, then look up `key` (`none` if parse fails or key absent). -/
private def parseLookup (content key : String) : Option Value :=
  match parseConfig content with
  | .ok cfg => lookup key cfg
  | .error _ => none

/-! ### query on a parsed config -/

#guard parseLookup "debug = true" "debug" == some (Value.bool true)
#guard parseLookup "name = \"linen\"" "name" == some (Value.string "linen")
#guard parseLookup "port = 3000" "port" == some (Value.number (Float.ofNat 3000))
#guard parseLookup "x = 1.5" "x" == some (Value.number 1.5)
#guard parseLookup "x = -2.5" "x" == some (Value.number (-2.5))
#guard parseLookup "flag = false" "flag" == some (Value.bool false)
#guard parseLookup "a = 1" "missing" == none

/-! ### comments, blanks, dotted keys, quotes, '=' in values -/

#guard parseLookup "# a comment\n\ndb.host = \"localhost\"" "db.host" == some (Value.string "localhost")
#guard parseLookup "uri = \"postgres://u:p@h/db?x=1\"" "uri" == some (Value.string "postgres://u:p@h/db?x=1")
#guard parseLookup "  spaced  =  42  " "spaced" == some (Value.number (Float.ofNat 42))
-- escapes inside quoted strings
#guard parseLookup "msg = \"a\\tb\"" "msg" == some (Value.string "a\tb")

/-! ### lookupDefault / require -/

#guard (match parseConfig "k = true" with
        | .ok c => lookupDefault (Value.string "?") "absent" c
        | .error _ => Value.string "ERR") == Value.string "?"
#guard (match parseConfig "k = true" with
        | .ok c => (require "k" c).toOption
        | .error _ => none) == some (Value.bool true)
#guard (match parseConfig "k = true" with
        | .ok c => (require "absent" c).toOption
        | .error _ => none) == none

/-! ### parse errors -/

#guard (parseConfig "novalue").toOption.isNone          -- missing '='
#guard (parseConfig "= 5").toOption.isNone               -- empty key
#guard (parseConfig "k = ").toOption.isNone              -- empty value
#guard (parseConfig "k = \"unterminated").toOption.isNone

/-! ### multi-line config -/

#guard parseLookup "# config\nhost = \"h\"\nport = 8080\ntls = true\n" "port"
        == some (Value.number (Float.ofNat 8080))
#guard parseLookup "# config\nhost = \"h\"\nport = 8080\ntls = true\n" "tls" == some (Value.bool true)

/-! ### empty config -/

#guard lookup "anything" empty == none
#guard (parseConfig "").toOption.isSome == true

end Tests.Data.Configurator
