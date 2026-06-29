/-
  Tests for `Linen.Data.Configurator.Types`.

  Float rendering is platform-formatted, so numeric values are checked via
  `BEq` rather than asserting an exact `toString`.
-/
import Linen.Data.Configurator.Types

open Data.Configurator

namespace Tests.Data.Configurator.Types

/-! ### Value.toString -/

#guard (Value.string "hi").toString == "\"hi\""
#guard (Value.bool true).toString == "true"
#guard (Value.bool false).toString == "false"
#guard (Value.list [Value.bool true, Value.string "x"]).toString == "[true, \"x\"]"
#guard (Value.list []).toString == "[]"
#guard (Value.list [Value.list [Value.bool true], Value.bool false]).toString == "[[true], false]"
#guard toString (Value.bool true) == "true"   -- via the ToString instance

/-! ### Value BEq -/

#guard (Value.string "a" == Value.string "a")
#guard ((Value.string "a" == Value.bool true) == false)
#guard (Value.number 1.0 == Value.number 1.0)
#guard (Value.list [Value.number 1.0] == Value.list [Value.number 1.0])
#guard ((Value.list [Value.bool true] == Value.list [Value.bool false]) == false)

/-! ### Config (HashMap String Value) -/

#guard (Std.HashMap.ofList [("k", Value.bool true)] : Config).getD "k" (Value.string "?") == Value.bool true
#guard (Std.HashMap.ofList [("k", Value.bool true)] : Config).getD "absent" (Value.string "?") == Value.string "?"
#guard toString (Std.HashMap.ofList [("k", Value.bool true)] : Config) == "k = true"

end Tests.Data.Configurator.Types
