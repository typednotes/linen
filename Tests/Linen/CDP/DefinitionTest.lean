/-
  Tests for `Linen.CDP.Definition`.
-/
import Linen.CDP.Definition
import Linen.Data.Json

open CDP.Definition
open Data.Json (FromJSON)
open Data.Json.Decode (decodeAs)

namespace Tests.CDP.Definition

/-! ### Items -/

#guard decodeAs "{\"type\": \"string\"}" (α := Items) = .ok { type := some "string", ref := none }
#guard decodeAs "{\"$ref\": \"Foo\"}" (α := Items) = .ok { type := none, ref := some "Foo" }
#guard decodeAs "{}" (α := Items) = .ok { type := none, ref := none }

/-! ### Property — required `name`, everything else optional/defaulted -/

#guard decodeAs "{\"name\": \"x\"}" (α := Property)
  = .ok { name := "x", items := none, experimental := false, type := none, enum := none
        , optional := false, ref := none, description := none, deprecated := false }
#guard decodeAs "{\"name\": \"x\", \"optional\": true, \"type\": \"integer\"}" (α := Property)
  = .ok { name := "x", items := none, experimental := false, type := some "integer", enum := none
        , optional := true, ref := none, description := none, deprecated := false }

/-! ### Command -/

#guard decodeAs "{\"name\": \"enable\"}" (α := Command)
  = .ok { name := "enable", experimental := false, returns := [], parameters := []
        , redirect := none, description := none, deprecated := false }
#guard (decodeAs
    "{\"name\": \"enable\", \"parameters\": [{\"name\": \"maxSize\", \"optional\": true}]}"
    (α := Command)).map (fun c => c.parameters.length) = .ok 1

/-! ### TypeDef (named `Type` upstream — reserved in Lean) -/

#guard (decodeAs "{\"id\": \"CacheId\", \"type\": \"string\"}" (α := TypeDef)).map
    (fun t => (t.id, t.type)) = .ok ("CacheId", "string")

/-! ### Event -/

#guard decodeAs "{\"name\": \"loadEventFired\"}" (α := Event)
  = .ok { name := "loadEventFired", experimental := false, parameters := [], description := none
        , deprecated := false }

/-! ### Domain -/

#guard (decodeAs "{\"domain\": \"Page\", \"commands\": []}" (α := Domain)).map
    (fun d => (d.domain, d.commands, d.dependencies)) = .ok ("Page", [], [])

/-! ### Version / TopLevel -/

#guard decodeAs "{\"major\": \"1\", \"minor\": \"3\"}" (α := Version) = .ok { major := "1", minor := "3" }

#guard (decodeAs
    "{\"version\": {\"major\": \"1\", \"minor\": \"3\"}, \"domains\": [{\"domain\": \"Page\", \"commands\": []}]}"
    (α := TopLevel)).map (fun t => (t.version.major, t.domains.length)) = .ok ("1", 1)

end Tests.CDP.Definition
