/-
  Tests for `Linen.CDP.Domains.HeadlessExperimental`.
-/
import Linen.CDP.Domains.HeadlessExperimental

open CDP.Domains.HeadlessExperimental
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.HeadlessExperimental

/-! ### ScreenshotParams.Format -/

#guard decodeAs "\"jpeg\"" (α := ScreenshotParams.Format) = .ok .jpeg
#guard encode (ToJSON.toJSON ScreenshotParams.Format.png) = "\"png\""

/-! ### ScreenshotParams -/

#guard decodeAs "{}" (α := ScreenshotParams) = .ok { format := none, quality := none }
#guard decodeAs "{\"format\": \"jpeg\", \"quality\": 80}" (α := ScreenshotParams)
  = .ok { format := some .jpeg, quality := some 80 }
#guard encode (ToJSON.toJSON ({ format := some .jpeg, quality := some 80 } : ScreenshotParams))
  = "{\"format\":\"jpeg\",\"quality\":80}"

/-! ### PBeginFrame / BeginFrame -/

#guard encode (ToJSON.toJSON ({} : PBeginFrame)) = "{}"
#guard Command.commandName ({} : PBeginFrame) = "HeadlessExperimental.beginFrame"
#guard decodeAs "{\"hasDamage\": true}" (α := BeginFrame) = .ok { hasDamage := true, screenshotData := none }
#guard decodeAs "{\"hasDamage\": false, \"screenshotData\": \"abc\"}" (α := BeginFrame)
  = .ok { hasDamage := false, screenshotData := some "abc" }

/-! ### PDisable / PEnable -/

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PDisable) = "HeadlessExperimental.disable"
#guard Command.commandName ({} : PEnable) = "HeadlessExperimental.enable"

end Tests.CDP.Domains.HeadlessExperimental
