/-
  Tests for `Linen.Network.WebApp.Server.Settings`.
-/
import Linen.Network.WebApp.Server.Settings

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.Settings

#guard defaultSettings.settingsPort == 3000
#guard defaultSettings.settingsHost == "0.0.0.0"
#guard defaultSettings.settingsServerName == "Linen/WebApp.Server"
#guard defaultSettings.settingsTimeout == 30
#guard defaultSettings.settingsBacklog == 128
#guard defaultSettings.settingsAddDateHeader == true
#guard defaultSettings.settingsAddServerHeader == true
#guard defaultSettings.settingsGracefulShutdownTimeout == none

example : defaultSettings.settingsTimeout > 0 ∧ defaultSettings.settingsBacklog > 0 :=
  defaultSettings_valid

#guard ({ defaultSettings with settingsPort := 8080 } : Settings).settingsPort == 8080

end Tests.Network.WebApp.Server.Settings
