/-
  Tests for `Linen.CDP.Domains.DeviceOrientation`.
-/
import Linen.CDP.Domains.DeviceOrientation

open CDP.Domains.DeviceOrientation
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.DeviceOrientation

#guard encode (ToJSON.toJSON ({} : PClearDeviceOrientationOverride)) = "null"
#guard Command.commandName ({} : PClearDeviceOrientationOverride)
  = "DeviceOrientation.clearDeviceOrientationOverride"

#guard Command.commandName ({ alpha := 1, beta := 2, gamma := 3 } : PSetDeviceOrientationOverride)
  = "DeviceOrientation.setDeviceOrientationOverride"
#guard encode (ToJSON.toJSON ({ alpha := 1, beta := 2, gamma := 3 } : PSetDeviceOrientationOverride))
  = "{\"alpha\":1,\"beta\":2,\"gamma\":3}"

end Tests.CDP.Domains.DeviceOrientation
